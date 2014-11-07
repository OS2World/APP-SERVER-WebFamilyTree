IMPLEMENTATION MODULE TransLog;

        (********************************************************)
        (*                                                      *)
        (*               Transaction logging                    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            16 March 1999                   *)
        (*  Last edited:        26 October 2004                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  ADR;

IMPORT Strings, OS2;

FROM MyClock IMPORT
    (* proc *)  CurrentTimeToString, AppendSyslogDateTimeString;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenAtEnd, FWriteString, FWriteLn, CloseFile;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, AddressFamily, SocketType, SockAddr,
    (* proc *)  socket, connect, getsockname, soclose, gethostid, send;

FROM Internet IMPORT
    (* const*)  Zero8;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, Swap4, AddressToHostName, ConvertCard;

FROM SplitScreen IMPORT
    (* proc *)  WriteString, WriteLn, WriteChar;

FROM Names IMPORT
    (* type *)  FilenameString, HostName;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  Copy;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release, CreateTask;

(************************************************************************)

CONST
    Nul = CHR(0);
    MaxLineLength = 256;

TYPE
    LogLinePtr = POINTER TO ARRAY [0..MaxLineLength-1] OF CHAR;
    TransactionLogID = POINTER TO ARRAY [0..127] OF CHAR;
    TargetType = (todisk, toscreen, topipe, tosyslog);

VAR
    (* Log ID for internal admin messages. *)

    DummyID: TransactionLogID;

    (* Log targets that are currently enabled. *)

    Target: ARRAY TargetType OF BOOLEAN;

    (* Critical section protection for the transaction log. *)

    TransactionLogLock: Lock;

    (* Flag to say that we are currently logging to at least one target. *)

    LoggingActive: BOOLEAN;

    (* Flag to say whether the transaction log file is currently open. *)

    InterimDiskFileOpen: BOOLEAN;

    (* Flag to say whether the log update task is running.  If TRUE, this       *)
    (* implies that StartTransactionLogging has been called at some stage with  *)
    (* level > 0; this is how StartTransactionLogging knows whether it is       *)
    (* being called for the first time, or whether it has to tidy up an         *)
    (* existing log file before starting another.                               *)

    UpdateTaskRunning: BOOLEAN;

    (* File handle for the interim transaction log. *)

    TransactionLogChannel: ChanId;

    (* Name of the transaction log file. *)

    TransactionLogName, InterimLogName: FilenameString;

    (* Name of the pipe for the case where we are logging to a pipe. *)

    PipeName: FilenameString;

    (* Socket for syslog logging. *)

    LogSocket: Socket;

    (* A tag (process name) for syslog messages. *)

    procname: ARRAY [0..31] OF CHAR;

    (* Host name for syslog messages. *)

    OurHostname: HostName;

    (* Facility code for syslog messages. *)

    Facility: CARDINAL;

    (* Flag to say that shutdown processing has commenced. *)

    ShuttingDown: BOOLEAN;

(********************************************************************************)
(*                               PIPE OPERATIONS                                *)
(********************************************************************************)

CONST
    PipeSize = 8192;
    PipeBufferSize = 32;

TYPE
    BufferIndex = [0..PipeBufferSize-1];
    StrDesc = RECORD
                  addr: LogLinePtr;
                  length: CARDINAL;
              END (*RECORD*);

VAR
    Pipe: RECORD
              access: Lock;
              handle: OS2.HPIPE;
              InIndex, OutIndex: BufferIndex;
              stuck: CARDINAL;
              open: BOOLEAN;
              count: Semaphore;
              Buffer: ARRAY BufferIndex OF StrDesc;
          END (*RECORD*);

    PipeOpenRequest: Semaphore;

    CRLF: ARRAY [0..1] OF CHAR;

(********************************************************************************)

PROCEDURE PipeTask;

    (* Feeds data from the pipe circular buffer to the pipe itself. *)

    VAR rc, actual: CARDINAL;
        success: BOOLEAN;
        towrite: StrDesc;

    BEGIN
        REPEAT
            Wait (PipeOpenRequest);

            (* Open the pipe.  We don't want critical section protection here   *)
            (* because the DosConnectNPipe request below could block all        *)
            (* logging if we still held Pipe.access; and anyway the protection  *)
            (* is not needed because nobody else will try to use the pipe       *)
            (* until we set Pipe.open = TRUE.                                   *)

            rc := OS2.DosCreateNPipe (PipeName, Pipe.handle, OS2.NP_ACCESS_OUTBOUND,
                                      (*OS2.NP_NOWAIT*) + 1, PipeSize, 0, 0);
            success := rc = 0;
            IF success THEN
                rc := OS2.DosConnectNPipe (Pipe.handle);
                success := (rc = 0) OR (rc = 233);
            END (*IF*);
            Pipe.open := success;
            Target[topipe] := success;

            (* Transfer data while we want the pipe open, or until the pipe is  *)
            (* closed at the client end.  If the pipe is closed at the other    *)
            (* end, we'll close it and then attempt to reopen it, in case the   *)
            (* client program is restarted.                                     *)

            rc := 0;
            WHILE Target[topipe] AND (rc = 0) DO

                WITH Pipe DO
                    Wait (count);
                    Obtain (access);
                    towrite := Buffer[OutIndex];
                    IF towrite.addr <> NIL THEN
                        WITH Buffer[OutIndex] DO
                            addr := NIL;
                            length := 0;
                        END (*WITH*);
                        OutIndex := (OutIndex + 1) MOD PipeBufferSize;
                    END (*IF*);
                    WHILE stuck > 0 DO
                        Signal (count);  DEC(stuck);
                    END (*WHILE*);
                    Release (access);
                END (*WITH*);
                rc := 0;
                WITH towrite DO
                    IF addr <> NIL THEN
                        IF length > 1 THEN
                            rc := OS2.DosWrite (Pipe.handle, addr, length-1, actual);
                        END (*IF*);
                        IF rc = 0 THEN
                            rc := OS2.DosWrite (Pipe.handle, ADR(CRLF), 2, actual);
                        END (*IF*);
                        DEALLOCATE (addr, length);
                        IF rc = OS2.ERROR_BROKEN_PIPE THEN
                            Signal (PipeOpenRequest);
                        END (*IF*);
                    END (*IF*);
                END (*WITH*);

            END (*WHILE*);

            (* Close the pipe when Target[topipe] becomes FALSE *)
            (* or when a write fails.                           *)

            Obtain (Pipe.access);
            Pipe.open := FALSE;
            OS2.DosDisConnectNPipe (Pipe.handle);
            CloseFile (Pipe.handle);
            Release (Pipe.access);

        UNTIL ShuttingDown;

    END PipeTask;

(********************************************************************************)

PROCEDURE InitialisePipe;

    (* Creates the data structure associated with the pipe. *)

    VAR j: BufferIndex;

    BEGIN
        CreateLock (Pipe.access);
        FOR j := 0 TO MAX(BufferIndex) DO
            Pipe.Buffer[j].addr := NIL;
            Pipe.Buffer[j].length := 0;
        END (*FOR*);
        Pipe.InIndex := 0;
        Pipe.OutIndex := 0;
        Pipe.stuck := 0;
        Pipe.open := FALSE;
        CreateSemaphore (Pipe.count, 0);
        CreateSemaphore (PipeOpenRequest, 0);
        CreateTask (PipeTask, 5, "pipe output");
    END InitialisePipe;

(********************************************************************************)

PROCEDURE CopyToPipe (ptext: LogLinePtr);

    (* Adds one entry to the queue of items waiting to be sent to the pipe. *)

    VAR overflow: BOOLEAN;

    BEGIN
        WITH Pipe DO
            Obtain (access);
            IF open THEN
                WITH Buffer[InIndex] DO
                    overflow := addr <> NIL;
                    IF overflow THEN
                        INC (stuck);
                    ELSE
                        length := Strings.Length(ptext^) + 1;
                        ALLOCATE (addr, length);
                        IF length > 1 THEN
                            Copy (ptext, addr, length-1);
                        END (*IF*);
                        addr^[length-1] := Nul;
                        InIndex := (InIndex + 1) MOD PipeBufferSize;
                        Signal (count);
                    END (*IF*);
                END (*WITH*);
            END (*IF*);
            Release (access);
        END (*WITH*);
    END CopyToPipe;

(************************************************************************)
(*                      SYSLOG SOCKET OPERATIONS                        *)
(************************************************************************)

PROCEDURE GetOurHostName (S: Socket);

    (* Sets OurHostname to what we are currently calling our host name  *)
    (* for our end of the connection using socket S.  If we can't get a *)
    (* reasonable answer then we use the IP address.  If we do get a    *)
    (* textual hostname then we discard the part after the first '.'.   *)

    VAR myaddr: SockAddr;  size: CARDINAL;  found: BOOLEAN;

    BEGIN
        size := SIZE(myaddr);
        IF NOT getsockname (S, myaddr, size) THEN
            AddressToHostName (myaddr.in_addr.addr, OurHostname);
        END (*IF*);
        IF OurHostname[0] <> '[' THEN
            Strings.FindNext ('.', OurHostname, 0, found, size);
            IF found THEN
                OurHostname[size] := Nul;
            END (*IF*);
        END (*IF*);
    END GetOurHostName;

(************************************************************************)

PROCEDURE StartSyslogging;

    (* Starts logging to syslog, by opening and connecting the necessary   *)
    (* socket.  If this operation fails, we clear Target[tosyslog].  Even  *)
    (* though this is a UDP connection, the connect() saves us the trouble *)
    (* of having to specify an address for each transfer.                  *)

    CONST loopback = 256*256*256*127 + 1;

    VAR addr: SockAddr;

    BEGIN
        LogSocket := socket (AF_INET, SOCK_DGRAM, AF_UNSPEC);

        (* Connect to the syslog service on local machine. *)
        (* Port 514 is as specified in RFC3164.            *)

        WITH addr DO
            family := AF_INET;
            WITH in_addr DO
                port := Swap2 (514);
                addr := Swap4 (loopback);
                zero := Zero8;
            END (*WITH*);
        END (*WITH*);

        Target[tosyslog] := (LogSocket <> NotASocket)
                            AND NOT connect (LogSocket, addr, SIZE(addr));
        GetOurHostName (LogSocket);

    END StartSyslogging;

(************************************************************************)

PROCEDURE StopSyslogging;

    (* Closes logging to syslog. *)

    BEGIN
        soclose (LogSocket);
        LogSocket := NotASocket;
    END StopSyslogging;

(************************************************************************)

PROCEDURE CopyToSyslog (ptext: LogLinePtr);

    (* Sends this line to syslog, with an appropriate header. *)

    VAR message: ARRAY [0..1023] OF CHAR;
        pos: CARDINAL;

    BEGIN
        message := "<";  pos := 1;
        ConvertCard (8*Facility + 6, message, pos);
        message[pos] := '>';  INC(pos);
        message[pos] := Nul;
        AppendSyslogDateTimeString (message);
        Strings.Append (' ', message);
        Strings.Append (OurHostname, message);
        Strings.Append (' ', message);
        Strings.Append (procname, message);
        Strings.Append (' ', message);
        Strings.Append (ptext^, message);
        send (LogSocket, message, LENGTH(message), 0);

    END CopyToSyslog;

(************************************************************************)
(*           COPYING INTERIM DISK FILE TO PERMANENT LOG FILE            *)
(************************************************************************)

PROCEDURE CopyTransactionLogUpdates;

    (* Appends all the data from the interim transaction log to the final       *)
    (* transaction log, then deletes the interim transaction log.  We assume    *)
    (* that caller has obtained the TransactionLogLock.                         *)

    BEGIN
        IF InterimDiskFileOpen THEN
            CloseFile (TransactionLogChannel);
            InterimDiskFileOpen := FALSE;
            OS2.DosCopy (InterimLogName, TransactionLogName, OS2.DCPY_APPEND);
            OS2.DosDelete (InterimLogName);
        END (*IF*);
    END CopyTransactionLogUpdates;

(************************************************************************)

PROCEDURE TransactionLogUpdateTask;

    (* A separate task that updates the transaction log approximately *)
    (* every 15 minutes.                                              *)

    BEGIN
        WHILE NOT ShuttingDown DO
            Sleep (1000000);
            Obtain (TransactionLogLock);
            CopyTransactionLogUpdates;
            Release (TransactionLogLock);
        END (*WHILE*);
    END TransactionLogUpdateTask;

(************************************************************************)
(*                       ADDING A NEW LOG ENTRY                         *)
(************************************************************************)

PROCEDURE AddToTransactionLog (ptext: LogLinePtr);

    (* Writes the text, preceded by the date and time, to the screen    *)
    (* and/or the transaction log and/or the pipe, depending on Target. *)

    VAR pLogLine: LogLinePtr;

    BEGIN
        IF LoggingActive THEN

            (* Create a string containing date/time and the text.  *)

            NEW (pLogLine);
            CurrentTimeToString (pLogLine^);
            Strings.Append (" ", pLogLine^);
            Strings.Append (ptext^, pLogLine^);

            (* Write to disk if Target[todisk) is TRUE. *)

            Obtain (TransactionLogLock);
            IF Target[todisk] THEN
                IF NOT InterimDiskFileOpen THEN
                    TransactionLogChannel := OpenAtEnd (InterimLogName);
                    InterimDiskFileOpen := TRUE;
                END (*IF*);
                FWriteString (TransactionLogChannel, pLogLine^);
                FWriteLn (TransactionLogChannel);
            END (*IF*);

            (* Write to the screen if Target[toscreen] is TRUE. *)

            IF Target[toscreen] THEN
                WriteString (pLogLine^);  WriteLn;
            END (*IF*);

            (* Do we also want to write to the pipe? *)

            IF Target[topipe] THEN
                CopyToPipe (pLogLine);
            END (*IF*);

            (* Write to the syslog socket if that option is active. *)

            IF Target[tosyslog] THEN
                CopyToSyslog (pLogLine);
            END (*IF*);

            Release (TransactionLogLock);

            DISPOSE (pLogLine);

        END (*IF*);

    END AddToTransactionLog;

(************************************************************************)
(*                     OTHER EXPORTED PROCEDURES                        *)
(************************************************************************)

PROCEDURE SetProcname (name: ARRAY OF CHAR;  facility: CARDINAL);

    (* Sets process name and facility number for use in syslog messages *)
    (* and for making the pipe name if we use a pipe.  Must be called   *)
    (* before StartTransactionLogging if you plan to use syslog and/or  *)
    (* a pipe for the log messages.                                     *)

    BEGIN
        Strings.Assign (name, procname);
        Strings.Assign ("\\PIPE\\", PipeName);
        Strings.Append (name, PipeName);
        Strings.Append ("TransLog", PipeName);
        Facility := facility;
    END SetProcname;

(************************************************************************)

PROCEDURE StartTransactionLogging (LogfileName: ARRAY OF CHAR;  level: CARDINAL);

    (* Sets the transaction log file name, and enables logging.  The    *)
    (* level parameter means: 0 for none, 1 for disk, 2 for screen, 4   *)
    (* for pipe, 8 for syslog, and sums of these for multiple log       *)
    (* targets.  This can be called more than once, to change the log   *)
    (* file name or level.  On a second or later call the existing log  *)
    (* file is closed and the next log entry will cause it to be        *)
    (* reopened, possibly as a new file with a new name.  Similarly,    *)
    (* the pipe and syslog socket will be opened or closed if the       *)
    (* change in level requires this.                                   *)

    VAR patternFound, PipeWasOpen, SyslogWasActive: BOOLEAN;
        posOfPattern: CARDINAL;
        t: TargetType;

    BEGIN
        Obtain (TransactionLogLock);

        (* Close out the interim disk file, if it is open. *)

        IF UpdateTaskRunning THEN
            CopyTransactionLogUpdates;
        END (*IF*);

        (* Store the new file name, and work out a new interim file name. *)

        Strings.Assign (LogfileName, TransactionLogName);
        Strings.FindPrev ('.', LogfileName, LENGTH(LogfileName) - 1,
                                       patternFound, posOfPattern);
        IF patternFound THEN
            LogfileName[posOfPattern] := Nul;
        END (*IF*);
        Strings.Append (".$$$", LogfileName);
        Strings.Assign (LogfileName, InterimLogName);

        (* Where do we want the logs to go to now? *)

        level := level MOD 16;
        LoggingActive := FALSE;
        PipeWasOpen := Target[topipe];
        SyslogWasActive := Target[tosyslog];
        FOR t := MIN(TargetType) TO MAX(TargetType) DO
            Target[t] := ODD(level);
            level := level DIV 2;
            LoggingActive := LoggingActive OR Target[t];
        END (*FOR*);

        (* Start the disk file updater, if necessary. *)

        IF Target[todisk] AND NOT UpdateTaskRunning THEN
            CreateTask (TransactionLogUpdateTask, 3, "trlog update");
            UpdateTaskRunning := TRUE;
        END (*IF*);

        (* Open or close the pipe, as necessary. *)

        IF Target[topipe] <> PipeWasOpen THEN
            IF PipeWasOpen THEN
                Signal (Pipe.count);
            ELSE
                Signal (PipeOpenRequest);
                Sleep (2000);
            END (*IF*);
        END (*IF*);

        (* Open or close the syslog socket, as necessary. *)

        IF Target[tosyslog] <> SyslogWasActive THEN
            IF SyslogWasActive THEN
                StopSyslogging;
            ELSE
                StartSyslogging;
            END (*IF*);
        END (*IF*);

        Release (TransactionLogLock);
        LogTransactionL (DummyID, "Transaction logging started or restarted");

    END StartTransactionLogging;

(************************************************************************)

PROCEDURE CreateLogID (prefix: ARRAY OF CHAR): TransactionLogID;

    (* Creates a new logfile ID. *)

    VAR result: TransactionLogID;

    BEGIN
        NEW (result);
        Strings.Assign (prefix, result^);
        RETURN result;
    END CreateLogID;

(************************************************************************)

PROCEDURE DiscardLogID (VAR (*INOUT*) id: TransactionLogID);

    (* Discards a previously created logfile ID. *)

    BEGIN
        DISPOSE (id);
    END DiscardLogID;

(************************************************************************)

PROCEDURE LogTransaction (id: TransactionLogID;
                          VAR (*IN*) text: ARRAY OF CHAR);

    (* Puts id+text in the transaction log (if logging enabled). *)

    VAR bufptr: LogLinePtr;

    BEGIN
        IF LoggingActive THEN
            NEW (bufptr);
            IF id = NIL THEN
                bufptr^ := "";
            ELSE
                Strings.Assign (id^, bufptr^);
            END (*IF*);
            Strings.Append ("  ", bufptr^);
            Strings.Append (text, bufptr^);
            AddToTransactionLog (bufptr);
            DISPOSE (bufptr);
        END (*IF*);
    END LogTransaction;

(************************************************************************)

PROCEDURE LogTransactionL (id: TransactionLogID;
                           text: ARRAY OF CHAR);

    (* Like LogTransaction, but for a literal text string. *)

    BEGIN
        LogTransaction (id, text);
    END LogTransactionL;

(************************************************************************)
(*                              INITIALISATION                          *)
(************************************************************************)

VAR t: TargetType;

BEGIN
    DummyID := CreateLogID ("*******");
    CRLF[0] := CHR(13);  CRLF[1] := CHR(10);
    ShuttingDown := FALSE;
    LoggingActive := FALSE;
    FOR t := MIN(TargetType) TO MAX(TargetType) DO
        Target[t] := FALSE;
    END (*FOR*);
    LogSocket := NotASocket;
    OurHostname := "localhost";
    SetProcname ("", 0);
    CreateLock (TransactionLogLock);
    InitialisePipe;
    InterimDiskFileOpen := FALSE;
    UpdateTaskRunning := FALSE;
FINALLY
    LogTransactionL (DummyID, "Transaction logging closing down");
    ShuttingDown := TRUE;
    Target[topipe] := FALSE;
    Signal (Pipe.count);
    IF LoggingActive THEN
        Obtain (TransactionLogLock);
        CopyTransactionLogUpdates;
        Release (TransactionLogLock);
    END (*IF*);
    DiscardLogID (DummyID);
END TransLog.

