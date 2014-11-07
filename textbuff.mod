IMPLEMENTATION MODULE TextBuffers;

        (********************************************************)
        (*                                                      *)
        (*        Text buffers for line-oriented file I/O       *)
        (*                                                      *)
        (*          (At present we support only input)          *)
        (*                                                      *)
        (*   This module maintains a read-ahead buffer for each *)
        (*   open file.  Obviously it will be efficient to do   *)
        (*   this only if input is mostly sequential.           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            16 July 2001                    *)
        (*  Last edited:        11 November 2004                *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT LOC, BYTE, ADDRESS, CAST, ADR;

FROM Types IMPORT CARD64, INT64;

IMPORT Strings;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilePos,
    (* proc *)  OpenOldFile, CloseFile, StartPosition,
                EndPosition, SetPosition, ReadRaw;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LONGLONG IMPORT
    (* proc *)  Add64, Diff64, Compare64, ShortSub;

FROM LowLevel IMPORT
    (* proc *)  Copy, AddOffset;

(************************************************************************)

CONST
    NumberOfBuffers = 8;
    BufferSize = 4096;
    Nul = CHR(0);
    CtrlZ = CHR(26);

TYPE
    (* Each data buffer contains the following fields.                  *)
    (*    StartPos  the position within the file from which these data  *)
    (*              were read                                           *)
    (*    count     the number of bytes of data                         *)
    (*    data      the actual cached data                              *)
    (* The data array is one byte longer than we need, with the last    *)
    (* character permanently set to Nul.  This prevents string searches *)
    (* from running off the end of the array.                           *)

    BufferPointer = POINTER TO
                        RECORD
                            StartPos: FilePos;
                            count: CARDINAL;
                            data: ARRAY [0..BufferSize] OF CHAR;
                        END (*RECORD*);

    (* A Buffer record contains NumberOfBuffers data buffers, each of   *)
    (* which is a cached section of the currently opened file.  The     *)
    (* fields are                                                       *)
    (*    cid            channel ID of the opened file                  *)
    (*    CurrentPosition  the location in the file from which the      *)
    (*                     next character will come, assuming that      *)
    (*                     we continue to read sequentially             *)
    (*    CurrentBuffer  the buffer from which we're currently reading  *)
    (*                   data.  If this is zero, we need to switch to   *)
    (*                   a new buffer.                                  *)
    (*    offset         offset within CurrentBuffer to the next        *)
    (*                    character to be read in a sequential read.    *)
    (*    BufPtr         points to an array of buffers                  *)

    Buffer = POINTER TO
                 RECORD
                     cid: ChanId;
                     CurrentPosition: FilePos;
                     CurrentBuffer: [0..NumberOfBuffers];
                     offset: CARDINAL;
                     BufPtr: ARRAY [1..NumberOfBuffers] OF BufferPointer;
                 END (*RECORD*);

(************************************************************************)
(*                           OPEN/CLOSE/ETC                             *)
(************************************************************************)

PROCEDURE OpenForReading (name: ARRAY OF CHAR): Buffer;

    (* Opens an existing file for read-only access, and returns its     *)
    (* buffer ID.                                                       *)

    VAR result: Buffer;  k: CARDINAL;

    BEGIN
        NEW (result);
        WITH result^ DO
            cid := OpenOldFile (name, FALSE);
            FOR k := 1 TO NumberOfBuffers DO
                NEW (BufPtr[k]);
                WITH BufPtr[k]^ DO
                    StartPos := CARD64{0,0};  count := 0;
                    offset := 0;
                    data[BufferSize] := Nul;
                END (*WITH*);
            END (*FOR*);
            CurrentBuffer := 1;
            IF cid <> NoSuchChannel THEN
                CurrentPosition := StartPosition(cid);
                ReadRaw (cid, BufPtr[1]^.data, BufferSize, BufPtr[1]^.count);
            END (*IF*);
            IF BufPtr[1]^.count = 0 THEN
                CurrentBuffer := 0;
            END (*IF*);
        END (*WITH*);
        RETURN result;
    END OpenForReading;

(************************************************************************)

PROCEDURE TBFileOpened (TB: Buffer): BOOLEAN;

    (* Use this to check whether an 'open' operation succeeded. *)

    BEGIN
        RETURN (TB <> NIL) AND (TB^.cid <> NoSuchChannel);
    END TBFileOpened;

(************************************************************************)

PROCEDURE CloseTB (VAR (*INOUT*) TB: Buffer);

    (* Closes a file. *)

    VAR k: CARDINAL;

    BEGIN
        IF TB <> NIL THEN
            CloseFile (TB^.cid);
            FOR k := 1 TO NumberOfBuffers DO
                DISPOSE (TB^.BufPtr[k]);
            END (*FOR*);
            DISPOSE (TB);
        END (*IF*);
    END CloseTB;

(************************************************************************)
(*                         LOADING THE DATA                             *)
(************************************************************************)

PROCEDURE LoadNextBuffer (TB: Buffer);

    (* Reads some more data from the file.  If TB^.CurrentBuffer = 0    *)
    (* after this operation, we must have reached the end of the file.  *)

    BEGIN
        WITH TB^ DO
            IF cid <> NoSuchChannel THEN
                SetPosition (cid, CurrentPosition);

                (* Find the next unused buffer, and default to the last *)
                (* buffer if all are in use.                            *)

                CurrentBuffer := 1;
                WHILE (BufPtr[CurrentBuffer]^.count <> 0)
                                 AND (CurrentBuffer < NumberOfBuffers) DO
                    INC (CurrentBuffer);
                END (*WHILE*);
                WITH BufPtr[CurrentBuffer]^ DO
                    StartPos := CurrentPosition;
                    ReadRaw (cid, data, BufferSize, count);
                    data[count] := Nul;
                    offset := 0;
                END (*WITH*);
            END (*IF*);
            IF BufPtr[CurrentBuffer]^.count = 0 THEN
                CurrentBuffer := 0;
            END (*IF*);
        END (*WITH*);
    END LoadNextBuffer;

(************************************************************************)

PROCEDURE SetCurrentBuffer (TB: Buffer);

    (* Updates TB^.CurrentBuffer to be a buffer whose data includes     *)
    (* a character at TB^.CurrentPosition.  This might require reading  *)
    (* more data from the file.  If TB^.CurrentBuffer = 0 after this    *)
    (* operation, we must have reached the end of the file.             *)

    VAR k: CARDINAL;  gap: INT64;

    BEGIN
        k := 1;
        LOOP
            IF k > NumberOfBuffers THEN
                LoadNextBuffer (TB);
                EXIT (*LOOP*);
            ELSE
                WITH TB^ DO
                    WITH BufPtr[k]^ DO
                        IF count > 0 THEN
                            gap := Diff64 (CurrentPosition, StartPos);
                            IF gap.high = 0 THEN
                                offset := gap.low;
                                IF offset < count THEN
                                    CurrentBuffer := k;
                                    EXIT (*LOOP*);
                                END (*IF*);
                            END (*IF*);
                        END (*IF*);
                    END (*WITH*);
                END (*WITH*);
                INC (k);
            END (*IF*);
        END (*LOOP*);
    END SetCurrentBuffer;

(************************************************************************)
(*                         FILE POSITION/SIZE                           *)
(************************************************************************)

PROCEDURE TBCurrentPosition (TB: Buffer): FilePos;

    (* Returns the current position within the file. *)

    BEGIN
        RETURN TB^.CurrentPosition;
    END TBCurrentPosition;

(************************************************************************)

PROCEDURE TBStartPosition (TB: Buffer): FilePos;

    (* Returns the start-of-file position. *)

    BEGIN
        RETURN StartPosition (TB^.cid);
    END TBStartPosition;

(************************************************************************)

PROCEDURE TBEndPosition (TB: Buffer): FilePos;

    (* Returns the end-of-file position. *)

    BEGIN
        RETURN EndPosition (TB^.cid);
    END TBEndPosition;

(************************************************************************)

PROCEDURE TBSetPosition (TB: Buffer;  position: FilePos);

    (* Sets the current position within the file. *)

    BEGIN
        TB^.CurrentPosition := position;
        SetCurrentBuffer (TB);
    END TBSetPosition;

(************************************************************************)
(*                              INPUT                                   *)
(************************************************************************)

PROCEDURE TBReadRaw (TB: Buffer;  VAR (*OUT*) data: ARRAY OF LOC;
                   limit: CARDINAL;  VAR (*OUT*) NumberRead: CARDINAL);

    (* Reads a buffer-full of information from a file. *)

    VAR ToGo, amount, available: CARDINAL;
        source, destination: ADDRESS;
        temp: BufferPointer;

    BEGIN
        NumberRead := 0;
        destination := ADR(data);
        ToGo := limit;
        IF ToGo > HIGH(data) THEN
            ToGo := HIGH(data) + 1;
        END (*IF*);
        WHILE ToGo > 0 DO
            IF TB^.CurrentBuffer = 0 THEN
                SetCurrentBuffer (TB);
                IF TB^.CurrentBuffer = 0 THEN
                    ToGo := 0;
                END (*IF*);
            END (*IF*);
            amount := ToGo;
            WITH TB^ DO
                WITH BufPtr[CurrentBuffer]^ DO
                    available := count - offset;
                    IF available > 0 THEN
                        source := ADR(data[offset]);
                    END (*IF*);
                END (*WITH*);
            END (*WITH*);
            IF amount > available THEN
                amount := available;
                TB^.CurrentBuffer := 0;
            END (*IF*);
            IF amount > 0 THEN
                Copy (source, destination, amount);
                DEC (ToGo, amount);
                INC (NumberRead, amount);
                WITH TB^ DO
                    INC (offset, amount);
                    Add64 (CurrentPosition, amount);
                END (*WITH*);
                destination := AddOffset (destination, amount);
            END (*IF*);
        END (*WHILE*);

        (* Promote current buffer as an approximate LRU calculation. *)

        WITH TB^ DO
            IF CurrentBuffer > 1 THEN
                temp := BufPtr[CurrentBuffer-1];
                BufPtr[CurrentBuffer-1] := BufPtr[CurrentBuffer];
                BufPtr[CurrentBuffer] := temp;
                DEC (CurrentBuffer);
            END (*IF*);
        END (*WITH*);

    END TBReadRaw;

(************************************************************************)

PROCEDURE TBReadLine (TB: Buffer;  VAR (*OUT*) line: ARRAY OF CHAR);

    (* Reads a line of text from a file.  Assumption: a line ends with  *)
    (* CRLF.  To avoid tortuous logic, I take the LF as end of line and *)
    (* skip the CR.  At end of file we return with line[0] = Ctrl/Z.    *)

    CONST CR = CHR(13);  LF = CHR(10);

    VAR length, pos, extra, space: CARDINAL;
        ToFind: ARRAY [0..0] OF CHAR;
        found: BOOLEAN;
        temp: BufferPointer;
        source, destination: ADDRESS;

    BEGIN
        ToFind[0] := LF;
        space := HIGH(line) + 1;
        destination := ADR(line);
        REPEAT
            found := FALSE;
            WITH TB^ DO
                IF CurrentBuffer = 0 THEN
                    SetCurrentBuffer (TB);
                    IF TB^.CurrentBuffer = 0 THEN
                        line[0] := CtrlZ;
                        DEC (space);
                        found := TRUE;
                    END (*IF*);
                END (*IF*);
                IF NOT found THEN
                    WITH BufPtr[CurrentBuffer]^ DO
                        source := ADR(data[offset]);
                        Strings.FindNext (ToFind, data, offset, found, pos);
                    END (*WITH*);
                    IF found THEN
                        extra := 1;
                    ELSE
                        pos := BufPtr[CurrentBuffer]^.count;
                        extra := 0;
                    END (*IF*);
                    length := pos - offset;
                    IF (length > 0) AND (BufPtr[CurrentBuffer]^.data[pos-1] = CR) THEN
                        DEC (length);  INC (extra);
                    END (*IF*);
                    IF length > space THEN
                        INC (extra, length-space);
                        length := space;
                    END (*IF*);
                    IF length > 0 THEN
                        Copy (source, destination, length);
                        DEC (space, length);
                        destination := AddOffset (destination, length);
                    END (*IF*);
                    INC (length, extra);
                    Add64 (CurrentPosition, length);
                    IF found THEN
                        IF length > 0 THEN
                            INC (offset, length);
                        END (*IF*);
                    ELSE
                        CurrentBuffer := 0;
                    END (*IF*);
                END (*IF*);
            END (*WITH*);
        UNTIL found;

        IF space > 0 THEN
            line[HIGH(line)+1-space] := Nul;
        END (*IF*);

        (* Promote current buffer as an approximate LRU calculation. *)

        WITH TB^ DO
            IF CurrentBuffer > 1 THEN
                temp := BufPtr[CurrentBuffer-1];
                BufPtr[CurrentBuffer-1] := BufPtr[CurrentBuffer];
                BufPtr[CurrentBuffer] := temp;
                DEC (CurrentBuffer);
            END (*IF*);
        END (*WITH*);

    END TBReadLine;

(************************************************************************)

END TextBuffers.

