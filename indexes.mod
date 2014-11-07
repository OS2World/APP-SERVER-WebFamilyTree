IMPLEMENTATION MODULE Indexes;

        (********************************************************)
        (*                                                      *)
        (*      Web Family Tree: module to deal with the        *)
        (*   index file and to read data via the index file     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            9 July 2001                     *)
        (*  Last edited:        12 September 2004               *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM SYSTEM IMPORT ADDRESS;

FROM Types IMPORT CARD64;

FROM OurTypes IMPORT
    (* type *)  FilenameString, Line, IDString;

FROM TextBuffers IMPORT
    (* type *)  Buffer,
    (* proc *)  OpenForReading, TBFileOpened, CloseTB,
                TBSetPosition, TBStartPosition, TBCurrentPosition,
                TBEndPosition, TBReadRaw, TBReadLine;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilePos,
    (* proc *)  OpenNewFile, CloseFile, DeleteFile,
                SetPosition, ReadRaw, WriteRaw, FWriteString;

FROM LONGLONG IMPORT
    (* proc *)  Mul64, ShortDiv;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);  CtrlZ = CHR(26);

TYPE
    Index = POINTER TO
                RECORD
                    TB: Buffer;
                    dataTB: Buffer;
                    MaxRecordNo: CARDINAL;
                    file: FilenameString;
                END (*RECORD*);

    IndexRecord = RECORD
                      pos: FilePos;
                      tag: IDString;
                  END (*IF*);

    IndexRecordPtr = POINTER TO IndexRecord;

(************************************************************************)
(*                       READING FROM INPUT FILE                        *)
(************************************************************************)

PROCEDURE ExtractLevel (VAR (*INOUT*) line: Line): CARDINAL;

    (* Takes one line, trims its level from the front and returns the   *)
    (* level as the function result.                                    *)

    TYPE CharSet = SET OF CHAR;

    CONST Digits = CharSet {'0'..'9'};

    VAR j, level: CARDINAL;

    BEGIN
        level := 0;
        j := 0;
        WHILE line[j] = ' ' DO
            INC (j);
        END (*WHILE*);
        IF line[j] IN Digits THEN
            REPEAT
                level := 10*level + (ORD(line[j]) - ORD('0'));
                INC (j);
            UNTIL NOT (line[j] IN Digits);
        ELSE
            level := MAX(CARDINAL);
        END (*IF*);
        WHILE line[j] = ' ' DO
            INC (j);
        END (*WHILE*);
        IF j > 0 THEN
            Strings.Delete (line, 0, j);
        END (*IF*);
        RETURN level;
    END ExtractLevel;

(************************************************************************)

PROCEDURE GetOneLine (TB: Buffer;  VAR (*OUT*) result: Line): CARDINAL;

    (* Reads one line, trims its level from the front and returns the   *)
    (* level as the function result.                                    *)

    BEGIN
        TBReadLine (TB, result);
        RETURN ExtractLevel(result);
    END GetOneLine;

(************************************************************************)

PROCEDURE GetFirstLine (TB: Buffer;  VAR (*OUT*) Lookahead: Line);

    (* Goes to the beginning of the file, loads Lookahead in            *)
    (* preparation for the first call to GetNextLine.                   *)

    BEGIN
        TBSetPosition (TB, TBStartPosition(TB));
        TBReadLine (TB, Lookahead);
    END GetFirstLine;

(************************************************************************)

PROCEDURE GetNextLine (TB: Buffer;  VAR (*OUT*) result: Line;
                               VAR (*INOUT*) Lookahead: Line): CARDINAL;

    (* Copies Lookahead to result, and then loads the following line    *)
    (* into Lookahead.                                                  *)

    BEGIN
        result := Lookahead;
        TBReadLine (TB, Lookahead);
        RETURN ExtractLevel(result);
    END GetNextLine;

(************************************************************************)
(*                        SORTING AN INDEX FILE                         *)
(************************************************************************)

PROCEDURE GetRecord (f: ChanId;  RecordNumber: CARDINAL;
                     VAR (*OUT*) result: IndexRecord);

    (* Reads index record 'RecordNumber' from the index file.  For      *)
    (* now we don't go through a buffered file interface because the    *)
    (* best one we have doesn't allow for writing back.  This procedure *)
    (* is used only during the initial construction of the index file,  *)
    (* not for operations after it is built and sorted.                 *)

    VAR amount: CARDINAL;  position: FilePos;

    BEGIN
        position := Mul64(CARD64{RecordNumber,0},
                          CARD64{SIZE(IndexRecord),0});
        SetPosition (f, position);
        ReadRaw (f, result, SIZE(IndexRecord), amount);
        Strings.Capitalize (result.tag);
    END GetRecord;

(************************************************************************)

PROCEDURE PutRecord (f: ChanId;  RecordNumber: CARDINAL;
                     VAR (*OUT*) value: IndexRecord);

    (* Writes index record 'RecordNumber' to the index file.  *)

    VAR position: FilePos;

    BEGIN
        position := Mul64(CARD64{RecordNumber,0},
                          CARD64{SIZE(IndexRecord),0});
        SetPosition (f, position);
        WriteRaw (f, value, SIZE(IndexRecord));
    END PutRecord;

(************************************************************************)

PROCEDURE Partition (f: ChanId;
                          low: CARDINAL;
                          VAR (*OUT*) mid: CARDINAL;
                          high: CARDINAL);

    (* By shuffling elements as necessary, ensures the property         *)
    (*          R[j] <= v       for low <= j < mid                      *)
    (*          R[mid] = v                                              *)
    (*          R[j] >= v       for mid < j <= high                     *)
    (* where R[j] represents record number j of the file, mid is the    *)
    (* function result, and v is some unspecified value chosen by the   *)
    (* procedure.                                                       *)

    VAR up, down: CARDINAL;
        temp, v: IndexRecord;

    BEGIN
        down := low;  up := high;  mid := (down + up) DIV 2;

        GetRecord (f, mid, v);

        (* The following loop maintains the invariants:                 *)
        (*      R[j] <= v       for low <= j < down                     *)
        (*      R[j] >= v       for up < j <= high                      *)
        (* We exit the outer loop when down >= mid and up <= mid.       *)
        (* Note that v is the value that should be stored as R[mid],    *)
        (* but to avoid redundant store and load operations as mid      *)
        (* changes we don't actually store this value back until        *)
        (* the final exit from the loop.  During loop execution, mid    *)
        (* refers to a "hole" in which a value has not yet been stored. *)
        (* Note also that temp holds either R[down] or R[up],           *)
        (* depending on whether we're adjusting down or up at the time. *)

        LOOP
            GetRecord (f, down, temp);
            WHILE (down < mid) AND
                      (Strings.Compare(v.tag, temp.tag) <> Strings.less) DO
                INC (down);
                GetRecord (f, down, temp);
            END (*WHILE*);

            IF down < mid THEN
                PutRecord (f, mid, temp);
                mid := down;
                INC (down);
            END (*IF*);

            (* Note that down >= mid at this point.     *)

            GetRecord (f, up, temp);
            WHILE (up > mid) AND
                      (Strings.Compare(temp.tag, v.tag) <> Strings.less) DO
                DEC (up);
                GetRecord (f, up, temp);
            END (*WHILE*);

            IF up <= mid THEN EXIT(*LOOP*) END(*IF*);

            PutRecord (f, mid, temp);
            mid := up;
            DEC (up);

        END (*LOOP*);

        PutRecord (f, mid, v);

    END Partition;

(************************************************************************)

PROCEDURE Sort (f: ChanId;  low, high: CARDINAL);

    (* In-place sort of part of a file.  We sort record numbers         *)
    (* low..high inclusive.  It is assumed that each record has type    *)
    (* IndexRecord, and that we will sort on the 'tag' field.           *)
    (* Assumption: file f is already open.                              *)

    VAR mid: CARDINAL;

    BEGIN
        IF high > low THEN
            Partition (f, low, mid, high);
            IF mid > low+1 THEN Sort (f, low, mid-1) END(*IF*);
            IF high > mid+1 THEN Sort (f, mid+1, high) END(*IF*);
        END (*IF*);
    END Sort;

(************************************************************************)
(*                 BUILDING AND REBUILDING AN INDEX FILE                *)
(************************************************************************)

PROCEDURE DeleteIndexFile (IX: Index);

    (* Deletes the index file if it exists.  *)

    BEGIN
        CloseTB (IX^.TB);
        DeleteFile (IX^.file);
    END DeleteIndexFile;

(************************************************************************)

PROCEDURE RebuildIndexFile (IX: Index);

    (* Creates an index file, after deleting the old one if it exists.  *)

    VAR SrcPos: FilePos;
        thisline: Line;  level, pos: CARDINAL;
        tag: IDString;  found: BOOLEAN;
        cid: ChanId;

    BEGIN
        DeleteIndexFile (IX);
        IX^.MaxRecordNo := 0;
        cid := OpenNewFile (IX^.file);
        IF cid <> NoSuchChannel THEN
            TBSetPosition (IX^.dataTB, TBStartPosition(IX^.dataTB));
            LOOP
                SrcPos := TBCurrentPosition (IX^.dataTB);
                level := GetOneLine (IX^.dataTB, thisline);
                IF (thisline[0] = Nul) OR (thisline[0] = CtrlZ) THEN EXIT(*LOOP*) END(*IF*);
                IF level = 0 THEN
                    IF thisline[0] = '@' THEN
                        Strings.FindNext ('@', thisline, 1, found, pos);
                        IF found THEN
                            Strings.Extract (thisline, 1, pos-1, tag);
                            WriteRaw (cid, SrcPos, SIZE(SrcPos));
                            WriteRaw (cid, tag, SIZE(tag));
                            INC (IX^.MaxRecordNo);
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
            END (*LOOP*);
            IF IX^.MaxRecordNo > 0 THEN
                DEC (IX^.MaxRecordNo);
                Sort (cid, 0, IX^.MaxRecordNo);
            END (*IF*);
            CloseFile (cid);
        END (*IF*);
        IX^.TB := OpenForReading (IX^.file);
    END RebuildIndexFile;

(************************************************************************)

PROCEDURE OpenIndex (DataFile: Buffer;  DatabaseName: FilenameString): Index;

    (* Opens an index file. *)

    VAR IX: Index;

    BEGIN
        NEW (IX);
        IX^.dataTB := DataFile;
        Strings.Assign ("data\", IX^.file);
        Strings.Append (DatabaseName, IX^.file);
        Strings.Append (".IDX", IX^.file);
        IX^.TB := OpenForReading (IX^.file);
        IF TBFileOpened (IX^.TB) THEN
            IX^.MaxRecordNo := ShortDiv (TBEndPosition(IX^.TB), SIZE(IndexRecord));
            IF IX^.MaxRecordNo > 0 THEN
                DEC (IX^.MaxRecordNo);
            END (*IF*);
        ELSE
            RebuildIndexFile (IX);
        END (*IF*);
        RETURN IX;
    END OpenIndex;

(************************************************************************)

PROCEDURE CloseIndex (VAR (*INOUT*) IX: Index);

    (* Closes an index file. *)

    BEGIN
        IF IX <> NIL THEN
            IF TBFileOpened (IX^.TB) THEN
                CloseTB (IX^.TB);
            END (*IF*);
            DISPOSE (IX);
        END (*IF*);
    END CloseIndex;

(************************************************************************)

PROCEDURE Match (string1, string2: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff string1 and string2 are equal, modulo character *)
    (* case.                                                            *)

    BEGIN
        Strings.Capitalize(string1);
        Strings.Capitalize(string2);
        RETURN Strings.Equal (string1, string2);
    END Match;

(************************************************************************)

PROCEDURE CompareIndex (ID: IDString;  IX: Index;  pos: CARDINAL;
                        VAR (*OUT*) place: FilePos): INTEGER;

    (* Returns >0 if ID > IX[pos], =0 if they're greater, <0 if ID is   *)
    (* the smaller of the two.                                          *)

    VAR amount: CARDINAL;  position: FilePos;  record: IndexRecord;
        test: Strings.CompareResults;

    BEGIN
        position := Mul64(CARD64{pos,0},
                          CARD64{SIZE(IndexRecord),0});
        TBSetPosition (IX^.TB, position);
        TBReadRaw (IX^.TB, record, SIZE(IndexRecord), amount);
        place := record.pos;
        test := Strings.Compare (ID, record.tag);
        IF test = Strings.less THEN RETURN -1
        ELSIF test = Strings.greater THEN RETURN +1
        ELSE RETURN 0
        END (*IF*);
    END CompareIndex;

(************************************************************************)

PROCEDURE PositionToRecord (IX: Index;  ID: IDString): BOOLEAN;

    (* Positions the database file to the start of the level 0 record   *)
    (* for the given ID.                                                *)

    VAR found: BOOLEAN;  ThisPos: FilePos;
        first, middle, last: CARDINAL;
        test: INTEGER;

    BEGIN
        found := TBFileOpened (IX^.TB);
        IF NOT found THEN
            RebuildIndexFile (IX);
            found := TBFileOpened (IX^.TB);
        END (*IF*);
        IF found THEN

            (* Binary search through the index file. *)

            Strings.Capitalize (ID);
            first := 0;  last := IX^.MaxRecordNo;  found := FALSE;
            LOOP
                middle := (first + last) DIV 2;
                test := CompareIndex (ID, IX, middle, ThisPos);
                IF test < 0 THEN
                    IF middle = 0 THEN
                        EXIT (*LOOP*);
                    ELSE
                        last := middle - 1;
                    END (*IF*);
                ELSIF test = 0 THEN
                    found := TRUE;  EXIT (*LOOP*);
                ELSIF test > 0 THEN
                    first := middle + 1;
                END (*IF*);
                IF first > last THEN EXIT (*LOOP*) END (*IF*);
            END (*LOOP*);

        END (*IF*);

        IF found THEN
            TBSetPosition (IX^.dataTB, ThisPos);
        END(*IF*);

        RETURN found;

    END PositionToRecord;

(************************************************************************)

END Indexes.

