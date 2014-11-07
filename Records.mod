IMPLEMENTATION MODULE Records;

        (********************************************************)
        (*                                                      *)
        (*         Input and output of GEDCOM records           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            13 July 2001                    *)
        (*  Last edited:        26 December 2004                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM TextBuffers IMPORT
    (* type *)  Buffer,
    (* proc *)  TBReadLine, TBSetPosition, TBStartPosition;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  FWriteChar, FWriteString, FWriteLn;

FROM OurTypes IMPORT
    (* type *)  IDString, DataString, Line;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);

TYPE
    (* Representation of a record, including subsidiary records. *)

    RecordTree = POINTER TO LineRecord;

    LineRecord = RECORD
                     next, down: RecordTree;
                     this:       Line;
                 END (*RECORD*);

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

PROCEDURE LoadRecordN (TB: Buffer;  VAR (*OUT*) RecTree: RecordTree;
                       N: CARDINAL;  VAR (*INOUT*) NextLevel: CARDINAL;
                       VAR (*INOUT*) NextLine: Line);

    (* On entry, NextLine holds the line just read from the file, and   *)
    (* NextLevel is the level of that line.  We load the level N and    *)
    (* greater lines into RecTree, stripping off the level numbers.     *)
    (* On exit, NextLevel and NextLine are the values for the first     *)
    (* line after the ones we've used.                                  *)

    VAR previous, current: RecordTree;

    BEGIN
        RecTree := NIL;  previous := NIL;

        WHILE (NextLevel <> MAX(CARDINAL)) AND (NextLevel >= N) DO

            NEW (current);
            WITH current^ DO
                next := NIL;
                down := NIL;
            END (*WITH*);

            IF N < NextLevel THEN

                (* Missing record at this level, insert a blank one. *)

                current^.this := "";

            ELSE

                current^.this := NextLine;
                NextLevel := GetOneLine (TB, NextLine);

            END (*IF*);

            IF previous = NIL THEN
                RecTree := current;
            ELSE
                previous^.next := current;
            END (*IF*);

            (* Add the lower-level records, if any. *)

            LoadRecordN (TB, current^.down, N+1, NextLevel, NextLine);

            previous := current;

        END (*WHILE*);

    END LoadRecordN;

(************************************************************************)

PROCEDURE StartReading (TB: Buffer;  VAR (*OUT*) NextLevel: CARDINAL;
                                          VAR (*OUT*) Lookahead: Line);

    (* Initialisation needed before the first call to LoadNextRecord.   *)

    BEGIN
        TBSetPosition (TB, TBStartPosition(TB));
        NextLevel := GetOneLine (TB, Lookahead);
    END StartReading;

(************************************************************************)

PROCEDURE LoadNextRecord (TB: Buffer;  VAR (*OUT*) RecTree: RecordTree;
                                        VAR (*INOUT*) NextLevel: CARDINAL;
                                        VAR (*INOUT*) Lookahead: Line;
                                        VAR (*OUT*) finished: BOOLEAN);

    (* Loads the next level 0 record in the database into RecTree.      *)
    (* Assumption: NextLevel and Lookahead have been "seeded" by a      *)
    (* previous call to LoadNextRecord or StartReading.                 *)
    (* Returns with finished = TRUE if there is no record.              *)

    BEGIN
        IF NextLevel = MAX(CARDINAL) THEN

            (* End of file. *)

            RecTree := NIL;
            finished := TRUE;

        ELSE

            finished := FALSE;
            NEW (RecTree);
            WITH RecTree^ DO
                next := NIL;
                down := NIL;
            END (*WITH*);
            IF NextLevel = 0 THEN
                RecTree^.this := Lookahead;
                NextLevel := GetOneLine (TB, Lookahead);
            ELSE

                (* Missing level 0, insert a dummy. *)

                RecTree^.this := '';

            END (*IF*);

            LoadRecordN (TB, RecTree^.down, 1, NextLevel, Lookahead);

        END (*IF*);

    END LoadNextRecord;

(************************************************************************)

PROCEDURE DiscardTopLevel (VAR (*INOUT*) RecTree: RecordTree);

    (* Removes the top level of information. *)

    VAR this, next: RecordTree;

    BEGIN
        IF RecTree <> NIL THEN
            DiscardTopLevel (RecTree^.next);
            this := RecTree;
            next := RecTree^.next;
            RecTree := this^.down;
            DISPOSE (this);
            this := RecTree;
            IF this = NIL THEN
                RecTree := next;
            ELSE
                WHILE this^.next <> NIL DO
                    this := this^.next;
                END (*WHILE*);
                this^.next := next;
            END (*IF*);
        END (*IF*);
    END DiscardTopLevel;

(************************************************************************)

PROCEDURE DiscardTree (VAR (*INOUT*) RecTree: RecordTree);

    (* Releases the storage taken by the tree. *)

    VAR current, next: RecordTree;

    BEGIN
        current := RecTree;
        WHILE current <> NIL DO
            DiscardTree (current^.down);
            next := current^.next;
            DISPOSE (current);
            current := next;
        END (*WHILE*);
        RecTree := NIL;
    END DiscardTree;

(************************************************************************)

PROCEDURE KwdMatch (RecTree: RecordTree;  keyword: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff keyword occurs in RecTree at the top level. *)

    VAR found: BOOLEAN;  dummy: CARDINAL;

    BEGIN
        Strings.FindNext (keyword, RecTree^.this, 0, found, dummy);
        RETURN found;
    END KwdMatch;

(************************************************************************)

PROCEDURE HeadMatch (VAR (*INOUT*) string: ARRAY OF CHAR;
                              template: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff a leading substring of string matches template, *)
    (* modulo character case.  If there is a match, the matched         *)
    (* substring and any following spaces are removed from string.      *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF (j > HIGH(template)) OR (template[j] = Nul) THEN
                EXIT (*LOOP*);
            END (*IF*);
            IF (j > HIGH(string)) OR (string[j] = Nul)
                    OR (CAP(string[j]) <> CAP(template[j])) THEN
                RETURN FALSE;
            END (*IF*);
            INC (j);
        END (*LOOP*);
        WHILE string[j] = ' ' DO
            INC (j);
        END (*WHILE*);
        Strings.Delete (string, 0, j);
        RETURN TRUE;
    END HeadMatch;

(************************************************************************)

PROCEDURE IsPeopleRecord (RecTree: RecordTree;
                          VAR (*OUT*) ID: IDString;
                          VAR (*OUT*) family: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff the top-level keyword in RecTree is INDI or     *)
    (* FAM.  If so, sets ID to the ID of this record and sets           *)
    (* family=TRUE for a FAM record and family=FALSE for INDI.          *)

    VAR info: Line;
        found: BOOLEAN;  pos: CARDINAL;

    BEGIN
        found := RecTree <> NIL;
        IF found THEN
            info := RecTree^.this;
            IF info[0] = '@' THEN
                Strings.FindNext ('@', info, 1, found, pos);
                IF NOT found THEN
                    pos := LENGTH(info);
                END (*IF*);
                Strings.Extract (info, 1, pos-1, ID);
                IF pos < LENGTH(info) THEN
                    REPEAT
                        INC (pos);
                    UNTIL info[pos] <> ' ';
                END (*IF*);
                Strings.Delete (info, 0, pos);
                found := TRUE;
                IF HeadMatch (info, 'INDI') THEN
                    family := FALSE;
                ELSIF HeadMatch (info, 'FAM') THEN
                    family := TRUE;
                ELSE
                    found := FALSE;
                END (*IF*);
            ELSE
                found := FALSE;
            END (*IF*);
        END (*IF*);
        RETURN found;
    END IsPeopleRecord;

(************************************************************************)
(*                   EXTRACTING DATA FROM THE TREE                      *)
(************************************************************************)

PROCEDURE ExtractSubrecord (keyword: ARRAY OF CHAR;
                            VAR (*INOUT*) T: RecordTree;
                            VAR (*OUT*) data: Line;
                            VAR (*OUT*) subtree: RecordTree): BOOLEAN;

    (* Finds the top-level record, if any, in T that starts with        *)
    (* keyword.  If not found, data is the empty string, subtree=NIL,   *)
    (* T is left unchanged, and the function result is FALSE.           *)
    (* If found, the record and its subsidiary records are removed      *)
    (* from T and the function result is TRUE.  The returned value of   *)
    (* 'data' is the top-level line with keyword and leading spaces     *)
    (* stripped.  The returned value of subtree gives the lower-level   *)
    (* records that were also extracted.                                *)

    VAR previous, current: RecordTree;

    BEGIN
        previous := NIL;  current := T;
        LOOP
            IF current = NIL THEN
                data[0] := Nul;  subtree := NIL;
                RETURN FALSE;
            END (*IF*);
            data := current^.this;
            IF HeadMatch (data, keyword) THEN
                subtree := current^.down;
                IF previous = NIL THEN
                    T := current^.next;
                ELSE
                    previous^.next := current^.next;
                END (*IF*);
                DISPOSE (current);
                RETURN TRUE;
            END (*IF*);
            previous := current;  current := current^.next;
        END (*LOOP*);
    END ExtractSubrecord;

(************************************************************************)

PROCEDURE GetField (keyword: ARRAY OF CHAR;  VAR (*INOUT*) T: RecordTree;
                                  VAR (*OUT*) data: DataString): BOOLEAN;

    (* Like ExtractSubrecord, but discards the sublist. *)

    VAR subtree: RecordTree;

    BEGIN
        IF ExtractSubrecord (keyword, T, data, subtree) THEN
            DiscardTree (subtree);
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END GetField;

(************************************************************************)

PROCEDURE GetName (T: RecordTree;
                    VAR (*OUT*) surname, wholename: Line): BOOLEAN;

    (* Returns both the surname and the whole name.  The function       *)
    (* result is FALSE for an empty tree.  It is TRUE if the record     *)
    (* exists, even if the name field is missing or blank.              *)

    VAR pos, pos2: CARDINAL;  found: BOOLEAN;

    BEGIN
        surname := '';
        wholename := '';
        IF T = NIL THEN
            RETURN FALSE;
        ELSE
            T := T^.down;
            WHILE (T <> NIL) AND NOT KwdMatch (T, 'NAME') DO
                T := T^.next;
            END (*WHILE*);
            IF T <> NIL THEN
                wholename := T^.this;
                Strings.FindNext ('NAME', wholename, 0, found, pos);
                IF found THEN
                    INC (pos, 4);
                    WHILE wholename[pos] = ' ' DO
                        INC (pos);
                    END (*WHILE*);
                    Strings.Delete (wholename, 0, pos);
                    pos := LENGTH (wholename);
                    WHILE (pos > 0) AND (wholename[pos-1] = ' ') DO
                        DEC (pos);
                    END (*WHILE*);
                    wholename[pos] := Nul;

                    (* We now have the whole name.  Try to extract the surname. *)

                    IF wholename[0] <> Nul THEN
                        Strings.FindNext ('/', wholename, 0, found, pos);
                        IF found THEN
                            Strings.FindPrev ('/', wholename,
                                              LENGTH(wholename)-1, found, pos2);
                            IF (NOT found) OR (pos2 = pos) THEN
                                pos2 := LENGTH(wholename);
                            END (*IF*);
                            Strings.Extract (wholename, pos+1, pos2-pos-1, surname);
                        END (*IF*);
                    END (*IF*);

                END (*IF*);
            END (*IF*);

            RETURN TRUE;

        END (*IF*);

    END GetName;

(************************************************************************)
(*                          OUTPUT TO FILE                              *)
(************************************************************************)

PROCEDURE FWriteCard (cid: ChanId;  N: CARDINAL);

    (* Writes a decimal number to a file. *)

    BEGIN
        IF N > 9 THEN
            FWriteCard (cid, N DIV 10);
            N := N MOD 10;
        END (*IF*);
        FWriteChar (cid, CHR(ORD('0') + N));
    END FWriteCard;

(************************************************************************)

PROCEDURE WriteRecordN (cid: ChanId;  T: RecordTree;  N: CARDINAL);

    (* Writes a sequence of records at level N, including all subrecords. *)

    BEGIN
        WHILE T <> NIL DO
            FWriteCard (cid, N);
            FWriteChar (cid, ' ');
            FWriteString (cid, T^.this);
            FWriteLn (cid);
            WriteRecordN (cid, T^.down, N+1);
            T := T^.next;
        END (*WHILE*);
    END WriteRecordN;

(************************************************************************)

PROCEDURE WriteRecord (cid: ChanId;  RecTree: RecordTree);

    (* Writes a record to the output file. *)

    BEGIN
        WriteRecordN (cid, RecTree, 0);
    END WriteRecord;

(************************************************************************)
(*                       PRUNING REDUNDANT SUBRECORDS                   *)
(************************************************************************)

PROCEDURE PruneINDIRecord (R: RecordTree);

    (* We assume that the caller has already verified that R is an INDI *)
    (* record.  This procedure looks for and eliminates certain         *)
    (* redundant subrecords.                                            *)

    VAR prev1, current1, prev2, current2: RecordTree;
        remove1, remove2: BOOLEAN;

    BEGIN
        prev1 := NIL;  current1 := R^.down;

        (* Outer loop looks at level 1 of the tree. *)

        WHILE current1 <> NIL DO
            prev2 := NIL;  current2 := current1^.down;

            (* Inner loop looks at level 2 of the tree. *)

            WHILE current2 <> NIL DO

                remove2 := FALSE;

                (* Empty DATE and PLAC records seem to be the most *)
                (* common sort of unused record.                   *)

                IF KwdMatch (current2, 'DATE') THEN
                    remove2 := LENGTH (current2^.this) <= 5;
                ELSIF KwdMatch (current2, 'PLAC') THEN
                    remove2 := LENGTH (current2^.this) <= 5;
                ELSIF KwdMatch (current2, 'CITY') THEN
                    remove2 := LENGTH (current2^.this) <= 5;
                ELSIF KwdMatch (current2, 'POST') THEN
                    remove2 := LENGTH (current2^.this) <= 5;
                END (*IF*);

                IF remove2 THEN
                    IF prev2 = NIL THEN
                        current1^.down := current2^.next;
                        DISPOSE (current2);
                        current2 := current1^.down;
                    ELSE
                        prev2^.next := current2^.next;
                        DISPOSE (current2);
                        current2 := prev2^.next;
                    END (*IF*);
                ELSE
                    prev2 := current2;  current2 := current2^.next;
                END (*IF*);

            END (*WHILE*);

            (* Now check for the most commonly-occurring level 1 *)
            (* useless empty records.                            *)

            remove1 := FALSE;
            IF current1^.down = NIL THEN
                IF KwdMatch (current1, 'OCCU') THEN
                    remove1 := LENGTH (current1^.this) <= 5
                ELSIF KwdMatch (current1, 'NOTE') THEN
                    remove1 := LENGTH (current1^.this) <= 5
                ELSE
                    remove1 := KwdMatch (current1, 'BIRT')
                                 OR KwdMatch (current1, 'DEAT')
                                 OR KwdMatch (current1, 'ADDR');
                END (*IF*);
            END (*IF*);

            IF remove1 THEN
                IF prev1 = NIL THEN
                    R^.down := current1^.next;
                    DISPOSE (current1);
                    current1 := R^.down;
                ELSE
                    prev1^.next := current1^.next;
                    DISPOSE (current1);
                    current1 := prev1^.next;
                END (*IF*);
            ELSE
                prev1 := current1;  current1 := current1^.next;
            END (*IF*);

        END (*WHILE*);

    END PruneINDIRecord;

(************************************************************************)

END Records.

