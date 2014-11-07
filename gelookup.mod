IMPLEMENTATION MODULE GELookup;

        (********************************************************)
        (*                                                      *)
        (*      Web Family Tree: module to find records in      *)
        (*                  a GEDCOM file                       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            7 July 2001                     *)
        (*  Last edited:        24 January 2005                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM OurTypes IMPORT
    (* type *)  IDString, DataString, FilenameString, Line;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM Indexes IMPORT
    (* type *)  Index,
    (* proc *)  OpenIndex, CloseIndex, GetOneLine, PositionToRecord,
                DeleteIndexFile, Match;

FROM TextBuffers IMPORT
    (* type *)  Buffer,
    (* proc *)  OpenForReading, TBFileOpened, CloseTB,
                TBCurrentPosition, TBSetPosition, TBStartPosition;

FROM FileOps IMPORT
    (* type *)  FilePos;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);  CtrlZ = CHR(26);

TYPE
    CharSet = SET OF CHAR;

    Database = POINTER TO
                   RECORD
                       TB: Buffer;
                       name: FilenameString;
                       Charset: FilenameString;
                       index: Index;
                   END (*RECORD*);

    (* Representation of a record, including subsidiary records. *)

    RecordTree = POINTER TO LineRecord;

    LineRecord = RECORD
                     next, down: RecordTree;
                     this:       Line;
                 END (*RECORD*);

    (* A list of open databases. *)

    OpenedPtr = POINTER TO OpenedRecord;

    OpenedRecord = RECORD
                       next: OpenedPtr;
                       this: Database;
                   END (*RECORD*);

CONST Digits = CharSet {'0'..'9'};

VAR
    (* Linear list of databases that are now open. *)

    AlreadyOpened: OpenedPtr;

(************************************************************************)
(*                  MISCELLANEOUS UTILITY PROCEDURES                    *)
(************************************************************************)

PROCEDURE ToLower (VAR (*INOUT*) string: ARRAY OF CHAR);

    (* Converts all letters in string to lower case. *)

    TYPE CharSet = SET OF CHAR;

    CONST shift = ORD('a') - ORD('A');

    VAR j, length: CARDINAL;

    BEGIN
        length := LENGTH(string);
        IF length > 0 THEN
            FOR j := 0 TO length-1 DO
                IF string[j] IN CharSet {'A'..'Z'} THEN
                    INC (string[j], shift);
                END (*IF*);
            END (*FOR*);
        END (*IF*);
    END ToLower;

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
(*                   OPENING AND CLOSING A DATABASE                     *)
(************************************************************************)

PROCEDURE ProcessHeadRecord (DB: Database);

    (* The first level 0 record in the database should be a HEAD        *)
    (* record.  We extract this information just after opening the      *)
    (* database.  In the present version, the only subrecord that       *)
    (* interests us is the level 1 CHAR record, giving the character    *)
    (* set.  If that is missing, or it is a character set we can't yet  *)
    (* handle, we record an empty name for the character set.           *)

    VAR thisline: Line;  level: CARDINAL;

    BEGIN
        DB^.Charset[0] := Nul;
        TBSetPosition (DB^.TB, TBStartPosition(DB^.TB));
        level := GetOneLine (DB^.TB, thisline);
        IF (level = 0) AND HeadMatch (thisline, "HEAD") THEN
            LOOP
                level := GetOneLine (DB^.TB, thisline);
                IF level = 0 THEN
                    EXIT(*LOOP*);
                ELSIF (level = 1) AND HeadMatch (thisline, "CHAR") THEN
                    Strings.Assign (thisline, DB^.Charset);
                    EXIT (*LOOP*);
                END (*IF*);
            END (*LOOP*);
        END (*IF*);

        (* Convert IBMPC (which is ambiguous) and ANSEL (which we       *)
        (* can't yet handle) to the empty string.  The caller will then *)
        (* convert the empty string to a default which is decided at    *)
        (* a higher level.                                              *)

        IF DB^.Charset[0] <> Nul THEN
            IF Strings.Equal (DB^.Charset, "IBMPC")
                       OR Strings.Equal (DB^.Charset, "ANSEL") THEN
                DB^.Charset[0] := Nul;
            END (*IF*);
        END (*IF*);

    END ProcessHeadRecord;

(************************************************************************)

PROCEDURE OpenDatabase (VAR (*OUT*) DB: Database;
                            DatabaseName: FilenameString): BOOLEAN;

    (* Opens a GEDCOM file. *)

    VAR filename: FilenameString;
        p, previous: OpenedPtr;

    BEGIN
        ToLower (DatabaseName);

        p := AlreadyOpened;  previous := NIL;
        LOOP
            IF p = NIL THEN

                (* Open a new database. *)

                NEW (p);
                p^.next := NIL;
                IF previous = NIL THEN
                    AlreadyOpened := p;
                ELSE
                    previous^.next := p;
                END (*IF*);
                NEW (DB);  p^.this := DB;
                DB^.name := DatabaseName;
                Strings.Assign ("data\", filename);
                Strings.Append (DatabaseName, filename);
                Strings.Append (".GED", filename);
                DB^.TB := OpenForReading (filename);
                IF NOT TBFileOpened (DB^.TB) THEN
                    CloseTB (DB^.TB);
                    DISPOSE (DB);
                    p^.this := NIL;
                    RETURN FALSE;
                ELSE
                    DB^.index := OpenIndex (DB^.TB, DatabaseName);
                    ProcessHeadRecord (DB);
                    RETURN TRUE;
                END (*IF*);

            ELSIF Strings.Equal (p^.this^.name, DatabaseName) THEN

                (* This is an already opened database. *)

                DB := p^.this;
                RETURN TRUE;

            ELSE

                previous := p;  p := p^.next;

            END (*IF*);

        END (*LOOP*);

    END OpenDatabase;

(************************************************************************)

PROCEDURE CloseDatabase (VAR (*INOUT*) DB: Database);

    (* Closes a GEDCOM file. *)

    VAR p, previous: OpenedPtr;

    BEGIN
        (* Remove this entry from the AlreadyOpened list. *)

        p := AlreadyOpened;  previous := NIL;
        LOOP
            IF p = NIL THEN
                EXIT (*LOOP*);
            ELSIF p^.this = DB THEN
                IF previous = NIL THEN
                    AlreadyOpened := p^.next;
                ELSE
                    previous^.next := p^.next;
                END (*IF*);
                DISPOSE (p);
                EXIT (*LOOP*);
            ELSE
                previous := p;  p := p^.next;
            END (*IF*);
        END (*LOOP*);

        CloseTB (DB^.TB);
        CloseIndex (DB^.index);
        DISPOSE (DB);

    END CloseDatabase;

(************************************************************************)

PROCEDURE CharSetOf (DB: Database;  VAR (*OUT*) CharSetName: DataString);

    (* Returns the character encoding for this database. *)

    BEGIN
        Strings.Assign (DB^.Charset, CharSetName);
    END CharSetOf;

(************************************************************************)

PROCEDURE WriteDatabaseName (DB: Database);

    (* Writes the name of this database. *)

    BEGIN
        WriteString (DB^.name);
    END WriteDatabaseName;

(************************************************************************)

PROCEDURE CrudeSeek (DB: Database;  ID: IDString);

    (* This does the job of SeekToMatchingID (see below) in the case    *)
    (* where an index file lookup fails and we have to go through the   *)
    (* file from the beginning.                                         *)

    VAR thisline: Line;  level, pos: CARDINAL;
        tag: IDString;  found: BOOLEAN;

    BEGIN
        TBSetPosition (DB^.TB, TBStartPosition(DB^.TB));
        LOOP
            level := GetOneLine (DB^.TB, thisline);
            IF (thisline[0] = Nul) OR (thisline[0] = CtrlZ) THEN EXIT(*LOOP*) END(*IF*);
            IF level = 0 THEN
                IF thisline[0] = '@' THEN
                    Strings.FindNext ('@', thisline, 1, found, pos);
                    IF found THEN
                        Strings.Extract (thisline, 1, pos-1, tag);
                        IF ID[0] = Nul THEN
                            REPEAT
                                INC (pos);
                            UNTIL thisline[pos] <> ' ';
                            Strings.Delete (thisline, 0, pos);
                            IF HeadMatch (thisline, 'INDI') THEN
                                EXIT (*LOOP*);
                            END (*IF*);
                        ELSIF Match (tag, ID) THEN
                            EXIT (*LOOP*);
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
            END (*IF*);
        END (*LOOP*);
    END CrudeSeek;

(************************************************************************)

PROCEDURE SeekToMatchingID (DB: Database;  ID: IDString);

    (* Assumption: the file is already open.  We position the file to   *)
    (* the start of the line just after the level 0 line that           *)
    (* matches the ID.                                                  *)

    (* Special case: If ID is the null string then we match the         *)
    (* first INDI record in the file.                                   *)

    VAR thisline: Line;  level: CARDINAL;  found: BOOLEAN;

    BEGIN
        IF DB <> NIL THEN
            found := PositionToRecord (DB^.index, ID);
            IF found THEN
                level := GetOneLine (DB^.TB, thisline);
                found := level = 0;
            END (*IF*);
            IF NOT found THEN
                DeleteIndexFile (DB^.index);
                CrudeSeek (DB, ID);
            END (*IF*);
        END (*IF*);
    END SeekToMatchingID;

(************************************************************************)

PROCEDURE SavePosition (DB: Database): FilePos;

    (* Returns the current position in the file. *)

    BEGIN
        RETURN TBCurrentPosition (DB^.TB);
    END SavePosition;

(************************************************************************)

PROCEDURE RestorePosition (DB: Database;  pos: FilePos);

    (* Repositions the data file to the given position. *)

    BEGIN
        TBSetPosition (DB^.TB, pos);
    END RestorePosition;

(************************************************************************)

PROCEDURE NonEmpty (T: RecordTree): BOOLEAN;

    (* Returns TRUE iff T is not NIL. *)

    BEGIN
        RETURN T <> NIL;
    END NonEmpty;

(************************************************************************)
(*                       BUILDING A RECORD TREE                         *)
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

PROCEDURE LoadRecord (DB: Database;  VAR (*OUT*) RecTree: RecordTree);

    (* Assumption: the file is already positioned at the start of the   *)
    (* first level 1 line.  We load the level 1 and greater lines into  *)
    (* RecTree, stripping off the level numbers.                        *)

    VAR NextLevel: CARDINAL;  NextLine: Line;

    BEGIN
        IF DB = NIL THEN
            RecTree := NIL;
        ELSE
            NextLevel := GetOneLine (DB^.TB, NextLine);
            LoadRecordN (DB^.TB, RecTree, 1, NextLevel, NextLine);
        END (*IF*);
    END LoadRecord;

(************************************************************************)

PROCEDURE LoadRecord0 (DB: Database;  ID: IDString): RecordTree;

    (* We find the level 0 record with the given ID, and then load the  *)
    (* level 0 and greater lines into the result tree.  The ID is       *)
    (* stripped from the top-level record.                              *)

    VAR RecTree: RecordTree;  found: BOOLEAN;
        k: CARDINAL;

    BEGIN
        NEW (RecTree);
        found := PositionToRecord (DB^.index, ID);
        IF found THEN
            RecTree^.next := NIL;
            found := GetOneLine (DB^.TB, RecTree^.this) = 0;
        END (*IF*);
        IF found THEN
            k := LENGTH(ID) + 2;
            WHILE RecTree^.this[k] = ' ' DO
                INC (k);
            END (*WHILE*);
            Strings.Delete (RecTree^.this, 0, k);
            LoadRecord (DB, RecTree^.down);
        ELSE
            DeleteIndexFile (DB^.index);
            DISPOSE (RecTree);
        END (*IF*);
        RETURN RecTree;
    END LoadRecord0;

(************************************************************************)

PROCEDURE StartReading (DB: Database;  VAR (*OUT*) NextLevel: CARDINAL;
                                          VAR (*OUT*) Lookahead: Line);

    (* Initialisation needed before the first call to LoadNextRecord.   *)

    BEGIN
        TBSetPosition (DB^.TB, TBStartPosition(DB^.TB));
        NextLevel := GetOneLine (DB^.TB, Lookahead);
    END StartReading;

(************************************************************************)

PROCEDURE LoadNextINDIRecord (DB: Database;  VAR (*OUT*) ID: IDString;
                                  VAR (*OUT*) RecTree: RecordTree;
                                  VAR (*INOUT*) NextLevel: CARDINAL;
                                  VAR (*INOUT*) Lookahead: Line): BOOLEAN;

    (* Loads the next record for an individual into RecTree.  Non-INDI  *)
    (* records are skipped.  On successful return RecTree holds the     *)
    (* level 1 and higher lines of the record.  If there is no next     *)
    (* INDI record, we return FALSE and RecTree = NIL.                  *)
    (* Assumption: NextLevel and Lookahead have been "seeded" by a      *)
    (* previous call to LoadNextRecord or StartReading.                 *)

    VAR found, NoMore: BOOLEAN;  pos: CARDINAL;

    BEGIN
        found := FALSE;  NoMore := FALSE;
        REPEAT
            IF NextLevel = MAX(CARDINAL) THEN
                RecTree := NIL;
                NoMore := TRUE;
            ELSIF NextLevel = 0 THEN
                IF HeadMatch (Lookahead, 'TRLR') THEN
                    RecTree := NIL;
                    NoMore := TRUE;
                ELSIF Lookahead[0] = '@' THEN
                    Strings.FindNext ('@', Lookahead, 1, found, pos);
                    IF NOT found THEN
                        pos := LENGTH(Lookahead);
                    END (*IF*);
                    Strings.Extract (Lookahead, 1, pos-1, ID);
                    IF pos < LENGTH(Lookahead) THEN
                        REPEAT
                            INC (pos);
                        UNTIL Lookahead[pos] <> ' ';
                    END (*IF*);
                    Strings.Delete (Lookahead, 0, pos);
                    IF HeadMatch (Lookahead, 'INDI') THEN
                        NextLevel := GetOneLine (DB^.TB, Lookahead);
                        LoadRecordN (DB^.TB, RecTree, 1, NextLevel, Lookahead);
                        found := TRUE;
                    ELSE
                        found := FALSE;
                    END (*IF*);
                END (*IF*);
            END (*IF*);
            IF NOT found THEN
                NextLevel := GetOneLine (DB^.TB, Lookahead);
            END (*IF*);
        UNTIL found OR NoMore;
        RETURN found;
    END LoadNextINDIRecord;

(************************************************************************)
(*                      ELEMENTARY TREE OPERATIONS                      *)
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

PROCEDURE CopyTree (RecTree: RecordTree): RecordTree;

    (* Creates a duplicate of the tree. *)

    VAR result: RecordTree;

    BEGIN
        IF RecTree = NIL THEN
            result := NIL;
        ELSE
            NEW (result);
            result^.this := RecTree^.this;
            result^.down := CopyTree (RecTree^.down);
            result^.next := CopyTree (RecTree^.next);
        END (*IF*);
        RETURN result;
    END CopyTree;

(************************************************************************)

PROCEDURE DisplayRawLinesN (T: RecordTree;  N: CARDINAL);

    (* Displays the contents of the tree, labelled with level N and     *)
    (* below.                                                           *)

    VAR j: CARDINAL;

    BEGIN
        WHILE T <> NIL DO
            WriteString ("<br>");
            FOR j := 1 TO 4*N DO
                WriteString ("&nbsp;");
            END (*FOR*);
            WriteString (T^.this);
            WriteLn;
            DisplayRawLinesN (T^.down, N+1);
            T := T^.next;
        END (*WHILE*);
    END DisplayRawLinesN;

(************************************************************************)

PROCEDURE DisplayRawLines (RecTree: RecordTree);

    (* Displays the contents of the tree. *)

    BEGIN
        DisplayRawLinesN (RecTree, 1);
    END DisplayRawLines;

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

PROCEDURE ExtractEither (kwd1, kwd2: ARRAY OF CHAR;
                            VAR (*INOUT*) T: RecordTree;
                            VAR (*OUT*) data: Line;
                            VAR (*OUT*) subtree: RecordTree;
                            VAR (*OUT*) which: CARDINAL): BOOLEAN;

    (* Like ExtractSubrecord, but finds the first keyword from two      *)
    (* possible candidates.  The output parameter is returned as 0 if   *)
    (* neither is found; as 1 if kwd1 is found; and as 2 if kwd2 is     *)
    (* found.                                                           *)

    VAR previous, current: RecordTree;

    BEGIN
        previous := NIL;  current := T;  which := 0;
        LOOP
            IF current = NIL THEN
                data[0] := Nul;  subtree := NIL;
                RETURN FALSE;
            END (*IF*);
            data := current^.this;
            IF HeadMatch (data, kwd1) THEN which := 1
            ELSIF HeadMatch (data, kwd2) THEN which := 2
            END (*IF*);
            IF which <> 0 THEN
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
    END ExtractEither;

(************************************************************************)

PROCEDURE CopySubrecord (keyword: ARRAY OF CHAR;
                            T: RecordTree;
                            VAR (*OUT*) data: Line;
                            VAR (*OUT*) subtree: RecordTree): BOOLEAN;

    (* Like ExtractSubrecord, but leaves the record in the tree and     *)
    (* returns a copy.                                                  *)

    VAR current: RecordTree;

    BEGIN
        current := T;
        LOOP
            IF current = NIL THEN
                data[0] := Nul;  subtree := NIL;
                RETURN FALSE;
            END (*IF*);
            data := current^.this;
            IF HeadMatch (data, keyword) THEN
                subtree := CopyTree (current^.down);
                RETURN TRUE;
            END (*IF*);
            current := current^.next;
        END (*LOOP*);
    END CopySubrecord;

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

PROCEDURE CopyField (keyword: ARRAY OF CHAR;  T: RecordTree;
                                  VAR (*OUT*) data: DataString): BOOLEAN;

    (* Like GetField, but the original tree T is left unchanged. *)

    VAR subtree: RecordTree;

    BEGIN
        IF CopySubrecord (keyword, T, data, subtree) THEN
            DiscardTree (subtree);
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END CopyField;

(************************************************************************)

PROCEDURE ProcessName (VAR (*INOUT*) name: DataString);

    (* Handles the '/' delimiters in name. *)

    VAR j1, j2: CARDINAL;  found: BOOLEAN;

    BEGIN

        (* Uppercase the part between '/' delimiters.         *)
        (* ALTERATION: For now I've disabled the uppercasing, *)
        (* to avoid problems with non-ASCII characters.       *)

        Strings.FindNext ('/', name, 0, found, j1);
        IF found THEN
            Strings.Delete (name, j1, 1);
            IF (j1 > 0) AND (name[j1-1] <> ' ') THEN
                Strings.Insert (' ', j1, name);
                INC (j1);
            END (*IF*);
            Strings.FindPrev ('/', name, LENGTH(name)-1, found, j2);
            IF found THEN
                Strings.Delete (name, j2, 1);
            ELSE
                j2 := LENGTH(name);
            END (*IF*);
            (*
            IF j2 > j1 THEN
                FOR j := j1 TO j2-1 DO
                    name[j] := CAP(name[j]);
                END (*FOR*);
            END (*IF*);
            *)
        END (*IF*);
    END ProcessName;

(************************************************************************)

PROCEDURE GetName (VAR (*INOUT*) T: RecordTree;
                             VAR (*OUT*) name: DataString): BOOLEAN;

    (* Removes the NAME record from T, returns the name. *)

    BEGIN
        IF GetField ('NAME', T, name) THEN
            ProcessName (name);
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END GetName;

(************************************************************************)

PROCEDURE CopyName (T: RecordTree;  VAR (*OUT*) name: DataString): BOOLEAN;

    (* Like GetName, but leaves T unaltered. *)

    BEGIN
        IF CopyField ('NAME', T, name) THEN
            ProcessName (name);
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END CopyName;

(************************************************************************)
(*                          DATE OPERATIONS                             *)
(************************************************************************)

PROCEDURE GetYear (T: RecordTree;  keyword: ARRAY OF CHAR;
                                   VAR (*OUT*) result: DataString);

    (* Finds a record in T that matches the keyword, then gets the      *)
    (* year part of the DATE subrecord.  The original tree is not       *)
    (* altered.                                                         *)

    VAR subtree: RecordTree;  pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        IF CopySubrecord (keyword, T, result, subtree) THEN

            IF GetField ('DATE', subtree, result) THEN

                (* Here's the hard work.  For now I'll take the easy    *)
                (* way out and find the last substring, but this needs  *)
                (* to be improved.                                      *)

                Strings.FindPrev (' ', result, LENGTH(result), found, pos);
                IF found THEN
                    Strings.Delete (result, 0, pos);
                END (*IF*);

            ELSE
                result[0] := Nul;
            END (*IF*);
            DiscardTree (subtree);

        END (*IF*);
    END GetYear;

(************************************************************************)

PROCEDURE GetDateRange (T: RecordTree;  VAR (*OUT*) range: DataString);

    (* Sets range to a string 'birth year' - 'death year'.  The *)
    (* original tree is not altered.                            *)

    VAR born, died: DataString;

    BEGIN
        GetYear (T, 'BIRT', born);
        GetYear (T, 'DEAT', died);
        IF born[0] = Nul THEN
            IF died[0] = Nul THEN
                range[0] := Nul;
            ELSE
                range := "- ";
                Strings.Append (died, range);
            END (*IF*);
        ELSE
            range := born;
            Strings.Append (" - ", range);
            Strings.Append (died, range);
        END (*IF*);
    END GetDateRange;

(************************************************************************)
(*                    WRITING A LINK TO STANDARD OUTPUT                 *)
(************************************************************************)

PROCEDURE WritePersonLink2 (lang: LangHandle;  DB: Database;
                               PersonID: IDString;  AddDate: BOOLEAN;
                                    VAR (*INOUT*) PersonData: RecordTree);

    (* Like WritePersonLink, but for the case where the caller  *)
    (* has already loaded the information for this person into  *)
    (* PersonData.                                              *)

    VAR HisName: DataString;  private: BOOLEAN;

    BEGIN
        private := GetField ('RESN PRIVACY', PersonData, HisName);

            (* Privacy restriction: we can report the name but not      *)
            (* provide a link.                                          *)

        IF NOT private THEN
            WriteString ('<a href="wft.cmd?D=');
            WriteString (DB^.name);
            WriteString (';P=');
            WriteString (PersonID);
            WriteString ('">');
        END (*IF*);

        (* Now fill in the person's name. *)

        IF (NOT GetName (PersonData, HisName)) OR (HisName[0] = Nul) THEN
            StrToBuffer (lang, "GELookup.NameNotRecorded", HisName);
        END (*IF*);
        WriteString (HisName);

        IF NOT private THEN
            WriteString ('</a>');
        END (*IF*);

        IF AddDate THEN
            GetDateRange (PersonData, HisName);
            WriteString ("&nbsp;&nbsp;");
            WriteString (HisName);
        END (*IF*);

    END WritePersonLink2;

(************************************************************************)

PROCEDURE WritePersonLink (lang: LangHandle;  DB: Database;
                                 PersonID: IDString);

    (* Turns PersonID into an HTML reference, and writes it to  *)
    (* standard output.                                         *)

    VAR T: RecordTree;

    BEGIN
        SeekToMatchingID (DB, PersonID);
        LoadRecord (DB, T);
        WritePersonLink2 (lang, DB, PersonID, FALSE, T);
        DiscardTree (T);
    END WritePersonLink;

(************************************************************************)

BEGIN
    AlreadyOpened := NIL;
END GELookup.

