IMPLEMENTATION MODULE GEDLook;

        (********************************************************)
        (*                                                      *)
        (*       Modified version of module GELookup for        *)
        (*                 use in Ged2HTML                      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            30 August 2005                  *)
        (*  Last edited:        17 February 2009                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD16;

IMPORT Strings;

FROM OurTypes IMPORT
    (* const*)  LineLength,
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

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM FileOps IMPORT
    (* type *)  ChanId, FilePos,
    (* proc *)  FWriteString, FWriteLn;

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
                       UsesANSEL: BOOLEAN;
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

        (* Convert non-MIME character sets to the nearest MIME          *)
        (* equivalent.                                                  *)
        (*       IBMPC -> empty string                                  *)
        (*       ANSI  -> Windows-1252                                  *)
        (*       ANSEL -> UTF-8                                         *)
        (*       LATIN1 -> ISO-8859-1                                   *)
        (*       UNICODE -> (not yet handled)                           *)
        (* In the IBMPC case the caller will convert the empty string   *)
        (* to a default which is decided at a higher level. In the      *)
        (* ANSEL case, DB^.UsesANSEL will cause an ANSEL-to-UTF8        *)
        (* translation to be invoked when a GEDCOM line is fetched.     *)

        IF DB^.Charset[0] <> Nul THEN
            IF Strings.Equal (DB^.Charset, "IBMPC") THEN
                DB^.Charset[0] := Nul;
            ELSIF Strings.Equal (DB^.Charset, "LATIN1") THEN
                Strings.Assign ("ISO-8859-1", DB^.Charset);
            ELSIF Strings.Equal (DB^.Charset, "ANSI") THEN
                Strings.Assign ("windows-1252", DB^.Charset);
            ELSIF Strings.Equal (DB^.Charset, "ANSEL") THEN
                Strings.Assign ("UTF-8", DB^.Charset);
                DB^.UsesANSEL := TRUE;
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
                Strings.Assign (DatabaseName, filename);
                Strings.Append (".GED", filename);
                DB^.TB := OpenForReading (filename, TRUE);
                IF NOT TBFileOpened (DB^.TB) THEN
                    WriteString ("Cannot open file ");
                    WriteString (filename);
                    WriteLn;
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
(*                        DEALING WITH ANSEL                            *)
(************************************************************************)

CONST
    SpecialCaseTableSize = 53;

TYPE
    SpecialCaseTableType = ARRAY [0..SpecialCaseTableSize-1] OF
                               RECORD
                                   ch1, ch2: CHAR;
                                   unicode: CARD16;
                               END (*RECORD*);

CONST
    grave = CHR(0E1H);
    acute = CHR(0E2H);
    circum = CHR(0E3H);
    tilde = CHR(0E4H);
    umlaut = CHR(0E8H);
    ring = CHR(0EAH);
    cedilla = CHR(0F0H);

    SCT = SpecialCaseTableType{
                {grave, 'A', 00C0H},
                {grave, 'E', 00C8H},
                {grave, 'I', 00CCH},
                {grave, 'O', 00D2H},
                {grave, 'U', 00D9H},
                {grave, 'a', 00E0H},
                {grave, 'e', 00E8H},
                {grave, 'i', 00ECH},
                {grave, 'o', 00F2H},
                {grave, 'u', 00F9H},

                {acute, 'A', 00C1H},
                {acute, 'E', 00C9H},
                {acute, 'I', 00CDH},
                {acute, 'O', 00D3H},
                {acute, 'U', 00DAH},
                {acute, 'Y', 00DDH},
                {acute, 'a', 00E1H},
                {acute, 'e', 00E9H},
                {acute, 'i', 00EDH},
                {acute, 'o', 00F3H},
                {acute, 'u', 00FAH},
                {acute, 'y', 00FDH},

                {circum, 'A', 00C2H},
                {circum, 'E', 00CAH},
                {circum, 'I', 00CEH},
                {circum, 'O', 00D4H},
                {circum, 'U', 00DBH},
                {circum, 'a', 00E2H},
                {circum, 'e', 00EAH},
                {circum, 'i', 00EEH},
                {circum, 'o', 00F4H},
                {circum, 'u', 00FBH},

                {tilde, 'A', 00C3H},
                {tilde, 'N', 00D1H},
                {tilde, 'O', 00D5H},
                {tilde, 'a', 00E3H},
                {tilde, 'n', 00F1H},
                {tilde, 'o', 00F5H},

                {umlaut, 'A', 00C4H},
                {umlaut, 'E', 00CBH},
                {umlaut, 'I', 00CFH},
                {umlaut, 'O', 00D6H},
                {umlaut, 'U', 00DCH},
                {umlaut, 'a', 00E4H},
                {umlaut, 'e', 00EBH},
                {umlaut, 'i', 00EFH},
                {umlaut, 'o', 00F6H},
                {umlaut, 'u', 00FCH},
                {umlaut, 'y', 00FFH},

                {ring, 'A', 00C5H},
                {ring, 'a', 00E5H},

                {cedilla, 'C', 00C7H},
                {cedilla, 'c', 00E7H}
                };

(************************************************************************)

PROCEDURE SpecialCase (prefix, nextch: CHAR;
                               VAR (*OUT*) code: CARD16): BOOLEAN;

    (* Deals with accented characters that are represented in ANSEL by  *)
    (* a prefix followed by the unaccented character.  If we can        *)
    (* translate this to a single Unicode character, we return the      *)
    (* Unicode value in code and return TRUE.  A return value of FALSE  *)
    (* means that we're leaving the job to the caller.                  *)

    VAR k: CARDINAL;  found: BOOLEAN;

    BEGIN
        found := FALSE;
        IF ORD(prefix) >= 0E0H THEN
            k := 0;
            WHILE (k < SpecialCaseTableSize) AND NOT found DO
                IF SCT[k].ch1 > prefix THEN
                    k := SpecialCaseTableSize;
                ELSIF (SCT[k].ch1 < prefix) OR (SCT[k].ch2 < nextch) THEN
                    INC (k);
                ELSIF SCT[k].ch2 > nextch THEN
                    k := SpecialCaseTableSize;
                ELSE
                    code := SCT[k].unicode;
                    found := TRUE;
                END (*IF*);
            END (*WHILE*);
        END (*IF*);
        RETURN found;
    END SpecialCase;

(************************************************************************)

TYPE ANSELTransTable = ARRAY [CHR(0A0H)..CHR(0FFH)] OF CARD16;

CONST ANSELtoUnicode = ANSELTransTable{
          0000H, 0141H, 00D8H, 0110H, 00DEH, 00C6H, 0152H, 02B9H,
          00B7H, 266DH, 00AEH, 00B1H, 01A0H, 01AFH, 02BCH, 0000H,
          02BBH, 0142H, 00F8H, 0111H, 00FEH, 00E6H, 0153H, 02BAH,
          0131H, 00A3H, 00F0H, 0000H, 01A1H, 01B0H, 0000H, 0000H,
          00B0H, 2113H, 2117H, 00A9H, 266FH, 00BFH, 00A1H, 0000H,
          0000H, 0000H, 0000H, 0000H, 0000H, 0000H, 0000H, 00DFH,
          0000H, 0000H, 0000H, 0000H, 0000H, 0000H, 0000H, 0000H,
          0000H, 0000H, 0000H, 0000H, 0000H, 0000H, 0000H, 0000H,
          0309H, 0300H, 0301H, 0302H, 0303H, 0304H, 0306H, 0307H,
          0308H, 030CH, 030AH, 0FE20H, 0FE21H, 0315H, 030BH, 0310H,
          0327H, 0328H, 0323H, 0324H, 0325H, 0333H, 0332H, 0326H,
          031CH, 032EH, 0FE22H, 0FE23H, 0000H, 0000H, 0313H, 0000H};

(************************************************************************)

PROCEDURE TranslateFromANSEL (VAR (*INOUT*) str: Line);

    (* Translates str from ANSEL to UTF-8. *)

    VAR srcline: Line;
        j, k: CARDINAL;
        code, temp: CARD16;
        ch, prefixcode: CHAR;
        needsuffix: BOOLEAN;

    BEGIN
        Strings.Assign (str, srcline);
        j := 0;  k := 0;
        prefixcode := Nul;
        needsuffix := FALSE;
        WHILE (k < LineLength-3) AND (srcline[j] <> Nul) DO
            temp := 0;
            IF needsuffix THEN
                ch := prefixcode;
                needsuffix := FALSE;
            ELSE
                ch := srcline[j];  INC(j);
                IF ORD(ch) >= 0E0H THEN
                    IF NOT SpecialCase (ch, srcline[j], temp) THEN
                        needsuffix := TRUE;
                        prefixcode := ch;
                        ch := srcline[j];
                    END (*IF*);
                    INC (j);
                END (*IF*);
            END (*IF*);

            IF ORD(ch) < 0A0H THEN
                str[k] := ch;
            ELSIF ORD(ch) = 0 THEN

                (* Special code for Nul. *)

                str[k] := CHR(0C0H);
                INC (k);
                str[k] := CHR(080H);

            ELSE

                IF temp <> 0 THEN
                    code := temp;
                ELSE
                    code := ANSELtoUnicode[ch];
                END (*IF*);

                IF code = 0 THEN

                    (* Unknown code, replace it with '?' *)
                    str[k] := '?';

                ELSIF code < 0800H THEN
                    (* An 11-bit number. *)

                    temp := code DIV 16; (* A 7-bit number *)
                    str[k] := CHR(0C0H + (temp DIV 4)); (* first 5 bits *)
                    INC (k);
                    str[k] := CHR(080H + 16*(temp MOD 4) + (code MOD 16));
                                                        (* last 6 bits *)
                ELSE
                    (* A 16-bit number. *)

                    temp := code DIV 16;  (* A 12-bit number *)
                    str[k] := CHR(0E0H + (temp DIV 256)); (* first 4 bits *)
                    INC (k);
                    temp := temp MOD 256;  (* An 8-bit number *)
                    str[k] := CHR(080H + (temp DIV 4)); (* next 6 bits *)
                    INC (k);
                    str[k] := CHR(080H + 16*(temp MOD 4) + (code MOD 16));
                                                        (* last 6 bits *)
                END (*IF*);
            END (*IF*);
            INC (k);
        END (*WHILE*);
        IF k < LineLength THEN
            str[k] := Nul;
        END (*IF*);
    END TranslateFromANSEL;

(************************************************************************)

PROCEDURE GetOneLineA (TB: Buffer;  VAR (*OUT*) result: Line;  UsesANSEL: BOOLEAN): CARDINAL;

    (* Reads one line, translates from ANSEL to UTF-8 if required,      *)
    (* trims the level from the front, and returns the level as the     *)
    (* function result.                                                 *)

    VAR level: CARDINAL;

    BEGIN
        level := GetOneLine (TB, result);
        IF UsesANSEL THEN
            TranslateFromANSEL (result);
        END (*IF*);
        RETURN level;
    END GetOneLineA;

(************************************************************************)
(*                       BUILDING A RECORD TREE                         *)
(************************************************************************)

PROCEDURE LoadRecordN (TB: Buffer;  VAR (*OUT*) RecTree: RecordTree;
                       N: CARDINAL;  VAR (*INOUT*) NextLevel: CARDINAL;
                       UseANSEL: BOOLEAN;
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
                NextLevel := GetOneLineA (TB, NextLine, UseANSEL);

            END (*IF*);

            IF previous = NIL THEN
                RecTree := current;
            ELSE
                previous^.next := current;
            END (*IF*);

            (* Add the lower-level records, if any. *)

            LoadRecordN (TB, current^.down, N+1, NextLevel, UseANSEL, NextLine);

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
            NextLevel := GetOneLineA (DB^.TB, NextLine, DB^.UsesANSEL);
            LoadRecordN (DB^.TB, RecTree, 1, NextLevel, DB^.UsesANSEL, NextLine);
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
                        NextLevel := GetOneLineA (DB^.TB, Lookahead, DB^.UsesANSEL);
                        LoadRecordN (DB^.TB, RecTree, 1, NextLevel, DB^.UsesANSEL, Lookahead);
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

PROCEDURE DisplayRawLinesN (cid: ChanId;  T: RecordTree;  N: CARDINAL);

    (* Displays the contents of the tree, labelled with level N and     *)
    (* below.                                                           *)

    VAR j: CARDINAL;

    BEGIN
        WHILE T <> NIL DO
            FWriteString (cid, "<br>");
            FOR j := 1 TO 4*N DO
                FWriteString (cid, "&nbsp;");
            END (*FOR*);
            FWriteString (cid, T^.this);
            FWriteLn (cid);
            DisplayRawLinesN (cid, T^.down, N+1);
            T := T^.next;
        END (*WHILE*);
    END DisplayRawLinesN;

(************************************************************************)

PROCEDURE DisplayRawLines (cid: ChanId;  RecTree: RecordTree);

    (* Writes the contents of the tree. *)

    BEGIN
        DisplayRawLinesN (cid, RecTree, 1);
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

PROCEDURE WritePersonLink2 (cid: ChanId;  lang: LangHandle;  DB: Database;
                               PersonID: IDString;
                                 AddDate, external: BOOLEAN;
                                    VAR (*INOUT*) PersonData: RecordTree);

    (* Like WritePersonLink, but for the case where the caller  *)
    (* has already loaded the information for this person into  *)
    (* PersonData.                                              *)

    VAR HisName: DataString;  private: BOOLEAN;

    BEGIN
        private := GetField ('RESN PRIVACY', PersonData, HisName);

            (* Privacy restriction: we can report the name but  *)
            (* not provide a link.                              *)

        IF NOT private THEN
            FWriteString (cid, '<a href=');
            IF external THEN
                FWriteString (cid, DB^.name);
                FWriteString (cid, '.html');
            END (*IF*);
            FWriteString (cid, '#');
            FWriteString (cid, PersonID);
            FWriteString (cid, '>');
        END (*IF*);

        (* Now fill in the person's name. *)

        IF (NOT GetName (PersonData, HisName)) OR (HisName[0] = Nul) THEN
            StrToBuffer (lang, "GELookup.NameNotRecorded", HisName);
        END (*IF*);
        FWriteString (cid, HisName);

        IF NOT private THEN
            FWriteString (cid, '</a>');
        END (*IF*);

        IF AddDate THEN
            GetDateRange (PersonData, HisName);
            FWriteString (cid, "&nbsp;&nbsp;");
            FWriteString (cid, HisName);
        END (*IF*);

    END WritePersonLink2;

(************************************************************************)

PROCEDURE WritePersonLink (cid: ChanId;  lang: LangHandle;  DB: Database;
                                 PersonID: IDString; external: BOOLEAN);

    (* Turns PersonID into an HTML reference, and writes it to  *)
    (* standard output.                                         *)

    VAR T: RecordTree;

    BEGIN
        SeekToMatchingID (DB, PersonID);
        LoadRecord (DB, T);
        WritePersonLink2 (cid, lang, DB, PersonID, FALSE, external, T);
        DiscardTree (T);
    END WritePersonLink;

(************************************************************************)

BEGIN
    AlreadyOpened := NIL;
END GEDLook.

