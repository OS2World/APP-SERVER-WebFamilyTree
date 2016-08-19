IMPLEMENTATION MODULE Person;

        (********************************************************)
        (*                                                      *)
        (*      Web Family Tree: module to look up and          *)
        (*           display the data for a person              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            6 July 2001                     *)
        (*  Last edited:        18 July 2012                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM OurTypes IMPORT
    (* type *)  IDString, DataString, FilenameString, Line;

FROM GELookup IMPORT
    (* const*)  progname,
    (* type *)  Database, RecordTree,
    (* proc *)  OpenDatabase, CloseDatabase,  CharSetOf,
                SeekToMatchingID, LoadRecord0, LoadRecord, DiscardTree,
                CopyTree, CopyField, CopyName, HeadMatch,
                GetName, GetField, ExtractSubrecord, ExtractEither,
                GetContinuation, DisplayRawLines, GetDateRange,
                WritePersonLink, WritePersonLinkClipped, WritePersonLink2,
                RemoveUnwantedRecords, WriteDatabaseName,
                StartReading, LoadNextINDIRecord, NonEmpty,
                SavePosition, RestorePosition;

FROM FileOps IMPORT
    (* type *)  FilePos;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferA, StrToBufferAB, StrToBufferN;

FROM LowLevel IMPORT
    (* proc *)  EVAL, Assert;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);

TYPE
    (* Information about one person.                       *)
    (*      ID      the ID in the GEDCOM file              *)
    (*      DB      the database we're looking up          *)
    (*      Tree    result of looking up the GEDCOM file   *)

    PersonData = POINTER TO RECORD
                     ID: IDString;
                     DB: Database;
                     Tree: RecordTree;
                 END (*RECORD*);

    (* Records in the 'to do' list of those procedures that replace     *)
    (* recursion by a job list.  The 'family' field, used only when     *)
    (* listing all descendants, is TRUE when this is a family record    *)
    (* rather than a person record.  In that case prevfemale records    *)
    (* the fact that the spouse we've already listed from that family   *)
    (* is female, so that we can now figure out the other spouse.       *)

    JobPtr = POINTER TO JobRecord;
    JobRecord = RECORD
                    next: JobPtr;
                    DB: Database;
                    ID: IDString;
                    level: CARDINAL;
                    prefix: DataString;
                    tree: RecordTree;
                    family: BOOLEAN;
                    prevfemale: BOOLEAN;
                END (*RECORD*);

    (* Date and place of some event. *)

    DatePlace = RECORD
                    empty: BOOLEAN;
                    date: DataString;
                    place: DataString;
                END (*RECORD*);

    (* Linear list of databases. *)

    DBList = POINTER TO RECORD
                            next: DBList;
                            this: Database;
                        END (*RECORD*);

    (* ID of a message to look up in the language database. *)

    MsgID = ARRAY [0..31] OF CHAR;

(************************************************************************)

PROCEDURE TreeForPerson (P: PersonData): RecordTree;

    (* Translates a person to a record tree. *)

    BEGIN
        RETURN P^.Tree;
    END TreeForPerson;

(************************************************************************)

PROCEDURE GetPersonData (DatabaseName: FilenameString;  PersonID: IDString;
                                  VAR (*OUT*) result: PersonData): BOOLEAN;

    (* Looks up the database for person with identifier PersonID, fills *)
    (* the result record with the information.                          *)

    BEGIN
        NEW (result);
        result^.ID := PersonID;
        IF NOT OpenDatabase (result^.DB, DatabaseName) THEN
            DISPOSE (result);
            RETURN FALSE;
        END (*IF*);
        IF PersonID[0] = Nul THEN
            result^.Tree := NIL;
        ELSE
            SeekToMatchingID (result^.DB, PersonID);
            LoadRecord (result^.DB, result^.Tree);
        END (*IF*);
        RETURN TRUE;
    END GetPersonData;

(**********************************************************************)

PROCEDURE OurCharSet (data: PersonData;
                      VAR (*OUT*) CharSetName: DataString);

    (* Returns the character encoding used by this database.  *)

    BEGIN
        IF data = NIL THEN
            CharSetName[0] := Nul;
        ELSE
            CharSetOf (data^.DB, CharSetName);
        END (*IF*);
    END OurCharSet;

(**********************************************************************)

PROCEDURE DiscardPersonData (VAR (*INOUT*) data: PersonData);

    (* Disposes of the PersonData information. *)

    BEGIN
        IF data <> NIL THEN
            CloseDatabase (data^.DB);
            DiscardTree (data^.Tree);
            DISPOSE (data);
        END (*IF*);
    END DiscardPersonData;

(************************************************************************)

PROCEDURE ExtractDateAndPlace (VAR (*INOUT*) info: RecordTree;
                               VAR (*OUT*) result: DatePlace);

    (* On entry, info is supposed to contain date and place information.*)
    (* We extract this to result, and dispose of the tree.              *)

    BEGIN
        WITH result DO
            empty := TRUE;
            date[0] := Nul;
            place[0] := Nul;
        END (*WITH*);
        EVAL (GetField ('DATE', info, result.date));
        EVAL (GetField ('PLAC', info, result.place));
        DiscardTree (info);
        WITH result DO
            empty := (date[0] = Nul) AND (place[0] = Nul);
        END (*WITH*);
    END ExtractDateAndPlace;

(***********************************************************************)

PROCEDURE SendLine (line: ARRAY OF CHAR);

    (* Sends a string, terminated with CRLF, to standard output. *)

    BEGIN
        WriteString (line);  WriteLn;
    END SendLine;

(************************************************************************)

PROCEDURE TranslateOneDate (lang: LangHandle;  VAR (*INOUT*) date: DataString);

    (* Changes date to a language-dependent string.  We're restricted   *)
    (* to Gregorian dates, where the month is written as a three-letter *)
    (* abbreviation that corresponds to the standard English-language   *)
    (* month abbreviations. My feeling is that French revolutionary     *)
    (* dates and Hebrew dates (which the GEDCOM 5.5 standard somewhat   *)
    (* naively assumes will be written in Roman characters rather than  *)
    (* in Hebrew script) should not be translated.                      *)
    (* There is no provision for translation into American month-first  *)
    (* notation because I don't know of any way of detecting that the   *)
    (* person reading a web page is American.                           *)

    TYPE
        NumericMonth = [1..12];
        EnglishMonthNames = ARRAY NumericMonth OF ARRAY [0..2] OF CHAR;

    CONST
        EnglishMonths = EnglishMonthNames {'JAN', 'FEB', 'MAR', 'APR',
                                           'MAY', 'JUN', 'JUL', 'AUG',
                                           'SEP', 'OCT', 'NOV', 'DEC'};

    (********************************************************************)

    PROCEDURE FindMonthName (VAR (*OUT*) mm: CARDINAL;
                             VAR (*OUT*) pos: CARDINAL): BOOLEAN;

        (* If the month name is found, the month number is returned in  *)
        (* mm and the position in date is returned in pos.  If the      *)
        (* function result is FALSE, mm and pos are meaningless.        *)

        VAR k: [0..2];  N: CARDINAL;

        BEGIN
            N := Strings.Length(date);
            IF N < 3 THEN
               RETURN FALSE;
            END (*IF*);
            mm := 1;
            LOOP
                (* Outer loop, once per month. *)
                pos := 0;
                LOOP
                    (* This loop is once per pos value. *)
                    k := 0;
                    LOOP
                        IF CAP(date[pos+k]) <> EnglishMonths[mm][k] THEN
                            EXIT (*LOOP*);
                        END (*IF*);
                        IF k = 2 THEN
                            RETURN TRUE;
                        END (*IF*);
                        INC (k);
                    END (*LOOP*);
                    IF pos >= N-3 THEN
                       EXIT (*LOOP*);
                    END (*IF*);
                    INC (pos);
                END (*LOOP*);
                IF mm = 12 THEN
                   RETURN FALSE;
                END (*IF*);
                INC (mm);
            END (*LOOP*);
        END FindMonthName;

    (********************************************************************)

    VAR mm, pos: CARDINAL;
        code: ARRAY [0..10] OF CHAR;
        monthname: ARRAY [0..255] OF CHAR;
        ch: ARRAY [0..0] OF CHAR;

    BEGIN
        IF FindMonthName(mm, pos) THEN
            code := "month.";
            IF mm > 9 THEN
                Strings.Append ("1", code);
                DEC (mm, 10);
            END (*IF*);
            ch[0] := CHR(ORD('0')+mm);
            Strings.Append (ch, code);
            StrToBuffer (lang, code, monthname);
            Strings.Delete (date, pos, 3);
            Strings.Insert (monthname, pos, date);
        END (*IF*);
    END TranslateOneDate;

(************************************************************************)

PROCEDURE TranslateDate (lang: LangHandle;  VAR (*INOUT*) date: DataString);

    (* Translates a DATE_VALUE from GEDCOM notation to a natural        *)
    (* language string.                                                 *)

    (********************************************************************)

    PROCEDURE DetectKeyword(): CARDINAL;

        (* Looks for one of the keywords that can come at the beginning *)
        (* of a DATE_VALUE.                                             *)

        TYPE
            KwdNum = [1..8];
            KwdNames = ARRAY KwdNum OF ARRAY [0..3] OF CHAR;

        CONST
            KwdList = KwdNames {'ABT', 'CAL', 'EST', 'FROM',
                                               'TO', 'BEF', 'AFT', 'BET'};

        (********************************************************************)

        VAR code: CARDINAL;  found: BOOLEAN;

        BEGIN
            code := 0;
            REPEAT
                INC (code);
                found := HeadMatch (date, KwdList[code]);
            UNTIL found OR (code >= MAX(KwdNum));
            IF NOT found THEN
                code := 0;
            END (*IF*);
            RETURN code;
        END DetectKeyword;

    (********************************************************************)

    VAR code, pos: CARDINAL;
        found: BOOLEAN;
        date1, date2: DataString;

    BEGIN
        (* In all cases below we'll have to translate at least one      *)
        (* simple date, so we might as well do it now.                  *)

        TranslateOneDate (lang, date);

        (* Check for the special-case keywords. *)

        code := DetectKeyword();
        WHILE date[0] = ' ' DO
            Strings.Delete (date, 0, 1);
        END (*WHILE*);
        Strings.Assign (date, date1);
        CASE code OF
            | 1: (* 'ABT' *);
                 StrToBufferA (lang, "Date.about", date1, date);

            | 2: (* 'CAL' *);
                 StrToBufferA (lang, "Date.calculated", date1, date);

            | 3: (* 'EST' *);
                 StrToBufferA (lang, "Date.estimated", date1, date);

            | 4: (* 'FROM' *);
                 Strings.FindNext ("TO", date1, 0, found, pos);
                 IF found THEN
                     Strings.Assign (date1, date2);
                     date1[pos] := Nul;
                     INC (pos, 2);
                     WHILE date2[pos] = ' ' DO
                         INC (pos);
                     END (*WHILE*);
                     IF pos > 0 THEN
                         Strings.Delete (date2, 0, pos);
                     END (*IF*);
                     TranslateOneDate (lang, date2);
                     pos := Strings.Length (date1);
                     WHILE (pos > 0) AND (date1[pos-1] = ' ') DO
                         DEC (pos);
                         date1[pos] := Nul;
                     END (*WHILE*);
                     StrToBufferAB (lang, "Date.fromto", date1, date2, date);
                 ELSE
                     StrToBufferA (lang, "Date.from", date1, date);
                 END (*IF*);

            | 5: (* 'TO' *);
                 StrToBufferA (lang, "Date.to", date1, date);

            | 6: (* 'BEF' *);
                 StrToBufferA (lang, "Date.before", date1, date);

            | 7: (* 'AFT' *);
                 StrToBufferA (lang, "Date.after", date1, date);

            | 8: (* 'BET' *);
                 Strings.FindNext ("AND", date1, 0, found, pos);
                 IF found THEN
                     Strings.Assign (date1, date2);
                     date1[pos] := Nul;
                     INC (pos, 3);
                     WHILE date2[pos] = ' ' DO
                         INC (pos);
                     END (*WHILE*);
                     IF pos > 0 THEN
                         Strings.Delete (date2, 0, pos);
                     END (*IF*);
                     TranslateOneDate (lang, date2);
                     pos := Strings.Length (date1);
                     WHILE (pos > 0) AND (date1[pos-1] = ' ') DO
                         DEC (pos);
                         date1[pos] := Nul;
                     END (*WHILE*);
                     StrToBufferAB (lang, "Date.between", date1, date2, date);
                 ELSE
                     StrToBufferAB (lang, "Date.between", date1, "?", date);
                 END (*IF*);

        ELSE
                (* do nothing *);
        END (*CASE*);
    END TranslateDate;

(************************************************************************)

PROCEDURE WrDateAndPlace (lang: LangHandle;  DP: DatePlace;
                                             DateRequired: BOOLEAN);

    (* Writes a date and place to standard output.  *)

    VAR message: ARRAY [0..255] OF CHAR;

    BEGIN
        IF DP.empty THEN
            StrToBuffer (lang, "Person.NotRecorded", message);
            WriteString (message);
        ELSE
            IF (DP.date[0] = Nul) AND DateRequired THEN
                StrToBuffer (lang, "Person.DateNotRecorded", message);
                WriteString (message);
            ELSE
                TranslateDate (lang, DP.date);
                WriteString (DP.date);
            END (*IF*);
            IF DP.place[0] <> Nul THEN
                IF (DP.date[0] <> Nul) OR DateRequired THEN
                    WriteString ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
                END (*IF*);
                WriteString (DP.place);
            END (*IF*);
        END (*IF*);
    END WrDateAndPlace;

(************************************************************************)

PROCEDURE ExtractTag (string: ARRAY OF CHAR;  VAR (*INOUT*) DB: Database;
                                              VAR (*OUT*) tag: IDString);

    (* Extracts the substring of string enclosed by @ delimiters.  *)

    VAR pos1, pos2: CARDINAL;  found: BOOLEAN;
        database: DataString;

    BEGIN

        Strings.FindNext ('@', string, 0, found, pos1);
        IF found THEN
            Strings.FindNext ('@', string, pos1+1, found, pos2);
            IF NOT found THEN
                pos2 := LENGTH (string);
            END (*IF*);
            Strings.Extract (string, pos1+1, pos2-pos1-1, tag);
        ELSE
            tag := "";
        END (*IF*);

        (* Now check whether the tag is of the form database:id *)

        Strings.FindNext (':', tag, 0, found, pos1);
        IF found AND (pos1 > 0) THEN
            Strings.Extract (tag, 0, pos1, database);
            Strings.Extract (tag, pos1+1, LENGTH(tag)-pos1-1, tag);
            EVAL (OpenDatabase (DB, database));
        END (*IF*);

    END ExtractTag;

(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);

    (* Writes a decimal number to standard output. *)

    BEGIN
        IF N > 9 THEN
            WriteCard (N DIV 10);
            N := N MOD 10;
        END (*IF*);
        WriteChar (CHR(ORD('0') + N));
    END WriteCard;

(************************************************************************)

PROCEDURE FindFirstNote (DB: Database;  VAR (*INOUT*) T: RecordTree;
                         VAR (*OUT*) datum: DataString;
                         VAR (*OUT*) subtree: RecordTree): BOOLEAN;

    (* Finds (and removes) the first nonblank note in T, returning the  *)
    (* result in datum and subtree.  This might require skipping        *)
    (* blank notes.  Returns FALSE if no nonblank note found.           *)

    VAR found, nonblank: BOOLEAN;
        newDB: Database;
        NoteID: IDString;
        T2: RecordTree;

    BEGIN
        subtree := NIL;
        REPEAT
            found := ExtractSubrecord ('NOTE', T, datum, subtree);
            IF found THEN
                IF datum[0] = '@' THEN
                    newDB := DB;
                    ExtractTag (datum, newDB, NoteID);
                    T2 := LoadRecord0 (newDB, NoteID);
                    found := FindFirstNote (newDB, T2, datum, subtree);
                    DiscardTree (T2);
                END (*IF*);
            END (*IF*);
            nonblank := (datum[0] <> Nul) OR NonEmpty(subtree);
        UNTIL NOT found OR nonblank;
        (*DiscardTree (subtree);*)
        RETURN found AND nonblank;
    END FindFirstNote;

(************************************************************************)

PROCEDURE WriteNotes (lang: LangHandle;  DB: Database;
                             VAR (*INOUT*) T: RecordTree;
                             heading: MsgID;  headinglevel: CARDINAL);

    (* Processes all of the NOTE records, if any, in T.  This includes  *)
    (* continuation records.  A heading is generated if "heading" is    *)
    (* a nonempty string.                                               *)

    VAR datum, message: DataString;
        subtree: RecordTree;
        found, IsCONT: BOOLEAN;

    BEGIN
        IF FindFirstNote (DB, T, datum, subtree) THEN

            (* Include a heading if required. *)

            IF heading[0] <> Nul THEN
                WriteString ("<h");
                WriteChar (CHR(ORD('0')+headinglevel));
                WriteString (">");
                StrToBuffer (lang, heading, message);
                WriteString (message);
                WriteString ("</h");
                WriteChar (CHR(ORD('0')+headinglevel));
                WriteString (">");
                WriteLn;
            END (*IF*);

            REPEAT
                SendLine ('<p>');
                WriteString (datum);
                REPEAT
                    (* Dealing with continuation lines. *)

                    found := GetContinuation (subtree, datum, IsCONT);

                    IF found THEN
                        IF IsCONT THEN WriteString ("<br>");
                        END (*IF*);
                        WriteString (datum);
                        WriteLn;
                    END (*IF*);

                UNTIL NOT found;
                SendLine ('</p>');
                DiscardTree (subtree);
            UNTIL NOT FindFirstNote (DB, T, datum, subtree);
        END (*IF*);
    END WriteNotes;

(************************************************************************)

PROCEDURE AddDatabase (newDB: Database;  DBL: DBList);

    (* Adds newDB to the DBL list, unless it is already present.  On    *)
    (* entry we are guaranteed that DBL <> NIL.                         *)

    VAR prev, p: DBList;

    BEGIN
        prev := NIL;  p := DBL;
        WHILE (p <> NIL) AND (p^.this <> newDB) DO
            prev := p;  p := p^.next;
        END (*WHILE*);
        IF p = NIL THEN
            NEW (p);
            p^.next := NIL;
            p^.this := newDB;
            prev^.next := p;
        END (*IF*);
    END AddDatabase;

(************************************************************************)

PROCEDURE WriteMarriage (lang: LangHandle;  M: CARDINAL;
                         DB: Database;  FamID: IDString;  sex: CHAR;
                         DBL: DBList);

    (* Writes the details of marriage M.  Parameter 'sex' is the sex of *)
    (* the person whose marriage we are noting. If DBL <> NIL, we add   *)
    (* any new databases we encounter to DBL.                           *)

    VAR T, subtree: RecordTree;  PersonID: IDString;
        string, title: DataString;
        label: ARRAY [0..255] OF CHAR;
        newDB: Database;
        DP: DatePlace;
        pos: CARDINAL;
        NeedBreak, found: BOOLEAN;
        sexstr: ARRAY [0..0] OF CHAR;

    BEGIN
        sexstr[0] := sex;
        IF sexstr[0] <> 'F' THEN sexstr[0] := 'M' END (*IF*);
        WriteString ("<h3>");
        StrToBuffer (lang, "Person.Family", label);
        WriteString (label);
        IF M > 1 THEN
            WriteString (" ");
            WriteCard (M);
        END (*IF*);

        SeekToMatchingID (DB, FamID);
        LoadRecord (DB, T);
        RemoveUnwantedRecords (T);

        (* Date last changed. *)

        IF ExtractSubrecord ('CHAN', T, string, subtree)
                            AND GetField ('DATE', subtree, string) THEN
            WriteString ('<font size=-1 style="normal">&nbsp;&nbsp;');
            StrToBuffer (lang, "Person.Changed", label);
            WriteChar ('(');
            WriteString (label);
            WriteChar (' ');
            WriteString (string);
            WriteChar (')');
            WriteString ("</font>");
            DiscardTree (subtree);
        END (*IF*);

        WriteString ("</h3>");
        NeedBreak := FALSE;

        (* Picture, if present. *)

        IF ExtractSubrecord ('OBJE', T, string, subtree) THEN
            IF GetField ('FILE', subtree, string) AND (string[0] <> Nul) THEN
                Strings.Extract (string, 0, 7, title);
                Strings.Capitalize (title);
                IF NOT Strings.Equal (title, "/IMAGES") THEN
                    Strings.FindPrev ('/', string, LENGTH(string), found, pos);
                    IF found THEN
                        Strings.Delete (string, 0, pos+1);
                    END (*IF*);
                    Strings.Insert ("/images/", 0, string);
                END (*IF*);
                IF NOT GetField ('TITL', subtree, title) THEN
                    title[0] := Nul;
                END (*IF*);
                WriteString ('<img style="float: right;" src="');
                WriteString (string);
                WriteString ('" alt="');
                WriteString (string);
                IF title[0] <> Nul THEN
                    WriteString ('" title="');
                    WriteString (title);
                END (*IF*);
                WriteString ('">');
                WriteLn;
            END (*IF*);
            DiscardTree (subtree);
        END (*IF*);

        (* Date and place of marriage. *)

        IF ExtractSubrecord ('MARR', T, string, subtree) THEN
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    StrToBuffer (lang, "Family.Marriage", label);
                    WriteString (label);
                    WrDateAndPlace (lang, DP, TRUE);
                    WriteLn;
                    NeedBreak := TRUE;
                END (*IF*);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Husband *)

        IF GetField ('HUSB', T, string) AND (sex <> 'M') THEN
            newDB := DB;
            ExtractTag (string, newDB, PersonID);
            IF (DBL <> NIL) AND (newDB <> DB) THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
            IF NeedBreak THEN
                WriteString ("<br>");
            END (*IF*);
            StrToBuffer (lang, "Person.Husband", label);
            WriteString (label);
            WritePersonLink (lang, newDB, PersonID);
            WriteLn;
            NeedBreak := TRUE;
        END (*IF*);

        (* Wife *)

        IF GetField ('WIFE', T, string) AND (sex <> 'F') THEN
            newDB := DB;
            ExtractTag (string, newDB, PersonID);
            IF (DBL <> NIL) AND (newDB <> DB) THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
            IF NeedBreak THEN
                WriteString ("<br>");
            END (*IF*);
            StrToBuffer (lang, "Person.Wife", label);
            WriteString (label);
            WritePersonLink (lang, newDB, PersonID);
            WriteLn;
            NeedBreak := TRUE;
        END (*IF*);

        (* Children *)

        IF GetField ('CHIL', T, string) THEN
            IF NeedBreak THEN
                WriteString ("<p>");
            END (*IF*);
            StrToBuffer (lang, "Person.Children", label);
            WriteString (label);
            WriteLn;
            REPEAT
                WriteString ("<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
                newDB := DB;
                ExtractTag (string, newDB, PersonID);
                IF (DBL <> NIL) AND (newDB <> DB) THEN
                    AddDatabase (newDB, DBL);
                END (*IF*);
                WritePersonLink (lang, newDB, PersonID);
                WriteLn;
            UNTIL NOT GetField ('CHIL', T, string);
            WriteString ("<br>");
        END (*IF*);

        (* Divorce filed. *)

        IF ExtractSubrecord ('DIVF', T, string, subtree) THEN
            WriteString ("<br>");
            string := "Family.DivorceFiled";
            StrToBuffer (lang, string, label);
            WriteString (label);
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                WrDateAndPlace (lang, DP, TRUE);
                WriteLn;
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Date and place of divorce. *)

        IF ExtractSubrecord ('DIV', T, string, subtree) THEN
            WriteString ("<br>");
            string := "Family.Divorced.";
            Strings.Append (sexstr, string);
            StrToBuffer (lang, string, label);
            WriteString (label);
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                WrDateAndPlace (lang, DP, TRUE);
                WriteLn;
                DiscardTree (subtree);
            END (*IF*);
            WriteString ("<br>");
        END (*IF*);

        (* Notes. *)

        WriteNotes (lang, DB, T, "Person.MarriageNotes", 4);

        (* Miscellaneous details. *)

        IF NonEmpty(T) THEN
            WriteString ("<h4>");
            StrToBuffer (lang, "Person.OtherMarriageDetails", label);
            WriteString (label);
            WriteString ("</h4>");
            DisplayRawLines (T);
        END (*IF*);

        DiscardTree (T);

    END WriteMarriage;

(************************************************************************)

PROCEDURE WritePartialTree (lang: LangHandle;  PersonID: IDString;
                                     DB: Database;  T: RecordTree);

    (* Writes a small subtree consisting of the given person plus a     *)
    (* small number of ancestors.                                       *)

    (* Rules: 'hotspot' is the location in the printed line where a top *)
    (* or bottom corner symbol will occur.  'newhotspot' is the same    *)
    (* for a line one generation up. 'prefix' is a string consisting of *)
    (* mostly space characters, but also some '|' characters, to be     *)
    (* written before the hotspot character.                            *)

    (********************************************************************)

    PROCEDURE WriteStrClipped (str: ARRAY OF CHAR;
                               VAR (*INOUT*) width: CARDINAL);

        (* Like WriteString, but does not write more than 'width'       *)
        (* characters.  On exit, width has been decremented by the      *)
        (* number of characters written.                                *)

        VAR k: CARDINAL;

        BEGIN
            k := 0;
            WHILE (str[k] <> Nul) AND (width > 0) DO
                WriteChar (str[k]);
                INC (k);
                DEC (width);
            END (*WHILE*);
        END WriteStrClipped;

    (********************************************************************)

    CONST
        topcorner = '.';  bottomcorner = '`';
        LineWidth = 120;

    VAR prefix: ARRAY [0..LineWidth] OF CHAR;

    (********************************************************************)

    PROCEDURE WriteBranch (PersonID: IDString;  DB: Database;
                            T: RecordTree;  hotspot, width: CARDINAL;
                              FirstTime, TopHalf: BOOLEAN);

        (* Writes a partial tree consisting of this person plus         *)
        (* ancestors.  Parameter 'width' is the number of characters    *)
        (* available beyond the hotspot location.  The hotspot is the   *)
        (* character position in the line where we are going to put     *)
        (* the corner character that starts this branch.  The prefix    *)
        (* array stores the characters, up to and including the hotspot *)
        (* character, that we must write in front of this branch.       *)

        (****************************************************************)

        VAR newhotspot: CARDINAL;
            FamcTree: RecordTree;
            FamilyDB: Database;

        (****************************************************************)

        PROCEDURE StartFromParent (father: BOOLEAN);

            (* Like WriteBranch, but one generation up.  *)

            VAR j: CARDINAL;
                NewTree: RecordTree;
                newDB: Database;
                ParentID: IDString;
                string: DataString;
                FieldName: ARRAY [0..4] OF CHAR;

            BEGIN
                IF father THEN
                    FieldName := 'HUSB';
                ELSE
                    FieldName := 'WIFE';
                END (*IF*);
                IF GetField (FieldName, FamcTree, string) THEN
                    newDB := FamilyDB;
                    ExtractTag (string, newDB, ParentID);
                    SeekToMatchingID (newDB, ParentID);
                    LoadRecord (newDB, NewTree);
                    IF NOT FirstTime THEN
                        FOR j := hotspot+1 TO newhotspot-1 DO
                            prefix[j] := ' ';
                        END (*FOR*);
                    END (*IF*);
                    WriteBranch (ParentID, newDB, NewTree, newhotspot,
                                     LineWidth-newhotspot, FALSE, father);
                    DiscardTree (NewTree);
                END (*IF*);
            END StartFromParent;

        (****************************************************************)

        VAR thisname, string: DataString;
            FamilyID: IDString;
            L: CARDINAL;
            HaveFamily: BOOLEAN;

        BEGIN
            Assert (width + hotspot = LineWidth);
            EVAL (CopyName (T, thisname));
            newhotspot := hotspot;

            (* The first time this procedure is called is a special     *)
            (* case because we are going to put the two parent branches *)
            (* at the same indentation level as the central branch,     *)
            (* i.e. in that case we will have newhotspot = hotspot.     *)

            IF FirstTime THEN
                L := 0;
            ELSE
                L := LENGTH(thisname) + 5;
                INC (newhotspot, L);
            END (*IF*);

            (* It is possible that newhotspot is now out of range; but  *)
            (* if so we won't have enough space to display the parent   *)
            (* branches, so the value of newhotspot won't be used.      *)

            HaveFamily := (L < width)
                            AND CopyField ('FAMC', T, string);

            (* Deal with the father's half of the family. *)

            IF HaveFamily THEN
                FamilyDB := DB;
                ExtractTag (string, FamilyDB, FamilyID);
                SeekToMatchingID (FamilyDB, FamilyID);
                LoadRecord (FamilyDB, FamcTree);
                prefix[newhotspot] := ' ';     (* essential to ensure that there   *)
                                               (* are no gaps in the prefix array  *)
                                               (* during multilevel recursion.     *)
                StartFromParent (TRUE);
            END (*IF*);

            (* Now the person him/herself. *)

            IF FirstTime THEN
                (* No point in drawing a one-person tree. *)
                IF HaveFamily THEN
                    WriteString ("  |----");
                    WriteString (thisname);
                    WriteLn;
                END (*IF*);
            ELSE
                IF TopHalf THEN
                    prefix[hotspot] := topcorner;
                ELSE
                    prefix[hotspot] := bottomcorner;
                END (*IF*);
                prefix[hotspot+1] := Nul;
                WriteString (prefix);
                WriteStrClipped ("--", width);
                IF width > 0 THEN
                    WritePersonLinkClipped (lang, DB, PersonID, width);
                    IF HaveFamily THEN
                        WriteStrClipped ("--|", width);
                    END (*IF*);
                END (*IF*);
                WriteLn;
            END (*IF*);
            IF TopHalf THEN
                prefix[hotspot] := '|';
            ELSE
                prefix[hotspot] := ' ';
            END (*IF*);

            (* Finally, the mother's side of the family. *)

            IF HaveFamily THEN
                StartFromParent (FALSE);
                DiscardTree (FamcTree);
            END (*IF*);

        END WriteBranch;

    (********************************************************************)

    CONST indent = 2;

    BEGIN                               (* body of WritePartialTree *)
        prefix := "  ";
        WriteString ("<PRE>");  WriteLn;
        WriteBranch (PersonID, DB, T, indent, LineWidth-indent, TRUE, TRUE);
        WriteString ("</PRE>");  WriteLn;
    END WritePartialTree;

(************************************************************************)

(*
PROCEDURE OldWritePartialTree (lang: LangHandle;  PersonID: IDString;
                                     DB: Database;  T: RecordTree);

    (* Writes a small subtree consisting of the given person plus a     *)
    (* small number of ancestors.                                       *)

    (********************************************************************)

    PROCEDURE WriteStrClipped (str: ARRAY OF CHAR;
                               VAR (*INOUT*) width: CARDINAL);

        (* Like WriteString, but does not write more than 'width'       *)
        (* characters.  On exit, width has been decremented by the      *)
        (* number of characters written.                                *)

        VAR k: CARDINAL;

        BEGIN
            k := 0;
            WHILE (str[k] <> Nul) AND (width > 0) DO
                WriteChar (str[k]);
                INC (k);
                DEC (width);
            END (*WHILE*);
        END WriteStrClipped;

    (********************************************************************)

    CONST
        topcorner = '.';  bottomcorner = '`';
        LineWidth = 80;

    VAR prefix: ARRAY [0..LineWidth] OF CHAR;

    (********************************************************************)

    PROCEDURE WriteBranch (PersonID: IDString;  DB: Database;
                            T: RecordTree;  hotspot, width: CARDINAL;
                              FirstTime, TopHalf: BOOLEAN);

        (* Writes a partial tree consisting of this person plus         *)
        (* ancestors.  Parameter 'width' is the number of characters    *)
        (* available beyond the hotspot location.  The hotspot is the   *)
        (* character position in the line where we are going to put     *)
        (* the corner character that starts this branch.  The prefix    *)
        (* array stores the characters, up to and including the hotspot *)
        (* character, that we must write in front of this branch.       *)

        (****************************************************************)

        VAR newhotspot: CARDINAL;
            FamcTree: RecordTree;
            FamilyDB: Database;

        (****************************************************************)

        PROCEDURE StartFromParent (father: BOOLEAN);

            (* Like WriteBranch, but one generation up.  On exit we     *)
            (* restore the condition prefix[hotspot+1] = Nul.           *)

            VAR j: CARDINAL;
                NewTree: RecordTree;
                newDB: Database;
                ParentID: IDString;
                string: DataString;
                FieldName: ARRAY [0..4] OF CHAR;

            BEGIN
                IF father THEN
                    FieldName := 'HUSB';
                ELSE
                    FieldName := 'WIFE';
                END (*IF*);
                IF GetField (FieldName, FamcTree, string) THEN
                    newDB := FamilyDB;
                    ExtractTag (string, newDB, ParentID);
                    SeekToMatchingID (newDB, ParentID);
                    LoadRecord (newDB, NewTree);
                    IF NOT FirstTime THEN
                        FOR j := hotspot+1 TO newhotspot-1 DO
                            prefix[j] := ' ';
                        END (*FOR*);
                    END (*IF*);
                    prefix[newhotspot+1] := Nul;
                    WriteBranch (ParentID, newDB, NewTree, newhotspot,
                                     LineWidth-newhotspot, FALSE, father);
                    prefix[hotspot+1] := Nul;
                    DiscardTree (NewTree);
                END (*IF*);
            END StartFromParent;

        (****************************************************************)

        VAR thisname, string: DataString;
            FamilyID: IDString;
            L: CARDINAL;
            HaveFamily: BOOLEAN;

        BEGIN
            Assert (width + hotspot = LineWidth);
            EVAL (CopyName (T, thisname));
            newhotspot := hotspot;

            (* The first time this procedure is called is a special     *)
            (* case because we are going to put the two parent branches *)
            (* at the same indentation level as the central branch,     *)
            (* i.e. in that case we will have newhotspot = hotspot.     *)

            IF FirstTime THEN
                L := 0;
            ELSE
                L := LENGTH(thisname) + 5;
                INC (newhotspot, L);
            END (*IF*);

            (* It is possible that newhotspot is now out of range; but  *)
            (* if so the updated 'width' for the parent branches will   *)
            (* be zero, which will lead to the value of newhotspot      *)
            (* not being used.                                          *)

            HaveFamily := (L < width)
                            AND CopyField ('FAMC', T, string);

            (* Deal with the father's half of the family. *)

            IF HaveFamily THEN
                FamilyDB := DB;
                ExtractTag (string, FamilyDB, FamilyID);
                SeekToMatchingID (FamilyDB, FamilyID);
                LoadRecord (FamilyDB, FamcTree);
                prefix[newhotspot] := ' ';
                StartFromParent (TRUE);
            END (*IF*);

            (* Now the person him/herself. *)

            IF FirstTime THEN
                (* No point in drawing a one-person tree. *)
                IF HaveFamily THEN
                    WriteString ("  |----");
                    WriteString (thisname);
                    WriteLn;
                END (*IF*);
            ELSE
                IF TopHalf THEN
                    prefix[hotspot] := topcorner;
                ELSE
                    prefix[hotspot] := bottomcorner;
                END (*IF*);
                prefix[hotspot+1] := Nul;
                WriteString (prefix);
                WriteStrClipped ("--", width);
                IF width > 0 THEN
                    WritePersonLinkClipped (lang, DB, PersonID, width);
                    IF HaveFamily THEN
                        WriteStrClipped ("--|", width);
                    END (*IF*);
                END (*IF*);
                WriteLn;
            END (*IF*);
            IF TopHalf THEN
                prefix[hotspot] := '|';
            ELSE
                prefix[hotspot] := ' ';
            END (*IF*);
            prefix[hotspot+1] := ' ';

            (* Finally, the mother's side of the family. *)

            IF HaveFamily THEN
                prefix[newhotspot] := '|';
                StartFromParent (FALSE);
                DiscardTree (FamcTree);
            END (*IF*);

        END WriteBranch;

    (********************************************************************)

    CONST indent = 2;

    VAR j: CARDINAL;

    BEGIN                               (* body of WritePartialTree *)
        FOR j := 0 TO LineWidth-1 DO
            prefix[j] := ' ';
        END (*FOR*);
        WriteString ("<PRE>");  WriteLn;
        WriteBranch (PersonID, DB, T, indent, LineWidth-indent, TRUE, TRUE);
        WriteString ("</PRE>");  WriteLn;
    END OldWritePartialTree;

(************************************************************************)

PROCEDURE OlderWritePartialTree (lang: LangHandle;  PersonID: IDString;
                                     DB: Database;  T: RecordTree);

    (* Writes a small subtree consisting of the given person plus a     *)
    (* small number of ancestors.                                       *)

    (********************************************************************)

    CONST
        topcorner = '.';  bottomcorner = '`';
        PreLength = 74;
    VAR prefix: ARRAY [0..PreLength] OF CHAR;

    (********************************************************************)

    PROCEDURE WriteBranch (PersonID: IDString;  DB: Database;
                            T: RecordTree;  hotspot: CARDINAL;
                              FirstTime, TopHalf: BOOLEAN);

        (* Writes a partial tree consisting of this person plus ancestors. *)

        (****************************************************************)

        VAR newhotspot: CARDINAL;
            FamcTree: RecordTree;
            FamilyDB: Database;

        (****************************************************************)

        PROCEDURE StartFromParent (father: BOOLEAN);

            (* Like WriteBranch, but one generation up. *)

            VAR j: CARDINAL;
                NewTree: RecordTree;
                newDB: Database;
                ParentID: IDString;
                string: DataString;
                FieldName: ARRAY [0..4] OF CHAR;

            BEGIN
                IF father THEN
                    FieldName := 'HUSB';
                ELSE
                    FieldName := 'WIFE';
                END (*IF*);
                IF GetField (FieldName, FamcTree, string) THEN
                    newDB := FamilyDB;
                    ExtractTag (string, newDB, ParentID);
                    SeekToMatchingID (newDB, ParentID);
                    LoadRecord (newDB, NewTree);
                    IF NOT FirstTime THEN
                        FOR j := hotspot+1 TO newhotspot-1 DO
                            prefix[j] := ' ';
                        END (*FOR*);
                    END (*IF*);
                    prefix[newhotspot+1] := Nul;
                    WriteBranch (ParentID, newDB, NewTree,
                                     newhotspot, FALSE, father);
                    prefix[hotspot+1] := Nul;
                    DiscardTree (NewTree);
                END (*IF*);
            END StartFromParent;

        (****************************************************************)

        VAR thisname, string: DataString;
            FamilyID: IDString;
            HaveFamily: BOOLEAN;

        BEGIN
            prefix[hotspot+1] := Nul;
            EVAL (CopyName (T, thisname));
            newhotspot := hotspot;
            IF NOT FirstTime THEN
                INC (newhotspot, LENGTH(thisname));
            END (*IF*);

            (* Before anything else, check whether we have enough  *)
            (* space to write this branch.                         *)

            IF FirstTime OR (newhotspot < PreLength) THEN

                IF NOT FirstTime THEN
                    INC (newhotspot, 5);
                END (*IF*);
                HaveFamily := (newhotspot < PreLength)
                                AND CopyField ('FAMC', T, string);

                (* Deal with the father's half of the family. *)

                IF HaveFamily THEN
                    FamilyDB := DB;
                    ExtractTag (string, FamilyDB, FamilyID);
                    SeekToMatchingID (FamilyDB, FamilyID);
                    LoadRecord (FamilyDB, FamcTree);
                    prefix[newhotspot] := ' ';
                    StartFromParent (TRUE);
                END (*IF*);

                (* Now the person him/herself. *)

                IF FirstTime THEN
                    (* No point in drawing a one-person tree. *)
                    IF HaveFamily THEN
                        WriteString ("  |----");
                        WriteString (thisname);
                        WriteLn;
                    END (*IF*);
                ELSE
                    IF TopHalf THEN
                        prefix[hotspot] := topcorner;
                    ELSE
                        prefix[hotspot] := bottomcorner;
                    END (*IF*);
                    WriteString (prefix);
                    WriteString ("--");
                    WritePersonLink (lang, DB, PersonID);
                    IF HaveFamily THEN
                        WriteString ("--|");
                    END (*IF*);
                    WriteLn;
                END (*IF*);
                IF TopHalf THEN
                    prefix[hotspot] := '|';
                ELSE
                    prefix[hotspot] := ' ';
                END (*IF*);

                (* Finally, the mother's side of the family. *)

                IF HaveFamily THEN
                    prefix[newhotspot] := '|';
                    StartFromParent (FALSE);
                    DiscardTree (FamcTree);
                END (*IF*);

            END (*IF*);

        END WriteBranch;

    (********************************************************************)

    VAR j: CARDINAL;

    BEGIN                               (* body of WritePartialTree *)
        FOR j := 0 TO PreLength DO
            prefix[j] := ' ';
        END (*FOR*);
        WriteString ("<PRE>");  WriteLn;
        WriteBranch (PersonID, DB, T, 2, TRUE, TRUE);
        WriteString ("</PRE>");  WriteLn;
    END OlderWritePartialTree;

(************************************************************************)
*)

PROCEDURE WriteParents (lang: LangHandle;  DB: Database;
                           FamID: IDString;  DBL: DBList);

    (* Writes the names of the two partners to the given family ID,     *)
    (* labelled as father and mother.  If DBL <> NIL, we add            *)
    (* any new databases we encounter to DBL.                           *)

    VAR T: RecordTree;  FatherID, MotherID: IDString;
        string: DataString;
        newDB: Database;
        NeedBreak: BOOLEAN;
        label: ARRAY [0..255] OF CHAR;

    BEGIN
        NeedBreak := FALSE;
        SeekToMatchingID (DB, FamID);
        LoadRecord (DB, T);
        IF GetField ('HUSB', T, string) THEN
            newDB := DB;
            ExtractTag (string, newDB, FatherID);
            IF (DBL <> NIL) AND (newDB <> DB) THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
            WriteString ("<p>");
            StrToBuffer (lang, "Person.Father", label);
            WriteString (label);
            WriteString (" &nbsp;");
            WritePersonLink (lang, newDB, FatherID);
            NeedBreak := TRUE;
        END (*IF*);
        IF GetField ('WIFE', T, string) THEN
            newDB := DB;
            ExtractTag (string, newDB, MotherID);
            IF (DBL <> NIL) AND (newDB <> DB) THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
            IF NeedBreak THEN
                WriteString ("<br>");
            END (*IF*);
            StrToBuffer (lang, "Person.Mother", label);
            WriteString (label);
            WriteString (" ");
            WritePersonLink (lang, newDB, MotherID);
        END (*IF*);
        DiscardTree (T);
    END WriteParents;

(************************************************************************)

PROCEDURE WriteIndented (indent: CARDINAL;  S: ARRAY OF CHAR);

    (* Writes S indented by "indent" space characters. *)

    VAR k: CARDINAL;

    BEGIN
        FOR k := 1 TO indent DO
            WriteString ("&nbsp;");
        END (*FOR*);
        WriteString (S);
    END WriteIndented;

(************************************************************************)

PROCEDURE WriteEventDetail (lang: LangHandle;  DB: Database;
                                     VAR (*INOUT*) T: RecordTree);

    (* Writes the date and place information in T. *)

    VAR datum: DataString;  subtree: RecordTree;
        separator: ARRAY [0..31] OF CHAR;

    BEGIN
        separator := "";
        IF GetField ('DATE', T, datum) AND (datum[0] <> Nul) THEN
            TranslateDate (lang, datum);
            WriteString (datum);
            separator := ": ";
        END (*IF*);
        IF GetField ('PLAC', T, datum) AND (datum[0] <> Nul) THEN
            separator := "<br>&nbsp;&nbsp;&nbsp;&nbsp;";
            WriteString (separator);
            WriteString (datum);
        END (*IF*);
        IF ExtractSubrecord ('ADDR', T, datum, subtree) THEN
            IF datum[0] <> Nul THEN
                WriteString (separator);
                WriteString (datum);
                separator := ", ";
            END (*IF*);
            WHILE GetField ('CONT', subtree, datum) DO
                WriteString (separator);
                WriteString (datum);
                separator := ", ";
            END (*WHILE*);
            IF GetField ('ADR1', subtree, datum) THEN
                WriteString (separator);
                WriteString (datum);
                separator := ", ";
            END (*IF*);
            IF GetField ('ADR2', subtree, datum) THEN
                WriteString (separator);
                WriteString (datum);
                separator := ", ";
            END (*IF*);
            IF GetField ('CITY', subtree, datum) THEN
                WriteString (separator);
                WriteString (datum);
                separator := ", ";
            END (*IF*);
            IF GetField ('STAE', subtree, datum) THEN
                WriteString (separator);
                WriteString (datum);
                separator := ", ";
            END (*IF*);
            IF GetField ('POST', subtree, datum) THEN
                WriteString (separator);
                WriteString (datum);
                separator := ", ";
            END (*IF*);
            IF GetField ('CTRY', subtree, datum) THEN
                WriteString (separator);
                WriteString (datum);
                separator := ", ";
            END (*IF*);
        END (*IF*);
        IF GetField ('PHON', T, datum) THEN
            WriteIndented (4, datum);
        END (*IF*);

        (* Notes. *)

        WriteNotes (lang, DB, T, "", 0);

        DiscardTree (T);
    END WriteEventDetail;

(************************************************************************)

PROCEDURE PersonHeading (lang: LangHandle;  T: RecordTree;
                            VAR (*OUT*) result: DataString);

    (* Constructs a string showing name and years of birth-death. *)

    VAR daterange: DataString;

    BEGIN
        IF NOT CopyName (T, result) THEN
            StrToBuffer (lang, "Person.NoName", result);
        END (*IF*);
        GetDateRange (T, daterange);
        IF daterange[0] <> Nul THEN
            Strings.Append ("&nbsp;&nbsp;", result);
            Strings.Append (daterange, result);
        END (*IF*);
    END PersonHeading;

(************************************************************************)

PROCEDURE DisplayPersonInfo (lang: LangHandle;  PersonID: IDString;
                             DB: Database;
                             VAR (*INOUT*) T: RecordTree;
                             prefix: ARRAY OF CHAR;
                             VAR (*IN*) heading: DataString;
                             DBL: DBList);

    (* Displays all the information for one person. If DBL <> NIL, adds *)
    (* any extra databases it encounters to DBL.                        *)

    VAR datum, message: DataString;
        subtree: RecordTree;
        ID: IDString;  M, pos: CARDINAL;
        newDB: Database;
        DP: DatePlace;
        sex: ARRAY [0..0] OF CHAR;
        knownsex: CHAR;
        found: BOOLEAN;

    BEGIN
        RemoveUnwantedRecords (T);

        (* Picture, if present. *)

        IF ExtractSubrecord ('OBJE', T, datum, subtree) THEN
            IF GetField ('FILE', subtree, datum) AND (datum[0] <> Nul) THEN
                Strings.Extract (datum, 0, 7, message);
                Strings.Capitalize (message);
                IF NOT Strings.Equal (message, "/IMAGES") THEN
                    Strings.FindPrev ('/', datum, LENGTH(datum), found, pos);
                    IF found THEN
                        Strings.Delete (datum, 0, pos+1);
                    END (*IF*);
                    Strings.Insert ("/images/", 0, datum);
                END (*IF*);
                IF NOT GetField ('TITL', subtree, message) AND (message[0] <> Nul) THEN
                    Strings.Assign (heading, message);
                END (*IF*);
                WriteString ('<img style="float: right;" src="');
                WriteString (datum);
                WriteString ('" alt="');
                WriteString (datum);
                WriteString ('" title="');
                WriteString (message);
                WriteString ('">');
                WriteLn;
            END (*IF*);
            DiscardTree (subtree);
        END (*IF*);

        (* Use name and date range as heading. *)

        WriteString ("<h2>");
        IF prefix[0] <> Nul THEN
            WriteString ('<font color=RED>');
            WriteString (prefix);
            WriteString ('</font>');
            WriteString ('&nbsp;&nbsp;&nbsp;');
        END (*IF*);
        WriteString (heading);
        SendLine ("</h2>");

        (* Alias. *)

        IF GetField ('ALIA', T, datum) AND (datum[0] <> Nul) THEN
            StrToBuffer (lang, "Person.Alias", message);
            WriteString (message);
            WriteString (datum);
            WriteString ("<br>");
            WriteLn;
        END (*IF*);

        (* Sex. *)

        knownsex := Nul;      (* Nul code means 'unknown' *)

        IF GetField ('SEX', T, datum) AND (datum[0] <> Nul) THEN
            StrToBuffer (lang, "Person.Sex", message);
            WriteString (message);
            WriteString (datum);
            WriteLn;
            knownsex := CAP(datum[0]);
        END (*IF*);

        (* From now on, the variable 'knownsex' will only be    *)
        (* used as a parameter to WriteMarriage invocations,    *)
        (* but we will need a string variable 'sex' whose value *)
        (* is guaranteed to be either 'M' or 'F', for use in    *)
        (* language-dependent labels.                           *)

        sex[0] := knownsex;
        IF sex[0] <> 'F' THEN sex[0] := 'M' END (*IF*);

        (* Occupation. *)

        WHILE ExtractSubrecord ('OCCU', T, datum, subtree)
                           AND (datum[0] <> Nul) DO
            WriteString ("<br>");
            WriteString (datum);
            EVAL (GetField ('DATE', subtree, datum));
            IF datum[0] <> Nul THEN
                TranslateDate (lang, datum);
                WriteString (" (");
                WriteString (datum);
                WriteChar (')');
            END (*IF*);
            WriteLn;
            DiscardTree (subtree);
        END (*WHILE*);
        WriteString ("<P>");

        (* Date and place of birth. *)

        IF ExtractSubrecord ('BIRT', T, datum, subtree) THEN
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    datum := "Person.Born.";
                    Strings.Append (sex, datum);
                    StrToBuffer (lang, datum, message);
                    WriteString (message);
                    WrDateAndPlace (lang, DP, TRUE);
                    WriteLn;
                    WriteString ("<br>");
                END (*IF*);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Baptism. *)

        IF ExtractSubrecord ('CHR', T, datum, subtree)
                OR ExtractSubrecord ('BAPM', T, datum, subtree) THEN
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    datum := "Person.Baptised.";
                    Strings.Append (sex, datum);
                    StrToBuffer (lang, datum, message);
                    WriteString (message);
                    WrDateAndPlace (lang, DP, FALSE);
                    WriteLn;
                    WriteString ("<br>");
                END (*IF*);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Confirmation. *)

        IF ExtractSubrecord ('CONF', T, datum, subtree)
                OR ExtractSubrecord ('CONL', T, datum, subtree) THEN
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    datum := "Person.Confirmed.";
                    Strings.Append (sex, datum);
                    StrToBuffer (lang, datum, message);
                    WriteString (message);
                    WrDateAndPlace (lang, DP, FALSE);
                    WriteLn;
                    WriteString ("<br>");
                END (*IF*);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Migrations. *)

        WHILE ExtractEither ('EMIG', 'IMMI', T, datum, subtree, M) DO
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    IF M = 1 THEN
                        datum := "Person.Emigrated.";
                    ELSE
                        datum := "Person.Immigrated.";
                    END (*IF*);
                    Strings.Append (sex, datum);
                    StrToBuffer (lang, datum, message);
                    WriteString (message);
                    WrDateAndPlace (lang, DP, TRUE);
                    WriteLn;
                    WriteString ("<br>");
                END (*IF*);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Date and place of death. *)

        IF ExtractSubrecord ('DEAT', T, datum, subtree) THEN
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    datum := "Person.Died.";
                    Strings.Append (sex, datum);
                    StrToBuffer (lang, datum, message);
                    WriteString (message);
                    WrDateAndPlace (lang, DP, TRUE);
                    WriteLn;
                    WriteString ("<br>");
                END (*IF*);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Buried. *)

        IF ExtractSubrecord ('BURI', T, datum, subtree) THEN
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    datum := "Person.Buried.";
                    Strings.Append (sex, datum);
                    StrToBuffer (lang, datum, message);
                    WriteString (message);
                    WrDateAndPlace (lang, DP, FALSE);
                    WriteLn;
                    WriteString ("<br>");
                END (*IF*);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Cremated. *)

        IF ExtractSubrecord ('CREM', T, datum, subtree) THEN
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    datum := "Person.Cremated.";
                    Strings.Append (sex, datum);
                    StrToBuffer (lang, datum, message);
                    WriteString (message);
                    WrDateAndPlace (lang, DP, FALSE);
                    WriteLn;
                    WriteString ("<br>");
                END (*IF*);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Partial tree. *)

        WritePartialTree (lang, PersonID, DB, T);
        EVAL (GetName (T, datum));     (* name field no longer needed *)

        (* Residences. *)

        WHILE ExtractSubrecord ('RESI', T, datum, subtree) DO
            StrToBuffer (lang, "Person.Residence", message);
            WriteString (message);
            WriteEventDetail (lang, DB, subtree);
            WriteLn;
            WriteString ("<br>");
            DiscardTree (subtree);
        END (*WHILE*);

        (* Parents. *)

        IF GetField ('FAMC', T, datum) THEN
            newDB := DB;
            ExtractTag (datum, newDB, ID);
            IF (DBL <> NIL) AND (newDB <> DB) THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
            WriteParents (lang, newDB, ID, DBL);
            WriteLn;
        END (*IF*);

        (* Marriages. *)

        M := 1;
        WHILE GetField ('FAMS', T, datum) DO
            newDB := DB;
            ExtractTag (datum, newDB, ID);
            IF (DBL <> NIL) AND (newDB <> DB) THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
            WriteMarriage (lang, M, newDB, ID, knownsex, DBL);
            INC (M);
        END (*WHILE*);

        (* Notes. *)

        WriteNotes (lang, DB, T, "Person.Notes", 3);

        (* Discard mailing address, which we consider to be private. *)

        WHILE ExtractSubrecord ('ADDR', T, datum, subtree) DO
            DiscardTree (subtree);
        END (*WHILE*);

        (* Date last changed. *)

        IF ExtractSubrecord ('CHAN', T, datum, subtree)
                            AND GetField ('DATE', subtree, datum) THEN
            WriteString ("<p><font size=-1>");
            StrToBuffer (lang, "Person.Changed", message);
            WriteChar ('(');
            WriteString (message);
            WriteChar (' ');
            WriteString (datum);
            WriteChar (')');
            WriteString ("</font>");
            WriteString ("</p>");
            DiscardTree (subtree);
        END (*IF*);

        (* Display what's left in raw form. *)

        IF NonEmpty(T) THEN
            WriteString ("<h3>");
            StrToBuffer (lang, "Person.OtherDetails", message);
            WriteString (message);
            WriteString ("</h3>");
            DisplayRawLines (T);
            DiscardTree (T);
        END (*IF*);

    END DisplayPersonInfo;

(************************************************************************)

PROCEDURE MakeButton (DB: Database;  ID: IDString;
                      option, label: ARRAY OF CHAR);

    (* Puts a button on the web page. *)

    BEGIN
        WriteString ('<form method="get" action="');
        WriteString (progname);
        SendLine ('">');
        WriteString ('<input type=hidden name="D" value="');
        WriteDatabaseName (DB);
        SendLine ('">');
        IF (option[0] <> 'C') AND (option[0] <> 'E') THEN
            WriteString ('<input type=hidden name="P" value="');
            WriteString (ID);
            SendLine ('">');
        END (*IF*);
        WriteString ('<input type=hidden name="V" value="');
        WriteString (option);
        SendLine ('">');
        WriteString ('<input type="submit" value="');
        WriteString (label);
        SendLine ('"></form>');
    END MakeButton;

(************************************************************************)

PROCEDURE DisplayPerson (lang: LangHandle;  data: PersonData;
                                      VAR (*IN*) heading: DataString);

    (* Displays the information in the PersonData record. *)

    VAR label: ARRAY [0..255] OF CHAR;

    BEGIN
        (* The option buttons. *)

        WriteString ('<TABLE BORDER=0 CELLPADDING=10 CELLSPACING=0 RULES="ROWS" >');
        WriteString ('<TR ALIGN=CENTER VALIGN=TOP>');

        WriteString ('<TD>');
        StrToBuffer (lang, "Person.Descendants", label);
        MakeButton (data^.DB, data^.ID, 'D', label);
        WriteString ('</TD>');

        WriteString ('<TD>');
        StrToBuffer (lang, "Person.Ancestors", label);
        MakeButton (data^.DB, data^.ID, 'A', label);
        WriteString ('</TD>');

        WriteString ('<TD>');
        StrToBuffer (lang, "Person.Everyone", label);
        MakeButton (data^.DB, data^.ID, 'E', label);
        WriteString ('</TD>');

        WriteString ('<TD>');
        StrToBuffer (lang, "Person.ReallyEveryone", label);
        MakeButton (data^.DB, data^.ID, 'G', label);
        WriteString ('</TD>');

        SendLine ('</TR></TABLE>');

        DisplayPersonInfo (lang, data^.ID, data^.DB, data^.Tree,
                                                 '', heading, NIL);

    END DisplayPerson;

(************************************************************************)

PROCEDURE AppendCard (N: CARDINAL;  VAR (*INOUT*) str: ARRAY OF CHAR);

    (* Converts N to decimal, appends it to str. *)

    VAR j: CARDINAL;

    BEGIN
        IF N > 9 THEN
            AppendCard (N DIV 10, str);
            N := N MOD 10;
        END (*IF*);
        j := LENGTH(str);
        str[j] := CHR(ORD('0') + N);
        str[j+1] := Nul;
    END AppendCard;

(************************************************************************)

PROCEDURE DisplayDescendants (lang: LangHandle;  data: PersonData;
                                                 ShowDetails: BOOLEAN);

    (* Displays the names of this person and all direct ancestors. *)

    VAR ToDo, InsertionPoint: JobPtr;

    (********************************************************************)

    PROCEDURE AddJob (DB: Database;  ID: IDString;  level: CARDINAL;
                      prefix: DataString;  Info: RecordTree;
                      IsFamily, prevfemale: BOOLEAN);

        (* Adds one record into the list of jobs to be done.  The       *)
        (* new record is inserted after the one identified as           *)
        (* InsertionPoint, and then InsertionPoint is changed to point  *)
        (* to the record just inserted.                                 *)

        VAR this: JobPtr;

        BEGIN
            NEW (this);
            this^.DB := DB;
            this^.ID := ID;
            this^.level := level;
            this^.prefix := prefix;
            this^.tree := Info;
            this^.next := NIL;
            this^.family := IsFamily;
            this^.prevfemale := prevfemale;
            IF InsertionPoint = NIL THEN
                this^.next := ToDo;
                ToDo := this;
            ELSE
                this^.next := InsertionPoint^.next;
                InsertionPoint^.next := this;
            END (*IF*);
            InsertionPoint := this;
        END AddJob;

    (********************************************************************)

    VAR this: JobPtr;
        j, level, dummy: CARDINAL;
        datum, prefix, newprefix, string, heading: DataString;
        ID, SpouseID, FamilyID: IDString;
        Info: RecordTree;
        T2, TChild, TFamily: RecordTree;
        DB, famsDB, newDB: Database;
        count: ARRAY [0..128] OF CARDINAL;
        IsFamily, prevfemale: BOOLEAN;

    BEGIN
        IF ShowDetails THEN
            StrToBuffer (lang, "Person.NamesOnly", string);
            MakeButton (data^.DB, data^.ID, 'D', string);
        ELSE
            StrToBuffer (lang, "Person.Complete", string);
            MakeButton (data^.DB, data^.ID, 'D+', string);
        END (*IF*);
        WriteString ("<h2>");
        StrToBuffer (lang, "Person.DirectDescendants", string);
        WriteString (string);
        SendLine ("</h2>");

        (* Start by putting a single job on the ToDo list. *)

        ToDo := NIL;  InsertionPoint := NIL;
        AddJob (data^.DB, data^.ID, 1, "1", data^.Tree, FALSE, FALSE);

        (* Note that we have moved the 'Tree' structure to an entry in  *)
        (* the ToDo list, from which it will eventually be discarded.   *)
        (* Thus, the caller must be informed that this tree has been    *)
        (* deleted.                                                     *)

        data^.Tree := NIL;

        (* Now deal with the jobs on the ToDo list.  Initially there is *)
        (* only one such job, but as we proceed we will be adding other *)
        (* jobs.  It would have been easier to do this operation via a  *)
        (* recursive solution, but the recursive approach could cause a *)
        (* stack overflow, which is why I've moved to the notion of     *)
        (* having a list of jobs that are waiting to be done.           *)

        WHILE ToDo <> NIL DO

            this := ToDo;
            ToDo := this^.next;
            InsertionPoint := NIL;
            DB := this^.DB;
            level := this^.level;
            prefix := this^.prefix;
            ID := this^.ID;
            prevfemale := this^.prevfemale;
            Info := this^.tree;
            IsFamily := this^.family;
            DISPOSE (this);

            IF IsFamily THEN

                (* Write the data for one family.                  *)
                (* Start with the name of the spouse if available  *)
                (* and if we're not showing full details.          *)

                IF NOT ShowDetails THEN
                    newDB := DB;
                    IF prevfemale THEN
                        IF GetField ('HUSB', Info, string) THEN
                            ExtractTag (string, newDB, SpouseID);
                        ELSE
                            SpouseID[0] := Nul;
                        END (*IF*);
                    ELSE
                        IF GetField ('WIFE', Info, string) THEN
                            ExtractTag (string, newDB, SpouseID);
                        ELSE
                            SpouseID[0] := Nul;
                        END (*IF*);
                    END (*IF*);
                    IF SpouseID[0] <> Nul THEN
                        WriteString ("<br>");
                        WriteLn;
                        FOR j := 0 TO level DO
                            WriteString ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
                        END (*FOR*);
                        WriteLn;
                        WriteString ("<em>(+ ");
                        WritePersonLink (lang, newDB, SpouseID);
                        WriteString (")</em>");
                        WriteLn;
                    END (*IF*);
                END (*IF*);

                (* Extract the children of this marriage, and add *)
                (* them to the list of pending jobs.              *)

                WHILE GetField ('CHIL', Info, datum) DO
                    newprefix := prefix;
                    INC (count[level]);
                    AppendCard (count[level], newprefix);
                    newDB := DB;
                    ExtractTag (datum, newDB, ID);
                    SeekToMatchingID (newDB, ID);
                    LoadRecord (newDB, TChild);
                    AddJob (newDB, ID, level+1,
                                  newprefix, TChild, FALSE, FALSE);
                END (*WHILE*);

            ELSE

                (* Write the link for this person. *)

                WriteString ("<br>");

                IF ShowDetails THEN
                    WriteString ("<hr>");
                    T2 := CopyTree (Info);
                    PersonHeading (lang, T2, heading);
                    DisplayPersonInfo (lang, ID, DB, T2, prefix, heading, NIL);
                    DiscardTree (T2);
                    WriteString ("<br>");
                    WriteLn;
                ELSE
                    dummy := MAX(CARDINAL);
                    FOR j := 1 TO level DO
                        WriteString ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
                    END (*FOR*);
                    WriteString (prefix);
                    WriteString ("&nbsp;&nbsp;&nbsp;");
                    WriteLn;
                    WritePersonLink2 (lang, DB, ID, TRUE, Info, dummy);
                    WriteLn;
                END (*IF*);
                Strings.Append ('.', prefix);

                (* We will need to remember the sex of this person when *)
                (* calling AddJob.                                      *)

                prevfemale := GetField ('SEX', Info, datum)
                                     AND (CAP(datum[0]) = 'F');

                (* For each marriage ... *)

                count[level] := 0;
                WHILE GetField ('FAMS', Info, datum) DO
                    famsDB := DB;
                    ExtractTag (datum, famsDB, FamilyID);
                    SeekToMatchingID (famsDB, FamilyID);
                    LoadRecord (famsDB, TFamily);
                    AddJob (famsDB, FamilyID, level,
                                prefix, TFamily, TRUE, prevfemale);
                END (*WHILE*);
            END (*IF*);

            DiscardTree (Info);

        END (*WHILE*);

    END DisplayDescendants;

(************************************************************************)

PROCEDURE DisplayAncestors (lang: LangHandle;  data: PersonData;
                                               ShowDetails: BOOLEAN);

    (* Displays the names of this person and all direct ancestors. *)

    VAR ToDo: JobPtr;

    (********************************************************************)

    PROCEDURE AddJob (DB: Database;  ID: IDString;  level: CARDINAL;
                      prefix: DataString;  Info: RecordTree);

        (* Adds one record to the head of the list of jobs to be done. *)

        VAR this: JobPtr;

        BEGIN
            NEW (this);
            this^.DB := DB;
            this^.ID := ID;
            this^.level := level;
            this^.prefix := prefix;
            this^.tree := Info;
            this^.next := ToDo;
            this^.family := FALSE;
            this^.prevfemale := FALSE;
            ToDo := this;
        END AddJob;

    (********************************************************************)

    VAR TFamily: RecordTree;
        prefix: DataString;
        level: CARDINAL;

    (********************************************************************)

    PROCEDURE AddParent (DB: Database;  FieldName, SexCode: ARRAY OF CHAR);

        (* Adds a husband or wife of TFamily to the head of the ToDo list. *)

        VAR TNewPerson: RecordTree;
            PersonID: IDString;
            string, newprefix: DataString;
            newDB: Database;

        BEGIN
            IF GetField (FieldName, TFamily, string) THEN
                newprefix := prefix;
                Strings.Append (SexCode, newprefix);
                newDB := DB;
                ExtractTag (string, newDB, PersonID);
                SeekToMatchingID (newDB, PersonID);
                LoadRecord (newDB, TNewPerson);
                AddJob (newDB, PersonID, level+1, newprefix, TNewPerson);
                WriteLn;
            END (*IF*);
        END AddParent;

    (********************************************************************)

    VAR this: JobPtr;
        j, dummy: CARDINAL;
        datum, heading: DataString;
        ID, FamilyID: IDString;
        Info, T2: RecordTree;
        DB, famcDB: Database;

    BEGIN
        IF ShowDetails THEN
            StrToBuffer (lang, "Person.NamesOnly", datum);
            MakeButton (data^.DB, data^.ID, 'A', datum);
        ELSE
            StrToBuffer (lang, "Person.Complete", datum);
            MakeButton (data^.DB, data^.ID, 'A+', datum);
        END (*IF*);
        StrToBuffer (lang, "Person.AncestorsDirect", datum);
        WriteString ("<h2>");
        WriteString (datum);
        SendLine ("</h2>");

        (* Start by putting a single job on the ToDo list. *)

        IF NOT GetField ('SEX', data^.Tree, prefix) THEN
           prefix := '?';
        END (*IF*);
        ToDo := NIL;
        AddJob (data^.DB, data^.ID, 1, prefix, data^.Tree);

        (* Note that we have moved the 'Tree' structure to an entry in  *)
        (* the ToDo list, from which it will eventually be discarded.   *)
        (* Thus, the caller must be informed that this tree has been    *)
        (* deleted.                                                     *)

        data^.Tree := NIL;

        (* Now deal with the jobs on the ToDo list.  Initially there is *)
        (* only one such job, but as we proceed we will be adding other *)
        (* jobs.  It would have been easier to do this operation via a  *)
        (* recursive solution, but the recursive approach could cause a *)
        (* stack overflow, which is why I've moved to the notion of     *)
        (* having a list of jobs that are waiting to be done.           *)

        WHILE ToDo <> NIL DO

            this := ToDo;
            ToDo := this^.next;
            DB := this^.DB;
            level := this^.level;
            prefix := this^.prefix;
            ID := this^.ID;
            Info := this^.tree;
            DISPOSE (this);

            (* Write the link for this person (or the details, if       *)
            (* ShowDetails is TRUE.                                     *)

            WriteString ("<br>");
            IF ShowDetails THEN
                WriteString ("<hr>");
                T2 := CopyTree (Info);
                PersonHeading (lang, T2, heading);
                DisplayPersonInfo (lang, ID, DB, T2, prefix, heading, NIL);
                DiscardTree (T2);
                WriteLn;
            ELSE
                dummy := MAX(CARDINAL);
                FOR j := 1 TO level DO
                    WriteString ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
                END (*FOR*);
                WriteString (prefix);
                WriteString ("&nbsp;&nbsp;&nbsp;");
                WriteLn;
                WritePersonLink2 (lang, DB, ID, TRUE, Info, dummy);
                WriteLn;
            END (*IF*);

            (* Look up the family, then get husband and wife. *)

            IF GetField ('FAMC', Info, datum) THEN
                famcDB := DB;
                ExtractTag (datum, famcDB, FamilyID);

                SeekToMatchingID (famcDB, FamilyID);
                LoadRecord (famcDB, TFamily);

                (* Add the two parents to the 'ToDo' list. *)

                AddParent (famcDB, 'WIFE', 'F');
                AddParent (famcDB, 'HUSB', 'M');

                DiscardTree (TFamily);

            END (*IF*);

            DiscardTree (Info);

        END (*WHILE*);

    END DisplayAncestors;

(************************************************************************)

PROCEDURE CheckParentCrossReferences (DB: Database;
                           FamID: IDString;  DBL: DBList);

    (* Like WriteParent, except that we don't write anything. We simply *)
    (* update DBL if a new database is encountered.                     *)

    VAR T: RecordTree;  FatherID, MotherID: IDString;
        string: DataString;
        newDB: Database;

    BEGIN
        SeekToMatchingID (DB, FamID);
        LoadRecord (DB, T);
        IF GetField ('HUSB', T, string) THEN
            newDB := DB;
            ExtractTag (string, newDB, FatherID);
            IF newDB <> DB THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
        END (*IF*);
        IF GetField ('WIFE', T, string) THEN
            newDB := DB;
            ExtractTag (string, newDB, MotherID);
            IF newDB <> DB THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
        END (*IF*);
        DiscardTree (T);
    END CheckParentCrossReferences;

(************************************************************************)

PROCEDURE CheckMarriageCrossReferences (DB: Database;
                           FamID: IDString;  DBL: DBList);

    (* Like WriteMarriage, except that we don't write anything. We simply *)
    (* update DBL if a new database is encountered.                     *)

    VAR T: RecordTree;
        newDB: Database;
        string: DataString;
        PersonID: IDString;

    BEGIN
        SeekToMatchingID (DB, FamID);
        LoadRecord (DB, T);

        (* Husband *)

        IF GetField ('HUSB', T, string) THEN
            newDB := DB;
            ExtractTag (string, newDB, PersonID);
            IF newDB <> DB THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
        END (*IF*);

        (* Wife *)

        IF GetField ('WIFE', T, string) THEN
            newDB := DB;
            ExtractTag (string, newDB, PersonID);
            IF newDB <> DB THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
        END (*IF*);

        (* Children *)

        IF GetField ('CHIL', T, string) THEN
            REPEAT
                newDB := DB;
                ExtractTag (string, newDB, PersonID);
                IF newDB <> DB THEN
                    AddDatabase (newDB, DBL);
                END (*IF*);
            UNTIL NOT GetField ('CHIL', T, string);
        END (*IF*);

        DiscardTree (T);

    END CheckMarriageCrossReferences;

(************************************************************************)

PROCEDURE UpdateCrossreferences (DB: Database;  ID: IDString;
                                  VAR (*INOUT*) T: RecordTree;
                                  DBL: DBList);

    (* Checks whether the information for person "ID" contains          *)
    (* cross-references to other databases, and if so updates DBL.      *)

    VAR datum: DataString;
        newDB: Database;

    BEGIN
        (* Parents. *)

        IF GetField ('FAMC', T, datum) THEN
            newDB := DB;
            ExtractTag (datum, newDB, ID);
            IF newDB <> DB THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
            CheckParentCrossReferences (newDB, ID, DBL);
            WriteLn;
        END (*IF*);

        (* Marriages. *)

        WHILE GetField ('FAMS', T, datum) DO
            newDB := DB;
            ExtractTag (datum, newDB, ID);
            IF newDB <> DB THEN
                AddDatabase (newDB, DBL);
            END (*IF*);
            CheckMarriageCrossReferences (newDB, ID, DBL);
        END (*WHILE*);
    END UpdateCrossreferences;

(************************************************************************)

PROCEDURE DisplayAll (lang: LangHandle;  DB: Database;
                                      ShowDetails: BOOLEAN;  DBL: DBList);

    (* Displays the names of everyone in the database. If DBL is not    *)
    (* NIL, updates DBL to include any Databases encounted as           *)
    (* cross-references.                                                *)

    VAR NextLevel, count, dummy: CARDINAL;  Lookahead: Line;
        heading: DataString;
        T: RecordTree;
        ID: IDString;  pos: FilePos;
        label: ARRAY [0..255] OF CHAR;

    BEGIN
        StartReading (DB, NextLevel, Lookahead);
        count := 0;
        WHILE LoadNextINDIRecord (DB, ID, T, NextLevel, Lookahead) DO
            INC (count);
            IF ShowDetails THEN
                pos := SavePosition (DB);
                PersonHeading (lang, T, heading);
                DisplayPersonInfo (lang, ID, DB, T, '', heading, DBL);
                RestorePosition (DB, pos);
                WriteString ("<br><hr>");
                WriteLn;
            ELSE
                dummy := MAX(CARDINAL);
                IF DBL <> NIL THEN
                    pos := SavePosition (DB);
                    UpdateCrossreferences (DB, ID, T, DBL);
                    RestorePosition (DB, pos);
                END (*IF*);
                WritePersonLink2 (lang, DB, ID, TRUE, T, dummy);
                WriteString ("<br>");
                WriteLn;
            END (*IF*);
            DiscardTree (T);
        END (*WHILE*);
        WriteString ("<p>");
        StrToBufferN (lang, "Person.Total", count, label);
        WriteString (label);
        WriteString ("</p>");
        WriteLn;
    END DisplayAll;

(************************************************************************)

PROCEDURE DisplayEveryone (lang: LangHandle;  dummy: PersonData;
                                              ShowDetails: BOOLEAN);

    (* Displays the names of everyone in the database.  The person      *)
    (* mentioned in the argument is unimportant, but we need some       *)
    (* person record so that we can identify the database.              *)

    VAR label: ARRAY [0..255] OF CHAR;

    BEGIN
        IF NOT ShowDetails THEN
            StrToBuffer (lang, "Person.Complete", label);
            MakeButton (dummy^.DB, "", 'E+', label);
        END (*IF*);
        WriteString ("<h2>");
        StrToBuffer (lang, "Person.AllPeople", label);
        WriteString (label);
        SendLine ("</h2>");
        DisplayAll (lang, dummy^.DB, ShowDetails, NIL);
    END DisplayEveryone;

(************************************************************************)

PROCEDURE DisplayEveryEveryone (lang: LangHandle;  dummy: PersonData;
                                              ShowDetails: BOOLEAN);

    (* Displays the names of everyone in the database, and in all       *)
    (* databases linked from this one.  The person mentioned in the     *)
    (* argument is unimportant, but we need some person record so that  *)
    (* we can identify a starting point.                                *)

    VAR DBL, p: DBList;
        label: ARRAY [0..255] OF CHAR;
        DividerWanted: BOOLEAN;

    BEGIN
        (* We suppress the "Show details" button here, on the grounds   *)
        (* that it would tie up the server and - especially - the       *)
        (* client for an unacceptably long time.                        *)

        (*
        IF NOT ShowDetails THEN
            StrToBuffer (lang, "Person.Complete", label);
            MakeButton (dummy^.DB, "", 'G+', label);
        END (*IF*);
        *)

        WriteString ("<h2>");
        StrToBuffer (lang, "Person.AllDatabases", label);
        WriteString (label);
        SendLine ("</h2>");

        (* Create a list of Databases to be processed. Initially the    *)
        (* list contains only one element, but the calls to DisplayAll  *)
        (* might add more databases.                                    *)

        NEW (DBL);
        WITH DBL^ DO
            this := dummy^.DB;
            next := NIL;
        END (*WITH*);
        DividerWanted := FALSE;

        (* Now work our way through the list, displaying one database   *)
        (* at a time.                                                   *)

        p := DBL;
        WHILE p <> NIL DO
            IF DividerWanted THEN
                WriteString ("<br><hr><hr>");
                WriteLn;
            END (*IF*);
            WriteString ("<h3>");
            StrToBuffer (lang, "WFT.DatabaseName", label);
            WriteString (label);
            WriteString (": ");
            WriteDatabaseName (p^.this);
            WriteString (".ged");
            SendLine ("</h3>");
            DisplayAll (lang, p^.this, ShowDetails, DBL);
            DividerWanted := TRUE;
            p := p^.next;
        END (*WHILE*);

        (* Discard the DBlist. *)

        WHILE DBL <> NIL DO
            p := DBL^.next;
            DISPOSE (DBL);
            DBL := p;
        END (*WHILE*);

    END DisplayEveryEveryone;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

END Person.

