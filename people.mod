IMPLEMENTATION MODULE People;

        (********************************************************)
        (*                                                      *)
        (*      Stripped-down version of module Person for      *)
        (*                 use in Ged2HTML                      *)
        (*                                                      *)
        (*   The two modules 'Person' and 'People' are almost   *)
        (*   identical, with this one being a little smaller    *)
        (*   because of some routines that are never needed.    *)
        (*   At present I'm trying to see whether they can be   *)
        (*   combined.  The really important difference is that *)
        (*   this module imports from GEDLook while Person      *)
        (*   imports from GELookup, so what I first have to     *)
        (*   resolve is whether those lower-level modules       *)
        (*   have to be kept separate.                          *)
        (*                                                      *)
        (*   It would be nice to have a merger if possible,     *)
        (*   because another difference is that WFT has new     *)
        (*   features that Ged2HTML has not caught up with.     *)
        (*                                                      *)
        (*   Conclusion so far: a merger would hurt WFT         *)
        (*   efficiency, because WFT writes to standard output  *)
        (*   while Ged2HTML writes to a file - a small          *)
        (*   difference, but all-pervading.                     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            29 August 2005                  *)
        (*  Last edited:        30 June 2009                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM OurTypes IMPORT
    (* type *)  IDString, DataString, FilenameString, Line;

FROM GEDLook IMPORT
    (* type *)  Database, RecordTree,
    (* proc *)  OpenDatabase, CloseDatabase,  CharSetOf,
                SeekToMatchingID, LoadRecord0, LoadRecord, DiscardTree,
                CopyField, CopyName, HeadMatch,
                GetName, GetField, ExtractSubrecord, ExtractEither,
                DisplayRawLines, GetDateRange,
                WritePersonLink,
                StartReading, LoadNextINDIRecord, NonEmpty,
                SavePosition, RestorePosition;

FROM FileOps IMPORT
    (* type *)  ChanId, FilePos,
    (* proc *)  FWriteChar, FWriteString, FWriteLn, FWriteCard;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferA, StrToBufferAB, StrToBufferN;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

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

VAR
    (* The database we started with. *)

    BaseDB: Database;

(************************************************************************)

PROCEDURE GetInitialData (DatabaseName: FilenameString;
                          VAR (*OUT*) result: PersonData);

    (* Opens the database, returns a result that can subsequently be    *)
    (* used for accessing that database.                                *)

    BEGIN
        NEW (result);
        result^.ID := "";
        result^.Tree := NIL;
        IF NOT OpenDatabase (result^.DB, DatabaseName) THEN
            DISPOSE (result);
        END (*IF*);
    END GetInitialData;

(************************************************************************)

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

PROCEDURE SendLine (cid: ChanId;  line: ARRAY OF CHAR);

    (* Sends a string, terminated with CRLF, to the channel. *)

    BEGIN
        FWriteString (cid, line);  FWriteLn (cid);
    END SendLine;

(************************************************************************)

PROCEDURE WrDateAndPlace (cid: ChanId;  lang: LangHandle;  DP: DatePlace;
                                             DateRequired: BOOLEAN);

    (* Writes a date and place to standard output.  *)

    VAR message: ARRAY [0..255] OF CHAR;

    BEGIN
        IF DP.empty THEN
            StrToBuffer (lang, "Person.NotRecorded", message);
            FWriteString (cid, message);
        ELSE
            IF (DP.date[0] = Nul) AND DateRequired THEN
                StrToBuffer (lang, "Person.DateNotRecorded", message);
                FWriteString (cid, message);
            ELSE
                FWriteString (cid, DP.date);
            END (*IF*);
            IF DP.place[0] <> Nul THEN
                IF (DP.date[0] <> Nul) OR DateRequired THEN
                    FWriteString (cid, "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
                END (*IF*);
                FWriteString (cid, DP.place);
            END (*IF*);
        END (*IF*);
    END WrDateAndPlace;

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
        REPEAT
            found := ExtractSubrecord ('NOTE', T, datum, subtree);
            IF found AND (datum[0] = '@') THEN
                newDB := DB;
                ExtractTag (datum, newDB, NoteID);
                T2 := LoadRecord0 (newDB, NoteID);
                found := FindFirstNote (newDB, T2, datum, subtree);
                DiscardTree (T2);
            END (*IF*);
            nonblank := (datum[0] <> Nul) OR NonEmpty(subtree);
        UNTIL NOT found OR nonblank;
        RETURN found AND nonblank;
    END FindFirstNote;

(************************************************************************)

PROCEDURE WriteMarriage (cid: ChanId;  lang: LangHandle;  M: CARDINAL;
                         DB: Database;  FamID: IDString;  sex: CHAR);

    (* Writes the details of marriage M.  Parameter 'sex' is the sex of *)
    (* the person whose marriage we are noting.                         *)

    VAR T, subtree: RecordTree;  PersonID: IDString;
        string: DataString;
        label: ARRAY [0..255] OF CHAR;
        newDB: Database;
        DP: DatePlace;
        NeedBreak: BOOLEAN;
        sexstr: ARRAY [0..0] OF CHAR;

    BEGIN
        sexstr[0] := sex;
        IF sexstr[0] <> 'F' THEN sexstr[0] := 'M' END (*IF*);
        FWriteString (cid, "<h3>");
        StrToBuffer (lang, "Person.Family", label);
        FWriteString (cid, label);
        IF M > 1 THEN
            FWriteString (cid, " ");
            FWriteCard (cid, M, 5);
        END (*IF*);
        FWriteString (cid, "</h3>");
        NeedBreak := FALSE;

        SeekToMatchingID (DB, FamID);
        LoadRecord (DB, T);

        (* Date last changed. *)

        IF ExtractSubrecord ('CHAN', T, string, subtree)
                            AND GetField ('DATE', subtree, string) THEN
            FWriteString (cid, "<font size=-1>");
            StrToBuffer (lang, "Person.Changed", label);
            FWriteChar (cid, '(');
            FWriteString (cid, label);
            FWriteChar (cid, ' ');
            FWriteString (cid, string);
            FWriteChar (cid, ')');
            FWriteString (cid, "</font>");
            FWriteString (cid, "<br><br>");
            DiscardTree (subtree);
        END (*IF*);

        (* Date and place of marriage. *)

        IF ExtractSubrecord ('MARR', T, string, subtree) THEN
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    StrToBuffer (lang, "Person.Marriage", label);
                    FWriteString (cid, label);
                    WrDateAndPlace (cid, lang, DP, TRUE);
                    FWriteLn (cid);
                    NeedBreak := TRUE;
                END (*IF*);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Husband *)

        IF GetField ('HUSB', T, string) AND (sex <> 'M') THEN
            newDB := DB;
            ExtractTag (string, newDB, PersonID);
            IF NeedBreak THEN
                FWriteString (cid, "<br>");
            END (*IF*);
            StrToBuffer (lang, "Person.Husband", label);
            FWriteString (cid, label);
            WritePersonLink (cid, lang, newDB, PersonID, newDB <> BaseDB);
            FWriteLn (cid);
            NeedBreak := TRUE;
        END (*IF*);

        (* Wife *)

        IF GetField ('WIFE', T, string) AND (sex <> 'F') THEN
            newDB := DB;
            ExtractTag (string, newDB, PersonID);
            IF NeedBreak THEN
                FWriteString (cid, "<br>");
            END (*IF*);
            StrToBuffer (lang, "Person.Wife", label);
            FWriteString (cid, label);
            WritePersonLink (cid, lang, newDB, PersonID, newDB <> BaseDB);
            FWriteLn (cid);
            NeedBreak := TRUE;
        END (*IF*);

        (* Children *)

        IF GetField ('CHIL', T, string) THEN
            IF NeedBreak THEN
                FWriteString (cid, "<p>");
            END (*IF*);
            StrToBuffer (lang, "Person.Children", label);
            FWriteString (cid, label);
            FWriteLn (cid);
            REPEAT
                FWriteString (cid, "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
                newDB := DB;
                ExtractTag (string, newDB, PersonID);
                WritePersonLink (cid, lang, newDB, PersonID, newDB <> BaseDB);
                FWriteLn (cid);
            UNTIL NOT GetField ('CHIL', T, string);
        END (*IF*);

        (* Date and place of divorce. *)

        IF ExtractSubrecord ('DIV', T, string, subtree) THEN
            FWriteString (cid, "<p>");
            string := "Person.Divorced.";
            Strings.Append (sexstr, string);
            StrToBuffer (lang, string, label);
            FWriteString (cid, label);
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                WrDateAndPlace (cid, lang, DP, TRUE);
                FWriteLn (cid);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Notes. *)

        IF FindFirstNote (DB, T, string, subtree) THEN
            FWriteString (cid, "<h4>");
            StrToBuffer (lang, "Person.MarriageNotes", label);
            FWriteString (cid, label);
            FWriteString (cid, "</h4>");
            FWriteLn (cid);
            REPEAT
                SendLine (cid, '<p>');
                FWriteString (cid, string);
                WHILE GetField ('CONT', subtree, string)
                               OR GetField ('CONT', subtree, string) DO
                    FWriteChar (cid, ' ');
                    FWriteString (cid, string);
                END (*IF*);
                FWriteLn (cid);
                DiscardTree (subtree);
                SendLine (cid, '</p>');
            UNTIL NOT FindFirstNote (DB, T, string, subtree);
        END (*IF*);

        (* Miscellaneous details. *)

        IF NonEmpty(T) THEN
            FWriteString (cid, "<h4>");
            StrToBuffer (lang, "Person.OtherMarriageDetails", label);
            FWriteString (cid, label);
            FWriteString (cid, "</h4>");
            DisplayRawLines (cid, T);
        END (*IF*);

        DiscardTree (T);

    END WriteMarriage;

(************************************************************************)

PROCEDURE WritePartialTree (cid: ChanId;  lang: LangHandle;
                                PersonID: IDString;
                                     DB: Database;  T: RecordTree);

    (* Writes a small subtree consisting of the given person plus a     *)
    (* small number of ancestors.                                       *)

    (********************************************************************)

    CONST
        topcorner = '.';  bottomcorner = '`';
        PreLength = 75;
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
            EVAL (CopyName (T, thisname));
            newhotspot := hotspot;
            IF NOT FirstTime THEN
                INC (newhotspot, LENGTH(thisname) + 5);
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
                    FWriteString (cid, "  |----");
                    FWriteString (cid, thisname);
                    FWriteLn (cid);
                END (*IF*);
            ELSE
                IF TopHalf THEN
                    prefix[hotspot] := topcorner;
                ELSE
                    prefix[hotspot] := bottomcorner;
                END (*IF*);
                FWriteString (cid, prefix);
                FWriteString (cid, "--");
                WritePersonLink (cid, lang, DB, PersonID, DB <> BaseDB);
                IF HaveFamily THEN
                    FWriteString (cid, "--|");
                END (*IF*);
                FWriteLn (cid);
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

        END WriteBranch;

    (********************************************************************)

    VAR j: CARDINAL;

    BEGIN                               (* body of WritePartialTree *)
        FOR j := 0 TO PreLength DO
            prefix[j] := ' ';
        END (*FOR*);
        FWriteString (cid, "<PRE>");  FWriteLn (cid);
        WriteBranch (PersonID, DB, T, 2, TRUE, TRUE);
        FWriteString (cid, "</PRE>");  FWriteLn (cid);
    END WritePartialTree;

(************************************************************************)

PROCEDURE WriteParents (cid: ChanId;  lang: LangHandle;
                              DB: Database;  FamID: IDString);

    (* Writes the names of the two partners to the given family ID,     *)
    (* labelled as father and mother.                                   *)

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
            FWriteString (cid, "<p>");
            StrToBuffer (lang, "Person.Father", label);
            FWriteString (cid, label);
            FWriteString (cid, " &nbsp;");
            WritePersonLink (cid, lang, newDB, FatherID, newDB <> BaseDB);
            NeedBreak := TRUE;
        END (*IF*);
        IF GetField ('WIFE', T, string) THEN
            newDB := DB;
            ExtractTag (string, newDB, MotherID);
            IF NeedBreak THEN
                FWriteString (cid, "<br>");
            END (*IF*);
            StrToBuffer (lang, "Person.Mother", label);
            FWriteString (cid, label);
            FWriteString (cid, " ");
            WritePersonLink (cid, lang, newDB, MotherID, newDB <> BaseDB);
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

PROCEDURE WriteEventDetail (lang: LangHandle;  VAR (*INOUT*) T: RecordTree);

    (* Writes the date and place information in T. *)

    VAR datum: DataString;  subtree: RecordTree;

    BEGIN
        EVAL (GetField ('DATE', T, datum));
        IF datum[0] <> Nul THEN
            TranslateDate (lang, datum);
            WriteString (datum);
        END (*IF*);
        EVAL (GetField ('PLAC', T, datum));
        IF datum[0] <> Nul THEN
            WriteString ("<br>&nbsp;&nbsp;&nbsp;&nbsp;");
            WriteString (datum);
        END (*IF*);
        IF ExtractSubrecord ('ADDR', T, datum, subtree) THEN
            IF datum[0] <> Nul THEN
                WriteIndented (4, datum);
            END (*IF*);
            WHILE GetField ('CONT', subtree, datum) DO
                WriteIndented (4, datum);
            END (*WHILE*);
            IF GetField ('ADR1', subtree, datum) THEN
                WriteIndented (4, datum);
            END (*IF*);
            IF GetField ('ADR2', subtree, datum) THEN
                WriteIndented (4, datum);
            END (*IF*);
            IF GetField ('CITY', subtree, datum) THEN
                WriteIndented (4, datum);
            END (*IF*);
            IF GetField ('STAE', subtree, datum) THEN
                WriteIndented (4, datum);
            END (*IF*);
            IF GetField ('POST', subtree, datum) THEN
                WriteIndented (4, datum);
            END (*IF*);
            IF GetField ('CTRY', subtree, datum) THEN
                WriteIndented (4, datum);
            END (*IF*);
        END (*IF*);
        IF GetField ('PHON', T, datum) THEN
            WriteIndented (4, datum);
        END (*IF*);
        DiscardTree (T);
    END WriteEventDetail;

(************************************************************************)

PROCEDURE DisplayPersonInfo (cid: ChanId;  lang: LangHandle;
                             PersonID: IDString;  DB: Database;
                             VAR (*INOUT*) T: RecordTree;
                             prefix: ARRAY OF CHAR);

    (* Displays all the information for one person. *)

    VAR datum, message: DataString;  subtree: RecordTree;
        ID: IDString;  M: CARDINAL;
        newDB: Database;
        DP: DatePlace;
        sex: ARRAY [0..0] OF CHAR;
        knownsex: CHAR;

    BEGIN
        (* Insert a tag. *)

        FWriteString (cid, '<a name="');
        FWriteString (cid, PersonID);
        FWriteString (cid, '"></a>');

        (* Use name and date range as heading. *)

        FWriteString (cid, "<h2>");
        IF prefix[0] <> Nul THEN
            FWriteString (cid, '<font color=RED>');
            FWriteString (cid, prefix);
            FWriteString (cid, '</font>');
            FWriteString (cid, '&nbsp;&nbsp;&nbsp;');
        END (*IF*);
        IF NOT CopyName (T, datum) THEN
            StrToBuffer (lang, "Person.NoName", datum);
        END (*IF*);
        FWriteString (cid, datum);
        GetDateRange (T, datum);
        IF datum[0] <> Nul THEN
            FWriteString (cid, "&nbsp;&nbsp;");  FWriteString (cid, datum);
        END (*IF*);
        SendLine (cid, "</h2>");

        (* Occupation. *)

        IF GetField ('OCCU', T, datum) AND (datum[0] <> Nul) THEN
            FWriteString (cid, datum);
            FWriteString (cid, "<br>");
            FWriteLn (cid);
        END (*IF*);

        (* Sex. *)

        knownsex := Nul;      (* Nul code means 'unknown' *)

        IF GetField ('SEX', T, datum) AND (datum[0] <> Nul) THEN
            StrToBuffer (lang, "Person.Sex", message);
            FWriteString (cid, message);
            FWriteString (cid, datum);
            FWriteLn (cid);
            knownsex := CAP(datum[0]);
        END (*IF*);
        FWriteString (cid, "<P>");

        (* From now on, 'sex' is used only for language translation,    *)
        (* and we must guarantee that its value is either 'M' or 'F'.   *)

        sex[0] := knownsex;
        IF sex[0] <> 'F' THEN sex[0] := 'M' END (*IF*);

        (* Date and place of birth. *)

        IF ExtractSubrecord ('BIRT', T, datum, subtree) THEN
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    datum := "Person.Born.";
                    Strings.Append (sex, datum);
                    StrToBuffer (lang, datum, message);
                    FWriteString (cid, message);
                    WrDateAndPlace (cid, lang, DP, TRUE);
                    FWriteLn (cid);
                    FWriteString (cid, "<br>");
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
                    FWriteString (cid, message);
                    WrDateAndPlace (cid, lang, DP, FALSE);
                    FWriteLn (cid);
                    FWriteString (cid, "<br>");
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
                    FWriteString (cid, message);
                    WrDateAndPlace (cid, lang, DP, TRUE);
                    FWriteLn (cid);
                    FWriteString (cid, "<br>");
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
                    FWriteString (cid, message);
                    WrDateAndPlace (cid, lang, DP, TRUE);
                    FWriteLn (cid);
                    FWriteString (cid, "<br>");
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
                    FWriteString (cid, message);
                    WrDateAndPlace (cid, lang, DP, FALSE);
                    FWriteLn (cid);
                    FWriteString (cid, "<br>");
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
                    FWriteString (cid, message);
                    WrDateAndPlace (cid, lang, DP, FALSE);
                    FWriteLn (cid);
                    FWriteString (cid, "<br>");
                END (*IF*);
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Partial tree. *)

        WritePartialTree (cid, lang, PersonID, DB, T);
        EVAL (GetName (T, datum));     (* name field no longer needed *)

        (* Residences. *)

        WHILE ExtractSubrecord ('RESI', T, datum, subtree) DO
            StrToBuffer (lang, "Person.Residence", message);
            WriteString (message);
            WriteEventDetail (lang, subtree);
            WriteLn;
            WriteString ("<br>");
            DiscardTree (subtree);
        END (*WHILE*);

        (* Parents. *)

        IF GetField ('FAMC', T, datum) THEN
            newDB := DB;
            ExtractTag (datum, newDB, ID);
            WriteParents (cid, lang, newDB, ID);
            FWriteLn (cid);
        END (*IF*);

        (* Marriages. *)

        M := 1;
        WHILE GetField ('FAMS', T, datum) DO
            newDB := DB;
            ExtractTag (datum, newDB, ID);
            WriteMarriage (cid, lang, M, newDB, ID, knownsex);
            INC (M);
        END (*WHILE*);

        (* Notes. *)

        IF FindFirstNote (DB, T, datum, subtree) THEN
            FWriteString (cid, "<h3>");
            StrToBuffer (lang, "Person.Notes", message);
            FWriteString (cid, message);
            FWriteString (cid, "</h3>");
            FWriteLn (cid);
            REPEAT
                SendLine (cid, '<p>');
                FWriteString (cid, datum);
                WHILE GetField ('CONT', subtree, datum)
                               OR GetField ('CONT', subtree, datum) DO
                    FWriteChar (cid, ' ');
                    FWriteString (cid, datum);
                END (*IF*);
                FWriteLn (cid);
                DiscardTree (subtree);
                SendLine (cid, '</p>');
            UNTIL NOT FindFirstNote (DB, T, datum, subtree);
        END (*IF*);

        (* Discard mailing address, which we consider to be private. *)

        WHILE ExtractSubrecord ('ADDR', T, datum, subtree) DO
            DiscardTree (subtree);
        END (*WHILE*);

        (* Date last changed. *)

        IF ExtractSubrecord ('CHAN', T, datum, subtree)
                            AND GetField ('DATE', subtree, datum) THEN
            FWriteString (cid, "<br><font size=-1>");
            StrToBuffer (lang, "Person.Changed", message);
            FWriteChar (cid, '(');
            FWriteString (cid, message);
            FWriteChar (cid, ' ');
            FWriteString (cid, datum);
            FWriteChar (cid, ')');
            FWriteString (cid, "</font>");
            FWriteString (cid, "<br>");
            DiscardTree (subtree);
        END (*IF*);

        (* Display what's left in raw form. *)

        IF NonEmpty(T) THEN
            FWriteString (cid, "<h3>");
            StrToBuffer (lang, "Person.OtherDetails", message);
            FWriteString (cid, message);
            FWriteString (cid, "</h3>");
            DisplayRawLines (cid, T);
            DiscardTree (T);
        END (*IF*);

    END DisplayPersonInfo;

(************************************************************************)

PROCEDURE DisplayEveryone (cid: ChanId;  lang: LangHandle;  dummy: PersonData);

    (* Displays the names of everyone in the database.  The person      *)
    (* mentioned in the argument is unimportant, but we need some       *)
    (* person record so that we can identify the database.              *)

    VAR NextLevel, count: CARDINAL;  Lookahead: Line;
        T: RecordTree;  DB: Database;  success: BOOLEAN;
        ID: IDString;  pos: FilePos;
        label: ARRAY [0..255] OF CHAR;

    BEGIN
        IF dummy <> NIL THEN
            DB := dummy^.DB;
            BaseDB := DB;
            StartReading (DB, NextLevel, Lookahead);
            count := 0;
            REPEAT
                success := LoadNextINDIRecord (DB, ID, T, NextLevel, Lookahead);
                IF success THEN
                    INC (count);
                    pos := SavePosition (DB);
                    DisplayPersonInfo (cid, lang, ID, DB, T, '');
                    RestorePosition (DB, pos);
                    FWriteString (cid, "<br><hr>");
                    FWriteLn (cid);
                    DiscardTree (T);
                END (*IF*);
            UNTIL NOT success;
            FWriteString (cid, "<p>");
            StrToBufferN (lang, "Person.Total", count, label);
            FWriteString (cid, label);
            FWriteLn (cid);
        END (*IF*);
    END DisplayEveryone;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

END People.

