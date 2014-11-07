IMPLEMENTATION MODULE Person;

        (********************************************************)
        (*                                                      *)
        (*      Web Family Tree: module to look up and          *)
        (*           display the data for a person              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            6 July 2001                     *)
        (*  Last edited:        28 January 2005                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM OurTypes IMPORT
    (* type *)  IDString, DataString, FilenameString, Line;

FROM GELookup IMPORT
    (* type *)  Database, RecordTree,
    (* proc *)  OpenDatabase, CloseDatabase,  CharSetOf,
                SeekToMatchingID, LoadRecord0, LoadRecord, DiscardTree,
                CopyTree, CopyField, CopyName,
                GetName, GetField, ExtractSubrecord, ExtractEither,
                DisplayRawLines, GetDateRange,
                WritePersonLink, WritePersonLink2, WriteDatabaseName,
                StartReading, LoadNextINDIRecord, NonEmpty,
                SavePosition, RestorePosition;

FROM FileOps IMPORT
    (* type *)  FilePos;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferN;

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
        SeekToMatchingID (result^.DB, PersonID);
        LoadRecord (result^.DB, result^.Tree);
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

PROCEDURE WriteMarriage (lang: LangHandle;  M: CARDINAL;
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
        WriteString ("<h3>");
        StrToBuffer (lang, "Person.Family", label);
        WriteString (label);
        IF M > 1 THEN
            WriteString (" ");
            WriteCard (M);
        END (*IF*);
        WriteString ("</h3>");
        NeedBreak := FALSE;

        SeekToMatchingID (DB, FamID);
        LoadRecord (DB, T);

        (* Date and place of marriage. *)

        IF ExtractSubrecord ('MARR', T, string, subtree) THEN
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                IF NOT DP.empty THEN
                    StrToBuffer (lang, "Person.Marriage", label);
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
                WritePersonLink (lang, newDB, PersonID);
                WriteLn;
            UNTIL NOT GetField ('CHIL', T, string);
        END (*IF*);

        (* Date and place of divorce. *)

        IF ExtractSubrecord ('DIV', T, string, subtree) THEN
            WriteString ("<p>");
            string := "Person.Divorced.";
            Strings.Append (sexstr, string);
            StrToBuffer (lang, string, label);
            WriteString (label);
            IF NonEmpty(subtree) THEN
                ExtractDateAndPlace (subtree, DP);
                WrDateAndPlace (lang, DP, TRUE);
                WriteLn;
                DiscardTree (subtree);
            END (*IF*);
        END (*IF*);

        (* Notes. *)

        IF FindFirstNote (DB, T, string, subtree) THEN
            WriteString ("<h4>");
            StrToBuffer (lang, "Person.MarriageNotes", label);
            WriteString (label);
            WriteString ("</h4>");
            WriteLn;
            REPEAT
                SendLine ('<p>');
                WriteString (string);
                WHILE GetField ('CONT', subtree, string) DO
                    WriteChar (' ');
                    WriteString (string);
                END (*IF*);
                WriteLn;
                DiscardTree (subtree);
                SendLine ('</p>');
            UNTIL NOT FindFirstNote (DB, T, string, subtree);
        END (*IF*);

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
    END WritePartialTree;

(************************************************************************)

PROCEDURE WriteParents (lang: LangHandle;  DB: Database;  FamID: IDString);

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

PROCEDURE DisplayPersonInfo (lang: LangHandle;  PersonID: IDString;
                             DB: Database;
                             VAR (*INOUT*) T: RecordTree;
                             prefix: ARRAY OF CHAR);

    (* Displays all the information for one person. *)

    VAR datum, message: DataString;  subtree: RecordTree;
        ID: IDString;  M: CARDINAL;
        newDB: Database;
        DP: DatePlace;
        sex: ARRAY [0..0] OF CHAR;

    BEGIN
        (* Use name and date range as heading. *)

        WriteString ("<h2>");
        IF prefix[0] <> Nul THEN
            WriteString ('<font color=RED>');
            WriteString (prefix);
            WriteString ('</font>');
            WriteString ('&nbsp;&nbsp;&nbsp;');
        END (*IF*);
        IF NOT CopyName (T, datum) THEN
            StrToBuffer (lang, "Person.NoName", datum);
            datum := "No name";
        END (*IF*);
        WriteString (datum);
        GetDateRange (T, datum);
        IF datum[0] <> Nul THEN
            WriteString ("&nbsp;&nbsp;");  WriteString (datum);
        END (*IF*);
        SendLine ("</h2>");

        (* Occupation. *)

        IF GetField ('OCCU', T, datum) AND (datum[0] <> Nul) THEN
            WriteString (datum);
            WriteString ("<br>");
            WriteLn;
        END (*IF*);

        (* Sex. *)

        sex[0] := Nul;      (* empty string means 'unknown' *)

        IF GetField ('SEX', T, datum) THEN
            StrToBuffer (lang, "Person.Sex", message);
            WriteString (message);
            WriteString (datum);
            WriteLn;
            sex[0] := CAP(datum[0]);
        END (*IF*);
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

        (* Parents. *)

        IF GetField ('FAMC', T, datum) THEN
            newDB := DB;
            ExtractTag (datum, newDB, ID);
            WriteParents (lang, newDB, ID);
            WriteLn;
        END (*IF*);

        (* Marriages. *)

        M := 1;
        WHILE GetField ('FAMS', T, datum) DO
            newDB := DB;
            ExtractTag (datum, newDB, ID);
            WriteMarriage (lang, M, newDB, ID, sex[0]);
            INC (M);
        END (*WHILE*);

        (* Notes. *)

        IF FindFirstNote (DB, T, datum, subtree) THEN
            WriteString ("<h3>");
            StrToBuffer (lang, "Person.Notes", message);
            WriteString (message);
            WriteString ("</h3>");
            WriteLn;
            REPEAT
                SendLine ('<p>');
                WriteString (datum);
                WHILE GetField ('CONT', subtree, datum) DO
                    WriteChar (' ');
                    WriteString (datum);
                END (*IF*);
                WriteLn;
                DiscardTree (subtree);
                SendLine ('</p>');
            UNTIL NOT FindFirstNote (DB, T, datum, subtree);
        END (*IF*);

        (* Discard mailing address, which we consider to be private. *)

        WHILE ExtractSubrecord ('ADDR', T, datum, subtree) DO
            DiscardTree (subtree);
        END (*WHILE*);

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
        WriteString ('<form method="post" action="wft.cmd?D=');
        WriteDatabaseName (DB);
        WriteString (';P=');
        WriteString (ID);
        WriteString (';V=');
        WriteString (option);
        SendLine ('">');
        SendLine ('<input type=hidden name="dummy" value="">');
        WriteString ('<input type="submit" value="');
        WriteString (label);
        WriteString ('">');
        SendLine ('</form>');
    END MakeButton;

(************************************************************************)

PROCEDURE DisplayPerson (lang: LangHandle;  data: PersonData);

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

        SendLine ('</TR></TABLE>');

        DisplayPersonInfo (lang, data^.ID, data^.DB, data^.Tree, '');

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
        j, level: CARDINAL;
        datum, prefix, newprefix, string: DataString;
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
                    DisplayPersonInfo (lang, ID, DB, T2, prefix);
                    DiscardTree (T2);
                    WriteString ("<br>");
                    WriteLn;
                ELSE
                    FOR j := 1 TO level DO
                        WriteString ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
                    END (*FOR*);
                    WriteString (prefix);
                    WriteString ("&nbsp;&nbsp;&nbsp;");
                    WriteLn;
                    WritePersonLink2 (lang, DB, ID, TRUE, Info);
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
        j: CARDINAL;
        datum: DataString;
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
                DisplayPersonInfo (lang, ID, DB, T2, prefix);
                DiscardTree (T2);
                WriteLn;
            ELSE
                FOR j := 1 TO level DO
                    WriteString ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
                END (*FOR*);
                WriteString (prefix);
                WriteString ("&nbsp;&nbsp;&nbsp;");
                WriteLn;
                WritePersonLink2 (lang, DB, ID, TRUE, Info);
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

PROCEDURE DisplayEveryone (lang: LangHandle;  dummy: PersonData;
                                              ShowDetails: BOOLEAN);

    (* Displays the names of everyone in the database.  The person      *)
    (* mentioned in the argument is unimportant, but we need some       *)
    (* person record so that we can identify the database.              *)

    VAR NextLevel, count: CARDINAL;  Lookahead: Line;
        T: RecordTree;  DB: Database;  success: BOOLEAN;
        ID: IDString;  pos: FilePos;
        label: ARRAY [0..255] OF CHAR;

    BEGIN
        DB := dummy^.DB;
        IF NOT ShowDetails THEN
            StrToBuffer (lang, "Person.Complete", label);
            MakeButton (DB, dummy^.ID, 'E+', label);
        END (*IF*);
        WriteString ("<h2>");
        StrToBuffer (lang, "Person.AllPeople", label);
        WriteString (label);
        SendLine ("</h2>");
        StartReading (DB, NextLevel, Lookahead);
        count := 0;
        REPEAT
            success := LoadNextINDIRecord (DB, ID, T, NextLevel, Lookahead);
            IF success THEN
                INC (count);
                IF ShowDetails THEN
                    pos := SavePosition (DB);
                    DisplayPersonInfo (lang, ID, DB, T, '');
                    RestorePosition (DB, pos);
                    WriteString ("<br><hr>");
                    WriteLn;
                ELSE
                    WriteString ("<br>");
                    WritePersonLink2 (lang, DB, ID, TRUE, T);
                    WriteLn;
                END (*IF*);
                DiscardTree (T);
            END (*IF*);
        UNTIL NOT success;
        WriteString ("<p>");
        StrToBufferN (lang, "Person.Total", count, label);
        WriteString (label);
        WriteLn;
    END DisplayEveryone;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

END Person.

