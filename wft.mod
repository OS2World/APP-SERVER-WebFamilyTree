MODULE WFT;

        (********************************************************)
        (*                                                      *)
        (*                    Web Family Tree                   *)
        (*                                                      *)
        (*     This is a CGI program, intended to be invoked    *)
        (*               from a web browser                     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            6 July 2001                     *)
        (*  Last edited:        10 August 2016                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


<* IF UseWindows THEN *>
    IMPORT Windows;
<* ELSE *>
    IMPORT OS2;
<* END *>

IMPORT Strings, FileSys, WFTV;

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, DirectoryEntry,
    (* proc *)  OpenOldFile, CloseFile, ReadLine,
                FirstDirEntry, DirSearchDone;

FROM SysClock IMPORT
    (* type *)  DateTime,
    (* proc *)  GetClock;

FROM WftClock IMPORT
    (* proc *)  ConvertDateTime;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  LanguageSupported, UseLanguage, DropLanguage,
                StrToBuffer, StrToBufferA;

FROM Person IMPORT
    (* type *)  PersonData,
    (* proc *)  GetPersonData, OurCharSet, DiscardPersonData, DisplayPerson,
                PersonHeading, TreeForPerson,
                DisplayDescendants, DisplayAncestors, DisplayEveryone,
                DisplayEveryEveryone;

FROM GELookup IMPORT
    (* type *)  RecordTree,
    (* proc *)  CopyName;

FROM OurTypes IMPORT
    (* type *)  IDString, DataString, FilenameString;

(********************************************************************************)

CONST
    (*version = "1.9";*)
    version = WFTV.version;
    Debugging = FALSE;
    DefaultInput = "D=moylan&P=I089";
    DebugLanguage = "en";
    Nul = CHR(0);
    CtrlZ = CHR(26);

TYPE ViewType = (normal, ancestors, descendants, everyone, absolutelyeveryone);

VAR (* while testing *)  QueryString: ARRAY [0..255] OF CHAR;

(********************************************************************************)

PROCEDURE GetEnvironmentVariable (varname: ARRAY OF CHAR;
                                  VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* Gets the value of environment variable 'varname'.  The result is         *)
    (* meaningless if the function result is FALSE.                             *)

    TYPE ArgPtr = POINTER TO ARRAY [0..255] OF CHAR;

    VAR found: BOOLEAN;
        <* IF NOT UseWindows THEN *>
        penvstr: OS2.PCSZ;
        parg: ArgPtr;
        <* END *>

    BEGIN
        <* IF UseWindows THEN *>
            Windows.GetEnvironmentVariable (varname, result, 255);
            found := result[0] <> Nul;
        <* ELSE *>
            found := OS2.DosScanEnv (varname, penvstr) = 0;
            IF found THEN
                parg := CAST (ArgPtr, penvstr);
                IF parg = NIL THEN
                    result[0] := Nul;
                ELSE
                    Strings.Assign (parg^, result);
                END (*IF*);
            END (*IF*);
        <* END *>
        RETURN found;
    END GetEnvironmentVariable;

(********************************************************************************)

PROCEDURE GetParameters (VAR (*OUT*) DatabaseName: FilenameString;
                        VAR (*OUT*) PersonID: IDString;
                        VAR (*OUT*) View: ViewType;
                        VAR (*OUT*) ShowMore: BOOLEAN);

    (* Picks up the CGI arguments from the environment variable QUERY_STRING. *)

    TYPE ArgPtr = POINTER TO ARRAY [0..255] OF CHAR;

    VAR arg: ARRAY [0..255] OF CHAR;
        j: CARDINAL;

    (****************************************************************************)

    PROCEDURE GetString (VAR (*OUT*) str: ARRAY OF CHAR);

        (* Loads a string from arg, skipping any initial '=', stopping at end   *)
        (* of string or when '&' or ';' found.                                  *)

        VAR k: CARDINAL;

        BEGIN
            IF arg[j] = '=' THEN INC(j) END(*IF*);
            k := 0;
            WHILE (k <= HIGH(str)) AND (j <= 255) AND (arg[j] <> Nul)
                      AND (arg[j] <> '&') AND (arg[j] <> ';') DO
                str[k] := arg[j];
                INC (j);
                INC (k);
            END (*WHILE*);

            (* Strip trailing spaces. *)

            WHILE (k > 0) AND (str[k-1] = ' ') DO
                DEC (k);
            END (*WHILE*);

            IF k <= HIGH(str) THEN
                str[k] := Nul;
            END (*IF*);

        END GetString;

    (****************************************************************************)

    PROCEDURE Hex2 (k: CARDINAL): CARDINAL;

        (* Returns the value of the two-digit hex number at arg[k]. *)

        (************************************************************************)

        PROCEDURE Hex1 (ch: CHAR): CARDINAL;

            (* Returns the value of a one-digit hex number. *)
            (* No error checking!                           *)

            TYPE CharSet = SET OF CHAR;

            CONST Digits = CharSet {'0'..'9'};

            BEGIN
                IF ch IN Digits THEN
                    RETURN ORD(ch) - ORD('0');
                ELSE
                    RETURN ORD(CAP(ch)) - ORD('A') + 10;
                END (*IF*);

            END Hex1;

        (************************************************************************)

        VAR result: CARDINAL;

        BEGIN                 (* body of Hex2 *)
            result := 16*Hex1(arg[k]) + Hex1(arg[k+1]);
            IF result > ORD(MAX(CHAR)) THEN
                result := ORD(' ');
            END (*IF*);
            RETURN result;
        END Hex2;

    (****************************************************************************)

    VAR ch: CHAR;  pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        IF Debugging THEN
            arg := DefaultInput;
        ELSIF NOT GetEnvironmentVariable ("QUERY_STRING", arg) THEN
            arg := "";
        END (*IF*);

        (* Save the query string for debugging information. *)

        Strings.Assign (arg, QueryString);

        (* Convert any %nn codes we find. *)

        pos := 0;
        REPEAT
            Strings.FindNext ('%', arg, pos, found, pos);
            IF found THEN
                INC (pos);
                arg[pos-1] := CHR(Hex2(pos));
                Strings.Delete (arg, pos, 2);
            END (*IF*);
        UNTIL NOT found;

        DatabaseName := "";
        PersonID := "";
        View := normal;
        ShowMore := FALSE;

        j := 0;
        LOOP
            ch := arg[j];  INC(j);
            CASE CAP(ch) OF
                Nul:      EXIT (*LOOP*);
              | '&', ';':
                          (* do nothing *)
              | 'D':      GetString (DatabaseName);
              | 'P':      GetString (PersonID);
              | 'V':      IF arg[j] = '=' THEN INC(j) END(*IF*);
                          IF arg[j] = 'D' THEN
                              View := descendants;  INC(j);
                          ELSIF arg[j] = 'A' THEN
                              View := ancestors;  INC(j);
                          ELSIF arg[j] = 'E' THEN
                              View := everyone;  INC(j);
                          ELSIF arg[j] = 'G' THEN
                              View := absolutelyeveryone;  INC(j);
                          END(*IF*);

                          IF arg[j] = '+' THEN
                              ShowMore := TRUE;  INC(j);
                          END(*IF*);
            ELSE
                (* Ignore unknown option. *)
                INC(j);
            END (*CASE*);
        END (*LOOP*);

    END GetParameters;

(********************************************************************************)

PROCEDURE WriteCard (value: CARDINAL);

    (* Writes a left-justified cardinal to standard output. *)

    BEGIN
        IF value > 9 THEN
            WriteCard (value DIV 10);  value := value MOD 10;
        END (*IF*);
        WriteChar (CHR(ORD("0")+value));
    END WriteCard;

(********************************************************************************)

PROCEDURE WriteDate (lang: LangHandle);

    (* Writes today's date to standard output. *)

    VAR now: DateTime;  k: CARDINAL;
        ch: ARRAY [0..0] OF CHAR;
        code, monthname: ARRAY [0..31] OF CHAR;

    BEGIN
        GetClock (now);
        WriteCard (now.day);
        WriteChar (' ');
        k := now.month;
        IF k > 12 THEN k := 0 END (*IF*);
        code := "month.";
        IF k > 9 THEN
            Strings.Append ("1", code);
            DEC (k, 10);
        END (*IF*);
        ch[0] := CHR(ORD('0')+k);
        Strings.Append (ch, code);
        StrToBuffer (lang, code, monthname);
        WriteString (monthname);
        WriteChar (' ');
        WriteCard (now.year);
    END WriteDate;

(********************************************************************************)

PROCEDURE SendLine (line: ARRAY OF CHAR);

    (* Sends a string, terminated with CRLF, to standard output. *)

    BEGIN
        WriteString (line);  WriteLn;
    END SendLine;

(********************************************************************************)

PROCEDURE IncludeFile (lang: LangHandle;  filename: ARRAY OF CHAR);     FORWARD;

(********************************************************************************)

PROCEDURE SendString (lang: LangHandle;  string: ARRAY OF CHAR);

    (* Sends string to standard output, expanding any macros found. *)

    VAR j: CARDINAL;

    (****************************************************************************)

    PROCEDURE ReadFilename (VAR (*OUT*) name: FilenameString);

        (* Reads a name, starting with string[j]. *)

        VAR pos: CARDINAL;  delim: ARRAY [0..0] OF CHAR;
            found: BOOLEAN;

        BEGIN
            Strings.Assign (string, name);
            IF j > 0 THEN
                Strings.Delete (name, 0, j);
            END (*IF*);
            delim[0] := name[0];
            IF (delim[0] = "'") OR (delim[0] = '"') THEN
                Strings.Delete (name, 0, 1);
                INC (j);
            ELSE
                delim[0] := ' ';
            END (*IF*);
            Strings.FindNext (delim, name, 0, found, pos);
            IF found THEN
                name[pos] := Nul;
                IF delim[0] <> ' ' THEN
                   INC (j);
                END (*IF*);
            END (*IF*);
            INC (j, Strings.Length (name));
        END ReadFilename;

    (****************************************************************************)

    VAR ch: CHAR;  HaveMacro: BOOLEAN;
        name: FilenameString;

    BEGIN
        j := 0;  HaveMacro := FALSE;
        LOOP
            ch := string[j];  INC(j);
            IF ch = Nul THEN
                EXIT (*LOOP*);
            ELSIF HaveMacro THEN
                IF ch = '%' THEN
                    WriteChar ('%');
                ELSIF ch = 'd' THEN
                    WriteDate (lang);
                ELSIF ch = 'i' THEN
                    ReadFilename (name);
                    IncludeFile (lang, name);
                ELSIF ch = 'v' THEN
                    WriteString (version);
                ELSE
                    WriteChar ('%');
                    WriteChar (ch);
                END (*IF*);
                HaveMacro := FALSE;
            ELSIF ch = '%' THEN
                HaveMacro := TRUE;
            ELSE
                WriteChar (ch);
            END (*IF*);
        END (*LOOP*);
    END SendString;

(********************************************************************************)

PROCEDURE IncludeFile (lang: LangHandle;  filename: ARRAY OF CHAR);

    (* Sends the contents of a text file, if it exists, to standard output. *)

    VAR cid: ChanId;
        InLine: ARRAY [0..1023] OF CHAR;

    BEGIN
        cid := OpenOldFile(filename, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            LOOP
                ReadLine (cid, InLine);
                IF InLine[0] = CtrlZ THEN EXIT(*LOOP*) END(*IF*);
                SendString (lang, InLine);
                WriteLn;
            END (*LOOP*);
            CloseFile (cid);
        END (*IF*);
    END IncludeFile;

(********************************************************************************)

PROCEDURE SetLanguage (VAR (*OUT*) code: ARRAY OF CHAR): LangHandle;

    (* Returns both a language handle and a textual language code. *)

    TYPE CharSet = SET OF CHAR;

    VAR j, k: CARDINAL;
        found: BOOLEAN;
        langstr: ARRAY [0..2047] OF CHAR;

    BEGIN
        IF Debugging THEN
            found := TRUE;
            Strings.Assign (DebugLanguage, code);
        ELSE
            found := GetEnvironmentVariable ("HTTP_ACCEPT_LANGUAGE", langstr);
            IF found THEN

                (* langstr is a comma-separated string, where each  *)
                (* item is a language code possibly followed by a   *)
                (* ";q=number" modifier.                            *)

                k := 0;
                found := FALSE;
                WHILE (NOT found) AND (langstr[k] <> Nul) DO

                    j := 0;
                    WHILE NOT (langstr[k] IN CharSet{Nul, '-', ',', ';'}) DO
                        code[j] := langstr[k];
                        INC (j);  INC(k);
                    END (*WHILE*);
                    code[j] := Nul;
                    found := LanguageSupported ('WFT', code);
                    IF NOT found THEN

                        (* Skip to next language, if any. *)

                        WHILE NOT (langstr[k] IN CharSet{Nul, ','}) DO
                            INC(k);
                        END (*WHILE*);
                        IF langstr[k] <> Nul THEN
                            INC(k);
                        END (*IF*);
                    END (*IF*);

                END (*WHILE*);

            END (*IF*);
        END (*IF*);

        IF NOT found THEN
            Strings.Assign ("en", code);
        END (*IF*);

        RETURN UseLanguage ('WFT', code);

    END SetLanguage;

(********************************************************************************)

PROCEDURE DisplayWebPage (found: BOOLEAN;  ID: IDString;  data: PersonData;
                                DatabaseName: FilenameString;  lang: LangHandle;
                                VAR (*IN*) langcode: ARRAY OF CHAR;
                                View: ViewType;  ShowDetails: BOOLEAN);

    (* Displays the information in the PersonData record. *)

    (****************************************************************************)

    PROCEDURE SendHeaderOrFooter (label: ARRAY OF CHAR);

        (* Sends the first of the following files that is found to exist: *)
        (*      DatabaseName.label.langcode                               *)
        (*      DatabaseName.label                                        *)
        (*      label.langcode                                            *)
        (*      label                                                     *)

        VAR name1, name2: FilenameString;

        BEGIN
            Strings.Assign ('data\', name1);
            Strings.Append (DatabaseName, name1);
            Strings.Append ('.', name1);
            Strings.Append (label, name1);

            Strings.Assign (name1, name2);
            Strings.Append ('.', name2);
            Strings.Append (langcode, name2);

            IF FileSys.Exists(name2) THEN
                IncludeFile (lang, name2);
            ELSIF FileSys.Exists(name1) THEN
                IncludeFile (lang, name1);
            ELSE
                Strings.Assign (label, name1);
                Strings.Append ('.', name1);
                Strings.Append (langcode, name1);
                IF FileSys.Exists(name1) THEN
                    IncludeFile (lang, name1);
                ELSE
                    IncludeFile (lang, label);
                END (*IF*);
            END (*IF*);

        END SendHeaderOrFooter;

    (****************************************************************************)

    PROCEDURE GetFileDate (VAR (*OUT*) DateTimeString: ARRAY OF CHAR);

        (* Gets the "last modified" date and time of the current file,  *)
        (* and returns it in the form required by HTTP 1.1.             *)

        VAR name: FilenameString;
            D: DirectoryEntry;

        BEGIN
            Strings.Assign ('data\', name);
            Strings.Append (DatabaseName, name);
            Strings.Append ('.GED', name);
            IF NOT FirstDirEntry (name, FALSE, FALSE, D) THEN
                D.datePkd := 33;
                D.timePkd := 0;
            END (*IF*);
            DirSearchDone (D);
            ConvertDateTime (D.datePkd, D.timePkd, DateTimeString);
            (* Format: "Mon, 29 Jun 1998 02:28:12 GMT", with all        *)
            (* fields fixed length.                                     *)
        END GetFileDate;

    (****************************************************************************)


    VAR CharSetName: DataString;
        result: PersonData;
        T: RecordTree;
        message: DataString;
        message2: ARRAY [0..255] OF CHAR;

    BEGIN
        (* Work out what character encoding to use. *)

        OurCharSet (data, CharSetName);
        IF CharSetName[0] = Nul THEN
            Strings.Assign ("IBM850", CharSetName);
        END (*IF*);

        (* Send the HTTP header lines. *)

        WriteString ("Last-Modified: ");
        GetFileDate (message);
        SendLine (message);
        SendLine ("Cache-Control: max-age=3600, must-revalidate");
        WriteString ("Content-type: text/html; charset=");
        SendLine (CharSetName);
        SendLine ("");

        (* Send the data as an HTML document. *)

        SendLine ("<html><head><title>");
        IF found THEN
            IF View = absolutelyeveryone THEN
                StrToBuffer (lang, "WFT.ListingAllAll", message);
            ELSIF View = everyone THEN
                StrToBuffer (lang, "WFT.ListingAll", message);
            ELSE
                IF GetPersonData (DatabaseName, ID, result) THEN
                    T := TreeForPerson(result);
                    PersonHeading (lang, T, message);
                    IF message[0] = Nul THEN
                        StrToBufferA (lang, "WFT.NoName", ID, message);
                    END (*IF*);
                ELSE
                    StrToBufferA (lang, "WFT.NoName", ID, message);
                END (*IF*);
            END (*IF*);
            WriteString (message);
            SendLine ("</title></head>");
            WriteString ('<body');
            IF ShowDetails OR (View = normal) THEN
                SendLine (' text="#000000" bgcolor="#FFFFCC" link="#0000EE" vlink="#551A8B" alink="#FF0000">');
            ELSE
                SendLine ('>');
            END (*IF*);
            SendHeaderOrFooter ("header");

            IF View = normal THEN
                DisplayPerson (lang, data, message);
            ELSIF View = descendants THEN
                DisplayDescendants (lang, data, ShowDetails);
            ELSIF View = ancestors THEN
                DisplayAncestors (lang, data, ShowDetails);
            ELSIF View = everyone THEN
                DisplayEveryone (lang, data, ShowDetails);
            ELSIF View = absolutelyeveryone THEN
                DisplayEveryEveryone (lang, data, ShowDetails);
            END (*IF*);
        ELSE
            WriteString ("");
            StrToBuffer (lang, "WFT.RecordNotFound", message);
            Strings.Assign (message, message2);
            Strings.Append ("</title></head>", message2);
            SendLine (message2);
            SendLine ('<body text="#000000" bgcolor="#FFFFCC" link="#0000EE" vlink="#551A8B" alink="#FF0000">');
            message2 := "<h2>";
            Strings.Append (message, message2);
            Strings.Append ("</h2>", message2);
            SendLine (message2);
            StrToBuffer (lang, "WFT.Nonexistent", message);
            SendLine (message);
            StrToBuffer (lang, "WFT.DatabaseName", message);
            message2 := "<br>";
            Strings.Append (message, message2);
            Strings.Append (" = '", message2);
            WriteString (message2);
            WriteString (DatabaseName);
            SendLine ("', ");
            WriteString ("ID = '");
            SendLine (ID);
            WriteString ("', Query string = ");
            SendLine (QueryString);
        END (*IF*);

        (* Finish off the HTML code. *)

        SendHeaderOrFooter ("footer");
        SendLine ("</body></html>");

    END DisplayWebPage;

(********************************************************************************)

PROCEDURE DisplayInfo;

    (* Displays the information for one person. *)

    VAR PersonInfo: PersonData;
        DatabaseName: FilenameString;
        PersonID: IDString;
        langcode: ARRAY [0..31] OF CHAR;
        View: ViewType;
        lang: LangHandle;
        found, ShowDetails: BOOLEAN;

    BEGIN
        lang := SetLanguage(langcode);
        GetParameters (DatabaseName, PersonID, View, ShowDetails);
        found := GetPersonData (DatabaseName, PersonID, PersonInfo);
        DisplayWebPage (found, PersonID, PersonInfo, DatabaseName,
                                lang, langcode, View, ShowDetails);
        DiscardPersonData (PersonInfo);
        DropLanguage (lang);
    END DisplayInfo;

(********************************************************************************)
(*                                 MAIN PROGRAM                                 *)
(********************************************************************************)

BEGIN
    DisplayInfo;
END WFT.

