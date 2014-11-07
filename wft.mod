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
        (*  Last edited:        1 February 2005                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT IOChan, TextIO, Strings, FileSys, OS2;

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  ADR, CAST;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent, NextArg;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId,
    (* proc *)  OpenOldFile, CloseFile, ReadLine;

FROM SysClock IMPORT
    (* type *)  DateTime,
    (* proc *)  GetClock;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  LanguageSupported, UseLanguage, DropLanguage,
                StrToBuffer, StrToBufferA;

FROM Person IMPORT
    (* type *)  PersonData,
    (* proc *)  GetPersonData, OurCharSet, DiscardPersonData, DisplayPerson,
                DisplayDescendants, DisplayAncestors, DisplayEveryone;

FROM OurTypes IMPORT
    (* type *)  IDString, DataString, FilenameString;

(********************************************************************************)

CONST
    version = "1.2";
    Debugging = FALSE;
    DefaultInput = "D=moylan;P=I055";
    Nul = CHR(0);
    CtrlZ = CHR(26);

TYPE ViewType = (normal, ancestors, descendants, everyone);

(********************************************************************************)

PROCEDURE GetArguments (VAR (*OUT*) DatabaseName: FilenameString;
                        VAR (*OUT*) PersonID: IDString;
                        VAR (*OUT*) View: ViewType;
                        VAR (*OUT*) ShowMore: BOOLEAN);

    (* Picks up the arguments to the program. *)

    CONST Nul = CHR(0);  ArgSeparator = ';';

    VAR arg: ARRAY [0..255] OF CHAR;
        j: CARDINAL;

    (****************************************************************************)

    PROCEDURE GetString (VAR (*OUT*) str: ARRAY OF CHAR);

        (* Loads a string from arg, skipping any initial '=', stopping at end   *)
        (* of string or when ArgSeparator found.                                *)

        VAR k: CARDINAL;

        BEGIN
            IF arg[j] = '=' THEN INC(j) END(*IF*);
            k := 0;
            WHILE (k <= HIGH(str)) AND (j <= 255) AND (arg[j] <> Nul)
                      AND (arg[j] <> ArgSeparator) DO
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

    TYPE CharSet = SET OF CHAR;

    CONST Digits = CharSet {'0'..'9'};

    VAR cid: IOChan.ChanId;  ch: CHAR;

    BEGIN
        IF Debugging THEN
            arg := DefaultInput;
        ELSE
            cid := ArgChan();
            IF IsArgPresent() THEN
                TextIO.ReadString (cid, arg);
            ELSE
                arg := "";
            END (*IF*);
        END (*IF*);

        DatabaseName := "";
        PersonID := "";
        View := normal;
        ShowMore := FALSE;

        j := 0;
        LOOP
            ch := arg[j];  INC(j);
            CASE CAP(ch) OF
                Nul:      EXIT (*LOOP*);
              | ArgSeparator:
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
                          END(*IF*);

                          IF arg[j] = '+' THEN
                              ShowMore := TRUE;  INC(j);
                          END(*IF*);
            ELSE
                (* Ignore unknown option. *)
                INC(j);
            END (*CASE*);
        END (*LOOP*);

    END GetArguments;

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
        cid := OpenOldFile(filename, FALSE);
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

PROCEDURE SetLanguage(): LangHandle;

    TYPE CharSet = SET OF CHAR;

    VAR j, k: CARDINAL;
        pEnv: OS2.PCSZ;
        found: BOOLEAN;
        code: ARRAY [0..31] OF CHAR;
        q: POINTER TO ARRAY [0..2047] OF CHAR;

    BEGIN
        found := OS2.DosScanEnv ("HTTP_ACCEPT_LANGUAGE", pEnv) = 0;
        IF found THEN
            q := CAST (ADDRESS, pEnv);

            (* q^ is a comma-separated string, where each item  *)
            (* is a language code possibly followed by a        *)
            (* ";q=number" modifier.                            *)

            k := 0;
            found := FALSE;
            WHILE (NOT found) AND (q^[k] <> Nul) DO

                j := 0;
                WHILE NOT (q^[k] IN CharSet{Nul, '-', ',', ';'}) DO
                    code[j] := q^[k];
                    INC (j);  INC(k);
                END (*WHILE*);
                code[j] := Nul;
                found := LanguageSupported ('WFT', code);
                IF NOT found THEN

                    (* Skip to next language, if any. *)

                    WHILE NOT (q^[k] IN CharSet{Nul, ','}) DO
                        INC(k);
                    END (*WHILE*);
                    IF q^[k] <> Nul THEN
                        INC(k);
                    END (*IF*);
                END (*IF*);

            END (*WHILE*);

        END (*IF*);

        IF NOT found THEN
            code := "en";
        END (*IF*);

        RETURN UseLanguage ('WFT', code);

    END SetLanguage;

(********************************************************************************)

PROCEDURE DisplayWebPage (found: BOOLEAN;  ID: IDString;  data: PersonData;
                                DatabaseName: FilenameString;  lang: LangHandle;
                                View: ViewType;  ShowDetails: BOOLEAN);

    (* Displays the information in the PersonData record. *)

    (****************************************************************************)

    PROCEDURE SendHeaderOrFooter (label: ARRAY OF CHAR);

        (* Sends the file DatabaseName.label if it exists; otherwise    *)
        (* sends the file label.                                        *)

        VAR name: FilenameString;

        BEGIN
            Strings.Assign ('data\', name);
            Strings.Append (DatabaseName, name);
            Strings.Append ('.', name);
            Strings.Append (label, name);
            IF FileSys.Exists(name) THEN
                IncludeFile (lang, name);
            ELSE
                IncludeFile (lang, label);
            END (*IF*);
        END SendHeaderOrFooter;

    (****************************************************************************)

    VAR CharSetName: DataString;
        message, message2: ARRAY [0..255] OF CHAR;

    BEGIN
        (* Work out what character encoding to use. *)

        OurCharSet (data, CharSetName);
        IF CharSetName[0] = Nul THEN
            Strings.Assign ("IBM850", CharSetName);
        END (*IF*);

        (* Send the HTTP response. *)

        WriteString ("Content-type: text/html; charset=");
        SendLine (CharSetName);
        SendLine ("Cache-Control: private");
        SendLine ("Pragma: no-cache");
        SendLine ("Expires: NOW");
        SendLine ("");

        (* Send the data as an HTML document. *)

        SendLine ("<html><head><title>");
        IF found THEN
            IF View = everyone THEN
                StrToBuffer (lang, "WFT.ListingAll", message);
            ELSE
                StrToBufferA (lang, "WFT.DataForPerson", ID, message);
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
                DisplayPerson (lang, data);
            ELSIF View = descendants THEN
                DisplayDescendants (lang, data, ShowDetails);
            ELSIF View = ancestors THEN
                DisplayAncestors (lang, data, ShowDetails);
            ELSIF View = everyone THEN
                DisplayEveryone (lang, data, ShowDetails);
            END (*IF*);
        ELSE
            WriteString ("");
            StrToBuffer (lang, "WFT.RecordNotFound", message);
            message2 := message;
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
            SendLine ("'");
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
        View: ViewType;
        lang: LangHandle;
        found, ShowDetails: BOOLEAN;

    BEGIN
        lang := SetLanguage();
        GetArguments (DatabaseName, PersonID, View, ShowDetails);
        found := GetPersonData (DatabaseName, PersonID, PersonInfo);
        DisplayWebPage (found, PersonID, PersonInfo, DatabaseName,
                                lang, View, ShowDetails);
        DiscardPersonData (PersonInfo);
        DropLanguage (lang);
    END DisplayInfo;

(********************************************************************************)
(*                                 MAIN PROGRAM                                 *)
(********************************************************************************)

BEGIN
    DisplayInfo;
END WFT.

