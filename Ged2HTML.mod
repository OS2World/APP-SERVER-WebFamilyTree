MODULE Ged2HTML;

        (********************************************************)
        (*                                                      *)
        (*                      GED to HTML                     *)
        (*                                                      *)
        (*     Converts from GEDCOM format to HTML format       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            29 August 2005                  *)
        (*  Last edited:        8 June 2015                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT IOChan, TextIO, Strings, FileSys, OS2;

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId,
    (* proc *)  OpenOldFile, OpenNewFile, CloseFile, ReadLine,
                Exists, DeleteFile, MoveFile,
                FWriteChar, FWriteString, FWriteLn;

FROM SysClock IMPORT
    (* type *)  DateTime,
    (* proc *)  GetClock;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  LanguageSupported, UseLanguage, DropLanguage,
                StrToBuffer;

FROM People IMPORT
    (* type *)  PersonData,
    (* proc *)  GetInitialData, OurCharSet, DiscardPersonData, DisplayEveryone;

FROM OurTypes IMPORT
    (* type *)  DataString, FilenameString;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

(************************************************************************)

CONST
    version = "1.5";
    Debugging = FALSE;
    DefaultInput = "data\VanDoo";
    Nul = CHR(0);
    CtrlZ = CHR(26);

(************************************************************************)

PROCEDURE GetArguments (VAR (*OUT*) DatabaseName: FilenameString);

    (* Picks up the arguments to the program. *)

    VAR cid: IOChan.ChanId;  k: CARDINAL;

    BEGIN
        IF Debugging THEN
            DatabaseName := DefaultInput;
        ELSE
            cid := ArgChan();
            IF IsArgPresent() THEN
                TextIO.ReadString (cid, DatabaseName);
                k := Strings.Length(DatabaseName);

                (* Delete trailing spaces. *)

                WHILE (k > 0) AND (DatabaseName[k-1] = ' ') DO
                    DEC (k);
                    DatabaseName[k] := Nul;
                END (*WHILE*);
            ELSE
                DatabaseName := "";
            END (*IF*);
        END (*IF*);
    END GetArguments;

(************************************************************************)

PROCEDURE WriteCard (cid: ChanId;  value: CARDINAL);

    (* Writes a left-justified cardinal to standard output. *)

    BEGIN
        IF value > 9 THEN
            WriteCard (cid, value DIV 10);  value := value MOD 10;
        END (*IF*);
        FWriteChar (cid, CHR(ORD("0")+value));
    END WriteCard;

(************************************************************************)

PROCEDURE WriteDate (cid: ChanId;  lang: LangHandle);

    (* Writes today's date to standard output. *)

    VAR now: DateTime;  k: CARDINAL;
        ch: ARRAY [0..0] OF CHAR;
        code, monthname: ARRAY [0..31] OF CHAR;

    BEGIN
        GetClock (now);
        WriteCard (cid, now.day);
        FWriteChar (cid, ' ');
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
        FWriteString (cid, monthname);
        FWriteChar (cid, ' ');
        WriteCard (cid, now.year);
    END WriteDate;

(************************************************************************)

PROCEDURE SendLine (cid: ChanId;  line: ARRAY OF CHAR);

    (* Sends a string, terminated with CRLF, to standard output. *)

    BEGIN
        FWriteString (cid, line);  FWriteLn (cid);
    END SendLine;

(************************************************************************)

PROCEDURE IncludeFile (outcid: ChanId;  lang: LangHandle;
                                       filename: ARRAY OF CHAR);  FORWARD;

(************************************************************************)

PROCEDURE SendString (cid: ChanId;  lang: LangHandle;  string: ARRAY OF CHAR);

    (* Sends string to the output file, expanding any macros found. *)

    VAR j: CARDINAL;

    (********************************************************************)

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

    (********************************************************************)

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
                    FWriteChar (cid, '%');
                ELSIF ch = 'd' THEN
                    WriteDate (cid, lang);
                ELSIF ch = 'i' THEN
                    ReadFilename (name);
                    IncludeFile (cid, lang, name);
                ELSIF ch = 'v' THEN
                    FWriteString (cid, version);
                ELSE
                    FWriteChar (cid, '%');
                    FWriteChar (cid, ch);
                END (*IF*);
                HaveMacro := FALSE;
            ELSIF ch = '%' THEN
                HaveMacro := TRUE;
            ELSE
                FWriteChar (cid, ch);
            END (*IF*);
        END (*LOOP*);
    END SendString;

(************************************************************************)

PROCEDURE IncludeFile (outcid: ChanId;  lang: LangHandle;
                                        filename: ARRAY OF CHAR);

    (* Sends the contents of a text file, if it exists, to outcid. *)

    VAR cid: ChanId;
        InLine: ARRAY [0..1023] OF CHAR;

    BEGIN
        cid := OpenOldFile(filename, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            LOOP
                ReadLine (cid, InLine);
                IF InLine[0] = CtrlZ THEN EXIT(*LOOP*) END(*IF*);
                SendString (outcid, lang, InLine);
                FWriteLn (outcid);
            END (*LOOP*);
            CloseFile (cid);
        END (*IF*);
    END IncludeFile;

(************************************************************************)

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

(************************************************************************)

PROCEDURE CreateWebPage (outcid: ChanId;  data: PersonData;
                         DatabaseName: FilenameString;  lang: LangHandle);

    (* Displays the information in the PersonData record. *)

    (********************************************************************)

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
                IncludeFile (outcid, lang, name);
            ELSE
                IncludeFile (outcid, lang, label);
            END (*IF*);
        END SendHeaderOrFooter;

    (********************************************************************)

    VAR CharSetName: DataString;
        message: ARRAY [0..255] OF CHAR;

    BEGIN

        (* Work out what character encoding to use.  *)

        OurCharSet (data, CharSetName);
        IF CharSetName[0] = Nul THEN
            Strings.Assign ("IBM850", CharSetName);
        END (*IF*);

        (* Send the data as an HTML document. *)

        SendLine (outcid, "<html><head><title>");
        StrToBuffer (lang, "WFT.ListingAll", message);
        FWriteString (outcid, message);
        SendLine (outcid, "</title>");
        FWriteString (outcid, '<meta http-equiv="Content-Type" content="text/html;charset=');
        FWriteString (outcid, CharSetName);
        SendLine (outcid, '" >');
        SendLine (outcid, "</head>");

        FWriteString (outcid, '<body');
        SendLine (outcid, ' text="#000000" bgcolor="#FFFFCC" link="#0000EE" vlink="#551A8B" alink="#FF0000">');
        SendHeaderOrFooter ("header");
        DisplayEveryone (outcid, lang, data);

        (* Finish off the HTML code. *)

        SendHeaderOrFooter ("footer");
        SendLine (outcid, "</body></html>");

    END CreateWebPage;

(************************************************************************)

PROCEDURE ConvertTheFile;

    (* Displays the information for one person. *)

    VAR PersonInfo: PersonData;
        DatabaseName, OutfileName, BAKname: FilenameString;
        lang: LangHandle;
        cid: ChanId;

    BEGIN
        lang := SetLanguage();
        GetArguments (DatabaseName);
        IF DatabaseName[0] = Nul THEN
            WriteString ("Usage: ged2html databasename");
            WriteLn;
        ELSE
            GetInitialData (DatabaseName, PersonInfo);
            OutfileName := DatabaseName;
            Strings.Append (".html", OutfileName);
            IF Exists (OutfileName) THEN
                BAKname := OutfileName;
                Strings.Append (".BAK", BAKname);
                DeleteFile (BAKname);
                EVAL(MoveFile (OutfileName, BAKname));
            END (*IF*);
            cid := OpenNewFile (OutfileName, FALSE);
            CreateWebPage (cid, PersonInfo, DatabaseName, lang);
            CloseFile (cid);
            DiscardPersonData (PersonInfo);
        END (*IF*);
        DropLanguage (lang);
    END ConvertTheFile;

(************************************************************************)
(*                             MAIN PROGRAM                             *)
(************************************************************************)

BEGIN
    ConvertTheFile;
END Ged2HTML.

