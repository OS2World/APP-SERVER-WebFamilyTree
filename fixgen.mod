MODULE FixGen;

        (********************************************************)
        (*                                                      *)
        (*          Utility to repair some faults that          *)
        (*           GenealogyJ puts into a GED file.           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            28 December 2004                *)
        (*  Last edited:        13 January 2005                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*    This program takes the name of one GEDCOM file    *)
        (*    as its only parameter.  It repairs three problems *)
        (*    that can be caused by editing your GEDCOM file    *)
        (*    with the GenealogyJ program:                      *)
        (*      1. GenJ deletes your CHAR specification and     *)
        (*         replaces it with CHAR IBMPC (which is of     *)
        (*         course ambiguous).  The original CHAR line   *)
        (*         is lost, so this program reads a file CHAR   *)
        (*         from the current directory to find the       *)
        (*         character set you really want.               *)
        (*      2. Surnames containing the '/' character have   *)
        (*         an extra space character inserted.  We       *)
        (*         remove the space.                            *)
        (*      3. External links, i.e. those containing a      *)
        (*         ':' character, have had the surrounding      *)
        (*         '@' delimiters deleted.  We reinsert them.   *)
        (*                                                      *)
        (********************************************************)


IMPORT IOChan, TextIO, FileSys, Strings;

FROM OurTypes IMPORT
    (* type *)  Line;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent, NextArg;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM TextBuffers IMPORT
    (* type *)  Buffer,
    (* proc *)  OpenForReading, TBFileOpened, CloseTB,
                TBReadLine, TBSetPosition, TBStartPosition;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId,
    (* proc *)  OpenNewFile1, OpenOldFile, CloseFile, DeleteFile, MoveFile,
                ReadLine, FWriteString, FWriteLn;

(********************************************************************************)

CONST
    Debugging = FALSE;
    DefaultInput = "test";
    Nul = CHR(0);  CtrlZ = CHR(26);

TYPE
    FilenameString = ARRAY [0..511] OF CHAR;

VAR
    (* Variable used in constructing a unique file name. *)

    NextName: ARRAY [0..7] OF CHAR;

(********************************************************************************)
(*                          GET THE DATABASE NAME                               *)
(********************************************************************************)

PROCEDURE GetArgument (VAR (*OUT*) DatabaseName: FilenameString);

    (* Picks up the argument - a single string - to the program. *)

    VAR cid: IOChan.ChanId;  j: CARDINAL;

    BEGIN
        IF Debugging THEN
            DatabaseName := DefaultInput;
        ELSE
            cid := ArgChan();
            IF IsArgPresent() THEN
                TextIO.ReadString (cid, DatabaseName);
                j := LENGTH (DatabaseName);
                REPEAT
                    DEC (j);
                UNTIL (j = 0) OR (DatabaseName[j] <> ' ');
                DatabaseName[j+1] := Nul;
            ELSE
                DatabaseName := "";
            END (*IF*);
        END (*IF*);

    END GetArgument;

(************************************************************************)
(*                    CREATING A UNIQUE FILENAME                        *)
(************************************************************************)

PROCEDURE MakeUniqueName (VAR (*OUT*) name: ARRAY OF CHAR);

    (* Generates a unique 8-character string.  The argument must of     *)
    (* course be big enough to take at least 8 characters.              *)

    (********************************************************************)

    PROCEDURE Increment (N: CARDINAL);

        (* Increments NextName[N], with carry as appropriate. *)

        BEGIN
            IF NextName[N] = '9' THEN
                NextName[N] := 'A';
            ELSIF NextName[N] = 'Z' THEN
                NextName[N] := '0';
                IF N > 0 THEN
                    Increment (N-1);
                END (*IF*);
            ELSE
                INC (NextName[N]);
            END (*IF*);
        END Increment;

    (********************************************************************)

    BEGIN
        Strings.Assign (NextName, name);
        Increment (7);
    END MakeUniqueName;

(************************************************************************)

PROCEDURE MakeNewFilename (VAR (*OUT*) NewName: ARRAY OF CHAR);

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist.     *)
    (* Note that BaseName and tail can include punctuation.             *)

    BEGIN
        REPEAT
            MakeUniqueName (NewName);
        UNTIL NOT FileSys.Exists(NewName);
    END MakeNewFilename;

(************************************************************************)

PROCEDURE OpenNewOutputFile (VAR (*OUT*) NewName: FilenameString): ChanId;

    (* Creates a file name that does not already exist, and *)
    (* opens that file.                                     *)

    VAR cid: ChanId;  duplication: BOOLEAN;

    BEGIN
        REPEAT
            MakeNewFilename (NewName);
            cid := OpenNewFile1 (NewName, duplication);
        UNTIL NOT duplication;
        RETURN cid;
    END OpenNewOutputFile;

(************************************************************************)
(*                       PROCESSING ONE RECORD                          *)
(************************************************************************)

PROCEDURE KwdMatch (L: Line;  keyword: ARRAY OF CHAR;
                              VAR (*INOUT*) pos: CARDINAL): BOOLEAN;

    (* Returns TRUE iff keyword occurs at L[pos].  If so, updates pos   *)
    (* to be just beyond the matched word.                              *)

    VAR match: BOOLEAN;  k, m, size: CARDINAL;

    BEGIN
        k := pos;  m := 0;  size := LENGTH(keyword);
        WHILE (m < size) AND (CAP(L[k]) = keyword[m]) DO
            INC (m);  INC (k);
        END (*WHILE*);
        match := m >= size;
        IF match THEN
            pos := k;
        END (*IF*);
        RETURN match;
    END KwdMatch;

(************************************************************************)

PROCEDURE ProcessLine (VAR (*INOUT*) L: Line;  k: CARDINAL);

    (* Do any necessary changes to line L.  On entry k is the position  *)
    (* following leading spaces and digits.                             *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        IF KwdMatch (L, 'NAME', k) THEN
            Strings.FindNext ('/ ', L, k, found, pos);
            IF found THEN
               Strings.Delete (L, pos+1, 1)
            END (*IF*);
        ELSIF KwdMatch (L, 'FAMC', k) OR KwdMatch (L, 'FAMS', k)
                   OR KwdMatch (L, 'HUSB', k) OR KwdMatch (L, 'WIFE', k)
                   OR KwdMatch (L, 'CHIL', k) THEN
            WHILE L[k] = ' ' DO
                INC(k);
            END (*WHILE*);
            IF L[k] <> '@' THEN
                Strings.Insert ('@', k, L);
                Strings.Append ('@', L);
            END (*IF*);
        END (*IF*);
    END ProcessLine;

(***********************************************************************)
(*                        THE MAIN PROCEDURE                           *)
(***********************************************************************)

PROCEDURE TidyDatabase (infile: FilenameString);

    (* Processes the GEDCOM file. *)

    TYPE CharSet = SET OF CHAR;

    VAR TB: Buffer;
        cid1: ChanId;
        tempfile: FilenameString;
        k: CARDINAL;
        ThisLine, OurCharset: Line;
        done: BOOLEAN;

    BEGIN
        (* Read the character set file if present. *)

        cid1 := OpenOldFile ('CHAR', FALSE);
        IF cid1 <> NoSuchChannel THEN
            ReadLine (cid1, OurCharset);
            CloseFile (cid1);
        END (*IF*);

        TB := OpenForReading (infile);
        IF NOT TBFileOpened(TB) THEN
            WriteString ("Sorry, file ");
            WriteString (infile);
            WriteString (" was not found.");
            WriteLn;
            RETURN;
        END (*IF*);

        cid1 := OpenNewOutputFile (tempfile);
        TBSetPosition (TB, TBStartPosition(TB));

        (* Process the character set specification. *)

        IF (OurCharset[0] <> Nul) AND (OurCharset[0] <> CtrlZ) THEN
            TBReadLine (TB, ThisLine);
            FWriteString (cid1, ThisLine);
            FWriteLn (cid1);
            k := 0;
            REPEAT
                TBReadLine (TB, ThisLine);
                done := (ThisLine[0] = Nul) OR (ThisLine[0] = CtrlZ);
                IF NOT done THEN
                    IF ThisLine[0] = '0' THEN
                        done := TRUE;
                    ELSIF KwdMatch (ThisLine, '1 CHAR IBMPC', k) THEN
                        ThisLine[7] := Nul;
                        Strings.Append (OurCharset, ThisLine);
                        k := 0;
                    END (*IF*);
                    FWriteString (cid1, ThisLine);
                    FWriteLn (cid1);
                END (*IF*);
            UNTIL done;
        END (*IF*);

        REPEAT
            TBReadLine (TB, ThisLine);
            k := 0;
            WHILE ThisLine[k] IN CharSet{' ', '0'..'9'} DO
                INC (k);
            END (*WHILE*);
            done := (ThisLine[k] = Nul) OR (ThisLine[k] = CtrlZ);
            IF NOT done THEN
                ProcessLine (ThisLine, k);
                FWriteString (cid1, ThisLine);
                FWriteLn (cid1);
            END (*IF*);
        UNTIL done;

        CloseTB (TB);
        CloseFile (cid1);

        (* The new version of the database is now in tempfil2.  Delete  *)
        (* the original, and rename tempfile to the original file name. *)

        DeleteFile (infile);
        IF NOT MoveFile (tempfile, infile) THEN
            WriteString ("ERROR: Could not rename ");  WriteString (tempfile);
            WriteString (" to ");  WriteString (infile);  WriteLn;
        END (*IF*);

    END TidyDatabase;

(************************************************************************)
(*                             MAIN PROGRAM                             *)
(************************************************************************)

PROCEDURE DoTheJob;

    (* Repairs some errors introduced by GenJ. *)

    VAR DatabaseName, MainFile: FilenameString;
        found: BOOLEAN;  pos: CARDINAL;

    BEGIN
        GetArgument (DatabaseName);
        IF DatabaseName[0] = Nul THEN
            WriteLn;
            WriteString ("Usage: FixGen gedfilename");
            WriteLn;
            WriteLn;
            WriteString ("Run this after using GenealogyJ.  It will repair some");
            WriteLn;
            WriteString ("errors that GenealogyJ has inserted into the GEDCOM file.");
            WriteLn;
            RETURN;
        END (*IF*);

        MainFile := DatabaseName;
        Strings.FindPrev ('.', MainFile, LENGTH(MainFile)-1, found, pos);
        IF NOT found THEN
            Strings.Append (".GED", MainFile);
        END (*IF*);

        TidyDatabase (MainFile);

    END DoTheJob;

(************************************************************************)

BEGIN
    NextName := "00000000";
    DoTheJob;
END FixGen.

