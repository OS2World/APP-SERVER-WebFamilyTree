MODULE Tidy;

        (********************************************************)
        (*                                                      *)
        (*           Utility to process a GEDCOM file           *)
        (*           and remove some empty subrecords           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            24 December 2004                *)
        (*  Last edited:        17 February 2009                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT IOChan, TextIO, FileSys, Strings;

FROM OurTypes IMPORT
    (* type *)  Line;

FROM Records IMPORT
    (* type *)  RecordTree,
    (* proc *)  StartReading, LoadNextRecord, KwdMatch, WriteRecord,
                DiscardTree, PruneINDIRecord;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent, NextArg;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM TextBuffers IMPORT
    (* type *)  Buffer,
    (* proc *)  OpenForReading, TBFileOpened, CloseTB;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenNewFile1, CloseFile, DeleteFile, MoveFile;

(********************************************************************************)

CONST
    Nul = CHR(0);
    Debugging = FALSE;
    DefaultInput = "test";

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

PROCEDURE ProcessRecord (R: RecordTree);

    BEGIN
        IF KwdMatch (R, 'INDI') THEN
            PruneINDIRecord (R);
        END (*IF*);
    END ProcessRecord;

(***********************************************************************)
(*                        THE MAIN PROCEDURE                           *)
(***********************************************************************)

PROCEDURE TidyDatabase (infile: FilenameString);

    (* Processes the GEDCOM file. *)

    VAR TB: Buffer;
        cid1: ChanId;
        tempfile: FilenameString;
        NextLevel: CARDINAL;
        NextLine: Line;
        RecTree: RecordTree;
        done: BOOLEAN;

    BEGIN
        TB := OpenForReading (infile, TRUE);
        IF NOT TBFileOpened(TB) THEN
            WriteString ("Sorry, file ");
            WriteString (infile);
            WriteString (" was not found.");
            WriteLn;
            RETURN;
        END (*IF*);

        cid1 := OpenNewOutputFile (tempfile);

        StartReading (TB, NextLevel, NextLine);
        done := FALSE;

        REPEAT
            LoadNextRecord (TB, RecTree, NextLevel, NextLine, done);
            IF NOT done THEN
                ProcessRecord (RecTree);
                WriteRecord (cid1, RecTree);
                DiscardTree (RecTree);
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

    (* Removes redundant subrecords from a GEDCOM file. *)

    VAR DatabaseName, MainFile: FilenameString;
        found: BOOLEAN;  pos: CARDINAL;

    BEGIN
        GetArgument (DatabaseName);
        IF DatabaseName[0] = Nul THEN
            WriteLn;
            WriteString ("Usage: Tidy gedfilename");
            WriteLn;
            WriteLn;
            WriteString ("This will reduce the size of a GEDCOM file by");
            WriteLn;
            WriteString ("removing some empty records.");
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
END Tidy.

