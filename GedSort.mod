MODULE GedSort;

        (********************************************************)
        (*                                                      *)
        (*           Utility to sort a GEDCOM file              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            13 July 2001                    *)
        (*  Last edited:        02 December 2001                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT IOChan, TextIO, FileSys, Strings;

FROM OurTypes IMPORT
    (* type *)  Line;

FROM Records IMPORT
    (* type *)  RecordTree,
    (* proc *)  StartReading, LoadNextRecord, KwdMatch, WriteRecord,
                DiscardTree;

FROM Sorter IMPORT
    (* proc *)  Compare, SortArray;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent, NextArg;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM TextBuffers IMPORT
    (* type *)  Buffer,
    (* proc *)  OpenForReading, TBFileOpened, CloseTB;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenNewFile1, OpenAtEnd, CloseFile,
                AppendFile, DeleteFile, MoveFile;

(********************************************************************************)

CONST
    Nul = CHR(0);
    Tracing = TRUE;
    Debugging = FALSE;
    DefaultInput = "test";
    SortBufferSize = 512;

TYPE
    FilenameString = ARRAY [0..511] OF CHAR;

VAR
    (* Variable used in constructing a unique file name. *)

    NextName: ARRAY [0..7] OF CHAR;

    (* Buffer for doing an in-memory sort. *)

    SortBuffer: ARRAY [0..SortBufferSize-1] OF RecordTree;

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

(********************************************************************************)
(*                               MERGE SORT                                     *)
(********************************************************************************)

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

PROCEDURE InMemorySort (VAR (*INOUT*) file: FilenameString;  N: CARDINAL);

    (* Sorts a file of N records, for the case where N is small enough to do    *)
    (* the sorting in the array SortBuffer.  The file name is an INOUT          *)
    (* parameter because a side-effect of this operation is that the name is    *)
    (* changed.                                                                 *)

    VAR TB: Buffer;  cid: ChanId;  count, NextLevel: CARDINAL;
        NextLine: Line;  RecTree: RecordTree;  done: BOOLEAN;

    BEGIN
        IF Tracing THEN
            WriteString (file);  WriteString (" (");  WriteCard (N);
            WriteString (")  -->  ");
        END (*IF*);

        IF N > 0 THEN
            TB := OpenForReading (file);
            StartReading (TB, NextLevel, NextLine);
            count := 0;

            REPEAT
                LoadNextRecord (TB, RecTree, NextLevel, NextLine, done);
                IF NOT done THEN
                    SortBuffer[count] := RecTree;
                    INC (count);
                END (*IF*);
            UNTIL done;
            CloseTB (TB);
            DeleteFile (file);

            SortArray (SortBuffer, N);

            cid := OpenNewOutputFile (file);
            FOR count := 0 TO N-1 DO
                WriteRecord (cid, SortBuffer[count]);
                DiscardTree (SortBuffer[count]);
            END (*FOR*);
            CloseFile (cid);
        END (*IF*);

        IF Tracing THEN
            WriteString (file);  WriteString (" (");  WriteCard (N);
            WriteString (")");  WriteLn;
        END (*IF*);

    END InMemorySort;

(********************************************************************************)

PROCEDURE FileMerge (file1, file2: FilenameString;
                            VAR (*OUT*) result: FilenameString);

    (* Creates a new file 'result' containing the merged data from file1 and    *)
    (* file2.  Also deletes file1 and file2.                                    *)

    VAR cid0:      ChanId;
        TB:        ARRAY [1..2] OF Buffer;
        file:      ARRAY [0..2] OF FilenameString;
        NextLevel: ARRAY [1..2] OF CARDINAL;
        NextLine:  ARRAY [1..2] OF Line;
        T:         ARRAY [1..2] OF RecordTree;
        j, N1, N2: CARDINAL;
        done:      ARRAY [1..2] OF BOOLEAN;

    BEGIN
        cid0 := OpenNewOutputFile(file[0]);
        file[1] := file1;  file[2] := file2;
        N1 := 0;  N2 := 0;

        FOR j := 1 TO 2 DO
            TB[j] := OpenForReading (file[j]);
            StartReading (TB[j], NextLevel[j], NextLine[j]);
            LoadNextRecord (TB[j], T[j], NextLevel[j], NextLine[j], done[j]);
        END (*FOR*);

        WHILE NOT (done[1] AND done[2]) DO
            WHILE (NOT done[1]) AND (Compare (T[1], T[2]) <= 0) DO
                INC (N1);
                WriteRecord (cid0, T[1]);
                DiscardTree (T[1]);
                LoadNextRecord (TB[1], T[1], NextLevel[1], NextLine[1], done[1]);
            END (*WHILE*);
            WHILE Compare (T[2], T[1]) < 0 DO
                INC (N2);
                WriteRecord (cid0, T[2]);
                DiscardTree (T[2]);
                LoadNextRecord (TB[2], T[2], NextLevel[2], NextLine[2], done[2]);
            END (*WHILE*);
        END (*WHILE*);

        CloseFile (cid0);
        FOR j := 1 TO 2 DO
            CloseTB (TB[j]);
            DeleteFile (file[j]);
        END (*FOR*);
        result := file[0];

        IF Tracing THEN
            WriteString (file[1]);  WriteString (" (");  WriteCard (N1);
            WriteString (") + ");
            WriteString (file[2]);  WriteString (" (");  WriteCard (N2);
            WriteString (")  -->  ");
            WriteString (file[0]);  WriteString (" (");  WriteCard (N1 + N2);
            WriteString (")");
            WriteLn;
        END (*IF*);

    END FileMerge;

(********************************************************************************)

PROCEDURE SortFile (VAR (*INOUT*) file: FilenameString;  N: CARDINAL);

    (* Sorts a file of N records.  The file name is an INOUT parameter because  *)
    (* a side-effect of this operation is that the name is changed.             *)

    VAR TB: Buffer;
        cid1: ChanId;  NextLevel, j: CARDINAL;
        NextLine: Line;  RecTree: RecordTree;
        file1, file2: FilenameString;
        checkcount: CARDINAL;
        eof: BOOLEAN;

    BEGIN
        IF N <= SortBufferSize THEN

            InMemorySort (file, N);

        ELSE

            (* Read half the file into file1, the other half into file2. *)

            TB := OpenForReading (file);
            StartReading (TB, NextLevel, NextLine);

            cid1 := OpenNewOutputFile (file1);
            FOR j := 0 TO N DIV 2 - 1 DO
                LoadNextRecord (TB, RecTree, NextLevel, NextLine, eof);
                WriteRecord (cid1, RecTree);
                DiscardTree (RecTree);
            END (*FOR*);
            CloseFile (cid1);

            checkcount := 0;
            cid1 := OpenNewOutputFile (file2);
            REPEAT
                LoadNextRecord (TB, RecTree, NextLevel, NextLine, eof);
                IF NOT eof THEN
                    INC (checkcount);
                    WriteRecord (cid1, RecTree);
                    DiscardTree (RecTree);
                END (*IF*);
            UNTIL eof;
            CloseFile (cid1);
            CloseTB (TB);

            IF Tracing THEN
                WriteString (file);  WriteString (" (");  WriteCard (N);
                WriteString (")  -->  ");
                WriteString (file1);  WriteString (" (");  WriteCard (N DIV 2);
                WriteString (") + ");
                WriteString (file2);  WriteString (" (");  WriteCard (checkcount);
                WriteString (")");
                WriteLn;
            END (*IF*);

            IF checkcount <> N - N DIV 2 THEN
                WriteString (">> CHECKCOUNT MISMATCH");
                WriteLn;
            END (*IF*);

            (* We've now finished with the original file. *)

            DeleteFile (file);

            (* Sort the two new files, and then merge them. *)

            SortFile (file1, N DIV 2);
            SortFile (file2, N - N DIV 2);
            FileMerge (file1, file2, file);

        END (*IF*);

    END SortFile;

(********************************************************************************)
(*                           SORT A GEDCOM FILE                                 *)
(********************************************************************************)

PROCEDURE SortDatabase (file: FilenameString);

    (* Sorts the GEDCOM file. *)

    VAR TB: Buffer;
        maincid, cid0, cid1: ChanId;
        file0, file1, file2: FilenameString;
        NextLevel, NumberOfRecords: CARDINAL;
        NextLine: Line;
        RecTree: RecordTree;
        done: BOOLEAN;

    BEGIN
        TB := OpenForReading (file);
        IF NOT TBFileOpened(TB) THEN
            WriteString ("Sorry, file ");
            WriteString (file);
            WriteString (" was not found.");
            WriteLn;
            RETURN;
        END (*IF*);

        (***********)
        (* PHASE 1 *)
        (***********)

        (* Phase 1: Put all non-INDI records into file0, put the INDI records   *)
        (* into file1, counting the INDI records as we go.                      *)

        WriteLn;
        WriteString ("Sorting file ");  WriteString (file);  WriteLn;
        WriteString ("Phase 1");  WriteLn;

        cid0 := OpenNewOutputFile (file0);
        cid1 := OpenNewOutputFile (file1);

        StartReading (TB, NextLevel, NextLine);
        NumberOfRecords := 0;  done := FALSE;

        REPEAT
            LoadNextRecord (TB, RecTree, NextLevel, NextLine, done);
            IF NOT done THEN
                IF KwdMatch (RecTree, 'INDI') THEN
                    WriteRecord (cid1, RecTree);
                    INC (NumberOfRecords);
                ELSE
                    WriteRecord (cid0, RecTree);
                END (*IF*);
                DiscardTree (RecTree);
            END (*IF*);
        UNTIL done;

        CloseTB (TB);
        CloseFile (cid0);
        CloseFile (cid1);

        (***********)
        (* PHASE 2 *)
        (***********)

        (* Now all the INDI records are in file1 and all the others are *)
        (* in file0.  Sort file1.                                       *)

        WriteString ("Phase 2");  WriteLn;
        SortFile (file1, NumberOfRecords);

        (***************)
        (* FINAL PASS  *)
        (***************)

        (* Final pass.  Write back the HEAD and optional SUBN records,  *)
        (* then all the INDI records, then everything else.             *)

        WriteString ("Final pass");  WriteLn;
        TB := OpenForReading (file0);
        maincid := OpenNewOutputFile (file2);
        StartReading (TB, NextLevel, NextLine);
        LoadNextRecord (TB, RecTree, NextLevel, NextLine, done);
        IF NOT done THEN
            IF KwdMatch (RecTree, 'HEAD') THEN
                WriteRecord (maincid, RecTree);
                DiscardTree (RecTree);
                LoadNextRecord (TB, RecTree, NextLevel, NextLine, done);
                IF NOT done THEN
                    IF KwdMatch (RecTree, 'SUBN') THEN
                        WriteRecord (maincid, RecTree);
                        DiscardTree (RecTree);
                        LoadNextRecord (TB, RecTree, NextLevel,
                                                        NextLine, done);
                    END (*IF*);
                END (*IF*);
            END (*IF*);
        END (*IF*);
        CloseFile (maincid);

        (* We have now written the leading records from file0 to file2. *)
        (* Append all the INDI records from file1, then delete file1.   *)

        IF AppendFile (file1, file2) THEN
            DeleteFile (file1);
        ELSE
            WriteString ("ERROR: Could not append ");  WriteString (file1);
            WriteString (" to ");  WriteString (file2);  WriteLn;
            RETURN;
        END (*IF*);

        (* At this stage file0 is still open.  Reopen file2 at the end  *)
        (* and copy all remaining records from file0 to file2.  Then    *)
        (* we can delete file0.                                         *)

        cid0 := OpenAtEnd (file2);
        WHILE NOT done DO
            WriteRecord (cid0, RecTree);
            DiscardTree (RecTree);
            LoadNextRecord (TB, RecTree, NextLevel, NextLine, done);
        END (*WHILE*);
        CloseFile (cid0);
        CloseTB (TB);
        DeleteFile (file0);

        (* The entire sorted database is now in file2.  Delete the      *)
        (* original, and rename file2 to the original file name.        *)

        DeleteFile (file);
        IF NOT MoveFile (file2, file) THEN
            WriteString ("ERROR: Could not rename ");  WriteString (file2);
            WriteString (" to ");  WriteString (file);  WriteLn;
        END (*IF*);

    END SortDatabase;

(********************************************************************************)
(*                                 MAIN PROGRAM                                 *)
(********************************************************************************)

PROCEDURE DoTheJob;

    (* Sorts the INDI records into alphabetical order. *)

    VAR DatabaseName, MainFile: FilenameString;
        found: BOOLEAN;  pos: CARDINAL;

    BEGIN
        GetArgument (DatabaseName);
        IF DatabaseName[0] = Nul THEN
            WriteLn;
            WriteString ("Usage: GedSort databasename");
            WriteLn;
            WriteLn;
            WriteString ("This will sort the individuals in a GEDCOM file databasename.ged");
            WriteLn;
            WriteString ("into alphabetical order.");
            WriteLn;
            RETURN;
        END (*IF*);

        MainFile := DatabaseName;
        Strings.FindPrev ('.', MainFile, LENGTH(MainFile)-1, found, pos);
        IF NOT found THEN
            Strings.Append (".GED", MainFile);
        END (*IF*);
        SortDatabase (MainFile);

    END DoTheJob;

(********************************************************************************)

BEGIN
    NextName := "00000000";
    DoTheJob;
END GedSort.

