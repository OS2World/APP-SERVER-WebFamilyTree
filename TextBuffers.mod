(**************************************************************************)
(*                                                                        *)
(*  Support modules for network applications                              *)
(*  Copyright (C) 2014   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

IMPLEMENTATION MODULE TextBuffers;

        (********************************************************)
        (*                                                      *)
        (*        Text buffers for line-oriented file I/O       *)
        (*                                                      *)
        (*          (At present we support only input)          *)
        (*                                                      *)
        (*   This module maintains a read-ahead buffer for each *)
        (*   open file.  Obviously it will be efficient to do   *)
        (*   this only if input is mostly sequential.           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            16 July 2001                    *)
        (*  Last edited:        23 July 2012                    *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT LOC, BYTE, ADDRESS, CAST, ADR;

FROM Types IMPORT CARD64, INT64;

IMPORT Strings;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilePos,
    (* proc *)  OpenOldFile, CloseFile, StartPosition,
                EndPosition, SetPosition, ReadRaw;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LONGLONG IMPORT
    (* proc *)  Add64, Diff64;

FROM LowLevel IMPORT
    (* proc *)  Copy, AddOffset;

(************************************************************************)

CONST
    NumberOfBuffers = 8;
    BufferSize = 4096;
    Nul = CHR(0);
    CtrlZ = CHR(26);

TYPE
    EncodingType = (byte, twobyte, fourbyte);

    (* Each data buffer contains the following fields.                  *)
    (*    StartPos  the position within the file from which these data  *)
    (*              were read                                           *)
    (*    count     the number of bytes of data                         *)
    (*    data      the actual cached data                              *)
    (* The data array is one byte longer than we need, with the last    *)
    (* character permanently set to Nul.  This prevents string searches *)
    (* from running off the end of the array.                           *)

    BufferPointer = POINTER TO BufferRecord;
    BufferRecord = RECORD
                       StartPos: FilePos;
                       count: CARDINAL;
                       data: ARRAY [0..BufferSize] OF CHAR;
                   END (*RECORD*);

    (* A Buffer record contains NumberOfBuffers data buffers, each of   *)
    (* which is a cached section of the currently opened file.  The     *)
    (* fields are                                                       *)
    (*    cid            channel ID of the opened file                  *)
    (*    CurrentPosition  the location in the file from which the      *)
    (*                     next character will come, assuming that      *)
    (*                     we continue to read sequentially             *)
    (*    CurrentBuffer  the buffer from which we're currently reading  *)
    (*                   data.  If this is zero, we need to switch to   *)
    (*                   a new buffer.                                  *)
    (*    BOMoffset      offset from start of file to start of text.    *)
    (*    offset         offset within CurrentBuffer to the next        *)
    (*                    character to be read in a sequential read.    *)
    (*    encoding       flag to label a UTF-16 or UTF-32 file.         *)
    (*    BigEndian      Meaningful only if encoding <> byte.           *)
    (*    BufPtr         points to an array of buffers                  *)

    Buffer = POINTER TO StateRecord;
    StateRecord = RECORD
                      cid: ChanId;
                      CurrentPosition: FilePos;
                      CurrentBuffer: [0..NumberOfBuffers];
                      BOMoffset, offset: CARDINAL;
                      encoding: EncodingType;
                      BigEndian: BOOLEAN;
                      BufPtr: ARRAY [1..NumberOfBuffers] OF BufferPointer;
                  END (*RECORD*);

(************************************************************************)
(*               DEALING WITH 16-bit and 32-bit UNICODE                 *)
(************************************************************************)

PROCEDURE ConvertToUTF8 (VAR (*INOUT*) data: ARRAY OF CHAR;
                             encoding: EncodingType;
                             BigEndian: BOOLEAN;
                             VAR (*INOUT*) bytecount: CARDINAL);

    (* Converts bytecount bytes from UTF-32 or UTF-16 to UTF-8, in      *)
    (* place, and updates bytecount to reflect the result.  If          *)
    (* bytecount is not a multiple of the basic encoding unit on entry  *)
    (* (which shouldn't happen), some trailing bytes of input are       *)
    (* dropped.                                                         *)

    VAR incount: CARDINAL;

    (********************************************************************)

    PROCEDURE GetTwoBytes(): CARDINAL;

        (* Fetches a 16-bit value from the data array. *)

        VAR result: CARDINAL;

        BEGIN
            result := ORD(data[incount]);  INC (incount);
            IF BigEndian THEN
                result := 256*result + ORD(data[incount]);
            ELSE
                result := result + 256*ORD(data[incount]);
            END (*IF*);
            INC (incount);
            RETURN result;
        END GetTwoBytes;

    (********************************************************************)

    PROCEDURE GetFourBytes(): CARDINAL;

        (* Fetches a 32-bit value from the data array. *)

        VAR result: CARDINAL;

        BEGIN
            result := GetTwoBytes();
            IF BigEndian THEN
                result := 65536*result + GetTwoBytes();
            ELSE
                result := result + 65536*GetTwoBytes();
            END (*IF*);
            RETURN result;
        END GetFourBytes;

    (********************************************************************)

    VAR outcount, code: CARDINAL;
        result: ARRAY [0..3*BufferSize/2-1] OF CHAR;

        (* The size of the result array is based on the worst case,     *)
        (* where a 16-bit UTF16 code translates to a 3-byte result.     *)
        (* In that (rare) worst case we might end up truncating the     *)
        (* result, but that's OK since the procedure that calls us      *)
        (* will pick up what's missing on the next call, if needed.     *)

    BEGIN
        IF encoding = byte THEN
            RETURN;       (* No transformation needed *)
        END (*IF*);

        IF ODD(bytecount) THEN
            DEC (bytecount);
        END (*IF*);
        incount := 0;  outcount := 0;
        WHILE incount < bytecount DO
            IF encoding = fourbyte THEN
                code := GetFourBytes();
            ELSE
                code := GetTwoBytes();
                IF (code >= 0D800H) AND (code <= 0DFFFFH) THEN
                    (* Get the second word of a two-word UTF16 code. *)
                    code := 1024*(code MOD 1024) + (GetTwoBytes() MOD 1024);
                END (*IF*);
            END (*IF*);

            (* Now that we have the Unicode value, translate it to UTF8. *)

            IF code = 0 THEN
                result[outcount] := '?';  INC(outcount);
            ELSIF code < 080H THEN
                result[outcount] := CHR(code);  INC(outcount);
            ELSE
                IF code < 0800H THEN
                    result[outcount] := CHR(0C0H + code DIV 64);  INC(outcount);
                ELSE
                    IF code < 01000H THEN
                        result[outcount] := CHR(0E0H + code DIV 4096);  INC(outcount);
                    ELSE
                        result[outcount] := CHR(0F0H + code DIV 262144);  INC(outcount);
                        code := code MOD 262144;
                        result[outcount] := CHR(080H + code DIV 4096);  INC(outcount);
                    END (*IF*);
                    code := code MOD 4096;
                    result[outcount] := CHR(080H + code DIV 64);  INC(outcount);
                END (*IF*);
                result[outcount] := CHR(080H + code MOD 64);  INC(outcount);
            END (*IF*);

        END (*WHILE*);

        IF outcount > 0 THEN
            Copy (ADR(result), ADR(data), outcount);
        END (*IF*);
        bytecount := outcount;

    END ConvertToUTF8;

(************************************************************************)
(*                           OPEN/CLOSE/ETC                             *)
(************************************************************************)

PROCEDURE OpenForReading (name: ARRAY OF CHAR;  AllowUTF16: BOOLEAN): Buffer;

    (* Opens an existing file for read-only access, and returns its     *)
    (* buffer ID.  If the second parameter is TRUE then we examine      *)
    (* the first few bytes in the file to see whether this is a         *)
    (* UTF-16 or UTF-32 file.  The second parameter should be FALSE     *)
    (* if the file is an index file.                                    *)

    VAR result: Buffer;  k: CARDINAL;

    BEGIN
        NEW (result);
        WITH result^ DO
            cid := OpenOldFile (name, FALSE, TRUE);
            FOR k := 1 TO NumberOfBuffers DO
                NEW (BufPtr[k]);
                WITH BufPtr[k]^ DO
                    StartPos := CARD64{0,0};  count := 0;
                    offset := 0;
                    data[BufferSize] := Nul;
                END (*WITH*);
            END (*FOR*);
            encoding := byte;
            BigEndian := FALSE;  BOMoffset := 0;
            CurrentBuffer := 1;
            IF cid <> NoSuchChannel THEN
                CurrentPosition := StartPosition(cid);
                ReadRaw (cid, BufPtr[1]^.data, BufferSize, BufPtr[1]^.count);
            END (*IF*);
            IF BufPtr[1]^.count = 0 THEN

                CurrentBuffer := 0;

            ELSIF AllowUTF16 THEN

                (* Check for Unicode.  We work out which variant we have *)
                (* by looking for either a byte order mark or a pattern  *)
                (* of Nul bytes.                                         *)

                IF BufPtr[1]^.data[0] = CHR(0) THEN
                    BigEndian := TRUE;
                    IF BufPtr[1]^.data[1] = CHR(0) THEN
                        encoding := fourbyte;
                        IF (BufPtr[1]^.data[2] = CHR(0FEH)) AND (BufPtr[1]^.data[3] = CHR(0FFH)) THEN
                            BOMoffset := 4;
                        END (*IF*);
                    ELSE
                        encoding := twobyte;
                    END (*IF*);
                ELSIF BufPtr[1]^.data[1] = CHR(0) THEN
                    encoding := twobyte;  BigEndian := FALSE;
                ELSIF (BufPtr[1]^.data[0] = CHR(0EFH)) AND (BufPtr[1]^.data[1] = CHR(0BBH))
                                                    AND (BufPtr[1]^.data[2] = CHR(0BFH)) THEN
                    encoding := byte;
                    BOMoffset := 3;
                ELSIF (BufPtr[1]^.data[0] = CHR(0FFH)) AND (BufPtr[1]^.data[1] = CHR(0FEH)) THEN
                    encoding := twobyte;     (* tentatively, see below *)
                    BigEndian := FALSE;
                    BOMoffset := 2;
                ELSIF (BufPtr[1]^.data[0] = CHR(0FEH)) AND (BufPtr[1]^.data[1] = CHR(0FFH)) THEN
                    encoding := twobyte;  BigEndian := TRUE;
                    BOMoffset := 2;
                END (*IF*);

                (* We still have to check the case where third and fourth bytes are zero. *)

                IF (BufPtr[1]^.data[2] = CHR(0)) AND (BufPtr[1]^.data[3] = CHR(0)) THEN
                    encoding := fourbyte;
                    IF BOMoffset > 0 THEN
                        BOMoffset := 4;
                    END (*IF*);
                END (*IF*);

                offset := BOMoffset;

                WITH BufPtr[1]^ DO

                    IF BOMoffset > 0 THEN
                        DEC (count, BOMoffset);
                        FOR k := 0 TO BufPtr[1]^.count-1 DO
                            data[k] := data[k+BOMoffset];
                        END (*FOR*);
                    END (*IF*);

                    IF encoding <> byte THEN
                        ConvertToUTF8 (data, encoding, BigEndian, count);
                    END (*IF*);

                END (*WITH*);

            END (*IF*);
        END (*WITH*);

        RETURN result;

    END OpenForReading;

(************************************************************************)

PROCEDURE TBFileOpened (TB: Buffer): BOOLEAN;

    (* Use this to check whether an 'open' operation succeeded. *)

    BEGIN
        RETURN (TB <> NIL) AND (TB^.cid <> NoSuchChannel);
    END TBFileOpened;

(************************************************************************)

PROCEDURE CloseTB (VAR (*INOUT*) TB: Buffer);

    (* Closes a file. *)

    VAR k: CARDINAL;

    BEGIN
        IF TB <> NIL THEN
            IF TB^.cid <> NoSuchChannel THEN
                CloseFile (TB^.cid);
            END (*IF*);
            FOR k := 1 TO NumberOfBuffers DO
                DEALLOCATE (TB^.BufPtr[k], SIZE(BufferRecord));
            END (*FOR*);
            DEALLOCATE (TB, SIZE(StateRecord));
        END (*IF*);
    END CloseTB;

(************************************************************************)
(*                         LOADING THE DATA                             *)
(************************************************************************)

PROCEDURE LoadNextBuffer (TB: Buffer);

    (* Reads some more data from the file.  If TB^.CurrentBuffer = 0    *)
    (* after this operation, we must have reached the end of the file.  *)

    BEGIN
        WITH TB^ DO
            IF cid <> NoSuchChannel THEN
                SetPosition (cid, CurrentPosition);

                (* Find the next unused buffer, and default to the last *)
                (* buffer if all are in use.                            *)

                CurrentBuffer := 1;
                WHILE (BufPtr[CurrentBuffer]^.count <> 0)
                                 AND (CurrentBuffer < NumberOfBuffers) DO
                    INC (CurrentBuffer);
                END (*WHILE*);
                WITH BufPtr[CurrentBuffer]^ DO
                    StartPos := CurrentPosition;
                    ReadRaw (cid, data, BufferSize, count);
                    IF encoding <> byte THEN
                        ConvertToUTF8 (data, encoding, BigEndian, count);
                    END (*IF*);
                    data[count] := Nul;
                    offset := 0;
                END (*WITH*);
            END (*IF*);
            IF BufPtr[CurrentBuffer]^.count = 0 THEN
                CurrentBuffer := 0;
            END (*IF*);
        END (*WITH*);
    END LoadNextBuffer;

(************************************************************************)

PROCEDURE SetCurrentBuffer (TB: Buffer);

    (* Updates TB^.CurrentBuffer to be a buffer whose data includes     *)
    (* a character at TB^.CurrentPosition.  This might require reading  *)
    (* more data from the file.  If TB^.CurrentBuffer = 0 after this    *)
    (* operation, we must have reached the end of the file.             *)

    VAR k: CARDINAL;  gap: INT64;

    BEGIN
        k := 1;
        LOOP
            IF k > NumberOfBuffers THEN
                LoadNextBuffer (TB);
                EXIT (*LOOP*);
            ELSE
                WITH TB^ DO
                    WITH BufPtr[k]^ DO
                        IF count > 0 THEN
                            gap := Diff64 (CurrentPosition, StartPos);
                            IF gap.high = 0 THEN
                                offset := gap.low;
                                IF offset < count THEN
                                    CurrentBuffer := k;
                                    EXIT (*LOOP*);
                                END (*IF*);
                            END (*IF*);
                        END (*IF*);
                    END (*WITH*);
                END (*WITH*);
                INC (k);
            END (*IF*);
        END (*LOOP*);
    END SetCurrentBuffer;

(************************************************************************)
(*                         FILE POSITION/SIZE                           *)
(************************************************************************)

PROCEDURE TBCurrentPosition (TB: Buffer): FilePos;

    (* Returns the current position within the file. *)

    BEGIN
        RETURN TB^.CurrentPosition;
    END TBCurrentPosition;

(************************************************************************)

PROCEDURE TBStartPosition (TB: Buffer): FilePos;

    (* Returns the start-of-file position. *)

    VAR result: FilePos;

    BEGIN
        result := StartPosition (TB^.cid);
        IF TB^.BOMoffset > 0 THEN
            Add64 (result, TB^.BOMoffset);
        END (*IF*);
        RETURN result;
    END TBStartPosition;

(************************************************************************)

PROCEDURE TBEndPosition (TB: Buffer): FilePos;

    (* Returns the end-of-file position. *)

    BEGIN
        RETURN EndPosition (TB^.cid);
    END TBEndPosition;

(************************************************************************)

PROCEDURE TBSetPosition (TB: Buffer;  position: FilePos);

    (* Sets the current position within the file. *)

    BEGIN
        TB^.CurrentPosition := position;
        SetCurrentBuffer (TB);
    END TBSetPosition;

(************************************************************************)
(*                              INPUT                                   *)
(************************************************************************)

PROCEDURE TBReadRaw (TB: Buffer;  VAR (*OUT*) data: ARRAY OF LOC;
                   limit: CARDINAL;  VAR (*OUT*) NumberRead: CARDINAL);

    (* Reads a buffer-full of information from a file. *)

    VAR ToGo, amount, available: CARDINAL;
        source, destination: ADDRESS;
        temp: BufferPointer;

    BEGIN
        NumberRead := 0;
        destination := ADR(data);
        ToGo := limit;
        source := NIL;
        IF ToGo > HIGH(data) THEN
            ToGo := HIGH(data) + 1;
        END (*IF*);
        WHILE ToGo > 0 DO
            IF TB^.CurrentBuffer = 0 THEN
                SetCurrentBuffer (TB);
                IF TB^.CurrentBuffer = 0 THEN
                    ToGo := 0;
                END (*IF*);
            END (*IF*);
            amount := ToGo;
            WITH TB^ DO
                IF CurrentBuffer = 0 THEN
                    available := 0;
                ELSE
                    WITH BufPtr[CurrentBuffer]^ DO
                        available := count - offset;
                        IF available > 0 THEN
                            source := ADR(data[offset]);
                        END (*IF*);
                    END (*WITH*);
                END (*IF*);
            END (*WITH*);
            IF amount > available THEN
                amount := available;
                TB^.CurrentBuffer := 0;
            END (*IF*);
            IF amount > 0 THEN
                Copy (source, destination, amount);
                DEC (ToGo, amount);
                INC (NumberRead, amount);
                WITH TB^ DO
                    INC (offset, amount);
                    Add64 (CurrentPosition, amount);
                END (*WITH*);
                destination := AddOffset (destination, amount);
            END (*IF*);
        END (*WHILE*);

        (* Promote current buffer as an approximate LRU calculation. *)

        WITH TB^ DO
            IF CurrentBuffer > 1 THEN
                temp := BufPtr[CurrentBuffer-1];
                BufPtr[CurrentBuffer-1] := BufPtr[CurrentBuffer];
                BufPtr[CurrentBuffer] := temp;
                DEC (CurrentBuffer);
            END (*IF*);
        END (*WITH*);

    END TBReadRaw;

(************************************************************************)

PROCEDURE TBReadLine (TB: Buffer;  VAR (*OUT*) line: ARRAY OF CHAR);

    (* Reads a line of text from a file.  Assumption: a line ends with  *)
    (* CRLF.  To avoid tortuous logic, I take the LF as end of line and *)
    (* skip the CR.  At end of file we return with line[0] = Ctrl/Z.    *)

    CONST CR = CHR(13);  LF = CHR(10);

    VAR length, pos, extra, space: CARDINAL;
        ToFind: ARRAY [0..0] OF CHAR;
        found: BOOLEAN;
        temp: BufferPointer;
        source, destination: ADDRESS;

    BEGIN
        ToFind[0] := LF;
        space := HIGH(line) + 1;
        destination := ADR(line);
        REPEAT
            found := FALSE;
            WITH TB^ DO
                IF CurrentBuffer = 0 THEN
                    SetCurrentBuffer (TB);
                    IF TB^.CurrentBuffer = 0 THEN
                        line[0] := CtrlZ;
                        IF space > 0 THEN
                            DEC (space);
                        END (*IF*);
                        found := TRUE;
                    END (*IF*);
                END (*IF*);
                IF NOT found THEN
                    WITH BufPtr[CurrentBuffer]^ DO
                        source := ADR(data[offset]);
                        Strings.FindNext (ToFind, data, offset, found, pos);
                    END (*WITH*);
                    IF found THEN
                        extra := 1;
                    ELSE
                        pos := BufPtr[CurrentBuffer]^.count;
                        extra := 0;
                    END (*IF*);
                    length := pos - offset;
                    IF (length > 0) AND (BufPtr[CurrentBuffer]^.data[pos-1] = CR) THEN
                        DEC (length);  INC (extra);
                    END (*IF*);
                    IF length > space THEN
                        INC (extra, length-space);
                        length := space;
                    END (*IF*);
                    IF length > 0 THEN
                        Copy (source, destination, length);
                        DEC (space, length);
                        destination := AddOffset (destination, length);
                    END (*IF*);
                    INC (length, extra);
                    Add64 (CurrentPosition, length);
                    IF found THEN
                        IF length > 0 THEN
                            INC (offset, length);
                        END (*IF*);
                    ELSE
                        CurrentBuffer := 0;
                    END (*IF*);
                END (*IF*);
            END (*WITH*);
        UNTIL found;

        IF space > 0 THEN
            line[HIGH(line)+1-space] := Nul;
        END (*IF*);

        (* Promote current buffer as an approximate LRU calculation. *)

        WITH TB^ DO
            IF CurrentBuffer > 1 THEN
                temp := BufPtr[CurrentBuffer-1];
                BufPtr[CurrentBuffer-1] := BufPtr[CurrentBuffer];
                BufPtr[CurrentBuffer] := temp;
                DEC (CurrentBuffer);
            END (*IF*);
        END (*WITH*);

    END TBReadLine;

(************************************************************************)

END TextBuffers.

