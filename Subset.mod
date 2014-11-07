MODULE Subset;

        (********************************************************)
        (*                                                      *)
        (*      Utility to build a subset of a GEDCOM file,     *)
        (*    containing one specified node, all descendants    *)
        (*   of that node (but not the ancestors or spouses),   *)
        (*    and ancestors and spouses of those descendants    *)
        (*         other than what the above rules out.         *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            19 December 2004                *)
        (*  Last edited:        23 December 2004                *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


IMPORT IOChan, TextIO, Strings;

FROM OurTypes IMPORT
    (* type *)  IDString, DataString, Line;

FROM Records IMPORT
    (* type *)  RecordTree,
    (* proc *)  StartReading, LoadNextRecord, DiscardTopLevel,
                IsPeopleRecord, DiscardTree, GetField, WriteRecord;

FROM Sets IMPORT
    (* type *)  Set,
    (* proc *)  CreateSet, DiscardSet, IsEmpty,
                Include, Exclude, TakeFrom, IsMember;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent, NextArg;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM TextBuffers IMPORT
    (* type *)  Buffer,
    (* proc *)  OpenForReading, TBFileOpened, CloseTB;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    Debugging = FALSE;
    DefaultInput = "test";
    DefaultID = "I004";

    MaxNumberOfNodes = 50000;
    HashTableSize = 49999;        (* preferably prime *)

TYPE
    FilenameString = ARRAY [0..511] OF CHAR;
    NodeIndex = [1..MaxNumberOfNodes];
    NodeCount = [0..MaxNumberOfNodes];
    HashCode = [0..HashTableSize-1];

    (* Linear list of families. *)

    ListOfFamilies = POINTER TO
                         RECORD
                             next: ListOfFamilies;
                             this: NodeCount;
                         END (*RECORD*);

    (* Linear list of people. *)

    ListOfPeople = POINTER TO
                         RECORD
                             next: ListOfPeople;
                             this: NodeCount;
                         END (*RECORD*);

    (* The nodes in the graph we are constructing.  The fields are:     *)
    (*                                                                  *)
    (*    included   TRUE iff this node forms part of the subgraph      *)
    (*               we are constructing                                *)
    (*    ID         the individual or family identifier, as recorded   *)
    (*               in the gedcom file                                 *)
    (*    nexthash   used to maintain a linear list of nodes that have  *)
    (*               the same hash code                                 *)
    (*    isfamily   true if this node represents a family, false if it *)
    (*               represents an individual                           *)
    (*                                                                  *)
    (* For an individual, we then record                                *)
    (*                                                                  *)
    (*    parents    node number for the family node for the family of  *)
    (*               which this individual is a child                   *)
    (*    filler     no meaning, this field is present only to make     *)
    (*               the record components line up, for neatness        *)
    (*    spice      head of a linked list of families of which this    *)
    (*               individual is a spouse                             *)
    (*                                                                  *)
    (* For a family, the fields are                                     *)
    (*                                                                  *)
    (*    husband    node number for the individual record for the      *)
    (*               male parent of this family                         *)
    (*    wife       node number for the individual record for the      *)
    (*               female parent of this family                       *)
    (*    children   head of a linked list of individuals which are the *)
    (*               children of this family                            *)

    NodeArray = ARRAY NodeIndex OF
                    RECORD
                        included: BOOLEAN;
                        ID:       IDString;
                        nexthash: NodeCount;
                        CASE isfamily: BOOLEAN OF
                             FALSE:
                                    parents: NodeCount;
                                    filler: NodeCount;
                                    spice: ListOfFamilies;
                           |
                             TRUE:
                                    husband: NodeCount;
                                    wife: NodeCount;
                                    children: ListOfPeople;
                        END (*CASE*);
                    END (*RECORD*);

(************************************************************************)

VAR
    (* Number of nodes found.  *)

    NumberOfNodes: NodeCount;

    (* The nodes in the graph we are constructing. *)

    Node: NodeArray;

    (* Hash table for converting ID to node number.  Each element of    *)
    (* this table is a node number, for the node which is at the head   *)
    (* of the linear list of nodes that have the index as their hash    *)
    (* code.  The 'next' pointer of this linear list lives in the       *)
    (* Node array.                                                      *)

    HashTable: ARRAY HashCode OF NodeCount;

(************************************************************************)
(*                         GET THE DATABASE NAME                        *)
(************************************************************************)

PROCEDURE GetArguments (VAR (*OUT*) DatabaseName: FilenameString;
                        VAR (*OUT*) NodeLabel: IDString);

    (* Picks up the arguments - a pair of strings - to the program. *)

    VAR cid: IOChan.ChanId;

    BEGIN
        IF Debugging THEN
            DatabaseName := DefaultInput;
            NodeLabel := DefaultID;
        ELSE
            cid := ArgChan();
            IF IsArgPresent() THEN
                TextIO.ReadToken (cid, DatabaseName);
                TextIO.ReadToken (cid, NodeLabel);
            ELSE
                DatabaseName := "";
                NodeLabel := "";
            END (*IF*);
        END (*IF*);

    END GetArguments;

(************************************************************************)
(*                      MISCELLANEOUS UTILITIES                         *)
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

PROCEDURE Hash (ID: IDString): HashCode;

    (* Converts an ID to a hash code. *)

    VAR result: HashCode;  j: CARDINAL;

    BEGIN
        result := 0;
        IF ID[0] <> Nul THEN
            FOR j := 0 TO LENGTH(ID)-1 DO
                result := (j*result + ORD(ID[j])) MOD HashTableSize;
            END (*FOR*);
        END (*IF*);
        RETURN result;
    END Hash;

(************************************************************************)

PROCEDURE ClearHashTable;

    (* Puts zero in every entry of the hash table. *)

    VAR j: HashCode;

    BEGIN
        FOR j := 0 TO HashTableSize-1 DO
            HashTable[j] := 0;
        END (*FOR*);
    END ClearHashTable;

(************************************************************************)
(*                       TRANSLATE ID TO NODE NUMBER                    *)
(************************************************************************)

PROCEDURE IDToNode (ID: IDString): NodeCount;

    (* Returns the node number corresponding to the given ID.  A result *)
    (* of 0 means 'not found'.                                          *)

    VAR result: NodeCount;

    BEGIN
        result := HashTable[Hash(ID)];
        WHILE (result <> 0) AND NOT Strings.Equal (ID, Node[result].ID) DO
            result := Node[result].nexthash;
        END (*WHILE*);

        RETURN result;

    END IDToNode;

(************************************************************************)
(*                         PRUNING THE GRAPH                            *)
(************************************************************************)

PROCEDURE RemovePersonFromList (VAR (*INOUT*) L: ListOfPeople;
                                        toremove: NodeCount);

    (* Removes node 'toremove' from list L. *)

    VAR prev, current: ListOfPeople;

    BEGIN
        prev := NIL;  current := L;
        WHILE (current <> NIL) AND (current^.this <> toremove) DO
            prev := current;  current := current^.next;
        END (*WHILE*);
        IF current <> NIL THEN
            IF prev = NIL THEN
                L := current^.next;
            ELSE
                prev^.next := current^.next;
            END (*IF*);
            DISPOSE (current);
        END (*IF*);
    END RemovePersonFromList;

(************************************************************************)

PROCEDURE RemoveFamilyFromList (VAR (*INOUT*) L: ListOfFamilies;
                                        toremove: NodeCount);

    (* Removes node 'toremove' from list L. *)

    VAR prev, current: ListOfFamilies;

    BEGIN
        prev := NIL;  current := L;
        WHILE (current <> NIL) AND (current^.this <> toremove) DO
            prev := current;  current := current^.next;
        END (*WHILE*);
        IF current <> NIL THEN
            IF prev = NIL THEN
                L := current^.next;
            ELSE
                prev^.next := current^.next;
            END (*IF*);
            DISPOSE (current);
        END (*IF*);
    END RemoveFamilyFromList;

(************************************************************************)

PROCEDURE RemoveChildFromFamily (childnode: NodeCount);

    (* Removes the links, in both directions, between this node and     *)
    (* the family of which it is a child.                               *)

    VAR famnode: NodeCount;

    BEGIN
        IF childnode <> 0 THEN
            famnode := Node[childnode].parents;
            Node[childnode].parents := 0;
            IF famnode <> 0 THEN
                RemovePersonFromList (Node[famnode].children, childnode);
            END (*IF*);
        END (*IF*);
    END RemoveChildFromFamily;

(************************************************************************)

PROCEDURE RemoveSpouseFromFamily (thisnode, familynode: NodeCount);

    (* Removes the links, in both directions, between this node and     *)
    (* the family of which it is a child.                               *)

    BEGIN
        IF familynode <> 0 THEN
            IF thisnode = Node[familynode].husband THEN
                Node[familynode].husband := 0;
            ELSIF thisnode = Node[familynode].wife THEN
                Node[familynode].wife := 0;
            END (*IF*);
        END (*IF*);
        IF thisnode <> 0 THEN
            RemoveFamilyFromList (Node[thisnode].spice, familynode);
        END (*IF*);
    END RemoveSpouseFromFamily;

(************************************************************************)

PROCEDURE PruneGraph (start: NodeCount);

    (* From the given start node, puts "included" marks in the nodes    *)
    (* that we want to keep in our result subgraph.                     *)

    VAR tobeexpanded: Set;
        node: CARDINAL;

    BEGIN
        (* Remove the parent link from the start node. *)

        IF Node[start].isfamily THEN
            RemoveChildFromFamily(Node[start].husband);
            RemoveChildFromFamily(Node[start].wife);
        ELSE
            RemoveChildFromFamily(start);
        END (*IF*);

        (* Put the start node into the set of nodes to be checked. *)

        tobeexpanded := CreateSet (NumberOfNodes);
        Include (start, tobeexpanded);

        (* Keep expanding nodes until 'tobeexpanded' is empty. *)

        WHILE TakeFrom (node, tobeexpanded) DO
            WITH Node[node] DO
                included := TRUE;
                IF isfamily THEN

                    (* Add the husband, wife, and children to the   *)
                    (* set of nodes to be expanded.                 *)

                    IF husband <> 0 THEN
                        Include (husband, tobeexpanded);
                        RemoveSpouseFromFamily (husband, node);
                    END (*IF*);
                    IF wife <> 0 THEN
                        Include (wife, tobeexpanded);
                        RemoveSpouseFromFamily (wife, node);
                    END (*IF*);
                    WHILE children <> NIL DO
                        Include (children^.this, tobeexpanded);
                        RemoveChildFromFamily (children^.this);
                    END (*WHILE*);

                ELSE
                    (* Add the parents and spouse families. *)

                    IF parents <> 0 THEN
                        Include (parents, tobeexpanded);
                        RemoveChildFromFamily (node);
                    END (*IF*);
                    WHILE spice <> NIL DO
                        Include (spice^.this, tobeexpanded);
                        RemoveSpouseFromFamily (node, spice^.this);
                    END (*WHILE*);

                END (*IF*);
            END (*WITH*);
        END (*WHILE*);

    END PruneGraph;

(************************************************************************)
(*                BUILD A NODE REPRESENTING ONE INDIVIDUAL              *)
(************************************************************************)

PROCEDURE ExtractTag (string: ARRAY OF CHAR;  VAR (*OUT*) tag: IDString);

    (* Extracts the substring of string enclosed by @ delimiters.  *)

    VAR pos1, pos2: CARDINAL;  found: BOOLEAN;

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
    END ExtractTag;

(************************************************************************)

PROCEDURE GetPersonInfoPass2 (index: NodeIndex;
                              VAR (*INOUT*) T: RecordTree);

    (* Updates Node[index] from tree information T, which is already    *)
    (* known to be for an INDI record.                                  *)

    VAR datum: DataString;  ID: IDString;  code: NodeCount;
        famptr: ListOfFamilies;

    BEGIN
        (* Process the FAMC record (should be only one). *)

        IF GetField ('FAMC', T, datum) THEN
            ExtractTag (datum, ID);
            code := IDToNode(ID);
            Node[index].parents := code;
        END (*IF*);

        (* Now handle the FAMS records (there could be more than one).  *)

        WHILE GetField ('FAMS', T, datum) DO
            ExtractTag (datum, ID);
            code := IDToNode(ID);
            NEW (famptr);
            famptr^.this := code;
            famptr^.next := Node[index].spice;
            Node[index].spice := famptr;
        END (*WHILE*);

    END GetPersonInfoPass2;

(************************************************************************)
(*                 CREATE A NODE REPRESENTING ONE FAMILY                *)
(************************************************************************)

PROCEDURE GetFamilyInfoPass2 (index: NodeIndex;
                              VAR (*INOUT*) T: RecordTree);

    (* Updates Node[index] from tree information T, which is already    *)
    (* known to be a FAM record.                                        *)

    VAR string: DataString;  ID: IDString;
        childptr: ListOfPeople;

    BEGIN
        (* Husband. *)

        IF GetField ('HUSB', T, string) THEN
            ExtractTag (string, ID);
            Node[index].husband := IDToNode(ID);
        END (*IF*);

        (* Wife *)

        IF GetField ('WIFE', T, string) THEN
            ExtractTag (string, ID);
            Node[index].wife := IDToNode(ID);
        END (*IF*);

        (* Children *)

        WHILE GetField ('CHIL', T, string) DO
            ExtractTag (string, ID);
            NEW (childptr);
            childptr^.this := IDToNode(ID);
            childptr^.next := Node[index].children;
            Node[index].children := childptr;
        END (*WHILE*);

    END GetFamilyInfoPass2;

(************************************************************************)
(*                          CREATE THE GRAPH                            *)
(************************************************************************)

PROCEDURE ClearNode (index: NodeIndex);

    (* Clean initialisation of the data in Node[index] - except for the *)
    (* ID and isfamily fields, which we assume were already filled in   *)
    (* during Pass 1.                                                   *)

    VAR code: HashCode;

    BEGIN
        WITH Node[index] DO
            code := Hash (Node[index].ID);
            nexthash := HashTable[code];
            HashTable[code] := index;
            IF isfamily THEN
                husband := 0;
                wife := 0;
                children := NIL;
            ELSE
                parents := 0;
                filler := 0;
                spice := NIL;
            END (*IF*);
            included := FALSE;
        END (*WITH*);
    END ClearNode;

(************************************************************************)

PROCEDURE Pass1 (file: FilenameString): BOOLEAN;

    (* Reads the entire file, and creates the mapping between indexes   *)
    (* and identifiers for all individuals and families.                *)

    VAR TB: Buffer;
        NextLevel, NumberOfPeople, NumberOfFamilies: CARDINAL;
        NextLine: Line;
        RecTree: RecordTree;
        success, done, family: BOOLEAN;
        thisID: IDString;

    BEGIN
        TB := OpenForReading (file);
        IF NOT TBFileOpened(TB) THEN
            WriteString ("Sorry, file ");
            WriteString (file);
            WriteString (" was not found.");
            WriteLn;
            RETURN FALSE;
        END (*IF*);

        StartReading (TB, NextLevel, NextLine);
        NumberOfPeople := 0;  NumberOfFamilies := 0;
        NumberOfNodes := 0;
        done := FALSE;  success := TRUE;

        REPEAT
            LoadNextRecord (TB, RecTree, NextLevel, NextLine, done);
            IF NOT done THEN
                IF IsPeopleRecord (RecTree, thisID, family) THEN

                    IF family THEN
                        INC (NumberOfFamilies);
                    ELSE
                        INC (NumberOfPeople);
                    END (*IF*);

                    IF success AND (NumberOfNodes = MaxNumberOfNodes) THEN
                        WriteString ("Sorry, this program can handle only ");
                        WriteCard (MaxNumberOfNodes);
                        WriteString (" (individual plus family) nodes.");
                        WriteLn;
                        WriteString ("Recompile and try again.");
                        WriteLn;
                        success := FALSE;
                    END (*IF*);

                    IF success THEN
                        INC (NumberOfNodes);
                        WITH Node[NumberOfNodes] DO
                            isfamily := family;
                            ID := thisID;
                        END (*WITH*);
                        ClearNode (NumberOfNodes);
                    END (*IF*);

                END (*IF*);
                DiscardTree (RecTree);
            END (*IF*);
        UNTIL done;

        CloseTB (TB);

        RETURN success;

    END Pass1;

(************************************************************************)

PROCEDURE Pass2 (file: FilenameString);

    (* Reads the entire file, and creates an in-memory representation   *)
    (* of the structural dependencies.  On entry the mapping between    *)
    (* indices and identifiers has already been constructed.            *)

    VAR index: NodeCount;
        thisID: IDString;

    (********************************************************************)

    PROCEDURE SetIndex;

        (* Sets the variable 'index' to select the node corresponding   *)
        (* to the current record.                                       *)

        BEGIN
            INC (index);
            IF NOT Strings.Equal (Node[index].ID, thisID) THEN
                WriteString ("Node ");
                WriteString (thisID);
                WriteString (" not found at expected position in file");
                WriteString (", continuing anyway.");
                WriteLn;
                index := 1;
                LOOP
                    IF index > NumberOfNodes THEN
                        index := 0;  EXIT(*LOOP*);
                    ELSIF Strings.Equal (Node[index].ID, thisID) THEN
                        EXIT(*LOOP*);
                    ELSIF index = MaxNumberOfNodes THEN
                        index := 0;  EXIT(*LOOP*);
                    ELSE
                        INC (index);
                    END (*IF*);
                END (*LOOP*);
            END (*IF*);
        END SetIndex;

    (********************************************************************)

    VAR TB: Buffer;
        NextLevel: CARDINAL;
        NextLine: Line;
        RecTree: RecordTree;
        done, family: BOOLEAN;

    BEGIN
        TB := OpenForReading (file);
        IF NOT TBFileOpened(TB) THEN
            WriteString ("Sorry, did not succeed in reopening file ");
            WriteString (file);
            WriteLn;
            RETURN;
        END (*IF*);

        StartReading (TB, NextLevel, NextLine);
        index := 0;
        done := FALSE;

        REPEAT
            LoadNextRecord (TB, RecTree, NextLevel, NextLine, done);
            IF NOT done THEN
                IF IsPeopleRecord (RecTree, thisID, family) THEN
                    SetIndex;
                    IF (index = 0) OR (family <> Node[index].isfamily) THEN
                        WriteString ("Internal inconsistency, aborting program.");
                        WriteLn;
                        done := TRUE;
                    ELSE
                        DiscardTopLevel (RecTree);
                        IF family THEN
                            GetFamilyInfoPass2 (index, RecTree);
                        ELSE
                            GetPersonInfoPass2 (index, RecTree);
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
                DiscardTree (RecTree);
            END (*IF*);
        UNTIL done;

        CloseTB (TB);

    END Pass2;

(************************************************************************)

PROCEDURE CreateGraph (file: FilenameString);

    (* Reads the entire file, and creates an in-memory representation   *)
    (* of the structural dependencies.                                  *)

    BEGIN
        ClearHashTable;
        IF Pass1(file) THEN
            Pass2(file);
        ELSE
            WriteString ("Pass 2 skipped because of fatal errors.");
            WriteLn;
        END (*IF*);
    END CreateGraph;

(************************************************************************)

PROCEDURE WriteResult (file: FilenameString);

    (* Rereads the entire file, and writes to standard output the nodes *)
    (* that we want to retain.                                          *)

    CONST outcid = 1;

    VAR TB: Buffer;
        NextLevel: CARDINAL;
        NextLine: Line;
        done, family: BOOLEAN;
        RecTree: RecordTree;
        thisID: IDString;
        index: NodeCount;

    (********************************************************************)

    PROCEDURE SetIndex;

        (* Sets the variable 'index' to select the node corresponding   *)
        (* to the current record.                                       *)

        BEGIN
            INC (index);
            IF NOT Strings.Equal (Node[index].ID, thisID) THEN
                index := 1;
                LOOP
                    IF index > NumberOfNodes THEN
                        index := 0;  EXIT(*LOOP*);
                    ELSIF Strings.Equal (Node[index].ID, thisID) THEN
                        EXIT(*LOOP*);
                    ELSIF index = MaxNumberOfNodes THEN
                        index := 0;  EXIT(*LOOP*);
                    ELSE
                        INC (index);
                    END (*IF*);
                END (*LOOP*);
            END (*IF*);
        END SetIndex;

    (********************************************************************)

    BEGIN
        index := 0;
        TB := OpenForReading (file);
        StartReading (TB, NextLevel, NextLine);
        done := FALSE;

        REPEAT
            LoadNextRecord (TB, RecTree, NextLevel, NextLine, done);
            IF NOT done THEN
                IF IsPeopleRecord (RecTree, thisID, family) THEN
                    SetIndex;
                    IF Node[index].included THEN
                        WriteRecord (outcid, RecTree);
                    END (*IF*);
                ELSE
                    WriteRecord (outcid, RecTree);
                END (*IF*);
                DiscardTree (RecTree);
            END (*IF*);
        UNTIL done;

    END WriteResult;

(************************************************************************)
(*                           MAIN PROGRAM                               *)
(************************************************************************)

PROCEDURE DoTheJob;

    (* We create a graph of the database, work out which nodes belong   *)
    (* to a subset of that graph, and then write the subset out to      *)
    (* standard output.                                                 *)

    VAR DatabaseName, MainFile: FilenameString;
        NodeLabel: IDString;
        found: BOOLEAN;  pos: CARDINAL;

    BEGIN
        GetArguments (DatabaseName, NodeLabel);
        IF DatabaseName[0] = Nul THEN
            WriteLn;
            WriteString ("Usage: Subset databasename nodelabel");
            WriteLn;
            WriteLn;
            WriteString ("This will write a subset of a GEDCOM file to standard output.");
            WriteLn;
            RETURN;
        END (*IF*);

        MainFile := DatabaseName;
        Strings.FindPrev ('.', MainFile, LENGTH(MainFile)-1, found, pos);
        IF NOT found THEN
            Strings.Append (".GED", MainFile);
        END (*IF*);
        CreateGraph (MainFile);
        PruneGraph (IDToNode(NodeLabel));
        WriteResult (MainFile);

    END DoTheJob;

(************************************************************************)

BEGIN
    DoTheJob;
END Subset.

