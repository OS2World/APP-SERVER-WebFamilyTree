MODULE Lint;

        (********************************************************)
        (*                                                      *)
        (*      Utility to build a tree from a GEDCOM file,     *)
        (*     and to report on possible structural problems    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            28 November 2001                *)
        (*  Last edited:        17 February 2009                *)
        (*  Status:                                             *)
        (*      Pass 1 and Pass 2 both OK                       *)
        (*      Loop detection now appears to be correct        *)
        (*                                                      *)
        (********************************************************)


IMPORT IOChan, TextIO, Strings;

FROM OurTypes IMPORT
    (* type *)  IDString, DataString, Line;

FROM Records IMPORT
    (* type *)  RecordTree,
    (* proc *)  StartReading, LoadNextRecord, DiscardTopLevel,
                IsPeopleRecord, DiscardTree, GetField;

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
    (* proc *)  ALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    Debugging = FALSE;
    DefaultInput = "data\moylan";

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
    (*    live       TRUE iff this node has not been removed from       *)
    (*               consideration                                      *)
    (*    sex        sex for an individual, ignored for a family        *)
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
                        live:     BOOLEAN;
                        sex:      CHAR;
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
    (* Number of nodes found.  So as not to leave gaps, the count       *)
    (* includes those nodes that have been removed by setting the       *)
    (* 'live' field to FALSE.                                           *)

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

PROCEDURE External (ID: IDString): BOOLEAN;

    (* Returns TRUE iff this is a reference to a different file. *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.FindNext (':', ID, 0, found, pos);
        RETURN found;
    END External;

(************************************************************************)
(*                    CHECKING PROPERTIES OF THE GRAPH                  *)
(************************************************************************)

PROCEDURE DetectEmptyFamilies;

    (* Reports on any families with no members, and clears their        *)
    (* 'live' markers.                                                  *)

    VAR j: NodeIndex;

    BEGIN
        FOR j := 1 TO NumberOfNodes DO
            IF Node[j].isfamily AND (Node[j].husband = 0)
                  AND (Node[j].wife = 0) AND (Node[j].children = NIL) THEN
                WriteString ("Family ");
                WriteString (Node[j].ID);
                WriteString (" has no members.");
                WriteLn;
                Node[j].live := FALSE;
            END (*IF*);
        END (*FOR*);
    END DetectEmptyFamilies;

(************************************************************************)

PROCEDURE NoParents (j: NodeIndex): BOOLEAN;

    (* Returns TRUE iff this node represents a family with no father    *)
    (* and no mother, or if it does not represent a family.             *)

    BEGIN
        RETURN (NOT Node[j].live) OR (NOT Node[j].isfamily) OR
                  ((Node[j].husband = 0) AND (Node[j].wife = 0));
    END NoParents;

(************************************************************************)

PROCEDURE NumberOfChildren (j: NodeIndex): CARDINAL;

    (* If node j represents an individual, returns the number of        *)
    (* children that person has.  If it is a family node, returns the   *)
    (* number of children in that family.  We assume that it is already *)
    (* known that a list of people contains only individuals, and that  *)
    (* a list of families contains only families.  (This sort of error  *)
    (* is detected in the Pass 2 processing.)                           *)

    VAR count: CARDINAL;  p: ListOfPeople;  q: ListOfFamilies;

    BEGIN
        count := 0;
        IF Node[j].live THEN
            IF Node[j].isfamily THEN
                p := Node[j].children;
                WHILE p <> NIL DO
                    IF p^.this <> 0 THEN
                        INC (count);
                    END (*IF*);
                    p := p^.next;
                END (*WHILE*);
            ELSE
                q := Node[j].spice;
                WHILE q <> NIL DO
                    IF q^.this <> 0 THEN
                        INC (count, NumberOfChildren(q^.this));
                    END (*IF*);
                    q := q^.next;
                END (*WHILE*);
            END (*IF*);
        END (*IF*);
        RETURN count;
    END NumberOfChildren;

(************************************************************************)

PROCEDURE NumberOfSpice (j: NodeIndex): CARDINAL;

    (* If node j represents an individual, returns the number of        *)
    (* husbands and wives that person has.  If it is a family node,     *)
    (* returns the number of spouses in that family.                    *)

    VAR count: CARDINAL;  q: ListOfFamilies;
        k: NodeCount;

    BEGIN
        count := 0;
        IF Node[j].live THEN
            IF Node[j].isfamily THEN
                IF Node[j].husband <> 0 THEN INC(count) END(*IF*);
                IF Node[j].wife <> 0 THEN INC(count) END(*IF*);
            ELSE
                q := Node[j].spice;
                WHILE q <> NIL DO
                    k := q^.this;

                    (* Person j is necessarily either the husband or    *)
                    (* wife in this marriage, therefore person j has a  *)
                    (* spouse recorded in this marriage iff there are   *)
                    (* both a husband and wife recorded.                *)

                    IF (k <> 0) AND (Node[k].husband <> 0)
                                AND (Node[k].wife <> 0) THEN
                        INC (count);
                    END (*IF*);
                    q := q^.next;
                END (*WHILE*);
            END (*IF*);
        END (*IF*);
        RETURN count;
    END NumberOfSpice;

(************************************************************************)

PROCEDURE FindTopmostIndividuals (isolated: BOOLEAN);

    (* Lists those individuals who have no ancestor.  If isolated=TRUE  *)
    (* then we confine our attention to those individuals who have no   *)
    (* relatives at all, and if we find them we report them and then    *)
    (* set their 'live' flag to FALSE.  The case isolated=FALSE is      *)
    (* intended to be for a second pass where we are looking for the    *)
    (* tree roots, i.e. the oldest individuals.                         *)

    (********************************************************************)

    VAR charcount: CARDINAL;

    PROCEDURE AddString (str: ARRAY OF CHAR);

        (* Writes str to standard output, preceded by the 'pad' string, *)
        (* and starting a new line if necessary.                        *)

        CONST LineLimit = 80;  pad = "   ";

        BEGIN
            IF charcount + LENGTH(pad) + LENGTH(str) >= LineLimit THEN
                WriteLn;
                charcount := 0;
            END (*IF*);
            WriteString (pad);  INC(charcount, LENGTH(pad));
            WriteString (str);  INC(charcount, LENGTH(str));
        END AddString;

    (********************************************************************)

    VAR j: NodeIndex;  par: NodeCount;  count: CARDINAL;

    BEGIN
        IF isolated THEN
            WriteString ("People with no relatives at all:");
        ELSE
            WriteString ("Topmost individuals, i.e. people with no recorded ancestors:");
        END (*IF*);
        WriteLn;
        count := 0;  charcount := 0;
        FOR j := 1 TO NumberOfNodes DO
            IF Node[j].live AND NOT Node[j].isfamily THEN
                par := Node[j].parents;
                IF (par = 0) OR NoParents(par) THEN
                    IF isolated THEN
                        IF (NumberOfChildren(j) = 0)
                                   AND (NumberOfSpice(j) = 0) THEN

                            (* Note: someone without parents recorded   *)
                            (* could still have siblings.               *)

                            Node[j].live := (par <> 0)
                                          AND (NumberOfChildren(par) > 1);

                        END (*IF*);
                    END (*IF*);
                    IF NOT isolated OR NOT Node[j].live THEN
                        INC (count);
                        AddString (Node[j].ID);
                    END (*IF*);
                END (*IF*);
            END (*IF*);
        END (*FOR*);
        IF count = 0 THEN
            AddString ("(none)");
        END (*IF*);
        WriteLn;
    END FindTopmostIndividuals;

(************************************************************************)
(*                         CONSISTENCY CHECKS                           *)
(************************************************************************)

PROCEDURE HasFAMSRecord (person, family: NodeIndex): BOOLEAN;

    (* Returns TRUE iff this person includes the given family in        *)
    (* his/her spice list.                                              *)

    VAR p: ListOfFamilies;

    BEGIN
        p := Node[person].spice;
        WHILE (p <> NIL) AND (p^.this <> family) DO
            p := p^.next;
        END (*WHILE*);
        RETURN p <> NIL;
    END HasFAMSRecord;

(************************************************************************)

PROCEDURE IsAChild (person, family: NodeIndex): BOOLEAN;

    (* Returns TRUE iff this person is in the list of children for      *)
    (* the given family.                                                *)

    VAR p: ListOfPeople;

    BEGIN
        p := Node[family].children;
        WHILE (p <> NIL) AND (p^.this <> person) DO
            p := p^.next;
        END (*WHILE*);
        RETURN p <> NIL;
    END IsAChild;

(************************************************************************)

PROCEDURE CheckFamilyConsistency;

    (* Checks that (a) if a person is a spouse in a family, then the    *)
    (* family record shows that person as a husband or wife; and (b) if *)
    (* a person is a child of a family, then the family record includes *)
    (* that individual as a child.  Note that each check must be done   *)
    (* twice, from the viewpoint of the family and from the viewpoint   *)
    (* of the individual.                                               *)

    (* Remark: the Pass 2 processing has already picked up errors of    *)
    (* the form "family does not exist" or "person does not exist", but *)
    (* now we're looking for more subtle errors.                        *)

    VAR j, k: NodeCount;  p: ListOfPeople;  q: ListOfFamilies;

    BEGIN
        FOR j := 1 TO NumberOfNodes DO
            IF Node[j].live THEN
                IF Node[j].isfamily THEN

                    (* We're looking at a family.  First check the      *)
                    (* husband and wife.                                *)

                    k := Node[j].husband;
                    IF (k <> 0) AND NOT HasFAMSRecord (k, j) THEN
                        WriteString ("Family ");
                        WriteString (Node[j].ID);
                        WriteString (" has person ");
                        WriteString (Node[k].ID);
                        WriteString (" listed as a husband, but that person");
                        WriteLn;
                        WriteString ("   does not have a corresponding FAMS record.");
                        WriteLn;
                    END (*IF*);

                    k := Node[j].wife;
                    IF (k <> 0) AND NOT HasFAMSRecord (k, j) THEN
                        WriteString ("Family ");
                        WriteString (Node[j].ID);
                        WriteString (" has person ");
                        WriteString (Node[k].ID);
                        WriteString (" listed as a wife, but that person");
                        WriteLn;
                        WriteString ("   does not have a corresponding FAMS record.");
                        WriteLn;
                    END (*IF*);

                    (* Now for the children. *)

                    p := Node[j].children;
                    WHILE p <> NIL DO
                        k := p^.this;
                        IF (k <> 0) AND (Node[k].parents <> j) THEN
                            WriteString ("Family ");
                            WriteString (Node[j].ID);
                            WriteString (" has person ");
                            WriteString (Node[k].ID);
                            WriteString (" listed as a child, but that person");
                            WriteLn;
                            WriteString ("   does not have a corresponding FAMC record.");
                            WriteLn;
                        END (*IF*);
                        p := p^.next;
                    END (*WHILE*);

                ELSE
                    (* We're looking at an individual.  See whether     *)
                    (* his parents claim him as a child.                *)

                    k := Node[j].parents;
                    IF (k <> 0) AND NOT IsAChild (j, k) THEN
                        WriteString ("Person ");
                        WriteString (Node[j].ID);
                        WriteString (" is a child of family ");
                        WriteString (Node[k].ID);
                        WriteString (" but that family");
                        WriteLn;
                        WriteString ("   does not have a corresponding CHIL record.");
                        WriteLn;
                    END (*IF*);

                    (* Next, check for consistency of each marriage in which    *)
                    (* this individual was supposedly a partner.                *)

                    q := Node[j].spice;
                    WHILE q <> NIL DO
                        k := q^.this;
                        IF (k <> 0) AND (Node[k].husband <> j)
                                    AND (Node[k].wife <> j) THEN
                            WriteString ("Person ");
                            WriteString (Node[j].ID);
                            WriteString (" appears to be a partner of family ");
                            WriteString (Node[k].ID);
                            WriteString (" but that family");
                            WriteLn;
                            WriteString ("   does not list that person as a husband or wife.");
                            WriteLn;
                        END (*IF*);
                        q := q^.next;
                    END (*WHILE*);

                END (*IF*);
            END (*IF*);
        END (*FOR*);
    END CheckFamilyConsistency;

(************************************************************************)
(*                  LOOKING FOR LOOPS IN THE GRAPH                      *)
(************************************************************************)

PROCEDURE RemoveDescendants (N: NodeIndex;  VAR (*INOUT*) FromSet: Set;
                             VAR (*INOUT*) suspect: Set;
                             VAR (*INOUT*) SetSize: NodeCount): BOOLEAN;

    (* Removes the individual at Node[N] and his/her descendants from   *)
    (* FromSet.  We assume that the original individual is initially    *)
    (* known to be in FromSet, and that SetSize is an accurate count of *)
    (* the remaining elements of the set.  If the search path length is *)
    (* greater than SetSize or if we encounter node N in the path then  *)
    (* we deduce that a loop has been found; in this case we add the    *)
    (* nodes involved to the set 'suspect', and return FALSE.           *)

    VAR p: ListOfFamilies;  q: ListOfPeople;
        family: NodeCount;  child: NodeCount;
        success: BOOLEAN;

    BEGIN
        success := TRUE;
        p := Node[N].spice;
        WHILE p <> NIL DO
            family := p^.this;
            IF family <> 0 THEN
                q := Node[family].children;
                WHILE q <> NIL DO
                    child := q^.this;
                    IF child = N THEN
                        success := FALSE;
                    ELSIF (child <> 0) AND IsMember(child, FromSet) THEN
                        IF (SetSize = 0)
                                OR NOT RemoveDescendants(child, FromSet,
                                                suspect, SetSize) THEN
                            success := FALSE;
                            Include (child, suspect);
                        END (*IF*);
                    END (*IF*);
                    q := q^.next;
                END (*WHILE*);
            END (*IF*);
            p := p^.next;
        END (*WHILE*);
        Exclude (N, FromSet);
        IF SetSize > 0 THEN
            DEC (SetSize);
        END (*IF*);
        RETURN success;
    END RemoveDescendants;

(************************************************************************)

PROCEDURE LookForLoops;

    (* Searches for loops in the data structure. *)

    (********************************************************************)

    VAR charcount: CARDINAL;

    PROCEDURE AddString (str: ARRAY OF CHAR);

        (* Writes str to standard output, preceded by the 'pad' string, *)
        (* and starting a new line if necessary.                        *)

        CONST LineLimit = 80;  pad = "   ";

        BEGIN
            IF charcount + LENGTH(pad) + LENGTH(str) >= LineLimit THEN
                WriteLn;
                charcount := 0;
            END (*IF*);
            WriteString (pad);  INC(charcount, LENGTH(pad));
            WriteString (str);  INC(charcount, LENGTH(str));
        END AddString;

    (********************************************************************)

    VAR val: CARDINAL;  LiveIndividuals: NodeCount;
        unchecked, suspect: Set;

    BEGIN
        (* Start by putting all the live non-family nodes into the      *)
        (* unchecked set.                                               *)

        LiveIndividuals := 0;
        unchecked := CreateSet (NumberOfNodes);
        suspect := CreateSet (NumberOfNodes);
        FOR val := 1 TO NumberOfNodes DO
            IF Node[val].live AND NOT Node[val].isfamily THEN
                Include (val, unchecked);
                INC (LiveIndividuals);
            END (*IF*);
        END (*FOR*);

        (* Each time around the next loop, we pick an arbitrary         *)
        (* unchecked node, and use it as a starting point.              *)

        WHILE TakeFrom (val, unchecked) DO
            Include (val, unchecked);
            IF RemoveDescendants (val, unchecked, suspect,
                                                LiveIndividuals) THEN
            ELSE
                Include (val, suspect);
            END (*IF*);
        END (*WHILE*);

        (* Report our results. *)

        IF IsEmpty (suspect) THEN
            WriteString ("Structure appears to be free of loops.");
        ELSE
            WriteString ("There are one or more loops (where people are their own ancestors)");
            WriteLn;
            WriteString ("involving the following individuals.");
            WriteLn;
            charcount := 0;
            WHILE TakeFrom (val, suspect) DO
                AddString (Node[val].ID);
            END (*WHILE*);
        END (*IF*);
        WriteLn;

        DiscardSet (unchecked);
        DiscardSet (suspect);

    END LookForLoops;

(************************************************************************)
(*            THE COMPLETE SET OF TESTS THAT FOLLOW PASS 2              *)
(************************************************************************)

PROCEDURE AnalyseGraph;

    (* To be called only after the graph is fully built.  Does some     *)
    (* checks on its structure.                                         *)

    BEGIN
        DetectEmptyFamilies;
        CheckFamilyConsistency;
        FindTopmostIndividuals (TRUE);
        FindTopmostIndividuals (FALSE);
        LookForLoops;
    END AnalyseGraph;

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
        (* Process the FAMC records (should be only one). *)

        IF GetField ('FAMC', T, datum) THEN
            ExtractTag (datum, ID);
            code := IDToNode(ID);
            Node[index].parents := code;
            IF code = 0 THEN
                IF External(ID) THEN
                    WriteString ("External link ");
                    WriteString (ID);
                    WriteString (" from ");
                    WriteString (Node[index].ID);
                ELSE
                    WriteString ("Person ");
                    WriteString (Node[index].ID);
                    WriteString (" is a child of family ");
                    WriteString (ID);
                    WriteLn;
                    WriteString ("  but that family is not in the database.");
                END (*IF*);
                WriteLn;
            ELSIF NOT Node[code].isfamily THEN
                WriteString ("Person ");
                WriteString (Node[index].ID);
                WriteString (" has a FAMC record with reference ");
                WriteString (ID);
                WriteLn;
                WriteString ("  but that reference is to an individual, not a family.");
                WriteLn;
            END (*IF*);
        END (*IF*);

        (* Any remaining FAMC record is an error. *)

        IF GetField ('FAMC', T, datum) THEN
            WriteString ("Person ");
            WriteString (Node[index].ID);
            WriteString (" belongs to more than one family.");
            WriteLn;
        END (*IF*);

        (* Now handle the FAMS records (there could be more than one).  *)

        WHILE GetField ('FAMS', T, datum) DO
            ExtractTag (datum, ID);
            code := IDToNode(ID);
            IF code = 0 THEN
                IF External(ID) THEN
                    WriteString ("External link ");
                    WriteString (ID);
                    WriteString (" from ");
                    WriteString (Node[index].ID);
                ELSE
                    WriteString ("Person ");
                    WriteString (Node[index].ID);
                    WriteString (" is a spouse of family ");
                    WriteString (ID);
                    WriteLn;
                    WriteString ("  but that family is not in the database.");
                END (*IF*);
                WriteLn;
            ELSIF NOT Node[code].isfamily THEN
                WriteString ("Person ");
                WriteString (Node[index].ID);
                WriteString (" has a FAMS record with reference ");
                WriteString (ID);
                WriteLn;
                WriteString ("  but that reference is to an individual, not a family.");
                WriteLn;
            END (*IF*);
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

    VAR string: DataString;  ID: IDString;  code: NodeCount;
        childptr: ListOfPeople;

    BEGIN
        (* Husband. *)

        IF GetField ('HUSB', T, string) THEN
            ExtractTag (string, ID);
            code := IDToNode(ID);
            Node[index].husband := code;
            IF code = 0 THEN
                IF External(ID) THEN
                    WriteString ("External link ");
                    WriteString (ID);
                    WriteString (" from ");
                    WriteString (Node[index].ID);
                ELSE
                    WriteString ("Family ");
                    WriteString (Node[index].ID);
                    WriteString (" includes as husband the person ");
                    WriteString (ID);
                    WriteLn;
                    WriteString ("  but that person is not in the database.");
                END (*IF*);
                WriteLn;
            ELSIF Node[code].isfamily THEN
                WriteString ("Family ");
                WriteString (Node[index].ID);
                WriteString (" has a HUSB record with reference ");
                WriteString (ID);
                WriteLn;
                WriteString ("  but that reference is to an family, not an individual.");
                WriteLn;
            ELSIF Node[code].sex = 'F' THEN
                WriteString ("Family ");
                WriteString (Node[index].ID);
                WriteString (" has a female husband.");
                WriteLn;
            END (*IF*);
        END (*IF*);

        (* Wife *)

        IF GetField ('WIFE', T, string) THEN
            ExtractTag (string, ID);
            code := IDToNode(ID);
            Node[index].wife := code;
            IF code = 0 THEN
                IF External(ID) THEN
                    WriteString ("External link ");
                    WriteString (ID);
                    WriteString (" from ");
                    WriteString (Node[index].ID);
                ELSE
                    WriteString ("Family ");
                    WriteString (Node[index].ID);
                    WriteString (" includes as wife the person ");
                    WriteString (ID);
                    WriteLn;
                    WriteString ("  but that person is not in the database.");
                END (*IF*);
                WriteLn;
            ELSIF Node[code].isfamily THEN
                WriteString ("Family ");
                WriteString (Node[index].ID);
                WriteString (" has a WIFE record with reference ");
                WriteString (ID);
                WriteLn;
                WriteString ("  but that reference is to a family, not an individual.");
                WriteLn;
            ELSIF Node[code].sex = 'M' THEN
                WriteString ("Family ");
                WriteString (Node[index].ID);
                WriteString (" has a male wife.");
                WriteLn;
            END (*IF*);
        END (*IF*);

        (* Children *)

        WHILE GetField ('CHIL', T, string) DO
            ExtractTag (string, ID);
            code := IDToNode(ID);
            IF code = 0 THEN
                IF External(ID) THEN
                    WriteString ("External link ");
                    WriteString (ID);
                    WriteString (" from ");
                    WriteString (Node[index].ID);
                ELSE
                    WriteString ("Person ");
                    WriteString (Node[index].ID);
                    WriteString (" is a child of family ");
                    WriteString (ID);
                    WriteLn;
                    WriteString ("  but that family is not in the database.");
                END (*IF*);
                WriteLn;
            ELSIF Node[code].isfamily THEN
                WriteString ("Person ");
                WriteString (Node[index].ID);
                WriteString (" has a CHIL record with reference ");
                WriteString (ID);
                WriteLn;
                WriteString ("  but that reference is to a family, not an individual.");
                WriteLn;
            END (*IF*);
            NEW (childptr);
            childptr^.this := code;
            childptr^.next := Node[index].children;
            Node[index].children := childptr;
        END (*WHILE*);

    END GetFamilyInfoPass2;

(************************************************************************)
(*                          CREATE THE GRAPH                            *)
(************************************************************************)

PROCEDURE ClearNode (index: NodeIndex);

    (* Clean initialisation of the data in Node[index] - except for the *)
    (* live and ID and isfamily fields, which we assume were already    *)
    (* filled in during Pass 1.                                         *)

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
        END (*WITH*);
    END ClearNode;

(************************************************************************)

PROCEDURE Pass1 (file: FilenameString): BOOLEAN;

    (* Reads the entire file, and creates the mapping between indexes   *)
    (* and identifiers for all individuals and families.  We also       *)
    (* record the sex of individuals.                                   *)

    VAR TB: Buffer;
        NextLevel, NumberOfPeople, NumberOfFamilies, dupcount: CARDINAL;
        NextLine: Line;
        RecTree: RecordTree;
        success, done, family: BOOLEAN;
        thisID, ID1, ID2: IDString;
        data: DataString;
        j, k: NodeCount;
        code: HashCode;

    BEGIN
        TB := OpenForReading (file, TRUE);
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
                            live := TRUE;
                            isfamily := family;
                            ID := thisID;
                            sex := ' ';
                            IF NOT family THEN
                                DiscardTopLevel (RecTree);
                                IF GetField ('SEX', RecTree, data) THEN
                                    sex := data[0];
                                END (*IF*);
                            END (*IF*);
                        END (*WITH*);
                        ClearNode (NumberOfNodes);
                    END (*IF*);

                END (*IF*);
                DiscardTree (RecTree);
            END (*IF*);
        UNTIL done;

        CloseTB (TB);

        WriteString ("Database contains ");
        WriteCard (NumberOfPeople);
        WriteString (" people, and ");
        WriteCard (NumberOfFamilies);
        WriteString (" families.");
        WriteLn;

        IF success AND (NumberOfNodes > 1) THEN

            (* Check for duplicate tags.  Duplicates will       *)
            (* necessarily have the same hash codes.            *)

            WriteString ("Duplicate tags:");
            dupcount := 0;
            FOR code := 0 TO HashTableSize-1 DO
                j := HashTable[code];
                WHILE j <> 0 DO
                    ID1 := Node[j].ID;
                    Strings.Capitalize (ID1);
                    k := Node[j].nexthash;
                    WHILE k <> 0 DO
                        ID2 := Node[k].ID;
                        Strings.Capitalize (ID2);
                        IF Strings.Equal (ID1, ID2) THEN
                            INC (dupcount);
                            WriteLn;
                            WriteString ("      ");
                            WriteString (ID2);
                            WriteString ("   (records ");
                            WriteCard (j);
                            WriteString (" and ");
                            WriteCard (k);
                            WriteString (")");
                        END (*IF*);
                        k := Node[k].nexthash;
                    END (*WHILE*);
                    j := Node[j].nexthash;
                END (*WHILE*);
            END (*FOR*);
            IF dupcount = 0 THEN
                WriteString (" none");
            ELSE
                success := FALSE;
            END (*IF*);
            WriteLn;

        END (*IF*);

        IF NOT success THEN
            WriteString ("Please correct the error(s) and then run");
            WriteString (" this program again.");
            WriteLn;
        END (*IF*);

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
        WriteString ("Starting Pass 2");  WriteLn;
        TB := OpenForReading (file, TRUE);
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
            AnalyseGraph;
        ELSE
            WriteString ("Pass 2 skipped because of fatal errors.");
            WriteLn;
        END (*IF*);
        WriteString ("Finished.");
        WriteLn;
    END CreateGraph;

(************************************************************************)
(*                           MAIN PROGRAM                               *)
(************************************************************************)

PROCEDURE DoTheJob;

    (* Analysis of one database. *)

    VAR DatabaseName, MainFile: FilenameString;
        found: BOOLEAN;  pos: CARDINAL;

    BEGIN
        GetArgument (DatabaseName);
        IF DatabaseName[0] = Nul THEN
            WriteLn;
            WriteString ("Usage: Lint databasename");
            WriteLn;
            WriteLn;
            WriteString ("This will check a GEDCOM file for possible problems.");
            WriteLn;
            RETURN;
        END (*IF*);

        MainFile := DatabaseName;
        Strings.FindPrev ('.', MainFile, LENGTH(MainFile)-1, found, pos);
        IF NOT found THEN
            Strings.Append (".GED", MainFile);
        END (*IF*);
        CreateGraph (MainFile);

    END DoTheJob;

(************************************************************************)

BEGIN
    DoTheJob;
END Lint.

