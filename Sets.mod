IMPLEMENTATION MODULE Sets;

        (********************************************************)
        (*                                                      *)
        (*      Implementation of SET OF CARDINAL[0..max]       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            04 December 2001                *)
        (*  Last edited:        26 June 2002                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  SET32, CARD32,
    (* proc *)  CAST;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

<* M2EXTENSIONS+ *>
<* STORAGE+ *>

    (* To make the implementation easier, I'm using the 'dynamic array' *)
    (* language extension.                                              *)

TYPE Set = POINTER TO
               RECORD
                   max:  CARDINAL;
                   pval: POINTER TO ARRAY OF SET32;
               END (*RECORD*);

(************************************************************************)

PROCEDURE CreateSet (max: CARDINAL): Set;

    (* Creates a set of [0..max], initially empty. *)

    VAR result: Set;  j, top: CARDINAL;

    BEGIN
        NEW (result);
        result^.max := max;
        top := max DIV 32;
        NEW (result^.pval, top + 1);
        FOR j := 0 TO top DO
            result^.pval^[j] := SET32{};
        END (*FOR*);
        RETURN result;
    END CreateSet;

(************************************************************************)

PROCEDURE DiscardSet (VAR (*INOUT*) S: Set);

    (* Destroys a set. *)

    BEGIN
        DISPOSE (S^.pval);
        DISPOSE (S);
    END DiscardSet;

(************************************************************************)

PROCEDURE IsEmpty (S: Set): BOOLEAN;

    (* Returns TRUE iff this is an empty set. *)

    VAR j: CARDINAL;

    BEGIN
        IF (S = NIL) OR (S^.pval = NIL) THEN
            RETURN TRUE;
        ELSE
            j := 0;
            LOOP
                IF j > HIGH(S^.pval^) THEN
                    RETURN TRUE;
                ELSIF CAST(CARD32, S^.pval^[j]) <> 0 THEN
                    RETURN FALSE;
                ELSE
                    INC (j);
                END (*IF*);
            END (*LOOP*);
        END (*IF*);
    END IsEmpty;

(************************************************************************)

PROCEDURE Include (val: CARDINAL;  VAR (*INOUT*) S: Set);

    (* Adds val to S. *)

    BEGIN
        INCL (S^.pval^[val DIV 32], val MOD 32);
    END Include;

(************************************************************************)

PROCEDURE Exclude (val: CARDINAL;  VAR (*INOUT*) S: Set);

    (* Removes val from S. *)

    BEGIN
        EXCL (S^.pval^[val DIV 32], val MOD 32);
    END Exclude;

(************************************************************************)

PROCEDURE IsMember (val: CARDINAL;  S: Set): BOOLEAN;

    (* Returns TRUE iff val is in set S. *)

    BEGIN
        IF val > S^.max THEN RETURN FALSE
        ELSE RETURN (val MOD 32) IN S^.pval^[val DIV 32];
        END (*IF*);
    END IsMember;

(************************************************************************)

PROCEDURE TakeFrom (VAR (*OUT*) val: CARDINAL;
                    VAR (*INOUT*) S: Set): BOOLEAN;

    (* Removes one element from the set, and returns it as val. *)
    (* Returns FALSE if the set was empty.                      *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        WHILE (j <= S^.max DIV 32) AND (CAST(CARD32, S^.pval^[j]) = 0) DO
            INC (j);
        END (*WHILE*);
        IF j > S^.max DIV 32 THEN
            RETURN FALSE;
        END (*IF*);
        val := 0;
        WHILE NOT (val IN S^.pval^[j]) DO
            INC (val);
        END (*WHILE*);
        EXCL (S^.pval^[j], val);
        INC (val, 32*j);
        RETURN TRUE;
    END TakeFrom;

(************************************************************************)

END Sets.

