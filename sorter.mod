IMPLEMENTATION MODULE Sorter;

        (********************************************************)
        (*                                                      *)
        (*     In-memory sort of an array of GEDCOM records     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            13 July 2001                    *)
        (*  Last edited:        13 January 2003                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM OurTypes IMPORT
    (* type *)  Line;

FROM Records IMPORT
    (* type *)  RecordTree,
    (* proc *)  GetName;

(************************************************************************)

PROCEDURE Compare (TA, TB: RecordTree): INTEGER;

    (* Returns <0 if TA<TB, =0 if TA=TB, >0 if TA>TB.                   *)
    (* Convention: a non-NIL argument will always be taken to be        *)
    (* smaller than a NIL argument.  On the other hand, a blank         *)
    (* surname is, by convention, greater than any nonblank one.        *)

    CONST Nul = CHR(0);

    VAR val: Strings.CompareResults;
        surname1, surname2, wholename1, wholename2: Line;

    BEGIN
        IF GetName (TA, surname1, wholename1) THEN
            IF GetName (TB, surname2, wholename2) THEN
                Strings.Capitalize (surname1);
                Strings.Capitalize (surname2);

                (* Sort on surname first.  (Special case: a blank       *)
                (* surname is treated as if greater than any nonblank   *)
                (* one.)  If surnames equal, sort on whole name.        *)

                IF surname1[0] = Nul THEN
                    IF surname2[0] = Nul THEN
                        val := Strings.equal;
                    ELSE
                        val := Strings.greater;
                    END (*IF*);
                ELSIF surname2[0] = Nul THEN
                    val := Strings.less;
                ELSE
                    val := Strings.Compare (surname1, surname2);
                END (*IF*);
                IF val = Strings.less THEN RETURN -1
                ELSIF val = Strings.equal THEN
                    Strings.Capitalize (wholename1);
                    Strings.Capitalize (wholename2);
                    val := Strings.Compare (wholename1, wholename2);
                    IF val = Strings.less THEN RETURN -1
                    ELSIF val = Strings.equal THEN
                        RETURN 0;
                    ELSE RETURN +1
                    END (*IF*);
                ELSE RETURN +1
                END (*IF*);

            ELSE

                (* TA not NIL, TB is NIL. *)

                RETURN -1;

            END (*IF*);

        ELSE

            (* TA is NIL. *)

            IF GetName (TB, surname2, wholename2) THEN
                RETURN +1;
            ELSE
                RETURN 0;
            END (*IF*);

        END (*IF*);

    END Compare;

(************************************************************************)

PROCEDURE RippleSort (VAR (*INOUT*) A: ARRAY OF RecordTree;  amount: CARDINAL);

    (* Sorts an array of 'amount' records. *)

    VAR NoChange: BOOLEAN;  j: CARDINAL;  temp: RecordTree;

    BEGIN
        IF amount > 1 THEN
            REPEAT
                NoChange := TRUE;
                FOR j := 0 TO amount-2 DO
                    IF Compare (A[j], A[j+1]) > 0 THEN
                        temp := A[j];  A[j] := A[j+1];
                        A[j+1] := temp;  NoChange := FALSE;
                    END (*IF*);
                END (*IF*);
            UNTIL NoChange;
        END (*IF*);
    END RippleSort;

(************************************************************************)

PROCEDURE SortArray (VAR (*INOUT*) A: ARRAY OF RecordTree;  amount: CARDINAL);

    (* Sorts an array of 'amount' records. *)

    BEGIN
        (* For now, use a ripple sort. *)
        RippleSort (A, amount);
    END SortArray;

(************************************************************************)

END Sorter.

