IMPLEMENTATION MODULE ProgName;

    (****************************************************************)
    (*                                                              *)
    (*       A module that lets low-level modules obtain            *)
    (*       application-specific information.  We achieve          *)
    (*       this by putting this module in a directory             *)
    (*       reserved for application-specific source files,        *)
    (*       while the library modules still live at                *)
    (*       "library" level of the source structure.               *)
    (*                                                              *)
    (*    Last edited:    24 December 2013                          *)
    (*    Status:         OK                                        *)
    (*                                                              *)
    (****************************************************************)


IMPORT Exceptq, Strings;

(************************************************************************)

PROCEDURE GetProgramName (VAR (*OUT*) name: ARRAY OF CHAR);

    (* Returns a name and version string. *)

    BEGIN
        Strings.Assign ("WFT", name);
    END GetProgramName;

(************************************************************************)

END ProgName.

