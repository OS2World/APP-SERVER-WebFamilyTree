IMPLEMENTATION MODULE WftClock;

        (********************************************************)
        (*                                                      *)
        (*       Conversion of date and time to string          *)
        (*                                                      *)
        (*    This is actually a modified version of my module  *)
        (*    MyClock, stripped down to include only the code   *)
        (*    needed for WFT.                                   *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            14 May 2007                     *)
        (*  Last edited:        18 May 2007                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM SysClock IMPORT
    (* type *)  DateTime,
    (* proc *)  GetClock;

(************************************************************************)

CONST Nul = CHR(0);

TYPE
    MonthNameType = ARRAY [0..15] OF ARRAY [0..2] OF CHAR;

CONST
    MonthName = MonthNameType {'M00', 'Jan', 'Feb', 'Mar', 'Apr', 'May',
                               'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov',
                               'Dec', 'M13', 'M14', 'M15'};

(************************************************************************)
(*                         TIME ZONE CORRECTION                         *)
(************************************************************************)

PROCEDURE AdjustTime (VAR (*INOUT*) date: DateTime;  addminutes: INTEGER);

    (* Adds the given number of minutes to date. *)

    TYPE MonthArray = ARRAY [1..12] OF CARDINAL;
    CONST DaysInMonth = MonthArray {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

    (********************************************************************)

    PROCEDURE IsLeapYear (year: CARDINAL): BOOLEAN;

        (* Returns TRUE iff this year is a leap year.  The result is *)
        (* good up to 2099.                                          *)

        BEGIN
            RETURN (year MOD 4) = 0;
        END IsLeapYear;

    (********************************************************************)

    PROCEDURE IncDay;

        (* Adds one day to date. *)

        BEGIN
            IF (date.day < DaysInMonth[date.month])
                       OR ((date.month = 2) AND IsLeapYear(date.year)) THEN
                INC (date.day);
            ELSE
                date.day := 1;
                IF date.month = 12 THEN
                    date.month := 1;  INC(date.year);
                ELSE
                    INC (date.month);
                END (*IF*);
            END (*IF*);
        END IncDay;

    (********************************************************************)

    PROCEDURE DecDay;

        (* Subtracts one day from date. *)

        BEGIN
            IF date.day > 1 THEN
                DEC (date.day);
            ELSE
                IF date.month > 1 THEN
                    DEC (date.month);
                ELSE
                    date.month := 12;  DEC (date.year);
                END (*IF*);
                date.day := DaysInMonth[date.month];
                IF (date.month = 2) AND IsLeapYear (date.year) THEN
                    INC (date.day);
                END (*IF*);
            END (*IF*);
        END DecDay;

    (********************************************************************)

    PROCEDURE AddHr (amount: CARDINAL);

        (* Increments date.hour by amount. *)

        BEGIN
            INC (amount, date.hour);
            WHILE amount >= 24 DO
                IncDay;
                DEC (amount, 24);
            END (*WHILE*);
            date.hour := amount;
        END AddHr;

    (********************************************************************)

    PROCEDURE SubHr (amount: CARDINAL);

        (* Decrements date.hour by amount. *)

        BEGIN
            WHILE amount >= 24 DO
                DecDay;
                DEC (amount, 24);
            END (*WHILE*);
            IF amount > 0 THEN
                IF amount <= date.hour THEN
                    DEC (date.hour, amount);
                ELSE
                    INC (date.hour, 24 - amount);
                    DecDay;
                END (*IF*);
            END (*IF*);
        END SubHr;

    (********************************************************************)

    VAR adjust: CARDINAL;

    BEGIN
        adjust := ABS (addminutes);            (* correction in minutes *)
        IF addminutes < 0 THEN
            SubHr (adjust DIV 60);
            adjust := adjust MOD 60;
            IF adjust <> 0 THEN
                IF adjust <= date.minute THEN
                    DEC (date.minute, adjust);
                ELSE
                    INC (date.minute, 60 - adjust);
                    SubHr (1);
                END (*IF*);
            END (*IF*);
        ELSIF addminutes > 0 THEN
            AddHr (adjust DIV 60);
            adjust := adjust MOD 60 + date.minute;
            IF adjust < 60 THEN
                date.minute := adjust;
            ELSE
                date.minute := adjust - 60;
                AddHr (1);
            END (*IF*);
        END (*IF*);
    END AdjustTime;

(************************************************************************)
(*                TWO-DIGIT NUMBERS TO CHARACTER STRING                 *)
(************************************************************************)

PROCEDURE Convert2 (value: CARDINAL;  VAR (*INOUT*) result: ARRAY OF CHAR;
                                      VAR (*INOUT*) j: CARDINAL);

    (* Puts a 2-digit number at result[j], updates j. *)

    (********************************************************************)

    PROCEDURE Convert1 (value: CARDINAL);

        (* Puts a 1-digit number at result[j], updates j. *)

        BEGIN
            result[j] := CHR(value + ORD('0'));  INC(j);
        END Convert1;

    (********************************************************************)

    BEGIN
        Convert1 (value DIV 10);  Convert1 (value MOD 10);
    END Convert2;

(************************************************************************)
(*                   CALCULATING THE DAY OF THE WEEK                    *)
(************************************************************************)

(* For reasons that I will probably never understand, the HTTP standard *)
(* requires date/time strings in headers to include the day of the      *)
(* week, even though it's a messy calculation and of no conceivable use *)
(* to the receiver of this information. The following section of code   *)
(* works out a day code where Sunday=0, Monday=1, etc.                  *)

TYPE
    DayOfWeek = [0..6];       (* 0 = Sunday *)
    DayNameList = ARRAY DayOfWeek OF ARRAY [0..2] OF CHAR;
    MonthData = ARRAY [1..13] OF CARDINAL;

CONST
    (* Days since beginning of year, for the 1st of each month.  In a   *)
    (* leap year you need an extra correction.                          *)

    FirstDayInMonth = MonthData {  0,  31,  59,  90, 120, 151,
                                 181, 212, 243, 273, 304, 334, 365};

    DayName = DayNameList {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};

(************************************************************************)

PROCEDURE DayOfWeekFor (VAR (*IN*) DT: DateTime): DayOfWeek;

    (* Calculates the weekday.  *)

    CONST BaseDay = 6;

    VAR year, dayofyear: CARDINAL;  FirstDayOfYear: DayOfWeek;
        LeapYear: BOOLEAN;

    BEGIN
        year := DT.year;
        WITH DT DO
            dayofyear := FirstDayInMonth[month] + day - 1;
            LeapYear := (year MOD 4) = 0;
            IF LeapYear AND (month > 2) THEN
                INC (dayofyear);
            END (*IF*);
        END (*WITH*);

        (* Every group of four years has 4*365+1 = 1461 days, and       *)
        (* 1461 MOD 7 = 5.  This means that the DayOfWeek changes by    *)
        (* 5 days per 4 years.                                          *)

        FirstDayOfYear := (BaseDay + 5*(year DIV 4)) MOD 7;

        (* Thereafter, it changes by 2 days in the first year, and one  *)
        (* day per year after that.                                     *)

        IF NOT LeapYear THEN
            FirstDayOfYear := (FirstDayOfYear + (year MOD 4) + 1) MOD 7;
        END (*IF*);

        RETURN (FirstDayOfYear + dayofyear - 1) MOD 7;

    END DayOfWeekFor;

(************************************************************************)

PROCEDURE FiledateToDateTime (date, time: CARDINAL;
                              VAR (*OUT*) result: DateTime);

    (* Converts a packed date and time to SysClock.DateTime format.  *)

    VAR temp: CARDINAL;

    BEGIN
        result.day := date MOD 32;
        temp := date DIV 32;
        result.year := temp DIV 16 + 1980;
        result.month := temp MOD 16;

        result.second := 2*(time MOD 32);
        temp := time DIV 32;
        result.hour := temp DIV 64;
        result.minute := temp MOD 64;
    END FiledateToDateTime;

(************************************************************************)

PROCEDURE AppendDateTime (date, time: CARDINAL;
                            VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Converts a date and time from the file system format to the GMT  *)
    (* string required by HTTP 1.1.  The format is                      *)
    (* "Mon, 29 Jun 1998 02:28:12 GMT", with all fields fixed length.   *)
    (* This string, without the quotes, is appended to result.          *)

    VAR j: CARDINAL;  k: [0..2];
        DT, Now: DateTime;

    BEGIN
        (* Convert date and time to SysClock.DateTime format, and then  *)
        (* convert it to GMT.                                           *)

        FiledateToDateTime (date, time, DT);
        GetClock (Now);
        AdjustTime (DT, Now.zone);

        (* Find the day of week, and append it to result. *)

        Strings.Append (DayName[DayOfWeekFor(DT)], result);
        Strings.Append (", ", result);

        (* Now append the date. *)

        j := Strings.Length (result);
        Convert2 (DT.day, result, j);  result[j] := ' ';  INC(j);
        FOR k := 0 TO 2 DO
            result[j] := MonthName[DT.month][k];  INC(j);
        END (*FOR*);
        result[j] := ' ';  INC(j);
        Convert2 (DT.year DIV 100, result, j);
        Convert2 (DT.year MOD 100, result, j);
        result[j] := ' ';  INC(j);

        (* Next, the time of day. *)

        Convert2 (DT.hour, result, j);  result[j] := ':';  INC(j);
        Convert2 (DT.minute, result, j);  result[j] := ':';  INC(j);
        Convert2 (DT.second, result, j);

        (* To finish off, add the fixed string "GMT". *)

        result[j] := Nul;
        Strings.Append (" GMT", result);

    END AppendDateTime;

(************************************************************************)

PROCEDURE ConvertDateTime (date, time: CARDINAL;
                            VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts a date and time from the file system format to the GMT  *)
    (* string required by HTTP 1.1.  The format is                      *)
    (* "Mon, 29 Jun 1998 02:28:12 GMT", with all fields fixed length.   *)

    BEGIN
        Strings.Assign ("", result);
        AppendDateTime (date, time, result);
    END ConvertDateTime;

(************************************************************************)

END WftClock.

