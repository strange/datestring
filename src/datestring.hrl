-define(MON, "MON").
-define(TUE, "TUE").
-define(WED, "WED").
-define(THU, "THU").
-define(FRI, "FRI").
-define(SAT, "SAT").
-define(SUN, "SUN").

-define(Mon, "Mon").
-define(Tue, "Tue").
-define(Wed, "Wed").
-define(Thu, "Thu").
-define(Fri, "Fri").
-define(Sat, "Sat").
-define(Sun, "Sun").

-define(MONDAY, "MONDAY").
-define(TUESDAY, "TUESDAY").
-define(WEDNESDAY, "WEDNESDAY").
-define(THURSDAY, "THURSDAY").
-define(FRIDAY, "FRIDAY").
-define(SATURDAY, "SATURDAY").
-define(SUNDAY, "SUNDAY").

-define(Monday, "Monday").
-define(Tuesday, "Tuesday").
-define(Wednesday, "Wednesday").
-define(Thursday, "Thursday").
-define(Friday, "Friday").
-define(Saturday, "Saturday").
-define(Sunday, "Sunday").

-define(JAN, "JAN").
-define(FEB, "FEB").
-define(MAR, "MAR").
-define(APR, "APR").
-define(MAY, "MAY").
-define(JUN, "JUN").
-define(JUL, "JUL").
-define(AUG, "AUG").
-define(SEP, "SEP").
-define(OCT, "OCT").
-define(NOV, "NOV").
-define(DEC, "DEC").

-define(Jan, "Jan").
-define(Feb, "Feb").
-define(Mar, "Mar").
-define(Apr, "Apr").
-define(May, "May").
-define(Jun, "Jun").
-define(Jul, "Jul").
-define(Aug, "Aug").
-define(Sep, "Sep").
-define(Oct, "Oct").
-define(Nov, "Nov").
-define(Dec, "Dec").

-define(JANUARY, "JANUARY").
-define(FEBRUARY, "FEBRUARY").
-define(MARS, "MARS").
-define(APRIL, "APRIL").
-define(MAYLONG, "MAY").
-define(JUNE, "JUNE").
-define(JULY, "JULY").
-define(AUGUST, "AUGUST").
-define(SEPTEMBER, "SEPTEMBER").
-define(OCTOBER, "OCTOBER").
-define(NOVEMBER, "NOVEMBER").
-define(DECEMBER, "DECEMBER").

-define(January, "January").
-define(February, "February").
-define(Mars, "Mars").
-define(April, "April").
-define(Maylong, "May").
-define(June, "June").
-define(July, "July").
-define(August, "August").
-define(September, "September").
-define(October, "October").
-define(November, "November").
-define(December, "December").

-define(DEFAULT_ENCODING, utf8).

-record(date, {
        y = 0,
        m = 1,
        d = 1,
        h = 0,
        'M' = 0,
        s = 0,
        u = 0,
        meridiem,
        offset,
        tz
    }).

