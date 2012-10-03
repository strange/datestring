Parse and Format Date and Time in Erlang
========================================

This library implements basic functionality to parse and format date and time
in Erlang.

## Usage

### Parsing

The `datestring` module exposes three functions for parsing:

* `parse_date/2`
* `parse_time/2`
* `parse_datetime/2`

All three functions take a *format string* as the first argument. The second
argument should be a string containing the date to be parsed.

Note that time is not returned as a standard time-tuple (`{H, M, S}`), but
a tuple containing a time-tuple and microseconds as an integer.

Examples:

    {ok,{2012,12,24}} = datestring:parse_date("Y-m-d", "2012-12-24").

    {ok,{{23,12,0},0}} = datestring:parse_time("I:M P", "11:12 p.m.").

    {ok,{{2012,12,24},{{23,12,0},0}}} =
        datestring:parse_datetime("Y-m-d H:M", "2012-12-24 23:12").

    {error,invalid_date} = datestring:parse_date("Y-m-d", "2012-13-24").

    {error,no_match} = datestring:parse_date("d-m-Y", "2012-12-24").

### Formatting Dates

* `format/2`

Format tuple containing date and/or time (with or without microseconds)
according to a *format string*. `format/2` takes a *format string* as it's
first argument. The second argument can be either one of:

* A tuple containing a date (`{2012, 12, 24}`).
* A tuple of two tuples (time and microseconds) (`{{23, 12, 0}, 0}`).
* A tuple of two tuples (date and time) {`{2012, 12, 24}, {23, 12, 0}}`)
* A tuple of two tuples (date and time+ms) {`{2012, 12, 24}, {{23, 12, 0}, 0}}`)

Examples:

    {ok,"2012-12-24"} = datestring:format("Y-m-d", {2012, 12, 24}).

    {ok,"11:59 p.m."} = datestring:format("I:M P", {{23, 59, 30}, 0}).

    {ok,"Monday, 24th December 2012"} =
        datestring:format("A, eo B Y", {2012, 12, 24}).

    {ok,"2012-12-24 at 13:12"} =
        datestring:format("Y-m-d \\a\\t H:M", {{2012, 12, 24}, {13, 12, 32}}). 

## Conversion Specification

The format used to parse and format dates closely resembles the one used
in `strftime()`[1]. The most notable exception is that meaningful characters
are not prefixed with a percentage sign (%) in *datestring*.

Characters not matching a conversion specification will be copied to the
output verbatim when formatting and matched against input when parsing.
Meaningful characters can be escaped with a backslash (\\).

<table>
    <thead>
        <tr>
            <th>Character Sequence</th>
            <th>Parsing</th>
            <th>Formatting</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>a</td>
            <td>Yes*</td>
            <td>Yes</td>
            <td>Abbreviated weekday name ("Mon", "Tue")</td>
        </tr>
        <tr>
            <td>A</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Weekday name ("Monday", "Tuesday")</td>
        </tr>
        <tr>
            <td>b</td>
            <td>Yes*</td>
            <td>Yes</td>
            <td>Abbreviated month name ("Jan", "Feb")</td>
        </tr>
        <tr>
            <td>B</td>
            <td>Yes*</td>
            <td>Yes</td>
            <td>Month name ("January", "February")</td>
        </tr>
        <tr>
            <td>d</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Day of month with leading zero ("01", "31")</td>
        </tr>
        <tr>
            <td>e</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Day of month without leading zero ("1", "31")</td>
        </tr>
        <tr>
            <td>F</td>
            <td>No</td>
            <td>Yes</td>
            <td>ISO 8601 date format (shortcut for "Y-m-d")</td>
        </tr>
        <tr>
            <td>H</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Hour (24 hours) with leading zero ("01", "23")</td>
        </tr>
        <tr>
            <td>I</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Hour (12 hours) with leading zero ("01", "11")</td>
        </tr>
        <tr>
            <td>k</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Hour (24 hours) without leading zero ("1", "23")</td>
        </tr>
        <tr>
            <td>l</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Hour (12 hours) without leading zero ("1", "11")</td>
        </tr>
        <tr>
            <td>m</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Month with leading zero ("1", "12")</td>
        </tr>
        <tr>
            <td>M</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Minute with leading zero ("00", "59")</td>
        </tr>
        <tr>
            <td>n</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Month without leading zero ("1", "12")</td>
        </tr>
        <tr>
            <td>o</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Ordinal number suffix abbreviation (st, nd, rd, th)</td>
        </tr>
        <tr>
            <td>p</td>
            <td>Yes*</td>
            <td>Yes</td>
            <td>AM/PM</td>
        </tr>
        <tr>
            <td>P</td>
            <td>Yes*</td>
            <td>Yes</td>
            <td>a.m./p.m.</td>
        </tr>
        <tr>
            <td>R</td>
            <td>No</td>
            <td>Yes</td>
            <td>The time as H:M (24 hour format) ("23:59")</td>
        </tr>
        <tr>
            <td>S</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Seconds with leading zero ("00", "59")</td>
        </tr>
        <tr>
            <td>T</td>
            <td>No</td>
            <td>Yes</td>
            <td>The time as H:M:S (24 hour format) ("23:49:49")</td>
        </tr>
        <tr>
            <td>u</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Microseconds, 6 digits with leading zero ("000034")</td>
        </tr>
        <tr>
            <td>y</td>
            <td>Yes**</td>
            <td>Yes</td>
            <td>Year without century ("02", "12")</td>
        </tr>
        <tr>
            <td>Y</td>
            <td>Yes</td>
            <td>Yes</td>
            <td>Year including century ("2002", "2012")</td>
        </tr>
        <tr>
            <td>z</td>
            <td>Yes</td>
            <td>No</td>
            <td>UTC offset (+0100, +01:00, +01, -0100, -01:00, -01)</td>
        </tr>
        <tr>
            <td>Z</td>
            <td>Yes</td>
            <td>No</td>
            <td>Abbreviated timezone (UTC, GMT, CET etc)</td>
        </tr>
    </tbody>
</table>

\* Case-insensitive when parsing

\*\* Falls back on current century of system when parsing years without
century.

Current Status
--------------

I wrote this library a ages ago. It was left forgotten in the darkest corners
of my hdd until recently, when I needed to parse dates in a new project.

Everything has been working fine for the formats I've thrown at it, but the
library needs more extensive testing. I'm also not sure about the API design.

Anyway, a little todo-list:

* More testcases.
* Convert TZ -> offset (probably just a translation table).
* Format offset (three notations "+HH", "+HHMM" and "+HH:MM") and TZs.
* Maybe add locale to the mix.
* Add specs, refactor a little, re-order patterns etc.

[1]: http://www.manpagez.com/man/3/strftime/
