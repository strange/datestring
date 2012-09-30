-module(datestring).

-export([parse_date/2, parse_time/2, parse_datetime/2]).
-export([format/2]).

parse_date(Fmt, S) -> datestring_parse:parse_date(Fmt, S).
parse_time(Fmt, S) -> datestring_parse:parse_time(Fmt, S).
parse_datetime(Fmt, S) -> datestring_parse:parse_datetime(Fmt, S).

format(Fmt, Date) -> datestring_format:format(Fmt, Date).
