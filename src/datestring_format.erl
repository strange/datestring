-module(datestring_format).

-compile([export_all]).

-include("datestring.hrl").

format(Fmt, {{Y, M, D}, {H, Min, S}}) ->
    format(Fmt, #date{y = Y, m = M, d = D, h = H, 'M' = Min, s = S});
format(Fmt, {{Y, M, D}, {{H, Min, S}, U}}) ->
    format(Fmt, #date{y = Y, m = M, d = D, h = H, 'M' = Min, s = S, u = U});
format(Fmt, {Y, M, D}) ->
    format(Fmt, #date{y = Y, m = M, d = D});
format(Fmt, {{H, M, S}, U}) ->
    format(Fmt, #date{h = H, 'M' = M, s = S, u = U});
format(Fmt, Date) when is_binary(Fmt) ->
    {ok, Formatted} = format(unicode:characters_to_list(Fmt, utf8), Date),
    {ok, unicode:characters_to_binary(Formatted, utf8)};
format(Fmt, Date) ->
    {ok, format(Fmt, Date, [])}.

format([], _, Acc) -> lists:flatten(lists:reverse(Acc));

format([$a|Fmt], #date{y = Y, m = M, d = D} = Date, Acc) ->
    format(Fmt, Date, [abbr_day(calendar:day_of_the_week(Y, M, D))|Acc]);
format([$A|Fmt], #date{y = Y, m = M, d = D} = Date, Acc) ->
    format(Fmt, Date, [day(calendar:day_of_the_week(Y, M, D))|Acc]);

format([$b|Fmt], #date{m = M} = Date, Acc) ->
    format(Fmt, Date, [abbr_month(M)|Acc]);
format([$B|Fmt], #date{m = M} = Date, Acc) ->
    format(Fmt, Date, [month(M)|Acc]);

format([$d|Fmt], #date{d = D} = Date, Acc) ->
    format(Fmt, Date, [pad(D)|Acc]);
format([$e|Fmt], #date{d = D} = Date, Acc) ->
    format(Fmt, Date, [integer_to_list(D)|Acc]);

format([$F|Fmt], Date, Acc) ->
    {ok, Date2} = format("Y-m-d", Date),
    format(Fmt, Date, [Date2|Acc]);

format([$H|Fmt], #date{h = H} = Date, Acc) ->
    format(Fmt, Date, [pad(H)|Acc]);
format([$I|Fmt], #date{h = H} = Date, Acc) ->
    format(Fmt, Date, [pad(twelve_hour_format(H))|Acc]);
format([$k|Fmt], #date{h = H} = Date, Acc) ->
    format(Fmt, Date, [integer_to_list(H)|Acc]);
format([$l|Fmt], #date{h = H} = Date, Acc) ->
    format(Fmt, Date, [twelve_hour_format(H)|Acc]);

format([$m|Fmt], #date{m = M} = Date, Acc) ->
    format(Fmt, Date, [pad(M)|Acc]);

format([$M|Fmt], #date{'M' = M} = Date, Acc) ->
    format(Fmt, Date, [pad(M)|Acc]);

format([$n|Fmt], #date{m = M} = Date, Acc) ->
    format(Fmt, Date, [integer_to_list(M)|Acc]);

format([$o|Fmt], #date{d = D} = Date, Acc) ->
    format(Fmt, Date, [ord_suffix(D)|Acc]);

format([$p|Fmt], #date{h = H} = Date, Acc) ->
    S = case am_or_pm(H) of
        am -> "AM";
        pm -> "PM"
    end,
    format(Fmt, Date, [S|Acc]);
format([$P|Fmt], #date{h = H} = Date, Acc) ->
    S = case am_or_pm(H) of
        am -> "a.m.";
        pm -> "p.m."
    end,
    format(Fmt, Date, [S|Acc]);

format([$R|Fmt], Date, Acc) ->
    {ok, Time} = format("H:M", Date),
    format(Fmt, Date, [Time|Acc]);

format([$S|Fmt], #date{s = S} = Date, Acc) ->
    format(Fmt, Date, [pad(S)|Acc]);

format([$T|Fmt], Date, Acc) ->
    {ok, Time} = format("H:M:S", Date),
    format(Fmt, Date, [Time|Acc]);

format([$y|Fmt], #date{y = Y} = Date, Acc) ->
    format(Fmt, Date, [remove_century(Y)|Acc]);
format([$Y|Fmt], #date{y = Y} = Date, Acc) ->
    format(Fmt, Date, [integer_to_list(Y)|Acc]);

format([$u|Fmt], #date{u = U} = Date, Acc) ->
    format(Fmt, Date, [right:pad(integer_to_list(U), 6, $0)|Acc]);

format([$\\, C|Fmt], D, Acc) ->
    format(Fmt, D, [C|Acc]);
format([C|Fmt], D, Acc) ->
    format(Fmt, D, [C|Acc]).

pad(N) when is_integer(N) -> pad(integer_to_list(N));
pad(N) -> string:right(N, 2, $0).

remove_century(Y) when is_integer(Y) -> remove_century(integer_to_list(Y));
remove_century([N1]) -> [N1];
remove_century([N1, N2]) -> [N1, N2];
remove_century([_, N2, N3]) -> [N2, N3];
remove_century([_, _, N3, N4]) -> [N3, N4].

am_or_pm(H) when H > 12 -> pm;
am_or_pm(_) -> am.

twelve_hour_format(H) when H > 12 -> H - 12;
twelve_hour_format(H) -> H.

ord_suffix(1) -> "st";
ord_suffix(2) -> "nd";
ord_suffix(3) -> "rd";
ord_suffix(_) -> "th".

abbr_day(1) -> ?Mon;
abbr_day(2) -> ?Tue;
abbr_day(3) -> ?Wed;
abbr_day(4) -> ?Thu;
abbr_day(5) -> ?Fri;
abbr_day(6) -> ?Sat;
abbr_day(7) -> ?Sun.

day(1) -> ?Monday;
day(2) -> ?Tuesday;
day(3) -> ?Wednesday;
day(4) -> ?Thursday;
day(5) -> ?Friday;
day(6) -> ?Saturday;
day(7) -> ?Sunday.

abbr_month(1) -> ?Jan;
abbr_month(2) -> ?Feb;
abbr_month(3) -> ?Mar;
abbr_month(4) -> ?Apr;
abbr_month(5) -> ?May;
abbr_month(6) -> ?Jun;
abbr_month(7) -> ?Jul;
abbr_month(8) -> ?Aug;
abbr_month(9) -> ?Sep;
abbr_month(10) -> ?Oct;
abbr_month(11) -> ?Nov;
abbr_month(12) -> ?Dec.

month(1) -> ?January;
month(2) -> ?February;
month(3) -> ?Mars;
month(4) -> ?April;
month(5) -> ?May;
month(6) -> ?June;
month(7) -> ?July;
month(8) -> ?August;
month(9) -> ?September;
month(10) -> ?October;
month(11) -> ?November;
month(12) -> ?December.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

format_test() ->
    ?assertEqual({ok, "2012-12-12"}, format("Y-m-d", {2012, 12, 12})),

    ?assertEqual({ok, "Wednesday 2012"}, format("A Y", {2012, 12, 12})),
    ?assertEqual({ok, "Wed 2012"}, format("a Y", {2012, 12, 12})),

    ?assertEqual({ok, "Dec 2012"}, format("b Y", {2012, 12, 12})),
    ?assertEqual({ok, "December 2012"}, format("B Y", {2012, 12, 12})),

    ?assertEqual({ok, "2012-01-01"}, format("Y-m-d", {2012, 1, 1})),
    ?assertEqual({ok, "Y"}, format("\\Y", {2012, 1, 1})),

    ?assertEqual({ok, "1999-09-09"}, format("F", {1999, 9, 9})),

    ?assertEqual({ok, "12:30"}, format("R", {{12, 30, 59}, 0})),
    ?assertEqual({ok, "23:30:59"}, format("T", {{23, 30, 59}, 0})),

    ?assertEqual({ok, "Thu, 21 Dec 2000 16:01:07"},
        format("a, d b Y H:M:S", {{2000, 12, 21}, {16, 1, 7}})),

    ?assertEqual({ok, "Thu, 21 Dec 2000 16:01:07"},
        format("a, d b Y H:M:S", {{2000, 12, 21}, {{16, 1, 7}, 0}})),

    ?assertEqual({ok, <<"Thu, 21 Dec 2000 16:01:07">>},
        format(<<"a, d b Y H:M:S">>, {{2000, 12, 21}, {{16, 1, 7}, 0}})),
    ok.

-endif.
