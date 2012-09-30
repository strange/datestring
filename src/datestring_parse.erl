-module(datestring_parse).

-compile([export_all]).

-include("datestring.hrl").

-define(is_num(D), (D >= $0 andalso D =< $9)).

parse_datetime(Fmt, S) when is_binary(Fmt) ->
    parse_datetime(S, unicode:characters_to_list(Fmt, ?DEFAULT_ENCODING));
parse_datetime(Fmt, S) when is_binary(S) ->
    parse_datetime(Fmt, unicode:characters_to_list(S, ?DEFAULT_ENCODING));
parse_datetime(Fmt, S) ->
    case parse(Fmt, string:to_upper(S), #date{}) of
        {ok, D} ->
            try
                {ok, Date} = valid_date(D),
                {ok, Time} = valid_time(D),
                {ok, Date, Time}
            catch 
                error:{badmatch, Error} -> Error
            end;
        Error -> Error
    end.

parse_time(Fmt, S) when is_binary(Fmt) ->
    parse_time(S, unicode:characters_to_list(Fmt, ?DEFAULT_ENCODING));
parse_time(Fmt, S) when is_binary(S) ->
    parse_time(Fmt, unicode:characters_to_list(S, ?DEFAULT_ENCODING));
parse_time(Fmt, S) ->
    case parse(Fmt, string:to_upper(S), #date{}) of
        {ok, D} -> valid_time(D);
        Error -> Error
    end.

parse_date(Fmt, S) when is_binary(Fmt) ->
    parse_date(S, unicode:characters_to_list(Fmt, ?DEFAULT_ENCODING));
parse_date(Fmt, S) when is_binary(S) ->
    parse_date(Fmt, unicode:characters_to_list(S, ?DEFAULT_ENCODING));
parse_date(Fmt, S) when is_list(S), is_list(Fmt) ->
    case parse(Fmt, string:to_upper(S), #date{}) of
        {ok, D} -> valid_date(D);
        Error -> Error
    end.

parse([], [], D) ->
    {ok, D};

parse(_, [], _) ->
    {error, no_match};
parse([], _, _) ->
    {error, no_match};

parse([$a|Fmt], ?MON ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$a|Fmt], ?TUE ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$a|Fmt], ?WED ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$a|Fmt], ?THU ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$a|Fmt], ?FRI ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$a|Fmt], ?SAT ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$a|Fmt], ?SUN ++ Rest, D) -> parse(Fmt, Rest, D);

parse([$A|Fmt], ?MONDAY ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$A|Fmt], ?TUESDAY ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$A|Fmt], ?WEDNESDAY ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$A|Fmt], ?THURSDAY ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$A|Fmt], ?FRIDAY ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$A|Fmt], ?SATURDAY ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$A|Fmt], ?SUNDAY ++ Rest, D) -> parse(Fmt, Rest, D);

parse([$b|Fmt], ?JAN ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 1});
parse([$b|Fmt], ?FEB ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 2});
parse([$b|Fmt], ?MAR ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 3});
parse([$b|Fmt], ?APR ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 4});
parse([$b|Fmt], ?MAY ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 5});
parse([$b|Fmt], ?JUN ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 6});
parse([$b|Fmt], ?JUL ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 7});
parse([$b|Fmt], ?AUG ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 8});
parse([$b|Fmt], ?SEP ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 9});
parse([$b|Fmt], ?OCT ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 10});
parse([$b|Fmt], ?NOV ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 11});
parse([$b|Fmt], ?DEC ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 12});

parse([$B|Fmt], ?JANUARY ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 1});
parse([$B|Fmt], ?FEBRUARY ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 2});
parse([$B|Fmt], ?MARS ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 3});
parse([$B|Fmt], ?APRIL ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 4});
parse([$B|Fmt], ?MAYLONG ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 5});
parse([$B|Fmt], ?JUNE ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 6});
parse([$B|Fmt], ?JULY ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 7});
parse([$B|Fmt], ?AUGUST ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 8});
parse([$B|Fmt], ?SEPTEMBER ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 9});
parse([$B|Fmt], ?OCTOBER ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 10});
parse([$B|Fmt], ?NOVEMBER ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 11});
parse([$B|Fmt], ?DECEMBER ++ Rest, D) -> parse(Fmt, Rest, D#date{m = 12});

parse([$d|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{d = list_to_integer([N1, N2])});
parse([$e|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{d = list_to_integer([N1, N2])});
parse([$e|Fmt], [N1|Rest], D) when ?is_num(N1) ->
    parse(Fmt, Rest, D#date{d = list_to_integer([N1])});

parse([$H|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{h = list_to_integer([N1, N2])});
parse([$I|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{h = list_to_integer([N1, N2])});
parse([$k|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{h = list_to_integer([N1, N2])});
parse([$k|Fmt], [N1|Rest], D) when ?is_num(N1) ->
    parse(Fmt, Rest, D#date{h = list_to_integer([N1])});
parse([$l|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{h = list_to_integer([N1, N2])});
parse([$l|Fmt], [N1|Rest], D) when ?is_num(N1) ->
    parse(Fmt, Rest, D#date{h = list_to_integer([N1])});

parse([$M|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{'M' = list_to_integer([N1, N2])});

parse([$m|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{m = list_to_integer([N1, N2])});
parse([$n|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{m = list_to_integer([N1, N2])});
parse([$n|Fmt], [N1|Rest], D) when ?is_num(N1) ->
    parse(Fmt, Rest, D#date{m = list_to_integer([N1])});

parse([$o|Fmt], "ST" ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$o|Fmt], "ND" ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$o|Fmt], "RD" ++ Rest, D) -> parse(Fmt, Rest, D);
parse([$o|Fmt], "TH" ++ Rest, D) -> parse(Fmt, Rest, D);

parse([$p|Fmt], "AM" ++ Rest, D) ->
    parse(Fmt, Rest, D#date{meridiem = am});
parse([$p|Fmt], "PM" ++ Rest, D) ->
    parse(Fmt, Rest, D#date{meridiem = pm});
parse([$P|Fmt], "A.M." ++ Rest, D) ->
    parse(Fmt, Rest, D#date{meridiem = am});
parse([$P|Fmt], "P.M." ++ Rest, D) ->
    parse(Fmt, Rest, D#date{meridiem = pm});

parse([$S|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{s = list_to_integer([N1, N2])});

parse([$u|Fmt], [N1, N2, N3, N4, N5, N6|Rest], D)
        when ?is_num(N1), ?is_num(N2), ?is_num(N3), ?is_num(N4),
        ?is_num(N5), ?is_num(N6)  ->
    U = list_to_integer([N1, N2, N3, N4, N5, N6]),
    parse(Fmt, Rest, D#date{u = U});

parse([$Y|Fmt], [N1, N2, N3, N4|Rest], D)
        when ?is_num(N1), ?is_num(N2), ?is_num(N3), ?is_num(N4)  ->
    parse(Fmt, Rest, D#date{y = list_to_integer([N1, N2, N3, N4])});
parse([$y|Fmt], [N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    {{CurrentYear, _, _}, _} = calendar:now_to_datetime(erlang:now()),
    [P1, P2|_] = integer_to_list(CurrentYear),
    parse(Fmt, Rest, D#date{y = list_to_integer([P1, P2, N1, N2])});

parse([$z|Fmt], [$+, N1, N2, N3, N4|Rest], D)
        when ?is_num(N1), ?is_num(N2), ?is_num(N3), ?is_num(N4)  ->
    parse(Fmt, Rest, D#date{offset = offset('+', [N1, N2], [N3, N4])});
parse([$z|Fmt], [$-, N1, N2, N3, N4|Rest], D)
        when ?is_num(N1), ?is_num(N2), ?is_num(N3), ?is_num(N4)  ->
    parse(Fmt, Rest, D#date{offset = offset('-', [N1, N2], [N3, N4])});
parse([$z|Fmt], [$+, N1, N2, $:, N3, N4|Rest], D)
        when ?is_num(N1), ?is_num(N2), ?is_num(N3), ?is_num(N4)  ->
    parse(Fmt, Rest, D#date{offset = offset('+', [N1, N2], [N3, N4])});
parse([$z|Fmt], [$-, N1, N2, $:, N3, N4|Rest], D)
        when ?is_num(N1), ?is_num(N2), ?is_num(N3), ?is_num(N4)  ->
    parse(Fmt, Rest, D#date{offset = offset('-', [N1, N2], [N3, N4])});
parse([$z|Fmt], [$+, N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2)  ->
    parse(Fmt, Rest, D#date{offset = offset('+', [N1, N2], 0)});
parse([$z|Fmt], [$-, N1, N2|Rest], D) when ?is_num(N1), ?is_num(N2) ->
    parse(Fmt, Rest, D#date{offset = offset('-', [N1, N2], 0)});

parse([$Z|Fmt], "ADT" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = adt});
parse([$Z|Fmt], "AST" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = ast});
parse([$Z|Fmt], "CAT" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = cat});
parse([$Z|Fmt], "CDT" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = cdt});
parse([$Z|Fmt], "CET" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = cet});
parse([$Z|Fmt], "CST" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = cst});
parse([$Z|Fmt], "EAT" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = eat});
parse([$Z|Fmt], "EDT" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = edt});
parse([$Z|Fmt], "EET" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = eet});
parse([$Z|Fmt], "EST" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = est});
parse([$Z|Fmt], "GMT" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = gmt});
parse([$Z|Fmt], "PST" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = pst});
parse([$Z|Fmt], "WAT" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = wat});
parse([$Z|Fmt], "WET" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = wet});
parse([$Z|Fmt], "UTC" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = utc});
parse([$Z|Fmt], "SAST" ++ Rest, D) -> parse(Fmt, Rest, D#date{tz = sast});

parse([$a|_], _, _) -> {error, {no_match, a}};
parse([$A|_], _, _) -> {error, {no_match, 'A'}};
parse([$b|_], _, _) -> {error, {no_match, b}};
parse([$B|_], _, _) -> {error, {no_match, 'B'}};
parse([$d|_], _, _) -> {error, {no_match, d}};
parse([$e|_], _, _) -> {error, {no_match, e}};
parse([$H|_], _, _) -> {error, {no_match, 'H'}};
parse([$I|_], _, _) -> {error, {no_match, 'I'}};
parse([$k|_], _, _) -> {error, {no_match, k}};
parse([$l|_], _, _) -> {error, {no_match, l}};
parse([$m|_], _, _) -> {error, {no_match, m}};
parse([$M|_], _, _) -> {error, {no_match, 'M'}};
parse([$n|_], _, _) -> {error, {no_match, n}};
parse([$o|_], _, _) -> {error, {no_match, o}};
parse([$p|_], _, _) -> {error, {no_match, p}};
parse([$P|_], _, _) -> {error, {no_match, 'P'}};
parse([$S|_], _, _) -> {error, {no_match, 'S'}};
parse([$y|_], _, _) -> {error, {no_match, y}};
parse([$Y|_], _, _) -> {error, {no_match, 'Y'}};
parse([$u|_], _, _) -> {error, {no_match, u}};
parse([$z|_], _, _) -> {error, {no_match, z}};
parse([$Z|_], _, _) -> {error, {no_match, 'Z'}};

parse([$\\, A|Fmt], [B|Rest], Date) ->
    case string:to_upper(A) of
        B -> parse(Fmt, Rest, Date);
        _ -> {error, no_match}
    end;
parse([X|Fmt], [X|Rest], Date) ->
    parse(Fmt, Rest, Date);
parse(Fmt, Rest, _) ->
    erlang:display(Fmt),
    erlang:display(Rest),
    {error, no_match}.

offset(Sign, H, M) when is_list(H) -> offset(Sign, list_to_integer(H), M);
offset(Sign, H, M) when is_list(M) -> offset(Sign, H, list_to_integer(M));
offset('+', H, M) -> H * 60 + M;
offset('-', H, M) -> -(H * 60 + M).

valid_date(#date{y = Y, m = M, d = D}) ->
    valid_date(Y, M, D).

valid_date(Y, M, D) when is_integer(Y), is_integer(M), is_integer(D) ->
    case calendar:valid_date(Y, M, D) of
        true -> {ok, {Y, M, D}};
        false -> {error, invalid_date}
    end;
valid_date(_, _, _) ->
    {error, invalid_date}.

valid_time(#date{h = H, 'M' = M, s = S, u = U, meridiem = Meridiem}) ->
    valid_time(H, M, S, U, Meridiem).

valid_time(H, M, S, _, _)
        when not is_integer(H); not is_integer(M); not is_integer(S) ->
    {error, invalid_time};
valid_time(_, _, S, _, _) when S < 0; S > 59 ->
    {error, invalid_time};
valid_time(_, M, _, _, _) when M < 0; M > 59 ->
    {error, invalid_time};
valid_time(H, _, _, _, undefined) when H < 0; H > 23 ->
    {error, invalid_time};
valid_time(H, _, _, _, am) when H < 1; H > 12 ->
    {error, invalid_time};
valid_time(H, _, _, _, pm) when H < 1; H > 12 ->
    {error, invalid_time};
valid_time(H, M, S, U, pm) when H < 12 ->
    valid_time(H + 12, M, S, U, undefined);
valid_time(H, M, S, U, _) ->
    {ok, {{H, M, S}, U}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(year(D), #date{y = Y}, Y).

offset_test() ->
    ?assertEqual(1, offset('+', 0, 1)),
    ?assertEqual(61, offset('+', 1, 1)),
    ?assertEqual(-1, offset('-', 0, 1)),
    ?assertEqual(-61, offset('-', 1, 1)),
    ?assertEqual(-1, offset('-', "0", 1)),
    ?assertEqual(-61, offset('-', 1, "1")),
    ?assertEqual(1, offset('+', "0", "1")),
    ?assertEqual(61, offset('+', "1", "1")),
    ok.

escape_test() ->
    ?assertEqual({ok, {2012, 12, 12}},
        parse_date("\\YY-m-d", "Y2012-12-12")),
    ?assertEqual({ok, {0, 1, 1}},
        parse_date("\\Y\\d", "Yd")),
    ok.

'F_test'() ->
    ?assertEqual({ok, {0, 1, 1}},
        parse_date("B", "january")),
    ?assertEqual({ok, {0, 2, 1}},
        parse_date("B", "FeBruAry")),
    ?assertEqual({ok, {0, 3, 1}},
        parse_date("B", "MARS")),
    ?assertEqual({ok, {0, 4, 1}},
        parse_date("B", "APRIL")),
    ?assertEqual({ok, {0, 5, 1}},
        parse_date("B", "MAY")),
    ?assertEqual({ok, {0, 6, 1}},
        parse_date("B", "JUNE")),
    ?assertEqual({ok, {0, 7, 1}},
        parse_date("B", "JULY")),
    ?assertEqual({ok, {0, 8, 1}},
        parse_date("B", "AUGUST")),
    ?assertEqual({ok, {0, 9, 1}},
        parse_date("B", "SEPTEMBER")),
    ?assertEqual({ok, {0, 10, 1}},
        parse_date("B", "OCTOBER")),
    ?assertEqual({ok, {0, 11, 1}},
        parse_date("B", "NOVEMBER")),
    ?assertEqual({ok, {0, 12, 1}},
        parse_date("B", "DECEMBER")),
    ?assertEqual({error, {no_match, 'B'}},
        parse_date("B", "DEXEMBER")),
    ok.

misc_test() ->
    ?assertEqual({ok, {2012, 12, 12}},
        parse_date("Y-m-d", "2012-12-12")),
    ?assertEqual({ok, {2012, 1, 31}},
        parse_date(<<"Y-m-d">>, <<"2012-01-31">>)),
    ?assertEqual({ok, {2012, 1, 31}},
        parse_date("\\YY-m-dƺ", "Y2012-01-31ƺ")),
    ok.

iso_formats_test() ->
    ?assertEqual({ok, {2000, 12, 21}},
        parse_date("a, d b Y H:M:S z", "Thu, 21 Dec 2000 16:01:07 +0200")),
    ?assertEqual({ok, {2008, 1, 2}},
        parse_date("Y-m-dTH:M:S.uz", "2008-01-02T10:30:00.000123+02:00")),

    ?assertEqual({ok, {{10,30,0}, 123}},
        parse_time("Y-m-dTH:M:S.uz", "2008-01-02T10:30:00.000123+02:00")),
    ?assertEqual({ok, {{16, 01, 07}, 0}},
        parse_time("a, d b Y H:M:S z", "Thu, 21 Dec 2000 16:01:07 +0200")),

    ?assertEqual({ok, {2008, 1, 2}, {{10,30,0}, 123}},
        parse_datetime("Y-m-dTH:M:S.uz", "2008-01-02T10:30:00.000123+02:00")),
    ?assertEqual({ok, {2000, 12, 21}, {{16, 01, 07}, 0}},
        parse_datetime("a, d b Y H:M:S z", "Thu, 21 Dec 2000 16:01:07 +0200")),
    ok.

-endif.
