-module(datestring_validate).

-export([valid_date/1]).
-export([valid_time/1]).
-export([valid_datetime/1]).

-include("datestring.hrl").

valid_datetime({{Y, M, D}, {H, I, S}}) ->
    valid_datetime(#date{y = Y, m = M, d = D, h = H, 'M' = I, s = S});
valid_datetime({{Y, M, D}, {{H, I, S}, U}}) ->
    valid_datetime(#date{y = Y, m = M, d = D, h = H, 'M' = I, s = S, u = U});
valid_datetime(D) ->
    try
        {ok, Date} = datestring_validate:valid_date(D),
        {ok, Time} = datestring_validate:valid_time(D),
        {ok, {Date, Time}}
    catch 
        error:{badmatch, Error} -> Error
    end.

valid_date({Y, M, D}) ->
    valid_date(#date{y = Y, m = M, d = D});
valid_date(#date{y = Y, m = M, d = D}) 
        when is_integer(Y), is_integer(M), is_integer(D) ->
    case calendar:valid_date(Y, M, D) of
        true -> {ok, {Y, M, D}};
        false -> {error, invalid_date}
    end;
valid_date(_) ->
    {error, invalid_date}.

valid_time({H, M, S}) ->
    valid_time(#date{h = H, 'M' = M, s = S});
valid_time({{H, M, S}, U}) ->
    valid_time(#date{h = H, 'M' = M, s = S, u = U});
valid_time(#date{h = H, 'M' = M, s = S})
        when not is_integer(H); not is_integer(M); not is_integer(S) ->
    {error, invalid_time};
valid_time(#date{s = S}) when S < 0; S > 59 ->
    {error, invalid_time};
valid_time(#date{'M' = M}) when M < 0; M > 59 ->
    {error, invalid_time};
valid_time(#date{h = H}) when H < 0; H > 23 ->
    {error, invalid_time};
valid_time(#date{h = H, meridiem = am}) when H < 1; H > 12 ->
    {error, invalid_time};
valid_time(#date{h = H, meridiem = pm}) when H < 1; H > 12 ->
    {error, invalid_time};
valid_time(#date{h = H, meridiem = pm} = Date) when H < 12 ->
    valid_time(Date#date{h = H + 12, meridiem = undefined});
valid_time(#date{h = H, meridiem = am} = Date) when H =:= 12 ->
    valid_time(Date#date{h = H - 12, meridiem = undefined});
valid_time(#date{h = H, 'M' = M, s = S, u = U}) ->
    {ok, {{H, M, S}, U}}.
