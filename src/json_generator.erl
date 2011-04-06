-module(json_generator).
-export([dump/1]).

%% String conversion
to_string(S) when is_list(S) ->
    S;
to_string(S) when is_binary(S) ->
    binary_to_list(S).

%% Hex conversion
to_hex(N) when is_integer(N) andalso N < 16 ->
    if
        N < 10 -> [$0 + N];
        true -> [$a + N - 10]
    end;
to_hex(N) when is_integer(N) andalso N >= 16 ->
    to_hex(N div 16) ++ to_hex(N rem 16).

%% Quote
% backspace
quote_char($\\) -> [$\\, $\\];
% quote
quote_char(34) -> [$\\, 34];
% new line
quote_char($\n) -> [$\\, $n];
% backspace
quote_char($\b) -> [$\\, $b];
% form feed
quote_char($\f) -> [$\\, $f];
% carriage return
quote_char($\r) -> [$\\, $r];
% tab
quote_char($\t) -> [$\\, $t];
% other
quote_char(I) ->
    if
        32 =< I andalso I =< 126 ->
            [I];
        true -> Hex = to_hex(I),
                true = string:len(Hex) =< 4,
                [[$\\, $u] ++ string:right(Hex, 4, $0)]
    end.

quote(S) ->
    "\"" ++ lists:flatten(lists:map(fun(X) -> quote_char(X) end, S)) ++ "\"".


%% Term to JSON
dump(true) ->
    {ok, "true"};

dump(false) ->
    {ok, "false"};

dump(null) ->
    {ok, "null"};

dump(I) when is_integer(I) ->
    {ok, integer_to_list(I)};

dump(F) when is_float(F) ->
    {ok, float_to_list(F)};

dump(B) when is_binary(B) ->
    {ok, quote(binary_to_list(B))};

dump(L) when is_list(L) ->
    Vals = lists:map(fun(X) ->
                             {ok, JSON} = dump(X),
                             JSON
                     end, L),
    {ok, "[" ++ string:join(Vals, ",") ++ "]"};

dump(D) when is_tuple(D) ->
    Lst = dict:to_list(D),
    Vals = lists:map(fun({Key, Val}) ->
                             {ok, JSON} = dump(Val),
                             quote(to_string(Key)) ++ ":" ++ JSON
                     end, Lst),
    {ok, "{" ++ string:join(Vals, ",") ++ "}"}.

