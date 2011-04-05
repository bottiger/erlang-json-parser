-module(json).
-export([term_to_json/1, json_to_term/1, read_number/1]).


json_to_term(JSON) ->
    json_parser:parse(JSON).


to_string(S) when is_list(S) ->
    S;
to_string(S) when is_binary(S) ->
    binary_to_list(S).


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
        true -> Hex = hex:int_to_hex(I),
                true = string:len(Hex) =< 4,
                [[$\\, $u] ++ string:right(Hex, 4, $0)]
    end.

quote(S) ->
    "\"" ++ lists:flatten(lists:map(fun(X) -> quote_char(X) end, S)) ++ "\"".


%% Term to JSON
term_to_json(true) ->
    {ok, "true"};

term_to_json(false) ->
    {ok, "false"};

term_to_json(null) ->
    {ok, "null"};

term_to_json(I) when is_integer(I) ->
    {ok, integer_to_list(I)};

term_to_json(F) when is_float(F) ->
    {ok, float_to_list(F)};

term_to_json(B) when is_binary(B) ->
    {ok, quote(binary_to_list(B))};

term_to_json(L) when is_list(L) ->
    Vals = lists:map(fun(X) ->
                             {ok, JSON} = term_to_json(X),
                             JSON
                     end, L),
    {ok, "[" ++ string:join(Vals, ",") ++ "]"};

term_to_json(D) when is_tuple(D) ->
    Lst = dict:to_list(D),
    Vals = lists:map(fun({Key, Val}) ->
                             {ok, JSON} = term_to_json(Val),
                             quote(to_string(Key)) ++ ":" ++ JSON
                     end, Lst),
    {ok, "{" ++ string:join(Vals, ",") ++ "}"}.



%% JSON to Term
read_number("", {int, S, N}) ->
    {ok, list_to_integer(S ++ lists:reverse(N)), ""};
read_number("", {float, S, F}) ->
    {ok, list_to_float(S ++ lists:reverse(F)), ""};

read_number([C | Rest], {T, S, [D|Acc]}) when $0 =< C andalso C =< $9 ->
    read_number(Rest, {T, S, [C,D|Acc]});

read_number([C | Rest], {int, S, ""}) when $1 =< C andalso C =< $9 ->
    read_number(Rest, {int, S, [C]});

read_number([$0, $. | Rest], {int, S, ""}) ->
    io:format(Rest, []),
    read_number(Rest, {float, S, ".0"});

read_number([$. | Rest], {int, S, [D|Acc]}) ->
    read_number(Rest, {float, S, [$.,D|Acc]});

read_number([$- | Rest], {int, "", ""}) ->
    read_number(Rest, {int, "-", ""}).

read_number(S) ->
    read_number(S, {int, "", ""}).
