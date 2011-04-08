-module(json).
-export([term_to_json/1, json_to_term/1]).

-include("deps/proper/include/proper.hrl").

json_to_term(JSON) ->
    json_parser:load(JSON).

term_to_json(Term) ->
    json_generator:dump(Term).

prop_identity_number() ->
    ?FORALL(F, number(),
            begin
                {ok, JSON} = term_to_json(F),
                {ok, F2} = json_to_term(JSON),
                abs(F - F2) =< 0.0000001
            end).

prop_identity_string() ->
    ?FORALL(S, string(),
            begin
                {ok, JSON} = term_to_json(S),
                {ok, S} = json_to_term(JSON),
                true
            end).

prop_identity_list() ->
    ?FORALL(L, list(union([number(), string(), bool()])),
            begin
                {ok, JSON} = term_to_json(L),
                {ok, L} = json_to_term(JSON),
                true
            end).

prop_identity_dict() ->
    ?FORALL(L, list(tuple([list(range(0,127)), union([number(), bool(), list(number())])])),
            begin
                L2 = lists:map(fun({K, V}) -> {list_to_binary(K), V} end, L),
                D = dict:from_list(L2),
                {ok, JSON} = term_to_json(D),
                {ok, D2} = json_to_term(JSON),
                lists:sort(dict:to_list(D)) ==
                    lists:sort(dict:to_list(D2))
            end).
