-module(json).
-export([term_to_json/1, json_to_term/1]).


json_to_term(JSON) ->
    json_parser:load(JSON).

term_to_json(Term) ->
    json_generator:dump(Term).
