-module(erl_term_analyzer).

-export([init/0]).
-on_load(init/0).

-export([echo/1, analyze_term/1, print_term_info/1, test/0]).

-define(APPLICATION, erl_term_analyzer).
-define(NIF_SO_FILE, "erl_term_analyzer").

-define(NIF_NOT_LOADED, elang:nif_error(not_loaded)).

init() ->
  ok = erlang:load_nif(filename:join(code:priv_dir(?APPLICATION), ?NIF_SO_FILE), []).

echo(_Term) ->
  ?NIF_NOT_LOADED.

analyze_term(_Term) ->
  ?NIF_NOT_LOADED.

print_term_info(Term) ->
  Map = analyze_term(Term),
  print_term_map(Map).

test() ->
  analyze_term("test").


print_term_map(Map) ->
  Binary = maps:get(binary, Map),
  Tag = maps:get(tag, Map),
  Type = maps:get(type, Map),
  Value = maps:get(value, Map),
  NextTerm = maps:find(next_term, Map),
  io:format("Binary: ~p~nTag: ~p~nType: ~p~nValue: ~p~n", [Binary ,Tag, Type, Value]),
  case NextTerm of
    {ok, NextMap} -> 
      io:format("Next term: ~n"),
      print_term_map(NextMap);
    error -> ok
  end.
