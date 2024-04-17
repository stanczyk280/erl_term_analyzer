-module(erl_term_analyzer).

-export([init/0]).
-on_load(init/0).

-export([echo/1, analyze_term/1]).

-define(APPLICATION, erl_term_analyzer).
-define(NIF_SO_FILE, "erl_term_analyzer").

-define(NIF_NOT_LOADED, elang:nif_error(not_loaded)).

init() ->
  ok = erlang:load_nif(filename:join(code:priv_dir(?APPLICATION), ?NIF_SO_FILE), []).

echo(_Term) ->
  ?NIF_NOT_LOADED.

analyze_term(_Term) ->
  ?NIF_NOT_LOADED.