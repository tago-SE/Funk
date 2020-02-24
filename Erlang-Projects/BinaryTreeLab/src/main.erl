%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. feb. 2020 11:58
%%%-------------------------------------------------------------------
-module(main).
-author("tiago").

%% Notes: Tutorial used for configuring Erlang in intelliJ, also contains info on how to import a project.
%% Ref: https://www.youtube.com/watch?v=Sb9FqbW8HTo

%% API
-export([print_hello/0, print_int/1, print_float/1]).

print_hello() ->
  io:format("Hello, World! ~n").


print_int(N) ->
  io:format("Int: ~p~n", [N]).


print_float(F) ->
  io:format("Float: ~.3f~n", [F]).

