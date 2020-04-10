%%%-------------------------------------------------------------------
%%% @author tiago & trainers@erlang-solution.com
%%% @copyright (C) 2020,
%%% @doc
%%%
%%% @end
%%% Created : 28. mars 2020 19:51
%%%-------------------------------------------------------------------
%%% Example: 3.1 An Echo Server
%%% Concurrency semantics - Spawns processes, passes messages, registering and termination.
%%%-------------------------------------------------------------------
-module(echo).
-author("tiago").

%% API
-export([start/0, stop/0, listen/0, print/1]).

-spec start() -> ok.
start() ->
  register(echo, spawn(echo, listen, [])).

%% Prints a term passed as an argument.
-spec print(term()) -> ok.
print(Message) ->
  echo ! {print, Message},
  ok.

%% Stops the echo server.
-spec stop() -> ok.
stop()->
  echo ! stop,
  ok.

%% The echo server loop
-spec listen() -> true.
listen()->
  receive
    {print, Message} ->
      io:format("~p~n",[Message]),
      listen();
    stop ->
      true
  end.
