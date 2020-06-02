%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. mars 2020 21:29

%%% Example Code
%%% Concurrency semantics - Spawns processes, passes messages, registering and termination.
%%%-------------------------------------------------------------------
-module(tut15).
-author("tiago").

%% API
-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
  Pong_PID ! finished,
  io:format("ping finished~n", []);

ping(N, Pong_PID) ->
  Pong_PID ! {ping, self()},
  receive
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N - 1, Pong_PID).

pong() ->
  receive
    finished ->
      io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
      %% Creates a p
      io:format("Pong received ping~n", []),
      Ping_PID ! pong,
      pong()
  end.

%% Creates a process called pong and executes ping
start() ->
  Pong_PID = spawn(tut15, pong, []),
  spawn(tut15, ping, [3, Pong_PID]).
