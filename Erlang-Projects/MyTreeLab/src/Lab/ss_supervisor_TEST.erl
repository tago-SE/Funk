%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. juni 2020 09:43
%%%-------------------------------------------------------------------
-module(ss_supervisor_TEST).
-author("tiago").

%% API
-export([all/0]).

all() ->
  io:format("Testing ss_docking_station...\n"),

  % A docking station is started with 5 slots and one occupied
  {ok, SupPid} = ss_supervisor:start_link(),
  io:format("Created supervisor with ref: ~p~n", [SupPid]),

  {ok, ChildA} = ss_supervisor:start_child(5, 1, "A"),
  io:format("Created child with ref: ~p~n", [ChildA]),

  {ok, ChildB} = ss_supervisor:start_child(5, 1, "A"),
  io:format("Created child with ref: ~p~n", [ChildB]),

  % Notice that the child processes successfully sends updates to the supervisor: "UpdateReceived: {5,1,"A",idle,<0.80.0>}"


  %Result = ss_supervisor:release_scooter(ChildA),
  Result = ss_docking_station:release_scooter(ChildA),
  io:format("Released scooter from A: ~p~n", [Result]),

  io:format(" - ok\n").