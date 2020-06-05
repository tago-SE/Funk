%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. juni 2020 09:18
%%%-------------------------------------------------------------------
-module(ss_docking_station_TEST).
-author("tiago").

%% API
-export([all/0]).

%% Execute to test the functionality related to 3.1.
%% Usage: ss_docking_station_TEST:all().
all() ->
  io:format("Testing ss_docking_station...\n"),

  % A docking station is started with 5 slots and one occupied
  {ok, Pid} = ss_docking_station:start_link(5, 1, "StationOne"),

  secure_scooter_test(Pid),
  release_scooter_test(Pid).


secure_scooter_test(Pid) ->
  io:format("secure_scooter_test~n", []),
  io:format(" 1~n", []),
  {ok,{5,2,"StationOne",idle,none}} = ss_docking_station:secure_scooter(Pid),
  {ok,{5,3,"StationOne",idle,none}} = ss_docking_station:secure_scooter(Pid),
  {ok,{5,4,"StationOne",idle,none}} = ss_docking_station:secure_scooter(Pid),

  % The docking station should now be full
  io:format(" 2~n", []),
  {ok,{5,5,"StationOne",full,none}} = ss_docking_station:secure_scooter(Pid),

  % if we try to add another it should return an error
  io:format(" 3~n", []),
  {error, full} = ss_docking_station:secure_scooter(Pid),

  io:format(" - ok\n").


release_scooter_test(Pid) ->
  io:format(" release_Scooter_test~n", []),

  % We start with a full docking station, so when one is released it should no longer be full
  io:format(" 1~n", []),
  {ok,{5,4,"StationOne",idle,none}} = ss_docking_station:release_scooter(Pid),
  io:format(" 2~n", []),
  {ok,{5,3,"StationOne",idle,none}} = ss_docking_station:release_scooter(Pid),
  io:format(" 3~n", []),
  {ok,{5,2,"StationOne",idle,none}} = ss_docking_station:release_scooter(Pid),
  io:format(" 4~n", []),
  {ok,{5,1,"StationOne",idle,none}} = ss_docking_station:release_scooter(Pid),

  % Finally, if we release the last station it should return to an empty state
  io:format(" 5~n", []),
  {ok,{5,0,"StationOne",empty,none}} = ss_docking_station:release_scooter(Pid),

  % Lets test if the state is as it says it is...
  State = ss_docking_station:get_info(Pid),
  io:format("State: ~p~n", [State]),
  {ok,[{total,5},{occupied,0},{state,empty},{free,5}]} = ss_docking_station:get_info(Pid),

  io:format(" - ok\n").




