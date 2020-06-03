%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. juni 2020 18:15
%%%
%%% https://gist.github.com/caingougou/62749634129c12dfac66
%%%-------------------------------------------------------------------
-module(ss_supervisor).
-behaviour(supervisor).
-author("tiago").

%% API
-export([init/1, start_link/0, start_child/2, release_scooter/1, secure_scooter/1]).

%% {ok, SupPid} = ss_supervisor:start_link().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_child(Total, Occupied) ->
  {ok, _Pid } = supervisor:start_child(?MODULE, [Total, Occupied, "Unknown"]).


release_scooter(Pid) ->
  ss_docking_station:release_scooter(Pid).


secure_scooter(Pid) ->
  ss_docking_station:secure_scooter(Pid).


init(_Args) ->
  {ok, {#{strategy => simple_one_for_one,
    intensity => 2, period => 3600},
    [#{id => ss_docking_station, start => {ss_docking_station, start_link, []},
      restart => permanent, shutdown => 2000,
      type => worker, modules => [ss_docking_station]}]}}.