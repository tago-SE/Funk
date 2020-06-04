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
-module(ss_supervisor_old).
-behaviour(supervisor).
-author("tiago").

%% API
-export([init/1, start_link/0, start_child/3, release_scooter/1, secure_scooter/1, terminate_child/2, count_children/1]).

%% {ok, SupPid} = ss_supervisor:start_link().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_child(Total, Occupied, Name) ->
  {ok, _WorkerPid } = supervisor:start_child(?MODULE, [Total, Occupied, Name]).

release_scooter(WorkerPid) ->
  _Result = ss_docking_station:release_scooter(WorkerPid).


secure_scooter(WorkerPid) ->
  _Result = ss_docking_station:secure_scooter(WorkerPid).


count_children(SupPid) ->
  supervisor:count_children(SupPid).


terminate_child(SupPid, WorkerPid) ->
  supervisor:terminate_child(SupPid, WorkerPid).


%% Initial callback function which is called by the supervisor module.
init(_Args) ->
  {ok,
    {#{strategy => simple_one_for_one,  % RestartStrategy
    intensity => 2,                     % MaxRestart
    period => 3600},                    % MaxTime

      % Notice that for temporary children, the child specification is automatically deleted when the child terminates;
      % thus, it is not possible to restart such children.

    [#{id => ss_docking_station, start => {ss_docking_station, start_link, []},
      restart => permanent, shutdown => 2000,
      type => worker, modules => [ss_docking_station]}]}}.


