%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. juni 2020 18:15
%%%
%%% https://gist.github.com/caingougou/62749634129c12dfac66
%%%
%%% https://stackoverflow.com/questions/9772357/monitoring-a-gen-server
%%%-------------------------------------------------------------------
-module(ss_supervisor).
-behaviour(supervisor).
-author("tiago").

%% API
-export([init/1, start_link/0, start_child/3, release_scooter/1, secure_scooter/1, terminate_child/2, count_children/1, monitor/1]).

%% {ok, SupPid} = ss_supervisor:start_link().
start_link() ->
  {ok, _SupPid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Total, Occupied, Name) ->
  {ok, WorkerPid } = supervisor:start_child(?MODULE, [Total, Occupied, Name]),
  Monitor = spawn(?MODULE, monitor, [WorkerPid]),
  % Monitor ! update,   % Requests the monitor to fetch latest updates...
  ss_docking_station:add_monitor(WorkerPid, Monitor),
  {ok, WorkerPid}.


monitor(Proc) ->
  io:format("Monitor started...~n"),
  erlang:monitor(process, Proc),
  %% This is the DB that would be used if update messages were being received properly...
  {ok, DbRef} = my_gen_server:start_link(),
  receive
    {update, Data} ->
      io:format("UpdateReceived: ~p~n", [Data]);
    {'DOWN', Ref, process, Pid,  normal} ->
      io:format("~p said that ~p died by natural causes~n",[Ref, Pid]);
    {'DOWN', Ref, process, Pid,  Reason} ->
      io:format("~p said that ~p died by unnatural causes~n~p",[Ref, Pid,Reason])
  end.


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


