%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. maj 2020 15:4
%%
%% 1: https://www.youtube.com/watch?v=Sb9FqbW8HTo
%%    Tutorial used for configuring Erlang in intelliJ, also contains info on how to import a project.
%%
%% 2: http://erlang.org/doc/man/gen_server.html
%%
%% 3: https://learnyousomeerlang.com/clients-and-servers
%%
%%%-------------------------------------------------------------------
-module(ss_docking_station).
-author("tiago").

%% API functions
-export([init/1, handle_call/3, start_link/3, release_scooter/1, secure_scooter/1, terminate/2, get_info/1, handle_event/3, handle_cast/2]).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% @doc
%% Creates a docking station server which calls Module:init/1 to initialize.
%% To ensure a synchronized start-up procedure, this function does not return
%% until Module:init/1 has returned.
%%
%% Example usage:
%%   Pid = ss_docking_station:start_link(100, 0, "DockOne").
%%
%% @spec start_link(Total, Occupied, Name) -> {ok, Pid} | {error, Error}
%%
%% Conditional: Total > 0 and Occupied <= Total
%%
%% @end
%%--------------------------------------------------------------------
start_link(Total, _Occupied, _Name) when Total =< 0 ->
  {error, "Total must be greater than 0"};
start_link(Total, Occupied, _Name) when Occupied > Total ->
  {error, "Occupied must be less than or equal to Total"};
start_link(Total, Occupied, Name) ->
  {ok, Pid} = gen_server:start_link(?MODULE, {Total, Occupied, Name}, []).

%% @doc
%% Releases a scooter from the docking station. Returns the atom ok on success or the tuple {error, empty} if there are
%% no scooters available in the docking station.
%%
%% @spec release_scooter(Pid) -> ok | {error, empty}
%% @end
release_scooter(Pid) ->
  gen_server:call(Pid, release).

%% @doc
%% Secure scooter is used to park a scooter in the docking station. Returns the atom ok on success or the tuple
%% {error, full} if there where no empty docking stations in which to secure the scooter.
%%
%% @spec secure_scooter(Pid) -> ok | {error, full}
%% @end
secure_scooter(Pid) ->
  gen_server:call(Pid, secure).

%% @doc
%% Returns information about the docking station. Such as total number of docking points and how many are occupied,
%% the name of the docking station and the current state.
%%
%% @spec secure_scooter(Pid) -> ok | {error, full}
%% @end
get_info(Pid) ->
  gen_server:call(Pid, help).

%%%-------------------------------------------------------------------
%%   Server Side
%%%-------------------------------------------------------------------

%% Allocates the state of the docking station on setup
init(Args) ->
  {Total, Occupied, Name} = Args,
  Data = {Total, Occupied, Name, calc_state(Total, Occupied)},
  io:format("Server_onInit ~p ~n", [Data]),
  {ok,  Data}.

terminate(normal, _State) ->
  io:format("Docking station ended.~n"),
  ok.

handle_call(help, _From, Data) ->
  {Total, Occupied, _Name, State} = Data,
  Response = [{total, Total}, {occupied, Occupied}, {state, State}],
  {reply, {ok, Response}, Data};
handle_call(release, _From, Data) ->
  handle_event(release, get_state(Data), Data);
handle_call(secure, _From, Data) ->
  handle_event(secure, get_state(Data), Data).

%%%===================================================================
%%% Events
%%%===================================================================

handle_event(Event, State, Data) when State == idle ->
  io:format("server_onHandleEvent: ~p~n", [idle]),
  idle(Event, Data);
handle_event(Event, State, Data) when State == empty ->
  io:format("server_onHandleEvent: ~p~n", [empty]),
  empty(Event, Data);
handle_event(Event, State, Data) when State == full ->
  io:format("server_onHandleEvent: ~p~n", [full]),
  full(Event, Data).

%%%===================================================================
%%% States
%%%===================================================================

idle(Event, Data)  when Event == secure ->
  {Total, Occupied, Name, _State} = Data,
  NewData = {Total, Occupied + 1, Name, calc_state(Total, Occupied + 1)},
  {reply, {ok, NewData}, NewData};
idle(Event, Data)  when Event == release ->
  {Total, Occupied, Name, _State} = Data,
  NewData = {Total, Occupied - 1, Name, calc_state(Total, Occupied - 1)},
  {reply, {ok, NewData}, NewData}.


empty(Event, Data)  when Event == secure ->
  {Total, Occupied, Name, _State} = Data,
  NewData = {Total, Occupied + 1, Name, calc_state(Total, Occupied + 1)},
  {reply, {ok, NewData}, NewData};
empty(Event, Data)  when Event == release ->
  {reply, {error, empty}, Data}.


full(Event, Data) when Event == secure ->
  {reply, {error, full}, Data};
full(Event, Data) when Event == release ->
  {Total, Occupied, Name, _State} = Data,
  NewData = {Total, Occupied - 1, Name, calc_state(Total, Occupied - 1)},
  {reply, {ok, NewData}, NewData}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_state(Data) ->
  {_Total, _Occupied, _Name, State} = Data,
  State.

calc_state(Total, Occupied) when Total == Occupied ->
  full;
calc_state(_Total, Occupied) when Occupied == 0 ->
  empty;
calc_state(Total, Occupied) when Occupied < Total ->
  idle.

handle_cast(Request, State) ->
  erlang:error(not_implemented).