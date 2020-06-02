%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. maj 2020 21:06
%%
%% 1: https://www.youtube.com/watch?v=Sb9FqbW8HTo
%%    Tutorial used for configuring Erlang in intelliJ, also contains info on how to import a project.
%%
%% 2: http://erlang.org/doc/man/gen_server.html
%%
%% 3: https://learnyousomeerlang.com/clients-and-servers
%%
%%%-------------------------------------------------------------------
-module(my_gen_server).
-author("tiago").

%% Implements client-server behaviour
-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/0, write/3, print/1, delete/2, read/2, match/2, close/1]).

%%%-------------------------------------------------------------------
%%   Client API
%%%-------------------------------------------------------------------


%% Starts a generic server
% Example usage:
%   {ok, Pid} = my_gen_server:start_link().
start_link() ->
  gen_server:start_link(?MODULE, [], []).


close(Pid) ->
  gen_server:call(Pid, terminate).

%% Creates a new entry into the database containing a key and a value
%
% Example usage:
%   :write(Pid, 1, "Hello").
-spec write(term(), term(), term()) -> ok.
write(Pid, Key, Element) ->
  gen_server:call(Pid, {write, Key, Element}). % Sends a synchronous request to the server


%% Prints the database at the server side
%
% Example usage:
%   :print(Pid).
-spec print(term()) -> ok.
print(Pid) ->
  gen_server:call(Pid, print).


%% Delete from the server repository by key.
%
% Example usage:
%   :write(Pid, 1, "Hello").
-spec delete(term(), term()) -> ok.
delete(Pid, Key) ->
  gen_server:call(Pid, {delete, Key}).


%% Read from the server repository by key.
%
% Example usage:
%   :read(Pid, 1).
-spec read(term(), term()) -> ok.
read(Pid, Key) ->
  gen_server:call(Pid, {read, Key}).


%% List all keys of matching query element
% Example usage:
%   :read(Pid, "hello").
-spec match(term(), term()) -> ok.
match(Pid, Element) ->
  gen_server:call(Pid, {match, Element}).


%%%-------------------------------------------------------------------
%%   Server Side
%%%-------------------------------------------------------------------

%% Allocates a empty database when the server starts
init(_Args) ->
  % Returns the status and an empty tree database
  % Alternative return values include: {ok State}, {ok, State, TimeOut}, {ok, State,
  {ok,  _Db = tree:new()}.

handle_call({write, Key, Element}, _From, Db) ->
  io:format("Server_onWrite: key=~p value=~p ~n", [Key, Element]),
  Db1 = tree:write(Key, Element, Db),
  {reply, ok, Db1};

handle_call(print, _From, Db) ->
  io:format("Server_onPrint: ~p~n", [Db]),
  {reply, ok, Db};

handle_call({delete, Key}, _From, Db) ->
  io:format("Server_onDelete: ~p~n", [Key]),
  Db1 = tree:delete(Key, Db),
  {reply, ok, Db1};

handle_call({read, Key}, _From, Db) ->
  io:format("Server_onRead ~p~n", [Key]),
  Result = tree:read(Key, Db),
  if
    Result == undefined ->
      {reply, {error, undefined, Db}};
    true ->
      {reply, Result, Db}
  end;

handle_call({match, Element}, _From, Db) ->
  io:format("Server_onMatch ~p~n", [Element]),
  {reply, tree:match(Element, Db), Db};

handle_call(terminate, _From, Db) ->
  {stop, normal, ok, Db}.

%% gen_server function
handle_cast(_Request, _State) ->
  io:format("handle_cast~n").

