%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. feb. 2020 11:58
%%
%% Notes: Tutorial used for configuring Erlang in intelliJ, also contains info on how to import a project.
%% Ref: https://www.youtube.com/watch?v=Sb9FqbW8HTo
%%
%%
%% Object Description

%% ## Assignment (1) 2.7 - Database Handling Using Trees
%%

%% ## Assignment (2) 4.1 - Database Server
%%
%%  Write a db server that stores it in its loop data. Should register server access through a functional interface.
%%
%%  See: my_db.erl from 3.1 (An Echo Server)
%%
%% API:

%% start() → ok.

%% stop() → ok.

%% write(Key, Element) → ok.

%% delete(Key) → ok.

%% read(Key) → {ok, Element} | {error, instance}.

%% match(Element) → [Key1, ..., KeyN].

%% Lecture Refs:
%%
%%%-------------------------------------------------------------------

-module(my_db).
-author("tiago").

%% API
-export([start/0, stop/0, listen/0, write/2, delete/1, read/1, match/1]).


%% Start the server.
-spec start() -> ok.
start() ->
  register(echo, spawn(?MODULE, listen, [])).


%% Stops the server.
-spec stop() -> ok.
stop()->
  echo ! stop,
  ok.


%% Write to the server repository, storing element at key location.
-spec write(term(), term()) -> ok.
write(Key, Element) ->
  echo ! {write, Key, Element},
  ok.


%% Delete from the server repository by key.
-spec delete(term()) -> ok.
delete(Key) ->
  echo ! {delete, Key},
  ok.


%% Read from the server repository by key.
-spec read(term()) -> ok.
read(Key) ->
  echo ! {read, Key}.


%% List all matching elements
-spec match(term()) -> ok.
match(Element) ->
  echo ! {match, Element}.


%% The server loop
-spec listen() -> true.
listen()->
  receive
    {write, Key, Element} ->
      io:format("K=~p, E=~p~n",[Key, Element]),
      listen();

    {delete, Key} ->
      io:format("Delete=~p~n",[Key]),
      listen();

    {read, Key} ->
      io:format("Read=~p~n",[Key]),
      echo ! {reply, ok, "Element"},
      listen();

    {match, Element} ->
      io:format("Match=~p~n",[Element]),
      echo ! {reply, ok, "Element"},
      listen();

    stop -> %% terminates the server
      io:format("Stopping Server...~n"),
      true
  end.




