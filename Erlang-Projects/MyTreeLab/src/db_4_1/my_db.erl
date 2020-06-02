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
-export([finish/0, listen/1, start/0, write/2, print/0, delete/1, read/1, match/1]).

%% Start the server.
-spec start() -> ok.
start() ->
  Db0 = init_db([{curt,1},{bert,2},{sune,3}]),
  io:format("~p ok\n", [Db0]),
  register(?MODULE, spawn(?MODULE, listen, [Db0])).


%% TEST CODE
init_db([]) ->
  tree:new();
init_db(Vals = [_Head | _Tail]) ->
  Db0 = tree:new(),
  write_db(Vals, Db0).

%% write_db([{Key,Val}], Database) -> Database.e
%%  Writes in all key-val into existing database.
write_db([Head | Tail], Db) ->
  {Key, Element} = Head,
  Db1 = tree:write(Key, Element, Db),
  write_db(Tail, Db1);

write_db([], Db) ->
  io:format(" - ok\n"),
  Db.

%% END TEST CODE

%% Stops the server.
-spec finish() -> ok.
finish() ->
  ?MODULE ! {finish, self()},
  ok.


%% Client: Request write to the server repository, storing element at key location.
-spec write(term(), term()) -> ok.
write(Key, Element) ->
  io:format("Client write: Key=~p Value=~p\n", [Key, Element]),
  ?MODULE ! {write, Key, Element},
  ok.


%% Client: Write to the server repository, storing element at key location.
-spec print() -> ok.
print() ->
  io:format("Client print\n"),
  ?MODULE ! {print},
  ok.


%% Delete from the server repository by key.
-spec delete(term()) -> ok.
delete(Key) ->
  ?MODULE ! {delete, Key},
  ok.


%% Read from the server repository by key.
-spec read(term()) -> ok.
read(Key) ->
  % Self is used to pass the PID to be able to respond to the client.
  ?MODULE ! {read, Key, self()},
  receive
    {reply, Db } -> Db;
    {error, Reason} -> {error, Reason}
  end.


%% List all keys of matching query element
-spec match(term()) -> ok.
match(Element) ->
  ?MODULE ! {match, Element, self()},
  receive
    {reply, Db } -> Db;
    {error, Reason} -> {error, Reason}
  end.


%% Server main loop listens to client commands
listen(Db)->
  receive

    {print} ->
      io:format("DB: ~p~n", [Db]),
      listen(Db);

    {write, Key, Element} ->
      Db1 = tree:write(Key, Element, Db),
      listen(Db1);

    {delete, Key} ->
      io:format("Delete=~p~n",[Key]),
      Db1 = tree:delete(Key, Db),
      listen(Db1);

    {read, Key, ClientProcessId} ->
      io:format("Read=~p~n",[Key]),
      Result = tree:read(Key, Db),
      if
        Result == undefined ->
          Error = {error, undefined},
          ClientProcessId ! {error, Error};
        true ->
          ClientProcessId ! {reply, Result}
      end,
      listen(Db);

    {match, Element, ClientProcessId} ->
      Result = tree:match(Element, Db),
      ClientProcessId ! {reply, Result},
      listen(Db);

    {finish, ClientProcessId} ->
      io:format("Server stopped~n"),
      true

  end.


