%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. feb. 2020 15:54
%%%
%%%   This is the database provided by the solutions.pdf
%%%-------------------------------------------------------------------
-module(db).
-author("Franceso Ceasarini & Simon Thompson").

%% API
-export([new/0, write/3, delete/2, destroy/1, match/2, read/2]).
-export_type([db/0]).
-type db() :: list().

%% @doc Create a new database
-spec new() -> db().
new() ->
  [].

%% @doc Insert a new element in the database
-spec write(Key::term(), Val::term(), db()) -> db().
write(Key, Element, []) ->
  [{Key, Element}];
write(Key, Element, [{Key, _} | Db]) ->
  [{Key, Element}|Db];
write(Key, Element, [Current | Db]) ->
  [Current | write(Key, Element, Db)].

%% @doc Remove an element from the database 27 -spec delete(Key::term(), db()) -> db().
-spec delete(Key::term(), db()) -> db().
delete(Key, [{Key, _Element}|Db]) ->
  Db;
delete(Key, [Tuple|Db]) ->
  [Tuple|delete(Key, Db)];
delete(_Key, []) ->
  [].

%% @doc Retrieve the first element in the database with a matching key
-spec read(Key::term(), db()) -> {ok, term()} | {error, instance}.
read(Key, [{Key, Element}|_Db]) ->
  {ok, Element};
read(Key, [_Tuple|Db]) ->
  read(Key, Db);
read(_Key, []) ->
  {error, instance}.

%% @doc Return all the keys whose values match the given element.
-spec match(Val::term(), db()) -> [term()].
match(Element, [{Key, Element}|Db]) ->
  [Key|match(Element, Db)];
match(Element, [_Tuple|Db]) ->
  match(Element, Db);
match(_Key, []) ->
  [].

%% @doc Deletes the database.
-spec destroy(db()) -> ok.
destroy(_Db) ->
  ok.
