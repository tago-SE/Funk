%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: db_TEST.erl
%%% @author trainers@erlang-solutions.com
%%% @copyright 1999-2013 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Test the 'db' module. Has the following interface:
%%%
%%% db_TEST:all() -
%%%     runs all tests.
%%%
%%% db_TEST:insert_read_all() -
%%%     insert 3 elements and read them. Also test reading a
%%%     non-existent key.
%%%
%%% db_TEST:insert_delete_one() -
%%%     insert 3 elements and delete each of them. Also test deleting
%%%     a non-existent key.
%%%
%%% db_TEST:insert_delete_all() -
%%%     insert 3 elements and delete all of them but in different
%%%     order. All should return empty database.
%%%
%%% db_TEST:insert_overwrite_one() -
%%%     insert 3 elements and overwrite each of them one at a time.
%%%
%%% db_TEST:insert_overwrite_all() -
%%%     insert 3 elements and overwrite all of them.

-module(tree_TEST).
-export([insert_read_all/0, insert_delete_one/0, insert_delete_all/0,
    insert_overwrite_one/0, insert_overwrite_all/0, init_db/1, write_db/2, insert_print_all/0, all/0]).

%% all() -> ok.
%%  Run all the tests.

all() ->
    io:format("Tree test...\n"),
    insert_print_all(),
    insert_read_all(),
    insert_delete_one(),
    insert_delete_all(),
    insert_overwrite_one(),
    insert_overwrite_all(),
    match_all().

match_all() ->
    io:format("Running match_all ~n", []),
    Db = init_db([{curt,1},{bert,2},{sune,3}, {jerry, 1}]),
    Result = tree:match(1, Db),
    io:format("~p~n", [Result]),
    io:format(" - ok\n").

%% insert_read_all() -> ok.
%%  Insert standard 3 elements and reads them.

insert_print_all() ->
    io:format("Running insert_print_all ~n", []),
    Db = init_db([{curt,1},{bert,2},{sune,3}]),
    tree:print(Db).

insert_read_all() ->
    io:format("Running insert_read_all ~n", []),
    Db0 = init_db([{curt,1},{bert,2},{sune,3}]),

    io:format(" 1", []),
    {ok,1} = tree:read(curt, Db0),

    io:format(" 2", []),
    {ok,2} = tree:read(bert, Db0),

    io:format(" 3", []),
    {ok,3} = tree:read(sune, Db0),

    io:format(" 4", []),			%Read non-existent key
    {error,instance} = tree:read(sten, Db0),

    io:format(" - ok\n").

%% insert_delete_one() -> ok.
%%  Insert standard 3 elements and try deleting one of them.

insert_delete_one() ->
    io:format("Running insert_delete_one", []),
    Db0 = init_db([{curt,1},{bert,2},{sune,3}]),

    io:format(" 1", []),
    {ok,1} = tree:read(curt, Db0),
    {error, instance} = tree:read(curt, tree:delete(curt, Db0)),

    io:format(" 2", []),
    {ok,2} = tree:read(bert, Db0),
    {error,instance} = tree:read(bert, tree:delete(bert, Db0)),

    io:format(" 3", []),
    {ok,3} = tree:read(sune, Db0),
    {error,instance} = tree:read(sune, tree:delete(sune, Db0)),

    io:format(" 4", []),			%Delete non-existent key
    {error,instance} = tree:read(sten, Db0),
    {error,instance} = tree:read(sten, tree:delete(sten, Db0)),

    io:format(" - ok\n").

%% insert_delete_all() -> ok.
%%  Insert standard 3 elements and try deleting all of them in
%%  different orders. Should return the empty database.

insert_delete_all() ->
    io:format("Running insert_delete_all~n", []),
    Empty = tree:new(),
    Db0 = init_db([{curt,1},{bert,2},{sune,3}]),

    io:format(" 1", []),
    Empty = lists:foldl(fun (K, D) -> tree:delete(K, D) end,
			Db0, [curt,bert,sune]),
    io:format(" 2", []),
    Empty = lists:foldl(fun (K, D) -> tree:delete(K, D) end,
			Db0, [bert,sune,curt]),
    io:format(" 3", []),
    Empty = lists:foldl(fun (K, D) -> tree:delete(K, D) end,
			Db0, [sune,bert,curt]),
    io:format(" - ok\n").

%% insert_overwrite_one() -> ok.
%%  Insert standard 3 elements and try overwriting one of them.

insert_overwrite_one() ->
    io:format("Running insert_overwrite_one", []),
    Db0 = init_db([{curt,1},{bert,2},{sune,3}]),

    io:format(" 1", []),
    {ok,1} = tree:read(curt, Db0),
    {ok,10} = tree:read(curt, tree:write(curt, 10, Db0)),

    io:format(" 2", []),
    {ok,2} = tree:read(bert, Db0),
    {ok,20} = tree:read(bert, tree:write(bert, 20, Db0)),

    io:format(" 3", []),
    {ok,3} = tree:read(sune, Db0),
    {ok,30} = tree:read(sune, tree:write(sune, 30, Db0)),

    io:format(" - ok\n").

%% insert_overwrite_all() -> ok.
%%  Insert standard 3 elements and try overwriting all of them.

insert_overwrite_all() ->
    io:format("Running insert_overwrite_all", []),
    Db0 = init_db([{curt,1},{bert,2},{sune,3}]),
    Db1 = write_db([{curt,10},{bert,20},{sune,30}], Db0),

    io:format(" 1", []),
    {ok,10} = tree:read(curt, Db1),

    io:format(" 2", []),
    {ok,20} = tree:read(bert, Db1),

    io:format(" 3", []),
    {ok,30} = tree:read(sune, Db1),

    io:format(" 4", []),			%Read non-existent key
    {error,instance} = tree:read(sten, Db1),
    io:format(" - ok\n").

%% init_db([{Key,Val}]) -> Database.
%%  Create and initialise a database with keys curt, bert and sune.

init_db([]) ->
    tree:new();
init_db(Vals = [_Head | _Tail]) ->
    Db0 = tree:new(),
    write_db(Vals, Db0).

%% write_db([{Key,Val}], Database) -> Database.
%%  Writes in all key-val into existing database.
write_db([Head | Tail], Db) ->
    {Key, Element} = Head,
    Db1 = tree:write(Key, Element, Db),
    write_db(Tail, Db1);

write_db([], Db) ->
    io:format(" - ok\n"),
    Db.