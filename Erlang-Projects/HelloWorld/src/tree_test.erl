%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. feb. 2020 19:04
%%%-------------------------------------------------------------------
-module(tree_test).
-author("tiago").

%% API
-export([write_test/0, write_test2/0, write_test3/0, write_test4/0, write_test5/0, print_test/0, delete_test/0, smallest/0, smallest_2/0, delete/0]).

write_test() ->
  _Tree = tree:new(),
  tree:write(3, "Hello", _Tree).

% Insert left branch
write_test2() ->
  _Tree = tree:new(),
  _Tree2 =  tree:write(3, "Hello", _Tree),
  _Tree3 =  tree:write(2, "There", _Tree2).

% Insert right branch
write_test3() ->
  _Tree = tree:new(),
  _Tree2 =  tree:write(3, "Hello", _Tree),
  _Tree3 =  tree:write(5, "Yooo", _Tree2).

% Insert Two Branches
write_test4() ->
  _Tree = tree:new(),
  _Tree2 =  tree:write(3, "Hello", _Tree),
  _Tree3 =  tree:write(2, "There", _Tree2),
  _Tree4 =  tree:write(5, "Yooo", _Tree3).

write_test5() ->
  init().

print_test() ->
  tree:print(init()).

delete_test() ->
  _Tree = tree:new(),
  _Tree2 =  tree:write(3, "Hello", _Tree),
  _Tree3 =  tree:write(2, "There", _Tree2),
  _Tree4 =  tree:write(5, "Yooo", _Tree3),
  _Tree5 = tree:write(1, "left-small", _Tree4),
  tree:delete(3, _Tree5),
  tree:delete(2, _Tree5),
  tree:delete(5, _Tree5),
  tree:delete(1, _Tree5),
  tree:delete(7, _Tree5).

smallest() ->
  _Tree = tree:new(),
  _Tree2 =  tree:write(3, "Hello", _Tree),
  _Tree3 =  tree:write(2, "There", _Tree2),
  _Tree4 =  tree:write(5, "Yooo", _Tree3),
  _Tree5 = tree:write(1, "left-small", _Tree4),
  tree:find_min(_Tree5).


smallest_2() ->
  _Tree = tree:new(),
  _Tree2 =  tree:write(3, "Hello", _Tree),
  _Tree3 =  tree:write(2, "There", _Tree2),
  _Tree4 =  tree:write(5, "Yooo", _Tree3),
  _Tree5 = tree:write(1, "left-small", _Tree4),
  tree:find_second_min(_Tree5).

init() ->
  _Tree = tree:new(),
  _Tree2 =  tree:write(3, "Hello", _Tree),
  _Tree3 =  tree:write(2, "There", _Tree2),
  _Tree4 =  tree:write(5, "Yooo", _Tree3),
  _Tree5 = tree:write(1, "left-small", _Tree4).

delete() ->
  tree:delete(3, init()).