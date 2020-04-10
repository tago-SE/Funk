%%%-------------------------------------------------------------------
%%% @author tiago
%%% @doc
%%%
%%% @end
%%% Created : 24. mar. 2020 15:54
%%%
%%% Interface is based on the interface provided from 2.4 (db.erl).
%%%-------------------------------------------------------------------
-module(tree).
-author("Tiago Redaelli").

%% API
-export([new/0, write/3, delete/2, destroy/1, match/2, read/2, print/1]).
-export_type([tree/0]).
-type tree() :: tuple().
-define(EMPTY_TREE, {null}).


%% @doc Create a new database
-spec new() -> tree().

new() ->
  ?EMPTY_TREE.

%% @doc Insert a new element in the database
-spec write(Key::term(), Val::term(), tree()) -> tree().

% If the Tree is empty we add the key and value and create the leafs.
write(Key, Element, ?EMPTY_TREE) ->
  {Key, Element, new(), new()};

% If the tree is not empty, we add a sub-tree either to the left or to the right
write(Key, Element, {NodeKey, NodeElement, LeftBranch, RightBranch}) ->
  if
    (Key < NodeKey) ->
      {NodeKey, NodeElement, write(Key, Element, LeftBranch), RightBranch};
    (Key > NodeKey) ->
      {NodeKey, NodeElement, LeftBranch, write(Key, Element, RightBranch) };
    true -> % Keys are equal, replace
      {Key, Element, LeftBranch, RightBranch}
  end.


%% @doc Remove an element from the database 27 -spec delete(Key::term(), db()) -> db().
-spec delete(Key::term(), tree()) -> tree().

% if empty return the Tree
delete(_Key, Tree = ?EMPTY_TREE) ->
  Tree;

delete(Key, Tree = {NodeKey, NodeElement, LeftBranch, RightBranch}) ->
  delete_node(Key, Tree).

delete_node(Key, {NodeKey, NodeElement, LeftBranch, RightBranch}) when Key == NodeKey ->
  if
  % Case 1: The node to be removed has no sub-trees, so we simply remove the node
    LeftBranch == ?EMPTY_TREE andalso RightBranch == ?EMPTY_TREE ->
      ?EMPTY_TREE;

  % Case 2: The node to be removed has one sub-tree, replace the node with the sub-tree
    LeftBranch == ?EMPTY_TREE ->
      RightBranch;
    RightBranch == ?EMPTY_TREE ->
      LeftBranch;
    true ->
      % The node to be deleted has two sub-trees
      {RightKey, RightElement, RLeftBranch, RRightBranch} = RightBranch,
      if
        RLeftBranch == ?EMPTY_TREE ->
          % Since the right node of the Node that is being deleted has no left branch we simply replace
          % the node that is being removed with the right branch.
          {RightKey, RightElement, LeftBranch, RRightBranch};
        true ->
          % We find the smallest node on the right side of the one that is being deleted
          {SKey, SElement, SLeftBranch, SRightBranch} = find_min(RightBranch),
          % We find the parent of the smallest node
          Temp = {PKey, PElement, PLeftBranch, PRightBranch} = find_second_min(RightBranch),
          % We make the parent left branch point to the smallest tree
          Temp = {PKey, PElement, {SKey, SElement, SLeftBranch, SRightBranch}, PRightBranch},
          % The smallest node exist in two places, and is therefore removed
          {SKey, SElement, LeftBranch, delete_node(SKey, RightBranch)}
      end
  end;

delete_node(Key, {NodeKey, NodeElement, LeftBranch, RightBranch}) when Key < NodeKey ->
  {NodeKey, NodeElement, delete_node(Key, LeftBranch), RightBranch};

delete_node(Key, {NodeKey, NodeElement, LeftBranch, RightBranch}) when Key > NodeKey ->
  {NodeKey, NodeElement, LeftBranch, delete_node(Key, RightBranch)};

delete_node(_Key, EmptyTree = ?EMPTY_TREE) ->
  EmptyTree.

find_min({_Key, _Element, LeftBranch, _RightBranch}) when LeftBranch /= ?EMPTY_TREE ->
  find_min(LeftBranch);
find_min(Tree) ->
  Tree.

find_second_min(Tree = {_, _, {_, _, LeftBranch, _}, _}) when LeftBranch == ?EMPTY_TREE ->
  Tree;
find_second_min({_Key, _Element, LeftBranch, _RightBranch}) ->
  find_second_min(LeftBranch).


%% @doc Retrieve the first element in the database with a matching key
-spec read(Key::term(), tree()) -> {ok, term()} | {error, instance}.

%read(Key, [{Key, Element} | _Db]) ->
%  {ok, Element};
%read(Key, [_Tuple|Db]) ->
%  read(Key, Db);
%read(_Key, []) ->
%  {error, instance}.

% if empty return
read(_Key, ?EMPTY_TREE) ->
  {error, instance};
read(Key, {NodeKey, NodeElement, _LeftBranch, _RightBranch}) when Key == NodeKey ->
  {ok, NodeElement};
read(Key, {NodeKey, NodeElement, _LeftBranch, _RightBranch}) when Key > NodeKey ->
  read(Key, _RightBranch);
read(Key, {NodeKey, NodeElement, _LeftBranch, _RightBranch}) when Key < NodeKey ->
  read(Key, _LeftBranch).

%% @doc Return all the keys whose values match the given element.
-spec match(Val::term(), tree()) -> [term()].
match(Element, {NodeKey, NodeElement, LeftBranch, RightBranch}) when Element == NodeElement ->
  [NodeKey | match(Element, LeftBranch) ++ match(Element, RightBranch)];
match(Element, {NodeKey, NodeElement, LeftBranch, RightBranch}) ->
  match(Element, LeftBranch) ++ match(Element, RightBranch);
match(_Key, ?EMPTY_TREE) ->
  [].

print(?EMPTY_TREE) ->
  io:format("null~n");
print({NodeKey, NodeValue, LeftBranch, RightBranch}) ->
  io:format("~p~p~n", [NodeKey, NodeValue]),
  print(LeftBranch),
  print(RightBranch).

%% @doc Deletes the database.
-spec destroy(tree()) -> ok.

destroy(_Db) ->
  ok.
