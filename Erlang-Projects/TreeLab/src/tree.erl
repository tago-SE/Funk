%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. feb. 2020 16:17
%%%-------------------------------------------------------------------
-module(tree).
-author("tiago").
-export_type([tree/0]).

-define(EMPTY_TREE, {null}).

-type tree() :: tuple().



%% API
-export([new/0, write/3, test/0, print/1, delete/2, find/2, find_parent/3, delete_node/2, find_min/1, find_second_min/1]).


%% @doc Create a new database
-spec new() -> tree().
new() ->
  ?EMPTY_TREE.

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

print(?EMPTY_TREE) ->
  io:format("null~n");
print({NodeKey, NodeValue, LeftBranch, RightBranch}) ->
  io:format("Empty~p~n", [NodeValue]),
  print(LeftBranch),
  print(RightBranch).

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
          Smallest = {SKey, SElement, SLeftBranch, SRightBranch} = find_min(RightBranch),

          Temp = {PKey, PElement, PLeftBranch, PRightBranch} = find_second_min(RightBranch),
          Temp = {PKey, PElement, Smallest, PRightBranch},

          % The smallest node exiss in two places, and is therefore removed
          Temp = {SKey, SElement, LeftBranch, delete_node(SKey, RightBranch)},
          Temp
      end
end;

delete_node(Key, {NodeKey, NodeElement, LeftBranch, RightBranch}) when Key > NodeKey ->
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


find(Key, Tree = ?EMPTY_TREE) ->
  Tree;
find(Key, {NodeKey, NodeElement, LeftBranch, RightBranch}) ->
  if
    (Key < NodeKey) ->
      find(Key, LeftBranch);
    (Key > NodeKey) ->
      find(Key, RightBranch);
    true -> % Keys are equal, replace
      {NodeKey, NodeElement, LeftBranch, RightBranch}
  end.

find_parent(_Key, Root = ?EMPTY_TREE, _Branch) ->
  Root;
find_parent(_Key, Root, ?EMPTY_TREE) ->
  Root;
find_parent(Key, Root, Branch = {NodeKey, _, LeftBranch, RightBranch}) ->
  if
    (Key < NodeKey) ->
      find_parent(Key, Branch, LeftBranch);
    (Key > NodeKey) ->
      find_parent(Key, Branch, RightBranch);
    true -> % Keys are equal, replace
      Root
  end.


test() ->
  T = {a,b,123,"Hello!"},
  E = element(4,T),
  io:format("~p~n", [E]),
  noop.

%write(Key, Element, [{Key, _} | Db]) ->
%  [{Key, Element}|Db];
%write(Key, Element, [Current | Db]) ->
%  [Current | write(Key, Element, Db)].

