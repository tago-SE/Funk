%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. feb. 2020 13:15
%%%-------------------------------------------------------------------
-module(mylist).
-author("tiago").

%% API
-export([sum/1, len/1, add/2, subtract/2, prepend_to_list/2, print_list/1, create_forward/1, create_backward/1]).


add(List1, List2) ->
  List1 ++ List2.

subtract(List1, List2) ->
  List1 -- List2.

% This is an example of how to append V to the front
prepend_to_list(V, List) ->
  [V | List].

% Prints list from 1 to N
print_list([Head | Tail]) ->
  io:format("~p~n", [Head]),
  print_list(Tail).

%% Create a list with items from 0 to N - 1, e.g: [0, 1, 2 ...].
create_forward(0) ->
  [];
create_forward(N) ->
  forward(0, N, []).

forward(I, N, List) when I == N ->
  List;
forward(I, N, List) when I < N ->
  forward(I + 1, N, [I | List]).

create_backward(0) ->
  [];
create_backward(N) ->
  backward(N, []).

backward(N, List) when N =< 0 ->
  List;
backward(N, List) when N > 0 ->
  V = N - 1,
  backward(V, [V | List]).

% can be read as a executable description:
% The sum of a empty-list is zero
% The sum of a non-empty list is the head of the list added to the sum of the tail.
sum([]) -> 0;
sum([Head | Tail]) ->
  % we pop the head and have a new list called Tail
  %io:format("Head: ~p~n", [Head]),
  %io:format("Tail: ~p~n", [Tail]),
  Head + sum(Tail).

% Not sure how to read this
len([_|T]) -> 1 + len(T);
len([]) -> 0.
