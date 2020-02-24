%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. feb. 2020 12:28
%%%-------------------------------------------------------------------
-module(recursive).
-author("tiago").

%% API
-export([fac/1, sum/1, sum/2]).

% Factorial example
fac(N) when N == 0 -> 1;
fac(N) when N > 0  -> N*fac(N-1).

% Sum all positives from 1 to N
sum(N) when N =< 0 -> 0;
sum(N) when N > 0 -> N + sum(N - 1).

% Adds integer between N and M
sum(Max, Max) ->
  Max;
sum(Min, Max) when Min =< Max -> Min + sum(Min + 1, Max).
