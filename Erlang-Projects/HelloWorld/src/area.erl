%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. feb. 2020 12:40
%%%-------------------------------------------------------------------
-module(area).
-author("tiago").

%% API

% Exec: area:area({square, 5}).
-export([area/1]).

area({square, Side}) ->
  Side*Side;

area({circle, Radius}) ->
  math:pi() * Radius * Radius;

area({triangle, A, B, C}) ->
  S = (A + B + C) / 2,    % ',' is an expression terminator
  math:sqrt(S*(S-A)*(S-B)*(S-C)). % '.' is a function terminator which
