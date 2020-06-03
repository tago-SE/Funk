%%%-------------------------------------------------------------------
%%% @author tiago
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. juni 2020 19:00
%%%-------------------------------------------------------------------
-module(my_db_sup).
-behaviour(supervisor).
-author("tiago").

%% API
-export([init/1, stop/0, start_link/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

stop() ->
  exit(whereis(?MODULE), shutdown). %OTP uses shutdown signal

init(Args) ->
  {ok, {{rest_for_one, 5, 2000},
    [child(my_gen_server, [])]}}.

child(Module, Args) ->
  {Module, {Module, start_link, Args}, permanent, 2000, worker, [Module]}.
