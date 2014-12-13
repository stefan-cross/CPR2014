%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Nov 2014 15:37
%%
%% See Page 17 of Supervisor example slides
%%
%%%-------------------------------------------------------------------
-module(top_sup).
-author("stefancross").

%% API
-behaviour(supervisor). -vsn('1.2').
-export([start_link/0, init/1, stop/0]).

start_link() ->
  supervisor:start_link({local,?MODULE},?MODULE, []).

stop() ->
  exit(whereis(?MODULE), shutdown).

init(_) ->
  {ok,{{one_for_one,2,3600}, [child(vehicle_sup), child(manager), child(planner)]}}.

child(Module) ->
  {Module, {Module, start_link, []},
    permanent, 2000, worker, [Module]}.

%%
%% 1> c(vehicle), c(testing), c(manager), c(planner), c(client), c(orchestration), c(dispatcher) ,c(vehicle_sup),c(top_sup).
%% {ok,top_sup}
%% 2> top_sup:start_link().
%% Manager started
%% {ok,<0.75.0>}
%% 3> whereis(manager).
%% <0.77.0>
%% 4> whereis(vehicle_sup).
%% <0.76.0>
%% 5> whereis(planner).
%% <0.78.0>
%% 6> exit(whereis(planner), kill).
%% true
%% 7> whereis(planner).
%% <0.83.0>