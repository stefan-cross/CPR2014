%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Nov 2014 15:37
%%%-------------------------------------------------------------------
-module(top_sup).
-author("stefancross").

%% API
-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([])->
  RestartStrategy = one_for_all,
  MaxRestarts = 100,
  MaxSecondsBetweenRestarts = 100,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  VehicleChild = {vehicle_sup, {vehicle_sup, start_link, []},
            Restart, Shutdown, Type, [vehicle_sup]},

  {ok, {SupFlags, [VehicleChild]}}.

%%
%% See Page 17 of Supervisor example slides
%%
%% -module(sup).
%% -behaviour(supervisor). -vsn('1.2').
%% -export([start_link/0, init/1, stop/0]).
%% start_link() ->
%%   supervisor:start_link({local,?MODULE},?MODULE, []).
%% stop() -> exit(whereis(?MODULE), shutdown).
%% init(_) ->{ok,{{one_for_one,2,3600}, [child(frequency)]}}.
%% child(Module) ->
%%   {Module, {Module, start_link, []},
%%     permanent, 2000, worker, [Module]}.