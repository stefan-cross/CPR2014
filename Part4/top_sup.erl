%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Nov 2014 15:37
%%
%% top_sup module starts and monitors the vehicle supervisor, the journey
%% planner and the parcel manager.
%%
%%%-------------------------------------------------------------------
-module(top_sup).
-author("stefancross").

%% API
-behaviour(supervisor). -vsn('1.2').
-export([start_link/0, init/1, stop/0]).


%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start a new process that is linked to the calling process.
%% @spec start_link() -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local,?MODULE},?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the supervisor and terminate all the linked processes.
%% @spec stop(SupPid) -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
  exit(whereis(?MODULE), shutdown).


%%%===================================================================
%%% Internal Functions
%%%===================================================================
init(_) ->
  {ok,{{one_for_one,2,3600}, [child(vehicle_sup), child(manager), child(planner)]}}.


%TODO more specific strategies!
child(Module) ->
  {Module, {Module, start_link, []},
    permanent, 2000, worker, [Module]}.

