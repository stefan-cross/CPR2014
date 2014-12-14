%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%
%%% Vehicle processes act as finite state machine with state being
%%% at-location and in-transit.
%%%
%%% Stragegy is to have autonomous vehicle Pids holding own ets tables
%%% Dispatcher class handles IO between vehicle ets and manager ets
%%%
%%%-------------------------------------------------------------------
-module(vehicle).
-author("stefancross").

%% API
-export([start/2, init/2]).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new manager process linking it to the starting supervisor
%% @spec start_link() -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start(Pid, Loc)->
  {ok, spawn_link(fun() -> init(Pid, Loc)end)}.

%%%===================================================================
%%% Vechile FSM abstraction, atLocation and inTransit as the only two states
%%%===================================================================

atlocation(Pid, Loc) ->
  io:format("Vehicle : ~p , at location: ~p~n", [Pid, Loc]),
  dispatcher:start(Pid, Loc),
  % useful for debugging msgbox, process_info(whereis(van1), messages).
  receive
    {route, {From, To, Dist}, Pid} ->
      intransit({Pid, From, To, Dist});
    {route, finished, Pid} ->
      dispatcher:goto_random_town(Pid, Loc);
    stop -> exit(graceful)
  end.

intransit({Pid, From, To, Dist}) ->
  io:format("Vehicle  ~p , in transit ~p~n", [Pid, {From, To, Dist}]),
  % Sleep to simulate drivetime, tune in to radio 6 ;-)
  timer:sleep(Dist),
  atlocation(Pid, To).

%%%===================================================================
%%% Internal initialisation
%%%===================================================================
init(Pid, Loc) ->
  register(Pid, self()),
  ets:new(Pid, [set, named_table, public, {heir, whereis(manager), []}]),
  atlocation(Pid, Loc).