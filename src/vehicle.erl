%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2014 21:59
%%%
%%%
%%% Think of our vehicle processes as the finiate state machine as per the
%%% lecture practicles on mutexes, state being at-location and in-transit.
%%%
%%%
%%%-------------------------------------------------------------------
-module(vehicle).
-author("stefancross").

%% API
-export([start/2, init/2]).

start(Pid, Loc)->
  {ok, spawn_link(?MODULE, init, [Pid, Loc])}.

% Dev stragegy is to have autonomous vehicle Pids holding own ets tables
% Dispatcher class handles IO between veh ets and manager ets
init(Pid, Loc) ->
  register(Pid, self()),
  ets:new(Pid, [set, named_table, public, {heir, whereis(manager), []}]),
  atlocation(Pid, Loc).

atlocation(Pid, Loc) ->
  io:format("Vehicle : ~p , at location: ~p~n", [Pid, Loc]),
  dispatcher:start(Pid, Loc),
  % useful for debugging msgbox, process_info(whereis(van1), messages).
  receive
    {route, {From, To, Dist}, Pid} ->
      intransit({Pid, From, To, Dist});
    {route, finished, Pid} ->
      %timer:sleep(5000),
      waiting(Pid, Loc);
    stop -> exit(graceful)
  end.

intransit({Pid, From, To, Dist}) ->
  io:format("Vehicle  ~p , in transit ~p~n", [Pid, {From, To, Dist}]),
  timer:sleep(Dist),
  atlocation(Pid, To).

waiting(Pid, Loc) ->
  dispatcher:goto_random_town(Pid, Loc),
  receive
    {route, {From, To, Dist}, Pid} ->
      intransit({Pid, From, To, Dist});
    {route, finished, Pid} ->
      dispatcher:goto_random_town(Pid, Loc);
    stop -> exit(graceful)
  end.

