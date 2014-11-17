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
  register(Pid, spawn(?MODULE, init, [Pid, Loc])),
  {pid_created, Pid}.

init(Pid, Loc) ->
  process_flag(trap_exit, true),
  % now we have a Pid we can go back to manager:lookup and work on VehiclePid and OwnerPid concepts
  % capacity set if logic, it is not a property to be imported...
  atlocation(Pid, Loc).

atlocation(Pid, Loc) ->
  io:format("Vehicle : ~p , at location: ~p~n", [Pid, Loc]),
  receive
    {Pid, From, To, Dist} ->
      intransit({Pid, From, To, Dist});
    stop -> exit(stopped)
  end.

intransit({Pid, From, To, Dist}) ->
  io:format("Vehicle  ~p , in transit ~p~n", [Pid, {From, To, Dist}]),
  timer:sleep(Dist),
  atlocation(Pid, To).

