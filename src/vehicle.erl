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
  orchestration:start(Pid, Loc),
  {pid_created, Pid}.

init(Pid, Loc) ->
  atlocation(Pid, Loc).

  % now we have a Pid we can go back to manager:lookup and work on VehiclePid and OwnerPid concepts
  % capacity set if logic, it is not a property to be imported...

  % Strategy is to focus on one vehicle for now, unsure how to reserve without the Pid?


atlocation(Pid, Loc) ->
  io:format("Vehicle : ~p , at location: ~p~n", [Pid, Loc]),
  receive
    {Pid, From, To, Dist} ->
      intransit({Pid, From, To, Dist});
    stop -> exit(stopped)
  end.

intransit({Pid, From, To, Dist}) ->
  io:format("Vehicle  ~p , in transit ~p~n", [Pid, {From, To, Dist}]),
  timer:sleep(Dist),%% 6> van10 ! {van10, a, b, 5000}, van10 ! {van10, b, c, 2000}, van1 ! {van1, a, b, 1000},van3 ! {van3, x, y, 500}.
%% Vehicle
atlocation(Pid, To).

%%
%% van10 , in transit {a,b,5000}
%% Vehicle  van1 , in transit {a,b,1000}
%% Vehicle  van3 , in transit {x,y,500}
%% {van3,x,y,500}
%% Vehicle : van3 , at location: y
%% Vehicle : van1 , at location: b
%% Vehicle : van10 , at location: b
%% Vehicle  van10 , in transit {b,c,2000}
%% Vehicle : van10 , at location: c