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
-export([start/2, init/2, go/2]).

start(Pid, Loc)->
  register(Pid, spawn(?MODULE, init, [Pid, Loc])),
  {pid_created, Pid}.

% Dev stragegy is to have autonomous vehicle Pids holding own ets tables
% Orchestration class handles IO between veh ets and manager ets
init(Pid, Loc) ->
  process_flag(trap_exit, true),
%%   ets:insert(pids, Pid, get(Pid)),
  ets:new(Pid, [set, named_table, public]), % public for testing purposes and ease of interegation
  atlocation(Pid, Loc).

%TODO remove, this is a backdoor/shortcut for testing!
go(Pid, Loc) ->
  atlocation(Pid, Loc).

atlocation(Pid, Loc) ->
  io:format("Vehicle : ~p , at location: ~p~n", [Pid, Loc]),
  %orchestration:start(Pid, Loc),
  receive
    {Pid, From, To, Dist} ->
      intransit({Pid, From, To, Dist});
    stop -> exit(stopped)
  end.

intransit({Pid, From, To, Dist}) ->
  io:format("Vehicle  ~p , in transit ~p~n", [Pid, {From, To, Dist}]),
  timer:sleep(Dist),
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