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
-export([start/1, init/0]).

% Config var as tuple that dynamically creates name and type eg van or truck, passed to init()
start(Name)->
  register(Name, spawn(?MODULE, init, [])),
  {pid_created, Name}.

init() ->
  % other config options to go here, set capacity?
  process_flag(trap_exit, true),
  atlocation("Kraków").
  % dynamically set starting location

% states as loops, heres something basic to get going
atlocation(Loc) ->
  io:format("Vehicle at location: ~p~n", [Loc]),
  receive
    {From, To, Dist} ->
      io:format("Recieved ~p~n", [{From, To, Dist}]),
      intransit({From, To, Dist});
    stop -> exit(stopped)
  end.

intransit({From, To, Dist}) ->
  io:format("Vehicle in transit ~p~n", [{From, To, Dist}]),
  timer:sleep(Dist),
  atlocation(To).

%% 10> c(vehicle).
%% {ok,vehicle}
%% 11> vehicle:start(van).
%% Vehicle at location: "Kraków"
%% {pid_created,van}
%% 12> van ! {"A", "B", 2000}.
%% Recieved {"A","B",2000}
%% Vehicle in transit {"A","B",2000}
%% {"A","B",2000}
%% Vehicle at location: "B"
%% 14> van ! {"B", "C", 1000}.
%% Recieved {"B","C",1000}
%% Vehicle in transit {"B","C",1000}
%% {"B","C",1000}
%% Vehicle at location: "C"
