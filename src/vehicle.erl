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
      intransit(),
      van ! {From, To, Dist};
    stop -> exit(stopped)
  end.

intransit() ->
  receive
    {From, To, Dist} ->
      io:format("Vehicle in transit ~p~n", [{From, To, Dist}]),
      timer:wait(Dist),
      atlocation(To)
  end.
