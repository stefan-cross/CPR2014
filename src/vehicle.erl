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
-export([start/1, init/0, loop/0]).

% Config var as tuple that dynamically creates name and type eg van or truck, passed to init()
start(Name)->
  register(Name, spawn(?MODULE, init, [])),
  {pid_created, Name}.

init() ->
  % other config options to go here, set capacity?
  process_flag(trap_exit, true),
  loop().
  % dynamically set starting location

% states as loops, heres something basic to get going
loop() ->
  receive
    {Msg} -> io:format("Recieved ~p~n", [Msg]),
             loop();
    stop -> exit(stopped)
  end.