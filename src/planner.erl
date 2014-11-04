%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Oct 2014 23:18
%%%
%%%
%%% The following API should be respected when implementing the server:
%%%
%%% planner:start_link()  -> {ok, Pid}.
%%%
%%% Start a new process that is linked to the calling process.
%%%
%%% planner:route(From, ToList) -> {ok, CityList} | {error, invalid}.
%%%
%%%-------------------------------------------------------------------
-module(planner).
-author("stefancross").

%% API
-export([start_link/0, route/2, loop/0, init/1]).

start_link() ->
  register(?MODULE, spawn_link(?MODULE, loop, [])),
  init(file:consult("../file.conf.csv")),
  {ok, ?MODULE}.

init({ok,
  [{towns, Towns},
    {distances, Distances},
    {depot, Depots},
    {truck, Trucks},
    {van, Vans}
  ]}) -> io:format("~w~n", [Towns]).
  %TODO ets inserts for better data structure support for use in routing algorithm

route(From, List) ->
  io:format("magic_happens_here... ~p~n", [{From, List}]),
  loop().

loop() ->
  receive
    {From, List} -> route(From, List);
    stop -> exit(stopped);
    _a -> io:format("Receieved ~p~n", [_a])
  end.