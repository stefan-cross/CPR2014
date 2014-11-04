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
-export([start_link/0, route/2, loop/0, import/1]).

start_link() ->
  register(?MODULE, spawn_link(?MODULE, loop, [])),
  {ok, ?MODULE},
  createtables(),
  import(file:consult("../file.conf.csv")).

import({ok,
  [{towns, Towns},
    {distances, Distances},
    {depot, Depots},
    {truck, Trucks},
    {van, Vans}
  ]}) ->
  inserttowns(Towns),
  insertdistances(Distances),
  insertdepots(Depots),
  inserttrucks(Trucks),
  insertvans(Vans).

createtables() ->
  ets:new(towns, [set, named_table]),
  ets:new(distances, [set, named_table]),
  ets:new(depots, [duplicate_bag, named_table]),
  ets:new(trucks, [duplicate_bag, named_table]),
  ets:new(vans, [duplicate_bag, named_table]).

inserttowns([H|T]) ->
  ets:insert(towns, H), inserttowns(T);
inserttowns([]) -> ok.

insertdistances([H|T]) ->
  ets:insert(distances, H), insertdistances(T);
insertdistances([]) -> ok.

insertdepots([H|T]) ->
  ets:insert(depots, {depot, H}), insertdepots(T);
insertdepots([]) -> ok.

inserttrucks([H|T]) ->
  ets:insert(trucks, {truck, H}), inserttrucks(T);
inserttrucks([]) -> ok.

insertvans([H|T]) ->
  ets:insert(vans, {van, H}), insertvans(T);
insertvans([]) -> ok.

route(From, List) ->
  io:format("magic_happens_here... ~p~n", [{From, List}]),
  loop().

loop() ->
  receive
    {From, List} -> route(From, List);
    stop -> exit(stopped);
    _a -> io:format("Receieved ~p~n", [_a])
  end.

