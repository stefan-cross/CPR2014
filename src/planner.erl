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
-export([start_link/0, route/2, start_orders/0, start_vehicles/0, loop/0]).

%TODO seperate all the startups out into a full init script as everything needs to be started in an order... and this isnt the place for it
start_link() ->
  register(?MODULE, spawn_link(?MODULE, loop, [])),
  {ok, ?MODULE},
  createtables(),
  import(file:consult("../file.conf.csv")),
  createDigraph(),
  manager:start_link().

start_orders() ->
  %% Get some data in!
  client:start(100, 0).

start_vehicles() ->
  createVehicles(van),
  createVehicles(truck).

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
  insertVehicles(truck, Trucks, 1),
  insertVehicles(van, Vans, 1),
  io:format("Config imported. ~n").

%TODO parameterise and reduce seperate functions for locations
createtables() ->
  ets:new(towns, [duplicate_bag, named_table]),
  ets:new(distances, [duplicate_bag, named_table]),
  ets:new(depots, [duplicate_bag, named_table]),
  ets:new(truck, [duplicate_bag, named_table]),
  ets:new(van, [duplicate_bag, named_table]),
  ets:new(delivered, [duplicate_bag, named_table, public]),
  ets:new(pids, [duplicate_bag, named_table]).

inserttowns([H|T]) ->
  ets:insert(towns, H), inserttowns(T);
inserttowns([]) -> ok.

insertdistances([H|T]) ->
  ets:insert(distances, H), insertdistances(T);
insertdistances([]) -> ok.

insertdepots([H|T]) ->
  ets:insert(depots, {depot, H}), insertdepots(T);
insertdepots([]) -> ok.

insertVehicles(Vehicle, [H|T], Acc) ->
  ets:insert(Vehicle, {Acc, H}), insertVehicles(Vehicle, T, Acc + 1);
insertVehicles(_Vehicle, [], _Acc) -> ok.



createDigraph() ->
  Graph = digraph:new(),
  ets:new(graph, [set, named_table]),
  ets:insert(graph, Graph),
  io:format("Digraph and ETS graph ref created as ~p.~n", [ets:lookup(graph, digraph)]),
  createDigraphVerticies(Graph, ets:match(towns, '$1')).

createDigraphVerticies(Graph, [[{Town, _D}]|Tail]) ->
  digraph:add_vertex(Graph, Town), createDigraphVerticies(Graph, Tail);
createDigraphVerticies(Graph, []) ->
  io:format("Verticies have been created. ~n"),
  createDigraphEdges(Graph).

createDigraphEdges(Graph) ->
  Distances = ets:select(distances, [{{'$1', '$2', '$3'}, [], ['$$']}]),
  createDigraphEdges(Graph, Distances).
createDigraphEdges(Graph, [[City1, City2, _Dist]|T]) ->
  digraph:add_edge(Graph, City1, City2),
  % Make our edges bidirectional
  digraph:add_edge(Graph, City2, City1),
  createDigraphEdges(Graph, T);
createDigraphEdges(_Graph, []) ->
  io:format("Edges have been created. ~n").

createVehicles(Vehicle) ->
  First = ets:first(Vehicle),
  createVehicles(ets:lookup(Vehicle, First), Vehicle).
createVehicles([{N, Loc}], Vehicle) ->
  Name = list_to_atom(atom_to_list(Vehicle) ++ integer_to_list(N)),
  vehicle:start(Name, Loc),
  createVehicles(ets:lookup(Vehicle, ets:next(Vehicle, N)), Vehicle);
createVehicles([], Vehicle) -> io:format("All ~ps registered. ~n", [Vehicle]).


%TODO, check we are passed a list
route(From, List) ->
  Sorted = lists:usort(List),
  From, routing(From, Sorted).
routing(From, [H|T]) when From /= H ->
  % Least number of hops, distance vector based rather then link cost based for now
  [ digraph:get_short_path(lists:nth(1, ets:lookup(graph, digraph)), From, H)
  | routing(H, T) ];
routing(To, [To|_T]) -> [];
routing(_From, []) -> [].
%TODO sort into single list with no dupes and reserve order

loop() ->
  receive
    {From, List} -> route(From, List);
    stop -> exit(stopped);
    _a -> io:format("Receieved ~p~n", [_a])
  end.
