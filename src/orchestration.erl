%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2014 23:12
%%%-------------------------------------------------------------------
-module(orchestration).
-author("stefancross").

%% API
-export([start_simulation/0, start_vehicles/0]).

start_simulation() ->
  {ok, ?MODULE},
  createtables(),
  import(file:consult("../file.conf.csv")),
  createDigraph(),
  manager:start_link(),
  planner:start_link(),
  order:place(100000, 0),
  vehicle_sup:start_link(),
  start_vehicles().

%%Top Sup should take care of this now... manager:start_link(), vehicle_sup:start_link(), planner:start_link()



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
  ets:new(delivered, [set, named_table, public]).

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
  %vehicle:start(Name, Loc), %TODO start via supervisior
  vehicle_sup:add_vehicle(Name, {Loc, 0}),
  createVehicles(ets:lookup(Vehicle, ets:next(Vehicle, N)), Vehicle);
createVehicles([], Vehicle) -> io:format("All ~ps registered. ~n", [Vehicle]).
