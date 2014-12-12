%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Dec 2014 21:08
%%%-------------------------------------------------------------------
-module(setup).
-author("stefancross").

%% API
-export([init/0]).

init() ->
  createtables(),
  import(file:consult("../file.conf.csv")),
  createDigraph(),
  planner:start_link().

import({ok,
  [{towns, Towns},
    {distances, Distances},
    {depot, Depots},
    {truck, _Trucks},
    {van, _Vans}
  ]}) ->
  inserttowns(Towns),
  insertdistances(Distances),
  insertdepots(Depots),
  io:format("Config imported. ~n").

createtables() ->
  ets:new(towns, [duplicate_bag, named_table]),
  ets:new(distances, [duplicate_bag, named_table]),
  ets:new(depots, [duplicate_bag, named_table]),
  ets:new(manager, [duplicate_bag, named_table, public]),
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

%% Create graph structure
createDigraph() ->
  Graph = digraph:new(),
  % Keep a refernce to the graph in an ETS Tab
  ets:new(graph, [set, named_table]),
  ets:insert(graph, Graph),
  io:format("Digraph and ETS graph ref created as ~p.~n", [ets:lookup(graph, digraph)]),
  createDigraphVerticies(Graph, ets:match(towns, '$1')).

%% Set up graph verticies
createDigraphVerticies(Graph, [[{Town, _D}]|Tail]) ->
  digraph:add_vertex(Graph, Town), createDigraphVerticies(Graph, Tail);
createDigraphVerticies(Graph, []) ->
  io:format("Verticies have been created. ~n"),
  createDigraphEdges(Graph).

%% Set up graph edges
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