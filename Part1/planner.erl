%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <stefancross>
%%% @doc
%%%
%%% Practical Part 1: Implementing a Journey Planner
%%%
%%% The following module implements a Journey planner, taking a config
%%% file to establish locations and their distances apart. The Planner
%%% server can provide a route from a specified starting location to a
%%% number of other locations. It is not necessarily optimised for the
%%% most efficient journey but is adequate.
%%% @end
%%%-------------------------------------------------------------------

-module(planner).
-author("stefancross").

%% API
-export([start_link/0, route/2]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start a new process that is linked to the calling process.
%% @spec start_link() -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  {ok, spawn(fun() -> init() end)}.

%%--------------------------------------------------------------------
%% @doc
%% Given the name of a starting city and a list of destinations (in no
%% particular order), this function returns a list that describes the
%% route (and order) the truck or van has to take in order to visit all
%% of the cities in ToList
%% @spec route(From, ToList) -> {ok, CityList} | {error, invalid}
%% @end
%%--------------------------------------------------------------------
route(From, List) ->
  % May not lead to the most efficient route, but lists:usort removes
  % duplicates effectively
  Sorted = lists:usort(List),
  {ok, routing(From, Sorted)}.
routing(From, [H|T]) when From /= H ->
  % Could look for route optimisation on the Route var
  % Least number of hops, distance vector rather then link cost based
  [digraph:get_short_path(lists:nth(1, ets:lookup(graph, digraph)), From, H)
  |routing(H, T) ];
routing(To, [To|_T]) -> [];
routing(_From, []) -> [].

%%%===================================================================
%%  Recieving loop pattern
%%%===================================================================
loop()->
  receive
    {route, {From, To}, Pid} ->
      io:format("Routing request received from ~p ~n", [Pid]),
      Pid ! route(From, To)
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Calls supporting functions to setup ETS tabs and import the config
init() ->
  register(?MODULE, self()),
  createtables(),
  import(file:consult("../file.conf.csv")),
  createDigraph(),
  loop().

%% Create tables for Towns and Distances
createtables() ->
  ets:new(towns, [duplicate_bag, named_table]),
  ets:new(distances, [duplicate_bag, named_table]).

%% Imports the config and inser the Towns and Distances which are used
%% to build the graph structure
import({ok,
  [{towns, Towns},
    {distances, Distances},
    {depot, _Depots},
    {truck, _Trucks},
    {van, _Vans}
  ]}) ->
  inserttowns(Towns),
  insertdistances(Distances),
  io:format("Config imported. ~n").

%%  Recursively enter the Town data
inserttowns([H|T]) ->
  ets:insert(towns, H), inserttowns(T);
inserttowns([]) -> ok.

%%  Recursively enter the Distance data
insertdistances([H|T]) ->
  ets:insert(distances, H), insertdistances(T);
insertdistances([]) -> ok.

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