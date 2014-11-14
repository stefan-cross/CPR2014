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

-define(Digraph, digraph:new()).

start_link() ->
  register(?MODULE, spawn_link(?MODULE, loop, [])),
  {ok, ?MODULE},
  createtables(),
  import(file:consult("../file.conf.csv")),
  createDigraph().

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

%TODO parameterise and reduce seperate functions
createtables() ->
  ets:new(towns, [duplicate_bag, named_table]),
  ets:new(distances, [duplicate_bag, named_table]),
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


createDigraph() ->
  Graph = digraph:new(),
  ets:new(graph, [set, named_table]),
  ets:insert(graph, Graph),
  io:format("Digraph and ETS graph ref created... ~n"),
  createDigraphVerticies(Graph, ets:match(towns, '$1')).

createDigraphVerticies(Graph, [[{Town, _D}]|Tail]) ->
  digraph:add_vertex(Graph, Town), createDigraphVerticies(Graph, Tail);
createDigraphVerticies(Graph, []) ->
  io:format("Verticies have been created ~n"),
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
  io:format("Edges have been created ~n").


% WE ARE IN BUSINESS!
%%
%% 34> digraph:get_short_path(lists:nth(1, ets:lookup(graph, digraph)), "Szczecin", "Białystok").
%% ["Szczecin",
%% [87,114,111,99,322,97,119],
%% [84,111,114,117,324],
%% [66,105,97,322,121,115,116,111,107]]
%% 35> io:format("~s~n", [[87,114,111,99]]).


% Does not have to be optimal, but needs to visit all cities
% Only go to cities where destination is stated in list
% if not route between cities return {error, invalid}
%
% Firt iteration will only handle direct links between towns


route(From, List) ->
  Sorted = lists:usort(List),
  routing(From, Sorted).

routing(From, [H|T]) ->
  Option1 = ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', From}], ['$2']}]),
  Option2 = ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$2', From}], ['$1']}]),

  if
    Option1 == H -> lists:append(Option1, From);
    Option2 == H -> lists:append(Option2, From);
    Option1 /= H , Option2 == H ->
      Return = [lists:append(Option1, From), lists:append(Option2, From)],
      io:format("List = ~p~n", [H]),
      io:format("Returning = ~p~n", [Return]),
      subRouting(Return, [H,T])
  end.

subRouting(List, To) ->
  {List, To}.


loop() ->
  receive
    {From, List} -> route(From, List);
    stop -> exit(stopped);
    _a -> io:format("Receieved ~p~n", [_a])
  end.

%
% "Warszawa","Radom",
% [{[66,105,97,322,121,115,116,111,107],
% [84,111,114,117,324],
% 357}]
%
% [X,Y,[{C1, C2, Dist}]] = A .
%
%

%{[66,105,97,322,121,115,116,111,107],[84,111,114,117,324,44,32,66,105,97,322,121,115,116,111,107],[{[66,105,97,322,121,115,116,111,107],[84,111,114,117,324],357}]}


%[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,[exact_match,{"Szczecin","Bydgoszcz",256}],ok,ok,ok,ok,ok,ok,ok|ok]