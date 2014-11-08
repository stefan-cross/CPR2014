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
-export([start_link/0, route/2, formatRoute/1, loop/0, import/1]).


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


% Does not have to be optimal, but needs to visit all cities
% Only go to cities where destination is stated in list
% if not route between cities return {error, invalid}
%
% Firt iteration will only handle direct links between towns


route(From, List) ->
  Sorted = lists:usort(List),
  Distances = ets:match(distances, '$1'),
  routeSorted(From, Sorted, Distances).
% Sorted our list and remove dupes
routeSorted(From, [H|T], Distances) ->
  [matchDistance(From, H, Distances)|routeSorted(H, T, Distances)]; % dont forget we want to move down the list so we disgard the from, and use the head
routeSorted(_From, [], _Distances) ->
  ok.

% Get out list of distances
matchDistance(From, To, [DistancesHead|DistancesTail]) ->
  [matchSingleDirectDistance(From, To, DistancesHead) | matchDistance(From, To, DistancesTail)]; % recursively search through distances list
matchDistance(_From, _To, []) -> ok.
% First iteration will only handle direct links between towns
matchSingleDirectDistance(From,To,[{From,To,_Dist}]) ->
  [exact_match, {From, To, _Dist}];
matchSingleDirectDistance(From,To,[{To,From,_Dist}]) -> % revese pattern match on form, to
  [exact_match, {From, To, _Dist}];
matchSingleDirectDistance(_From,_To,[{_Town1,_Town2,_Dist}]) -> ok.

% format our returning route list, remove non-matches
formatRoute(Data) when is_list(Data) ->
  Pred = fun(El) -> El /= ok end,
  lists:filter(Pred, Data).

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
