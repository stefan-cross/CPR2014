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
  import(file:consult("../file.conf.tmp")).

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
  case directRoute(From, Sorted) of
    [[],[]] -> indirectRoute(From, Sorted);
    A -> A
  end.

%TODO tidy up direct match results, currently returning;
% 45> planner:route("Szczecin", ["Bydgoszcz"]).
% [[["Szczecin","Bydgoszcz"]],[]]
directRoute(From, [H|T]) ->
  [ ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', From}, {'==', '$2', H}], [['$1', '$2']]}]),
    ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', H}, {'==', '$2', From}], [['$2', '$1']]}]) | directRoute(H, T) ];
directRoute(From, To) ->
  ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', From}, {'==', '$2', To}], [['$1', '$2']]}]),
  ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', To}, {'==', '$2', From}], [['$2', '$1']]}]).

% keep distance and from data? Trick is to work out the format of the data that we feed into recursion and our passed var perpective
indirectRoute(From, To) ->
    [
    ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', From}], [['$2','$1']]}]), % original from becomes tail of list so we can better work with head
    ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$2', From}], [['$1','$2']]}])
    ].

% can match the first round of links
% > planner:route("Szczecin", ["Toruń"]).
%[[["Bydgoszcz","Szczecin"]],
%[[[80,111,122,110,97,324],"Szczecin"]]]

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