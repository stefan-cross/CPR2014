%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2014 21:33
%%%-------------------------------------------------------------------
-module(testing).
-author("stefancross").

%% API
-export([parse/1, read/0, pattern1/1, init/0, routeMatcher/2, filter_out_nils/1, test/0]).

read() ->
  file:consult("../file.conf.csv").

parse(Filename) ->
  {ok, Data} = file:read_file(Filename),
  binary:split(Data, [<<"\n">>], [global]).



pattern1({ok,
  [{towns, Towns},
   {distances, Distances},
   {depot, Depots},
   {truck, Trucks},
   {van, Vans}
  ]}) -> io:format("~w~n", [Towns]). %, ets:insert(towns, Towns).

%pattern1({ok, [{towns, [Towns]}, {distances, [Distances]}]}) -> io:format("~w~n", [Distances]).

%pattern1({ok, [L]}) -> io:format("~w~n", [L]), pattern1(L);
%pattern1({towns, [L]}) -> io:format("~w~n", [L]), pattern1(L);
%pattern1({distances, [L]}) -> io:format("~w~n", [L]), pattern1(L) .

%init([{Town, Pop}| Tail ]) -> ets:insert(locations, {Town, Pop}), init(Tail).

%TODO - have a init function that imports data so route can preform algorithm
%init() -> File = file:consult("../file.conf.csv"), init(File).
%init(File) ->
%  ets:new(locations, [set, named_table]),
%  case File of
%    {_, [{"towns" ,[Towns]}, {"distances", [Distances]}, {depot, [Depots]}, {"truck", [Trucks]}, {van, [Vans]}]} -> init(Towns);
%    [{Town, Pop}, List] -> ets:insert(locations, {Town, Pop}), init(List)
%  end.

-record(town, {town, population}).

init()->
  ets:new(town, [duplicate_bag, named_table]),
  import(file:consult("../file.conf.csv")).

import({ok,
  [{towns, Towns},
    {distances, Distances},
    {depot, Depots},
    {truck, Trucks},
    {van, Vans}
  ]}) ->
  inserttowns(Towns).


inserttowns([{Town, Pop}|T]) ->
  ets:insert(town, [#town{town = Town, population = Pop}]), inserttowns(T);
inserttowns([]) -> ok.

test()->
ets:select(town, ets:fun2ms(fun(N = #town{town = "Radom"}) -> N end)).

% 46> ets:select(town, ets:fun2ms(fun(N = #town{town = "Radom"}) -> N end)).
% [#town{town = "Radom",population = 220}]

% from is a town, list is destination
%algorithm1({From, ) ->

filter_out_nils(Data) when is_list(Data) ->
  Pred = fun(Element) -> Element /= ok end,
  lists:filter(Pred, Data).


routeMatcher(From, [H|T]) ->
  if
      H == From -> true
  end.






