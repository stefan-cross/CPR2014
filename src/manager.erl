%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2014 21:18
%%%-------------------------------------------------------------------
-module(manager).
-author("stefancross").

%% API
-export([start_link/0, deliver/1, reserve/3, reserve/2, loop/0, send/3]).

start_link() ->
  register(?MODULE, spawn_link(?MODULE, loop, [])),
  {ok, ?MODULE},
  ets:new(manager, [set, named_table]).


uniqueref({A, B, C}) ->
  %TODO math:pow(10, 12). {1415,821017,790736} as int
  (A * 1000000000000) + (B * 1000000) + C.


send(From, To, Kg) ->
  Ref = uniqueref(now()),
  ets:insert(manager, {Ref, waiting, From, To, Kg}),
  {ok, Ref}.

% Assuming deliveries are potentially parcels in transit,
deliver(Loc) ->
  %TODO deliveries that are going to Loc
  % Deliveries, items in transit to Loc
  Deliveries = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', intransit}, {'==', '$4', Loc}], [['$1', '$2', '$4']]}]),
  % Pickups, items waiting from Loc
  Pickups = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', waiting}, {'==', '$3', Loc}], [['$1', '$2', '$3']]}]),
  Order = Pickups++Deliveries,
  Length = length(Order),
  if
    Length > 0 -> {ok, Order};
    Length =< 0 -> {error, instance}
  end.

%% 51> manager:send("A", "B", 10).
%% {ok,1415971287284657}
%% 52> manager:send("A", "C", 10).
%% {ok,1415971289820290}
%% 53> manager:send("A", "D", 10).
%% {ok,1415971292269273}
%% 54> manager:send("A", "B", 10).
%% {ok,1415971293837010}
%% 55> manager:send("A", "B", 20).
%% {ok,1415971296020752}
%% 56> ets:insert(manager, {1415971287284612,intransit,"A","B",10}).
%% true
%% 58> ets:insert(manager, {1415971287284000,intransit,"B","A",10}).
%% true
%% 59> manager:deliver("A").
%% {ok,[[1415971292269273,waiting,"A"],
%% [1415971293837010,waiting,"A"],
%% [1415971287284657,waiting,"A"],
%% [1415971296020752,waiting,"A"],
%% [1415971289820290,waiting,"A"],
%% [1415971287284000,intransit,"A"]]}
%% 60>




%TODO consider allocation to vehicle
reserve(From, To, Kg) ->
  % Reverse select not quite ordered, but improvement on ets:select which gets oldest first
  Reserved = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', waiting}, {'==', '$3', From}, {'==', '$4', To}], ['$$']}]),
  WeightedReserve = weightedReserve(Reserved, Kg),
  updateReserved(WeightedReserve),
  {ok, WeightedReserve}.

%TODO consider allocation to vehicle
reserve(From, Kg) ->
  % Reverse select not quite ordered, but improvement on ets:select which gets oldest first
  Reserved = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', waiting}, {'==', '$3', From}], ['$$']}]),
  WeightedReserve = weightedReserve(Reserved, Kg),
  updateReserved(WeightedReserve),
  {ok, WeightedReserve}.

weightedReserve([[Ref, _Status, From, To, Kg] | T], Ac) when Ac >= Kg ->
  [[Ref, reserved, From, To, Kg] | weightedReserve(T, (Ac - Kg))];
weightedReserve([[_Ref, _Status, _From, _To, _Kg] | _T], _Ac) -> [];
weightedReserve([], _Ac) -> [].
updateReserved([[Ref, _Status, From, To, Kg] | T]) ->
  ets:delete(manager, Ref),
  % unfortuantely we cant update bags, only sets,
  ets:insert(manager, {Ref, reserved, From, To, Kg}),
  updateReserved(T);
updateReserved([]) -> ok.
%% working!
%% 92> c(manager).
%% {ok,manager}
%% 93> manager:start_link().
%% In loopmanager
%% 94> manager:send("A", "B", 10).
%% {ok,1415965996288405}
%% 95> manager:send("A", "B", 10).
%% {ok,1415965996952045}
%% 96> manager:send("A", "B", 10).
%% {ok,1415965997607945}
%% 97> manager:reserve("A", "B", 20).
%% {ok,[[1415965997607945,reserved,"A","B",10],
%% [1415965996288405,reserved,"A","B",10]]}

loop() ->
  io:format("In loop"),
  receive
    {From, To, Kg} ->
      io:format("Message receieved ~p~n", [self()]),
      send(From, To, Kg),
      loop();
    exit ->
      io:format("Exiting"),
      ok
  end.