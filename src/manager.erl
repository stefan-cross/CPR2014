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
-export([start_link/0, deliver/1, reserve/3, reserve/2, pick/1, drop/1, uniqueref/1, transit/2, cargo/1, lookup/1, loop/0, send/3, reorder/0]).

start_link() ->
  register(?MODULE, spawn_link(?MODULE, loop, [])),
  {ok, ?MODULE},
  io:format("Manager started ~n"),
  ets:new(manager, [duplicate_bag, named_table, public]). %TODO make ordered_set?

send(From, To, Kg) ->
  Ref = uniqueref(now()),
  ets:insert(manager, {Ref, waiting, From, To, Kg}),
  %io:format("Order sent: ~p ~p ~p ~p ~n", [Ref, From, To, Kg]),
  {ok, Ref}.

uniqueref({A, B, C}) ->
  (A * 1000000000000) + (B * 1000000) + C.

% Assuming deliveries are potentially parcels in transit,
deliver(Loc) ->
  % Deliveries, items in transit to Loc ~TODO check logic
  Deliveries = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$4', Loc}], ['$3']}]),
  % Pickups, items waiting from Loc
  Pickups = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$3', Loc}], ['$4']}]),
  Order = Pickups++Deliveries,
  Length = length(Order),
  if
    Length > 0 -> {ok, Order};
    Length =< 0 -> {error, instance}
  end.


reserve(From, To, Kg) ->
  % Reverse select not quite ordered, but improvement on ets:select which gets oldest first
  Reserved = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', waiting}, {'==', '$3', From}, {'==', '$4', To}], ['$$']}]),
  WeightedReserve = weightedReserve(Reserved, Kg),
  updateReserved(WeightedReserve),
  {ok, WeightedReserve}.

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
updateReserved([[Ref, _Status, _From, _To, _Kg] | T]) ->
  ets:delete(manager, Ref),
  % Now we delete them and handle them in vehicle tabs
  updateReserved(T);
updateReserved([]) -> ok.

pick(Ref) ->
  Pick = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$1', Ref}], ['$$']}]), % had to remove match on {'==', '$2', reserved},
  Length = length(Pick),
  if
    Length >= 1 -> updateManagerByRef(Pick, intransit), {ok, intransit, Ref};
    Length =< 0 -> {error, Ref, not_intransit, process_instance} %TODO make process_instance identifier dynamic
  end.

drop(Ref) ->
  Drop = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', intransit}, {'==', '$1', Ref}], ['$$']}]),
  Length = length(Drop),
  if
    Length >= 1 -> updateManagerByRef(Drop, delivered), {ok, delivered, Ref};
    Length =< 0 -> {error, not_reserved, process_instance} %TODO make process_instance identifier dynamic
  end.

% Used by pick and drop
updateManagerByRef([[Ref, _Status, _From, _To, _Kg]], _State) ->
  ets:delete(manager, Ref).

transit(Ref, Loc) ->
  Trans = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', intransit}, {'==', '$1', Ref}], ['$$']}]),
  Length = length(Trans),
  if
    Length >= 1 -> updateTransitState(Trans, Loc), {ok, indepot, Ref};
    Length =< 0 -> {error, not_indepot, process_instance} %TODO make process_instance identifier dynamic
  end.

updateTransitState([[Ref, _Status, _From, To, Kg]], Loc) ->
  ets:delete(manager, Ref),
  ets:insert(manager, {Ref, indepot, Loc, To, Kg}).

cargo(Loc) ->
  Depots = ets:select(depots, [{{'$1', '$2'}, [], ['$2']}]),
   ets:new(cargoroute, [ordered_set, named_table]),
  routeCargo(Loc, Depots).

routeCargo(Loc, [H|T]) ->
  Route = digraph:get_short_path(lists:nth(1, ets:lookup(graph, digraph)), Loc, H),
  RouteDistance = length(Route),
  ets:insert(cargoroute, {RouteDistance, H}),
  routeCargo(Loc, T);
routeCargo(_Loc, []) ->
  KV = ets:lookup(cargoroute, ets:first(cargoroute)),
  formatCargoRoute(KV).
formatCargoRoute([{_K, V}]) ->
  % dispose of our temp cargoroute table used to determine shortest route
  ets:delete(cargoroute),
  {ok, V}.


%TODO this is going to have to look in pid tables to checkwhere abouts of an item, i foresee issues here... but a design trade off to stick to the enforeced api
lookup(Ref) ->
  {ok, ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$1', Ref}], ['$$']}]), vehiclePid, ownerPid}.

% See page 132 Process Design Patterns, A Generic Event Manager Handler
loop() ->
  receive
    {Pid, {deliveries, Loc}} ->
      io:format("Delivery request received ~p ~p ~n", [Pid, Loc]),
      Pid ! manager:deliver(Loc);
    {delivered, Pid, Ref} ->
      io:format("** MANAGER UPDATE ** Package delivered by ~p Ref: ~p~n", [Pid, Ref]),
      loop();
    exit ->
      io:format("Exiting"),
      ok
%% TODO sort error
%%         =ERROR REPORT==== 26-Nov-2014::23:24:27 ===
%%       Error in process <0.73.0> with exit value: {badarg,[{dispatcher,notifyDrop,2,[{file,"dispatcher.erl"},{line,130}]},{dispatcher,start,2,[{file,"dispatcher.erl"},{line,38}]},{vehicle,atlocation,2,[{file,"vehicle.erl"},{line,34}]}]}

%%   after 5000 ->
%%     reorder()
  end.

reorder()->
  Count = ets:select_count(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'/=', '$1', 0}], [true]}]),
  reorder(Count).
reorder(Count) when Count =< 0 ->
  order:place(random:uniform(1000), 0);
reorder(Count) when Count > 0 ->
  {orders, Count}.