%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2014 21:18
%%
%% 3.2 Practical Part 2: Implementing the Parcel Manager
%%
%% The parcel manager keeps track of parcels and provides vans and trucks
%% with locations that require pickups. It can access the journey planner’s
%% tables and retrieve information on cities and routes as required.
%%
%%%-------------------------------------------------------------------
-module(manager).
-author("stefancross").

%% API
-export([start_link/0, init/0, deliver/1, reserve/3, reserve/2, pick/1]).
-export([drop/1, uniqueref/1, transit/2, cargo/1, lookup/1, loop/0, send/3]).

start_link() ->
  {ok, spawn_link(?MODULE, init, [])}.

init() ->
  register(?MODULE, self()),
  io:format("Manager started ~n"),
  ets:new(manager, [duplicate_bag, named_table, public]),
  loop().

send(From, To, Kg) ->
  Ref = uniqueref(now()),
  ets:insert(manager, {Ref, waiting, From, To, Kg}),
  %io:format("Order sent: ~p ~p ~p ~p ~n", [Ref, From, To, Kg]),
  {ok, Ref}.

uniqueref({A, B, C}) ->
  (A * 1000000000000) + (B * 1000000) + C.

% Assuming deliveries are potentially parcels in transit,
deliver(Loc) ->
  % Deliveries, items in transit to Loc
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
  Reserved = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$3', From}, {'==', '$4', To}], ['$$']}]),
  WeightedReserve = weightedReserve(Reserved, Kg),
  updateReserved(WeightedReserve),
  {ok, WeightedReserve}.

reserve(From, Kg) ->
  % Reverse select not quite ordered, but improvement on ets:select which gets oldest first
  Reserved = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$3', From}], ['$$']}]),
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
    Length =< 0 -> {error, Ref, not_intransit, self()} %TODO make process_instance identifier dynamic
  end.

drop(Ref) ->
  Drop = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', intransit}, {'==', '$1', Ref}], ['$$']}]),
  Length = length(Drop),
  if
    Length >= 1 -> updateManagerByRef(Drop, delivered), {ok, delivered, Ref};
    Length =< 0 -> {error, not_reserved, self()} %TODO make process_instance identifier dynamic
  end.

% Used by pick and drop
updateManagerByRef([[Ref, _Status, _From, _To, _Kg]], _State) ->
  ets:delete(manager, Ref).

transit(Ref, Loc) ->
  Trans = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', intransit}, {'==', '$1', Ref}], ['$$']}]),
  Length = length(Trans),
  if
    Length >= 1 -> updateTransitState(Trans, Loc), {ok, indepot, Ref};
    Length =< 0 -> {error, not_indepot, self()} %TODO make process_instance identifier dynamic
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


lookup(Ref) ->
  %% It would have been nice to simply
  %% {ok, ets:lookup(?MODULE, Ref), self(), ?MODULE}
  %% and return [] for no results but in the interests of API compliance...
  Result = ets:lookup(?MODULE, Ref),
  Length = length(Result),
  if
    Length > 0 -> {ok, Result, self(), ?MODULE};
    Length =< 0 -> {error, instance}
  end.



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
  end.
