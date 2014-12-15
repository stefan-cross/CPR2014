%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <stefancross>
%%% @doc
%%%
%%% Practical Part 2: Implementing the Parcel Manager
%%%
%%% The parcel manager keeps track of parcels and provides vans and trucks
%%% with locations that require pickups. It can access the journey planner’s
%%% tables and retrieve information on cities and routes as required.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(manager).
-author("stefancross").

%% API
-export([start_link/0, deliver/1, reserve/3, reserve/2, pick/1]).
-export([drop/1, uniqueref/1, transit/2, cargo/1, lookup/1, loop/0, send/3]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a new manager process
%% @spec start_link() -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  {ok, spawn(fun() -> init() end)}.

%%--------------------------------------------------------------------
%% @doc
%% Will place an order for an item given a starting point and destination
%% and also the weight of the item to be sent. The function will return a
%% unique reference to track the item in the system.
%% @spec send(From, To, Kg) -> {ok, Ref}
%% @end
%%--------------------------------------------------------------------
send(From, To, Kg) ->
  Ref = uniqueref(now()),
  ets:insert(manager, {Ref, waiting, From, To, Kg}),
  io:format("Order sent: ~p ~p ~p ~p ~n", [Ref, From, To, Kg]),
  {ok, Ref}.

%%--------------------------------------------------------------------
%% @doc
%% This function will give a list of destiantions that have parcels which
%% need delivering or picking up, sssuming deliveries are potentially parcels
%% in transit.
%% @spec deliver(Loc) -> {ok,LocList}|
%%                       {error,instance}
%% @end
%%--------------------------------------------------------------------
deliver(Loc) ->
  % Deliveries, items in transit to Loc
  Deliveries = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$4', Loc}], ['$3']}]),
  % Pickups, items waiting from Loc
  Pickups = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$3', Loc}], ['$4']}]),
  Order = Pickups++Deliveries,
  Length = length(Order),
  if
    Length > 0 -> {ok, Order};
    Length =< 0 -> {error, instance}
  end.

%%--------------------------------------------------------------------
%% @doc
%% This function will reserve a set of orders from and to specified
%% locations weighing upto the specified Kg limit.
%% @spec reserve(From, To, Kg) -> {ok, RefList}
%% @end
%%--------------------------------------------------------------------
reserve(From, To, Kg) ->
  Reserved = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$3', From}, {'==', '$4', To}], ['$$']}]),
  WeightedReserve = weightedReserve(Reserved, Kg),
  updateReserved(WeightedReserve),
  {ok, WeightedReserve}.

%%--------------------------------------------------------------------
%% @doc
%% This function will reserve a set of orders only from a specified
%% location weighing upto the specified Kg limit.
%% @spec reserve(To, Kg) -> {ok, RefList}
%% @end
%%--------------------------------------------------------------------
reserve(From, Kg) ->
  Reserved = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$3', From}], ['$$']}]),
  WeightedReserve = weightedReserve(Reserved, Kg),
  updateReserved(WeightedReserve),
  {ok, WeightedReserve}.

%%--------------------------------------------------------------------
%% @doc
%% The pick function will allow a vehicle to pickup a reserved item by
%% the specified reference identifier.
%% @spec pick(Ref) -> ok |
%%                    {error, not_reserved | instance}.
%% @end
%%--------------------------------------------------------------------
pick(Ref) ->
  Pick = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$1', Ref}, {'==', '$2', reserved}], ['$$']}]),
  Length = length(Pick),
  if
    Length >= 1 -> updateManagerByRef(Pick, intransit), ok; % although {ok, intransit, Ref} would give more clarity
    Length =< 0 -> {error, not_reserved, Ref} % Sure this should be not_picked?! but following spec..
  end.

%%--------------------------------------------------------------------
%% @doc
%% The drop function will allow a vehicle to drop off a 'intransit' item by
%% the specified reference identifier.
%% @spec drop(Ref) -> ok |
%%                    {error, not_reserved | instance}.
%% @end
%%--------------------------------------------------------------------
drop(Ref) ->
  Drop = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', intransit}, {'==', '$1', Ref}], ['$$']}]),
  Length = length(Drop),
  if
    Length >= 1 -> updateManagerByRef(Drop, delivered), ok; % although {ok, delivered, Ref} would give more clarity
    Length =< 0 -> {error, not_picked, Ref} % Sure this should be not_dropped?! but following spec..
  end.

%%--------------------------------------------------------------------
%% @doc
%% The transit function will allow a vehicle to drop items picked up on
%% its journey when it reaches its end or if it comes across a cargo station
%% en route to its destination
%% @spec transit(Ref, Loc) -> ok |
%%                            {error, not_picked | instance}
%% @end
%%--------------------------------------------------------------------
transit(Ref, Loc) ->
  Trans = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', intransit}, {'==', '$1', Ref}], ['$$']}]),
  Length = length(Trans),
  if
    Length >= 1 -> updateTransitState(Trans, Loc), ok; % although {ok, indepot, Ref} would give more clarity
    Length =< 0 -> {error, not_picked, Ref} % Sure this should be not_transit?! but following spec..
  end.

%%--------------------------------------------------------------------
%% @doc
%% The cargo function is called when deliveries are complete and provided
%% with a location the function will return the nearest cargo station, by
%% location hop count, not distance based...
%% @spec cargo(Loc) -> {ok, Loc} |
%%                     {error, instance}
%% @end
%%--------------------------------------------------------------------
cargo(Loc) ->
  Depots = ets:select(depots, [{{'$1', '$2'}, [], ['$2']}]),
  ets:new(cargoroute, [ordered_set, named_table]),
  routeCargo(Loc, Depots).

%%--------------------------------------------------------------------
%% @doc
%% The lookup function provides useful inforamtion on an orders status in the
%% system.
%% @spec lookup(Ref) -> {error, instance} |
%%                      {ok,{Ref,From,To,Kg,Loc|VehiclePid,OwnerPid}}
%% @end
%%--------------------------------------------------------------------
lookup(Ref) ->
  %% It would have been nice to simply{ok, ets:lookup(?MODULE, Ref), self(), ?MODULE}
  %% and return [] for no results but in the interests of API compliance...
  Result = ets:lookup(?MODULE, Ref),
  Length = length(Result),
  if
    Length > 0 -> {ok, Result, self(), ?MODULE};
    Length =< 0 -> {error, Ref}
  end.


%%%===================================================================
%% Generic recieving loop pattern
%%%===================================================================
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


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Calls supporting functions to setup ETS tab
init() ->
  register(?MODULE, self()),
  io:format("Manager started ~n"),
  % moved setup operations to its own module
  setup:init(),
  loop().

%% Creates unique references to track packages using the now() function
uniqueref({A, B, C}) ->
  (A * 1000000000000) + (B * 1000000) + C.

%% Caluclates how many items can be reserved up to a specified weight
%% using an accumilator/buffer
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

% Used by pick and drop API functions
updateManagerByRef([[Ref, _Status, From, To, Kg]], State) ->
  ets:delete(manager, Ref),
  % unfortuantely we cant update bags, only sets,
  ets:insert(manager, {Ref, State, From, To, Kg}).

% Utilised by the transit/2 function in the API
updateTransitState([[Ref, _Status, _From, To, Kg]], Loc) ->
  ets:delete(manager, Ref),
  % unfortuantely we cant update bags, only sets,
  ets:insert(manager, {Ref, indepot, Loc, To, Kg}).

% Used by cargo/1 API function to work out efficient route by hop count
routeCargo(Loc, [Loc|_T]) ->
  ets:delete(cargoroute),
  {ok, Loc};
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