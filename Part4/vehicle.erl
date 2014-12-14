%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%
%%% Vehicle processes act as finite state machine with state being
%%% at-location and in-transit.
%%%
%%% Strategy is to have autonomous vehicle Pids holding own ets tables
%%%
%%%-------------------------------------------------------------------
-module(vehicle).
-author("stefancross").

%% API
-export([start/2]).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new manager process linking it to the starting supervisor
%% @spec start_link() -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start(Pid, Loc)->
  {ok, spawn_link(fun() -> init(Pid, Loc)end)}.

%%%===================================================================
%%% Vechile FSM abstraction, atLocation and inTransit as the only two states
%%%===================================================================

atlocation(Pid, Loc) ->
  %% 4. When in a city, you can decide if you want to pick up other parcels within the
  %% vehicle’s weight allowance using manger:reserve/2 that have not been
  %% reserved or do nothing.
  updateLocation(Pid, Loc),
  Capacity = checkCapacity(Pid),
  formatReserve(Loc, Capacity, Pid),

  Pickups = checkPickUps(Pid, Loc),
  formatPick(Pickups, Pid),

  io:format("Vehicle - ~p , at location: ~p with capacity ~p ~n", [Pid, Loc, Capacity]),
  % useful for debugging msgbox, process_info(whereis(van1), messages).

  %% 7. It is enough for a van to reach a city for a parcel to be picked up or delivered.
  %% Use manager:drop/1 to deliver the parcel.
  checkDrop(Pid, Loc),

  Route = formatRoute(Pid, Loc),
  Pid ! {route, Route, Pid}, % note Route var is a tuple {From, To, Dist}

  receive
    {route, {From, To, Dist}, Pid} ->
      intransit({Pid, From, To, Dist});
    {route, finished, Pid} ->
      findWork(Pid, Loc);
    stop -> exit(graceful)
  end.

intransit({Pid, From, To, Dist}) ->
  io:format("Vehicle  ~p , in transit ~p~n", [Pid, {From, To, Dist}]),
  % Sleep to simulate drivetime, tune in to radio 6 ;-)
  timer:sleep(Dist),
  atlocation(Pid, To).

%%%===================================================================
%%% Internal initialisation
%%%===================================================================
init(Pid, Loc) ->
  register(Pid, self()),

  % Strategy is for each vehicle PID to have own ETS tab to try and reduce load
  ets:new(Pid, [set, named_table, public, {heir, whereis(manager), []}]),

  updateLocation(Pid, Loc),
  %% Precurser, see if theres anything to drop before we start anything else,
  %%this frees up potential capacity as well!
  checkDrop(Pid, Loc),
  Capacity = checkCapacity(Pid),

  %% 1. Call manager:deliver/1 to get a route assigned.
  Deliveries = manager:deliver(Loc),
  %Deliveries = manager ! {self(), {deliveries, Loc}},
  %io:format("~p deliveries available: ~p. ~n", [Pid, Deliveries]),

  %% 2. Reserve space for a set of parcel being picked up from one city to another,
  %% ensuring no other truck picks them up using manger:reserve/3.
  formatReserve(Loc, Deliveries, Capacity, Pid),

  %% 3. The selected vehicle needs to appear in the sender's city and load the reserved
  %% parcels using manager:pick/1.
  Pickups = checkPickUps(Pid, Loc),
  formatPick(Pickups, Pid),
  % Check there are no orders to and from the same place...
  checkDrop(Pid, Loc),
  % Enter into vehicle finate state machine loop
  atlocation(Pid, Loc).


%%%-------------------------------------------------------------------
%% Polcies for finding work...
%%%-------------------------------------------------------------------

updateLocation(Pid, Loc) ->
  ets:update_element(vehicle_sup, Pid, {3, Loc}).

findDepot(Pid, Loc) ->
  Depots = ets:select(depot, [{{'$1', '$2'}, [], ['$2']}]),
  DepotsIndex = length(Depots),
  Random = randomIndex(DepotsIndex),
  RandomDepot = list:nth(Random, Depots),
  if
    Loc == RandomDepot -> findDepot(Pid, Loc); % roll again
    Loc /= Random ->
      {From, To, Dist} = simpleRoute(Loc, RandomDepot),
      io:format("~p going to DEPOT ~p for work. ~n", [Pid, RandomDepot]),
      intransit({Pid, From, To, Dist})
  end.

findWork(Pid, Loc) ->
  Towns = ets:select(towns, [{{'$1', '$2'}, [], ['$1']}]),
  TownsIndex = length(Towns),
  Random = randomIndex(TownsIndex),
  RandomTown = lists:nth(Random, Towns),
  % avoid going to same location, lists:delete was causing issues so we have to use the sequential way.
  if
    Loc == RandomTown -> findWork(Pid, Loc);
    Loc /= RandomTown ->
      {From, To, Dist} = simpleRoute(Loc, RandomTown),
      io:format("Finding work for ~p , going to ~p ~n", [Pid, RandomTown]),
      intransit({Pid, From, To, Dist})
  end.


%%%===================================================================
%%% Formatting functions to maintain API spec
%%%===================================================================
%% There are more efficient and nearer ways to achieve this functionality
%% but in order to maintain the previous planner and manger APIs we format
%% those functions output.
formatReserve(Loc, {ok, List}, Weight, Pid) ->
  formatReserve(Loc, List, Weight, Pid);
formatReserve(Loc, [H|_T], Weight, Pid) ->
  Reserved = manager:reserve(Loc, H, Weight),
  processReserved(Reserved, Pid);
formatReserve(_Loc, [], _Weight, _Pid) -> ok;
formatReserve(_Loc, {error, _}, _Weight, _Pid) -> ok.
formatReserve(Loc, Weight, Pid) ->
  Reserved = manager:reserve(Loc, Weight),
  processReserved(Reserved, Pid).

% Takes the output from the manager reserve and inserts into this Pids ets
processReserved({ok, List}, Pid) -> processReserved(List, Pid);
processReserved([[Ref, _Status, From, To, Kg]|T], Pid) ->
  ets:insert(Pid, {Ref, reserved, From, To, Kg}),
  processReserved(T, Pid);
processReserved([], _Pid) -> ok.

% Check if a a vehicle has any packages eligible for picking up
formatPick([Ref|T], Pid) ->
  manager:pick(Ref),
  ets:update_element(Pid, Ref, {2, intransit}),
  formatPick(T, Pid);
formatPick([], _Pid) -> ok;
formatPick(ok, _Pid) -> ok.

% From our reserved and picked orders create a planned route
formatRoute(Pid, Loc) ->
  Deliveries = ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'}, [], ['$4']}]),
  Route = planner:route(Loc, Deliveries),
  %Route = planner ! {route,{Loc, Deliveries}, Pid},
  nextDestination(Loc, Route).

% format our route from above to just get the next hop
nextDestination(Loc, [[Loc, Next| _T] | _Other]) ->
  % now to calculate the distance
  Distance = getDistance(Loc, Next),
  {Loc, Next, Distance}; % from, to, dist
nextDestination(_Loc, []) -> finished.

% Used to caluculate the travel time
getDistance(To, From) ->
  Direction1 = ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', To}, {'==', '$2', From}], ['$3']}]),
  Direction2 = ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', From}, {'==', '$2', To}], ['$3']}]),
  lists:sum(Direction1++Direction2).

% for find work method
simpleRoute(From, To) ->
  Route = planner:route(From, [To]),
  nextDestination(From, Route).

% Work around as random:uniform returns same across all pids
randomIndex(TownsIndex) ->
  {_A, _B, C} = now(),
  N = C rem TownsIndex,
  if
    N == 0 -> randomIndex(TownsIndex);
    N /= 0 -> N % , io:format("MyRand = ~p ~n ", [N])
  end.

%%  Check if a a vehicle has any packages eligible for dropping
checkDrop(Pid, Loc) ->
  Drop = ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'},[{'==','$4', Loc}],['$$']}]),
  notifyDrop(Drop, Pid).
notifyDrop([[Ref, _Status, From, To, Kg] | T], Pid) ->
  % notify the manager
  manager ! {delivered, Pid, Ref},
  DeliveryTime = manager:uniqueref(now()) - Ref,
  ets:insert(delivered, {Ref, delivered, Pid, From, To, Kg, DeliveryTime}),
  ets:delete(Pid, Ref),
  notifyDrop(T, Pid);
notifyDrop([], _Pid) -> na.

%%  Check if a a vehicle has any packages eligible for dropping at cargo
cargoDrop(Pid, Loc) ->
  Depots = ets:select(cargo, [{{'$1', '$2'},[],['$2']}]),
  Result = lists:member(Loc, Depots),
  if
    Result == true ->
      {Ref, _Status, _From, To, Kg} = ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'},[{'==','$4', Loc}],['$$']}]),
      ets:insert(Pid, {Ref, indepot, Loc, To, Kg});
    Result /= true -> no_depot_drop
  end.

%%  Check if a a vehicle has any packages eligible for picking up in current location
checkPickUps(Pid, Loc) ->
  ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'},[{'==','$2', reserved},{'==','$3', Loc}],['$1']}]).


checkCapacity(Pid) ->
  VehicleType = getType(atom_to_list(Pid)),
  CurrentLoad = lists:sum(ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'}, [], ['$5']}])),
  calculateCapacity(VehicleType, CurrentLoad).

%% Capacity as per spec
calculateCapacity(van, Load) ->
  1000 - Load;
calculateCapacity(truck, Load) ->
  20000 - Load.

%% Identify by staring char
getType([H|_]) ->
  if
    H == 118 -> van;
    H == 116 -> truck
  end.
