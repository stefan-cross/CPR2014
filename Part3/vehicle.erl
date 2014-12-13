%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2014 21:59
%%%
%%%
%%% Vehicle processes act as finite state machine with state being
%%% at-location and in-transit.
%%%
%%%
%%%-------------------------------------------------------------------

-module(vehicle).
-author("stefancross").

%% API
-export([start/2]).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new manager process
%% @spec start_link() -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start(Pid, Loc)->
  {ok, spawn(fun() -> init(Pid, Loc)end)}.


%%%===================================================================
%%% Vechile FSM abstraction, atLocation and inTransit as the only two states
%%%===================================================================

atlocation(Pid, Loc) ->
  %% 4. When in a city, you can decide if you want to pick up other parcels within the
  %% vehicle’s weight allowance using manger:reserve/2 that have not been
  %% reserved or do nothing.
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
      %% 8. End your round in a cargo station dropping off all remaining parcels you picked
      %% up en route using manager:transit/2. Find your closest cargo station using manager:cargo/1.
      checkDrop(Pid, Loc),
      % Can replace with other routing function such as gotoDepot/2 but findWork/2 is
      % best effort with time so far
      findWork(Pid, Loc)
  end.

intransit({Pid, From, To, Dist}) ->
  io:format("Vehicle  ~p , in transit ~p~n", [Pid, {From, To, Dist}]),
  timer:sleep(Dist),
  atlocation(Pid, To).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init(Pid, Loc) ->
  register(Pid, self()),
  %% 1. Call manager:deliver/1 to get a route assigned.
  Deliveries = manager:deliver(Loc),
  %% 2. Reserve space for a set of parcel being picked up from one city to another,
  %% ensuring no other truck picks them up using manger:reserve/3.
  Capacity = checkCapacity(Pid),
  formatReserve(Loc, Deliveries, Capacity, Pid),
  %% 3. The selected vehicle needs to appear in the sender's city and load the reserved
  %% parcels using manager:pick/1.
  Pickups = checkPickUps(Pid, Loc),
  formatPick(Pickups, Pid),
  % Check there are no orders to and from the same place...
  checkDrop(Pid, Loc),
  % Enter into vehicle finate state machine loop
  atlocation(Pid, Loc).

%%%===================================================================
%%% Formatting functions to maintain API spec
%%%===================================================================
%% There are more efficient and nearer ways to achieve this functionality
%% but in order to maintain the previous planner and manger APIs we format
%% those functions output.

formatReserve(Loc, {ok, List}, Weight, Pid) -> % how to maximise weight feature
  formatReserve(Loc, List, Weight, Pid);
formatReserve(Loc, [H|_T], Weight, Pid) ->
  Reserved = manager:reserve(Loc, H, Weight),
  processReserved(Reserved, Pid);
formatReserve(_Loc, [], _Weight, _Pid) -> ok;
formatReserve(_Loc, {error, _}, _Weight, _Pid) -> ok.
formatReserve(Loc, Weight, Pid) ->
  Reserved = manager:reserve(Loc, Weight),
  processReserved(Reserved, Pid).

% Takes the output from the manager reserve updates manager ETS
processReserved({ok, List}, Pid) -> processReserved(List, Pid);
processReserved([[Ref, _Status, _Pid, From, To, Kg]|T], Pid) ->
  ets:delete(manager, Ref),
  ets:insert(manager, {Ref, reserved, Pid, From, To, Kg}),
  processReserved(T, Pid);
processReserved([], _Pid) -> ok.

% Allows us to pick up a list or reservations
formatPick([H|T], Pid) ->
  manager:pick(H), formatPick(T, Pid),
  formatPick(T, Pid);
formatPick([], _Pid) -> ok;
formatPick(ok, _Pid) -> ok.

% From our reserved and picked orders create a planned route
formatRoute(Pid, Loc) ->
  Deliveries = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5', '$6'}, [{'==', '$2', intransit}, {'==', '$3', Pid}], ['$5']}]),
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

%%  Check if a a vehicle has any packages eligible for dropping
checkDrop(Pid, Loc) ->
  Drop = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5', '$6'},[{'==','$2', intransit}, {'==','$3', Pid},{'==','$5', Loc}],['$$']}]),
  notifyDrop(Drop, Pid).
notifyDrop([[Ref, _Status, _Pid, From, To, Kg] | T], Pid) ->
  % notify the manager
  manager ! {delivered, Pid, Ref},
  ets:delete(manager, Ref),
  ets:insert(manager, {Ref, delivered, Pid, From, To, Kg}),
  notifyDrop(T, Pid);
notifyDrop([], _Pid) -> na.

%%  Check if a a vehicle has any packages eligible for picking up
checkPickUps(Pid, Loc) ->
  ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5', '$6'},[{'==','$2', reserved}, {'==','$3', Pid},{'==','$4', Loc}],['$1']}]).

checkCapacity(Pid) ->
  VehicleType = getType(atom_to_list(Pid)),
  CurrentLoad = lists:sum(ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5', '$6'}, [{'==','$2', intransit}, {'==','$3', Pid}], ['$6']}])),
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

%%%===================================================================
%%% Routing functions
%%%===================================================================

gotoDepot(Pid, Loc) ->
  Depots = ets:select(depots, [{{'$1', '$2'}, [], ['$2']}]),
  DepotsIndex = length(Depots),
  Random = randomIndex(DepotsIndex),
  RandomDepot = lists:nth(Random, Depots),
  if
    Loc == RandomDepot -> gotoDepot(Pid, Loc); % roll again
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

% Work around as random:uniform returns same across all pids
randomIndex(Index) ->
  {_A, _B, C} = now(),
  N = C rem Index,
  if
    N == 0 -> randomIndex(Index);
    N /= 0 -> N % , io:format("MyRand = ~p ~n ", [N])
  end.

% for find work method
simpleRoute(From, To) ->
  Route = planner:route(From, [To]),
  nextDestination(From, Route).