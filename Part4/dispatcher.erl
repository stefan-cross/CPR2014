%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2014 22:15
%%
%% These functions handle the vehicle routing to keep the vehicle
%% module a bit cleaner and less cluttered. The only functions that
%% need to be exported are start/2, goto_depot/2, goto_random_town/2
%%
%%%-------------------------------------------------------------------
-module(dispatcher).
-author("stefancross").

%% API
-export([start/2, goto_depot/2, goto_random_town/2]).

%%%-------------------------------------------------------------------
%% Policy manager
%%%-------------------------------------------------------------------
start(Pid, Loc) ->

  updateLocation(Pid, Loc),
  %% Precurser, see if theres anything to drop before we start anything else,
  %%this frees up potential capacity as well!
  Drop = checkDrop(Pid, Loc),

  VehicleType = getType(atom_to_list(Pid)),
  CurrentLoad = lists:sum(ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'}, [], ['$5']}])),
  Capacity = calculateCapacity(VehicleType, CurrentLoad),
  io:format("~p dropping: ~p , capacity is: ~p ~n", [Pid, Drop, Capacity]),

  %% 1. Call manager:deliver/1 to get a route assigned.
  Deliveries = manager:deliver(Loc),
  %Deliveries = manager ! {self(), {deliveries, Loc}},
  %io:format("~p deliveries available: ~p. ~n", [Pid, Deliveries]),

  %% 2. Reserve space for a set of parcel being picked up from one city to another,
  %% ensuring no other truck picks them up using manger:reserve/3.
  Reservations = formatReserve(Loc, Deliveries, Capacity, Pid),
  %io:format("~p Reservations complete: ~p ~n", [Pid, Reservations]),

  %% 3. The selected vehicle needs to appear in the sender's city and load the reserved
  %% parcels using manager:pick/1.
  Pickups = formatPick(Reservations, Pid),
  io:format("~p Pickups complete: ~p ~n", [Pid, Pickups]),

  % These orders are from/to the same place! Drop em!
  checkDrop(Pid, Loc),

  % Will just concern outselves with creating a route from this point and traveling said route,
  Route = formatRoute(Pid, Loc),
  Pid ! {route, Route, Pid}. % note Route var is a tuple {From, To, Dist}


%%%-------------------------------------------------------------------
%% Polcies for finding work...
%%%-------------------------------------------------------------------

updateLocation(Pid, Loc) ->
  ets:update_element(vehicle_sup, Pid, {3, Loc}).

goto_depot(Pid, Loc) ->
  Depots = ets:select(depot, [{{'$1', '$2'}, [], ['$2']}]),
  DepotsIndex = length(Depots),
  Random = randomIndex(DepotsIndex),
  RandomDepot = list:nth(Random, Depots),
  if
    Loc == RandomDepot -> goto_depot(Pid, Loc); % roll again
    Loc /= Random ->
      Route = simpleRoute(Loc, RandomDepot),
      io:format("~p going to depot ~p for work. ~n", [Pid, RandomDepot]),
      Pid ! {route, Route, Pid} % note Route var is a tuple {From, To, Dist}
  end.

goto_random_town(Pid, Loc) ->
  Towns = ets:select(towns, [{{'$1', '$2'}, [], ['$1']}]),
  TownsIndex = length(Towns),
  Random = randomIndex(TownsIndex),
  RandomTown = lists:nth(Random, Towns),
  % avoid going to same location, lists:delete was causing issues so we have to use the sequential way.
  if
    Loc == RandomTown -> goto_random_town(Pid, Loc);
    Loc /= RandomTown ->
      Route = simpleRoute(Loc, RandomTown),
      io:format("Finding work for ~p , going to ~p ~n", [Pid, RandomTown]),
      Pid ! {route, Route, Pid} % note Route var is a tuple {From, To, Dist}
  end.


%%%-------------------------------------------------------------------
%% Supporting and formatting functions
%%%-------------------------------------------------------------------
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
      {Ref, _Status, From, To, Kg} = ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'},[{'==','$4', Loc}],['$$']}]),
      ets:insert(Pid, {Ref, indepot, Loc, To, Kg});
    Result /= true -> no_depot_drop
  end.

% as our Pids start with vehicle type we can get type by inspecting the starting letter..
% we can use this to determine capacity
getType([H|_]) ->
  %TODO fix
  % this is ugly but the followoing is an illegal guard expression
  % "van" =:= string:sub_string(atom_to_list(Pid), 1, 3) -> van
  if
    H == 118 -> van;
    H == 116 -> truck % wtf.. 116 == lists:nth(1, atom_to_list(t)).
  end.

%Calculate vehicle capacity depending on vehicle type
%TODO allow for a config to set loads dynamically
calculateCapacity(van, Load) ->
  1000 - Load;
calculateCapacity(truck, Load) ->
  20000 - Load.

% Unpleasant formatting, but so to satisfy the API spec
formatReserve(Loc, {ok, List}, Weight, Pid) ->
  formatReserve(Loc, List, Weight, Pid);
formatReserve(Loc, [H|_T], Weight, Pid) ->
  Reserved = manager:reserve(Loc, H, Weight),
  processReserved(Reserved, Pid);
formatReserve(_Loc, [], _Weight, _Pid) -> ok;
formatReserve(_Loc, {error, _}, _Weight, _Pid) -> ok.

% Takes the output from the manager reserve and inserts into this Pids ets
processReserved({ok, List}, Pid) -> processReserved(List, Pid);
processReserved([[Ref, _Status, From, To, Kg]|T], Pid) ->
  ets:insert(Pid, {Ref, reserved, From, To, Kg}),
  processReserved(T, Pid);
processReserved([], _Pid) -> ok.

% Check if a a vehicle has any packages eligible for picking up
formatPick({ok, List}, Pid) -> formatPick(List, Pid);
formatPick([[Ref, _Status, _From, _To, _Weight]|T], Pid) ->
  manager:pick(Ref), formatPick(T, Pid);
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