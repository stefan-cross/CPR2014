%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2014 22:15
%%
%% 1. Call manager:deliver/1 to get a route assigned.
%% 2. Reserve space for a set of parcel being picked up from one city to another,
%% ensuring no other truck picks them up using manger:reserve/3.
%% 3. The selected vehicle needs to appear in the sender's city and load the reserved
%% parcels using manager:pick/1.
%% 4. When in a city, you can decide if you want to pick up other parcels within the
%% vehicle’s weight allowance using manger:reserve/2 that have not been
%% reserved or do nothing.
%% 5. Decide if you want to transport the parcel to the receiver or a cargo station.
%% 6. If the parcel is in the cargo station, decide if it should be transported to another
%% cargo station closer to its final destination.
%% 7. It is enough for a van to reach a city for a parcel to be picked up or delivered.
%% Use manager:drop/1 to deliver the parcel.
%% 8. End your round in a cargo station dropping off all remaining parcels you picked
%% up en route using manager:transit/2. Find your closest cargo station using manager:cargo/1.
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
  %%%% Precurser, see if theres anything to drop before we start anything else,
  %%%% this frees up potential capacity as well!
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
  %TODO make reservations ahead of arrival, for now only when in location
  %TODO also, look like a vehicle isnt picking up full capacity, but it does id all potential deliveries above...
  Reservations = formatReserve(Loc, Deliveries, Capacity, Pid), % hardcode capacity for now
  %io:format("~p Reservations complete: ~p ~n", [Pid, Reservations]),

  %% 3. The selected vehicle needs to appear in the sender's city and load the reserved
  %% parcels using manager:pick/1.
  Pickups = formatPick(Reservations, Pid),
  io:format("~p Pickups complete: ~p ~n", [Pid, Pickups]),

  % These orders are from/to the same place! Drop em!
  checkDrop(Pid, Loc),

  % Will just concern outselves with creating a route from this point and traveling said route,
  % will then address the issue of cargo drops etc...
  Route = formatRoute(Pid, Loc),
  Pid ! {route, Route, Pid}. % note Route var is a tuple {From, To, Dist}

% 4. When in a city, you can decide if you want to pick up other parcels within the
%% vehicle’s weight allowance using manger:reserve/2 that have not been
%% reserved or do nothing.
%% 5. Decide if you want to transport the parcel to the receiver or a cargo station.
%% 6. If the parcel is in the cargo station, decide if it should be transported to another
%% cargo station closer to its final destination.
%% 7. It is enough for a van to reach a city for a parcel to be picked up or delivered.
%% Use manager:drop/1 to deliver the parcel.
%% 8. End your round in a cargo station dropping off all remaining parcels you picked
%% up en route using manager:transit/2. Find your closest cargo station using manager:cargo/1.
%


%%%-------------------------------------------------------------------
%% Polcies for finding work...
%%%-------------------------------------------------------------------

updateLocation(Pid, Loc) ->
  ets:update_element(vehicle_sup, Pid, {2, Loc}).

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
%% Supporting and formatting functions, pattern matching heven :-)
%%%-------------------------------------------------------------------

checkDrop(Pid, Loc) ->
  Drop = ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'},[{'==','$4', Loc}],['$$']}]),
  notifyDrop(Drop, Pid).
notifyDrop([[Ref, _Status, From, To, Kg] | T], Pid) ->
  % notify the manager
  manager ! {delivered, Pid, Ref},
  %TODO make auditing set in config?
  DeliveryTime = manager:uniqueref(now()) - Ref,
  ets:insert(delivered, {Ref, delivered, Pid, From, To, Kg, DeliveryTime}),
  ets:delete(Pid, Ref),
  notifyDrop(T, Pid); % dont forget the tail
notifyDrop([], _Pid) -> na.

cargoDrop(Pid, Loc) ->
  Depots = ets:select(Pid, [{{'$1', '$2'},[],['$2']}]),
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

%TODO allow for a config to set loads dynamically?
calculateCapacity(van, Load) ->
  1000 - Load;
calculateCapacity(truck, Load) ->
  20000 - Load.

% err formatting, not pretty, but as per API spec
formatReserve(Loc, {ok, List}, Weight, Pid) -> % how to maximise weight feature
  formatReserve(Loc, List, Weight, Pid);
formatReserve(Loc, [H|_T], Weight, Pid) ->
  Reserved = manager:reserve(Loc, H, Weight),
  processReserved(Reserved, Pid);
  %reserve(Loc, T, Weight); % igonore recusrsion so vehilces dont reserve too much
formatReserve(_Loc, [], _Weight, _Pid) -> ok;
formatReserve(_Loc, {error, _}, _Weight, _Pid) -> ok.

% Takes the output from the manager reserve and inserts into this Pids ets
processReserved({ok, List}, Pid) -> processReserved(List, Pid);
processReserved([[Ref, _Status, From, To, Kg]|T], Pid) ->
  ets:insert(Pid, {Ref, reserved, From, To, Kg}),
  processReserved(T, Pid);
processReserved([], _Pid) -> ok.

%
formatPick({ok, List}, Pid) -> formatPick(List, Pid);
formatPick([[Ref, _Status, _From, _To, _Weight]|T], Pid) ->
  manager:pick(Ref), formatPick(T, Pid);
formatPick([], _Pid) -> ok;
formatPick(ok, _Pid) -> ok.


formatRoute(Pid, Loc) ->
  Deliveries = ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'}, [], ['$4']}]),
  Route = planner:route(Loc, Deliveries),
  %Route = planner ! {route,{Loc, Deliveries}, Pid},
  nextDestination(Loc, Route).

nextDestination(Loc, [[Loc, Next| _T] | _Other]) ->
  % now to calculate the distance
  Distance = getDistance(Loc, Next),
  {Loc, Next, Distance}; % from, to, dist
nextDestination(_Loc, []) -> finished.

getDistance(To, From) ->
  Direction1 = ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', To}, {'==', '$2', From}], ['$3']}]),
  Direction2 = ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', From}, {'==', '$2', To}], ['$3']}]),
  lists:sum(Direction1++Direction2).


% for find work method
simpleRoute(From, To) ->
  Route = planner:route(From, [To]),
  nextDestination(From, Route).

% Work arounf as random:uniform returns same across all pids
randomIndex(TownsIndex) ->
  {_A, _B, C} = now(),
  N = C rem TownsIndex,
  if
    N == 0 -> randomIndex(TownsIndex);
    N /= 0 -> N % , io:format("MyRand = ~p ~n ", [N])
  end.