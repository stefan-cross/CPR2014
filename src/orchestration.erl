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
-module(orchestration).
-author("stefancross").

%% API
-export([start/2]).

%%%-------------------------------------------------------------------
%% Policy manager
%%%-------------------------------------------------------------------
start(Pid, Loc) ->

  % ** Precurser, see if theres anything to drop before we start anything else,
  % ** this is free up potential capacity as well!
  Drop = checkDrop(Pid, Loc),
  io:format("~p dropping : ~p ~n", [Pid, Drop]),

  VehicleType = getType(atom_to_list(Pid)),
  CurrentLoad = lists:sum(ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'}, [], ['$5']}])),
  Capacity = calculateCapacity(VehicleType, CurrentLoad),
  io:format("~p capacity is: ~p ~n", [Pid, Capacity]),

  %% 1. Call manager:deliver/1 to get a route assigned.
  Deliveries = manager:deliver(Loc),
  io:format("Deliveries available: ~p. ~n", [Deliveries]),

  %% 2. Reserve space for a set of parcel being picked up from one city to another,
  %% ensuring no other truck picks them up using manger:reserve/3.
  %TODO make reservations ahead of arrival, for now only when in location
  Reservations = formatReserve(Loc, Deliveries, Capacity, Pid), % hardcode capacity for now
  io:format("Reservations complete: ~p ~n", [Reservations]),

  %% 3. The selected vehicle needs to appear in the sender's city and load the reserved
  %% parcels using manager:pick/1.
  Pickups = formatPick(Reservations, Pid),
  io:format("Pickups complete: ~p ~n", [Pickups]).


%%%-------------------------------------------------------------------
%% Supporting and formatting functions
%%%-------------------------------------------------------------------

checkDrop(Pid, Loc) ->
  Drop = ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'},[{'==','$4', Loc}],['$$']}]),
  notifyDrop(Drop, Pid).
notifyDrop([[Ref, _Status, From, To, Kg] | T], Pid) ->
  % notify the manager
  manager ! {delivered, Pid, Ref},
  %TODO make auditing set in config?
  DeliveryTime = manager:uniqueref(now()) - Ref,
  ets:insert(delivered, {Ref, delivered, From, To, Kg, DeliveryTime}),
  ets:delete(Pid, Ref),
  notifyDrop(T, Pid); % dont forget the tail
notifyDrop([], _Pid) -> na.

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
% TODO this additional formatting function to place Pid on orders
formatReserve(Loc, {ok, List}, Weight, Pid) -> % how to maximise weight feature
  formatReserve(Loc, List, Weight, Pid);
formatReserve(Loc, [H|_T], Weight, Pid) ->
  Reserved = manager:reserve(Loc, H, Weight),
  processReserved(Reserved, Pid);
  %reserve(Loc, T, Weight); % igonore recusrsion for now, lets get it working with one
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