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

start(Pid, Loc) ->
  %% 1. Call manager:deliver/1 to get a route assigned.
  Deliveries = manager:deliver(Loc),
  io:format("Deliveries available: ~p. ~n", [Deliveries]),

  %% 2. Reserve space for a set of parcel being picked up from one city to another,
  %% ensuring no other truck picks them up using manger:reserve/3.
  Reservations = formatReserve(Loc, Deliveries, 2000), % hardcode capacity for now
  io:format("Reservations complete: ~p ~n", [Reservations]),

  %% 3. The selected vehicle needs to appear in the sender's city and load the reserved
  %% parcels using manager:pick/1.
  Pickups = formatPick(Reservations),
  io:format("Pickups complete: ~p ~n", [Pickups]).


% err formatting, not pretty, but as per API spec
% TODO this additional formatting function to place Pid on orders
formatReserve(Loc, {ok, List}, Weight) -> % how to maximise weight feature
  formatReserve(Loc, List, Weight);
formatReserve(Loc, [H|_T], Weight) ->
  manager:reserve(Loc, H, Weight);
  %reserve(Loc, T, Weight); % igonore recusrsion for now, lets get it working with one
formatReserve(_Loc, [], _Weight) -> ok;
formatReserve(_Loc, {error, _}, _Dist) -> ok.

%
formatPick({ok, List}) -> formatPick(List);
formatPick([[Ref, _Status, _From, _To, _Weight]|T]) ->
  io:format("LOKKING FOR REF: ~p ~n", [Ref]),
  manager:pick(Ref), formatPick(T);
formatPick([]) -> ok;
formatPick(ok) -> ok.