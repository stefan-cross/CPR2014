%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2014 17:38
%%%
%%% At a vehilce level it suffices to have a one_for_one supervision tree structure,
%%% we roll our own here as opposed to using the OTP sup. We will look to make use of
%%% OTP sups at the top level perhaps.
%%%
%%%-------------------------------------------------------------------
-module(vehicle_sup).

-export([start_link/0, init/0, stop/1, add_vehicle/2, remove_vehicle/2]).


start_link() ->
  register(?MODULE, spawn(?MODULE, init, [])).

init() ->
  process_flag(trap_exit, true),
  ets:new(?MODULE, [set, named_table, public]),
  loop().

stop(_SupPid) ->
  Vehicles = ets:select(?MODULE, [{{'$1', '$2', '$3'}, [], [['$1']]}]),
  stopVehicles(Vehicles),
  timer:sleep(1000),
  exit(self(), normal).

stopVehicles([[H|_]|T]) ->
  H ! stop, stopVehicles(T);
stopVehicles([]) -> ok.

add_vehicle(Name, {Loc, _Kg}) ->
  ?MODULE ! {add_vehicle, Name, Loc}.

%% Remove the vehicle from the supervision tree.
remove_vehicle(SupPid, _Id) ->
  ?MODULE ! {remove_vehicle, SupPid}.

returnPackages(Pid) ->
  Packages = ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'}, [], ['$$']}]),
%%   TODO work out why this fails..
%%   [Loc|_T] = ets:select(vehicle_sup, [{{'$1', '$2', '$3'}, [{'==', '$1', Pid}], ['$3']}]),  % I have no idea why this isnt working!?
  returnPackages(Packages, "Warszawa"). % Hardcoded return to "Warszawa"
returnPackages([[Ref, _Status, _From, To, Kg]|T], Loc) ->
  ets:insert(manager,{Ref, returned, Loc, To, Kg}),
  returnPackages(T, Loc);
returnPackages([], _Loc) -> {ok, all_packages_returned}.



loop() ->
  io:format("Supervisor Looping ~n"),
  receive
    {'EXIT', Pid, graceful} ->
      io:format("Supervisor process ~p exiting gracefully. ~n", [Pid]),
      loop();
    {'EXIT', Pid, Reason} ->
      io:format("Supervisor - ERROR: ~p, ~p ~n", [Pid, Reason]),
      [[Name, Id, Loc]] = ets:select(?MODULE, [{{'$1', '$2', '$3'}, [{'==', '$2', pid_to_list(Pid)}], [['$1', '$2', '$3']]}]),
      ets:delete(?MODULE, Name),
      returnPackages(Name),
      ets:delete(Name),
      add_vehicle(Name, {Loc, Id}),
      loop();
    {add_vehicle, Name, Loc} ->
      {ok, Pid} = vehicle:start(Name, Loc),
      ets:insert(?MODULE, {Name, pid_to_list(Pid), Loc}),
      loop();
    {remove_vehicle, Name} ->
      ets:delete(?MODULE, Name),
      Name ! stop,
      loop()
  end.
