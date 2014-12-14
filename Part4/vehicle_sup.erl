%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2014 17:38
%%%
%%% Practical Part 4: Implementing a Vehicle Supervisor
%%%
%%% At a vehilce level it suffices to have a one_for_many supervision tree structure,
%%% we roll our own here as opposed to using the OTP sup. We will look to make use of
%%% OTP sups at the top level.
%%%
%%%-------------------------------------------------------------------
-module(vehicle_sup).

-export([start_link/0, stop/1, add_vehicle/2, remove_vehicle/2]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start a new process that is linked to the calling process, top_sup
%% @spec start_link() -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  {ok, spawn_link(fun() -> init() end)}.

%%--------------------------------------------------------------------
%% @doc
%% Stop the vehicle supervisor and terminate all the linked vehicles.
%% @spec stop(SupPid) -> ok
%% @end
%%--------------------------------------------------------------------
stop(_SupPid) ->
  Vehicles = ets:select(?MODULE, [{{'$1', '$2', '$3'}, [], [['$1']]}]),
  stopVehicles(Vehicles),
  timer:sleep(1000),
  exit(self(), normal).

%%--------------------------------------------------------------------
%% @doc
%% Start a new vehicle process and adds it to the supervision tree.
%% @spec add_vehichle(SupPid,{Loc,Kg}) -> {ok, Id}
%% @end
%%--------------------------------------------------------------------
add_vehicle(Name, {Loc, _Kg}) ->
  ?MODULE ! {add_vehicle, Name, Loc}.

%%--------------------------------------------------------------------
%% @doc
%% Remove the vehicle from the supervision tree.
%% @spec remove_vehichle(SupPid, Id) -> ok
%% @end
%%--------------------------------------------------------------------
remove_vehicle(SupPid, _Id) ->
  ?MODULE ! {remove_vehicle, SupPid}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Register and enter in to recieving loop
init() ->
  register(?MODULE, self()),
  process_flag(trap_exit, true),
  ets:new(?MODULE, [set, named_table, public]),
  loop().

stopVehicles([[H|_]|T]) ->
  H ! stop, stopVehicles(T);
stopVehicles([]) -> ok.

returnPackages(Pid) ->
  Packages = ets:select(Pid, [{{'$1', '$2', '$3', '$4', '$5'}, [], ['$$']}]),
%%   TODO work out why this fails..
%%   [Loc|_T] = ets:select(vehicle_sup, [{{'$1', '$2', '$3'}, [{'==', '$1', Pid}], ['$3']}]),
  returnPackages(Packages, "Warszawa"). % Hardcoded return to "Warszawa" because of above...
returnPackages([[Ref, _Status, _From, To, Kg]|T], Loc) ->
  ets:insert(manager,{Ref, returned, Loc, To, Kg}),
  returnPackages(T, Loc);
returnPackages([], _Loc) -> {ok, all_packages_returned}.

%%%===================================================================
%%  Recieving loop pattern
%%%===================================================================
loop() ->
  receive
    {'EXIT', Pid, graceful} ->
      io:format("Vehicle Supervised process ~p exiting gracefully. ~n", [Pid]),
      loop();
    {'EXIT', Pid, Reason} ->
      io:format("Vehicle Supervisor - ERROR: ~p, ~p ~n", [Pid, Reason]),
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
