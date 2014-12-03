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
-author("stefancross").

%% API
-export([start_link/0, init/1, stop/1, add_vehicle/2, remove_vehicle/2]).


%% Starts a new empty vehicle supervisor that is linked to the calling process.
start_link() ->
  proc_lib:start_link(?MODULE, init, [self()]).

init(Pid) ->
  % Could use pg2 to store Pids as intended
  ets:new(?MODULE, [set, named_table, public]),
  io:format("Supervisor ets tab created. ~n"),
  proc_lib:init_ack({ok, Pid}),
  loop().

%% Stop the phone supervisor and terminate all the linked vehicles.
stop(SupPid) ->
  SupPid ! stop.

%% Start a new vehicle process and adds it to the supervision tree.
add_vehicle(SupPid, {Loc, _Kg}) ->
  % ETS tab used instead of list
  ets:insert(?MODULE, {SupPid, Loc}),
  vehicle:start(SupPid, Loc),
  io:format("Vehicle ~p  inserted to ~p table", [SupPid, ?MODULE]),
  {ok, SupPid}.


%% Remove the vehicle from the supervision tree.
remove_vehicle(SupPid, Id) ->
  %TODO Add the contents of the orders back to the manager table
  Packages = ets:select(SupPid, [{{'$1', '$2', '$3', '$4', '$5'}, [], ['$$']}]),
  ets:delete(SupPid),
  returnPackages(Packages).

%TODO currently orders will start process all over again, instead insert where from is current loc?
returnPackages([[Ref, _Status, From, To, Kg]|T]) ->
  ets:insert(manager,[Ref, returned, From, To, Kg]),
  returnPackages(T);
returnPackages([]) -> {ok, all_packages_returned}.


loop() ->
  receive
    {'EXIT', Pid, _Reason} ->
      remove_vehicle(Pid, id),
      add_vehicle(Pid, 0),
      loop()
  end.


