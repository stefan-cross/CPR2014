%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2014 21:18
%%%-------------------------------------------------------------------
-module(manager).
-author("stefancross").

%% API
-export([start_link/0, deliver/1, reserve/3, reserve/2, pick/1, drop/1, transit/2, cargo/1, lookup/1, loop/0, send/3]).

start_link() ->
  register(?MODULE, spawn_link(?MODULE, loop, [])),
  {ok, ?MODULE},
  ets:new(manager, [set, named_table]).


uniqueref({A, B, C}) ->
  %TODO math:pow(10, 12). {1415,821017,790736} as int
  (A * 1000000000000) + (B * 1000000) + C.


send(From, To, Kg) ->
  Ref = uniqueref(now()),
  ets:insert(manager, {Ref, waiting, From, To, Kg}),
  {ok, Ref}.

% Assuming deliveries are potentially parcels in transit,
deliver(Loc) ->
  %TODO deliveries that are going to Loc
  % Deliveries, items in transit to Loc
  Deliveries = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', intransit}, {'==', '$4', Loc}], [['$1', '$2', '$4']]}]),
  % Pickups, items waiting from Loc
  Pickups = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', waiting}, {'==', '$3', Loc}], [['$1', '$2', '$3']]}]),
  Order = Pickups++Deliveries,
  Length = length(Order),
  if
    Length > 0 -> {ok, Order};
    Length =< 0 -> {error, instance}
  end.

%% 51> manager:send("A", "B", 10).
%% {ok,1415971287284657}
%% 52> manager:send("A", "C", 10).
%% {ok,1415971289820290}
%% 53> manager:send("A", "D", 10).
%% {ok,1415971292269273}
%% 54> manager:send("A", "B", 10).
%% {ok,1415971293837010}
%% 55> manager:send("A", "B", 20).
%% {ok,1415971296020752}
%% 56> ets:insert(manager, {1415971287284612,intransit,"A","B",10}).
%% true
%% 58> ets:insert(manager, {1415971287284000,intransit,"B","A",10}).
%% true
%% 59> manager:deliver("A").
%% {ok,[[1415971292269273,waiting,"A"],
%% [1415971293837010,waiting,"A"],
%% [1415971287284657,waiting,"A"],
%% [1415971296020752,waiting,"A"],
%% [1415971289820290,waiting,"A"],
%% [1415971287284000,intransit,"A"]]}
%% 60>




%TODO consider allocation to vehicle
reserve(From, To, Kg) ->
  % Reverse select not quite ordered, but improvement on ets:select which gets oldest first
  Reserved = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', waiting}, {'==', '$3', From}, {'==', '$4', To}], ['$$']}]),
  WeightedReserve = weightedReserve(Reserved, Kg),
  updateReserved(WeightedReserve),
  {ok, WeightedReserve}.

%TODO consider allocation to vehicle
reserve(From, Kg) ->
  % Reverse select not quite ordered, but improvement on ets:select which gets oldest first
  Reserved = ets:select_reverse(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', waiting}, {'==', '$3', From}], ['$$']}]),
  WeightedReserve = weightedReserve(Reserved, Kg),
  updateReserved(WeightedReserve),
  {ok, WeightedReserve}.

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
%% working!
%% 92> c(manager).
%% {ok,manager}
%% 93> manager:start_link().
%% In loopmanager
%% 94> manager:send("A", "B", 10).
%% {ok,1415965996288405}
%% 95> manager:send("A", "B", 10).
%% {ok,1415965996952045}
%% 96> manager:send("A", "B", 10).
%% {ok,1415965997607945}
%% 97> manager:reserve("A", "B", 20).
%% {ok,[[1415965997607945,reserved,"A","B",10],
%% [1415965996288405,reserved,"A","B",10]]}

pick(Ref) ->
  Pick = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', reserved}, {'==', '$1', Ref}], ['$$']}]),
  Length = length(Pick),
  if
    Length > 0 -> updateManagerByRef(Pick, intransit), {ok, intransit, Ref};
    Length =< 1 -> {error, not_reserved, process_instance} %TODO make process_instance identifier dynamic
  end.

drop(Ref) ->
  Drop = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', intransit}, {'==', '$1', Ref}], ['$$']}]),
  Length = length(Drop),
  if
    Length > 0 -> updateManagerByRef(Drop, delivered), {ok, delivered, Ref};
    Length =< 1 -> {error, not_reserved, process_instance} %TODO make process_instance identifier dynamic
  end.

% Used by pick and drop
updateManagerByRef([[Ref, _Status, From, To, Kg]], State) ->
  ets:delete(manager, Ref),
  % unfortuantely we cant update bags, only sets,
  ets:insert(manager, {Ref, State, From, To, Kg}).

%% 85> manager:send("A", "B", 10).
%% {ok,1415976283352170}
%% 86> manager:send("A", "B", 10).
%% {ok,1415976283944349}
%% 87> manager:send("A", "B", 10).
%% {ok,1415976284480379}
%% 88> manager:send("A", "B", 10).
%% {ok,1415976285056521}
%% 89> manager:drop(1415975263454240).
%% {ok,delivered,1415975263454240}
%% 90> manager:drop(1415975263454240).
%% {error,not_reserved,process_instance}
%% 91> manager:drop(1415975262934475).
%% {error,not_reserved,process_instance}
%% 92> manager:drop(1415975264599003).
%% {ok,delivered,1415975264599003}
%% 93> manager:reserve("A", "B", 40).
%% {ok,[[1415976285056521,reserved,"A","B",10],
%% [1415976283352170,reserved,"A","B",10],
%% [1415976284480379,reserved,"A","B",10],
%% [1415975263974289,reserved,"A","B",10]]}
%% 94> manager:pick("A").
%% {error,not_reserved,process_instance}
%% 95> manager:pick(1415976285056521).
%% {ok,intransit,1415976285056521}
%% 96> manager:pick(1415976283352170).
%% {ok,intransit,1415976283352170}
%% 97> manager:pick([1415976283352170, 1415976283352170]).
%% {error,not_reserved,process_instance}
%% 98> manager:drop(1415976283352170).
%% {ok,delivered,1415976283352170}
%% 99> manager:drop(1415976285056521).
%% {ok,delivered,1415976285056521}


transit(Ref, Loc) ->
  Trans = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', intransit}, {'==', '$1', Ref}], ['$$']}]),
  Length = length(Trans),
  if
    Length > 0 -> updateTransitState(Trans, Loc), {ok, indepot, Ref};
    Length =< 1 -> {error, not_indepot, process_instance} %TODO make process_instance identifier dynamic
  end.

updateTransitState([[Ref, _Status, _From, To, Kg]], Loc) ->
  ets:delete(manager, Ref),
  % unfortuantely we cant update bags, only sets,
  ets:insert(manager, {Ref, indepot, Loc, To, Kg}).

%% 106> manager:send("A", "B", 10).
%% {ok,1415977993346737}
%% 107> manager:send("A", "B", 10).
%% {ok,1415977993874814}
%% 108> manager:send("A", "C", 10).
%% {ok,1415977996858774}
%% 109> manager:send("A", "D", 10).
%% {ok,1415977999946719}
%% 110> manager:reserve("A", "B", 20).
%% {ok,[[1415977993874814,reserved,"A","B",10],
%% [1415977992083836,reserved,"A","B",10]]}
%% 111> manager:pick(1415977993874814).
%% {ok,intransit,1415977993874814}
%% 112> manager:transit(1415977993874814, "X").
%% {ok,indepot,1415977993874814}
%% 113> manager:transit(1415977996858774, "X").
%% {error,not_indepot,process_instance}
%% 114> manager:transit(1415977996858774, "X").


cargo(Loc) ->
  Depots = ets:select(depots, [{{'$1', '$2'}, [], ['$2']}]),
   ets:new(cargoroute, [ordered_set, named_table]),
  routeCargo(Loc, Depots).

routeCargo(Loc, [H|T]) ->
  Route = digraph:get_short_path(lists:nth(1, ets:lookup(graph, digraph)), Loc, H),
  RouteDistance = length(Route),
  ets:insert(cargoroute, {RouteDistance, H}),
  routeCargo(Loc, T);
routeCargo(_Loc, []) ->
  % why didnt this work?! ets:select(cargoroute, [{{'$1', '$2'}, ['==', '$1', Val], ['$2']}]).
  KV = ets:lookup(cargoroute, ets:first(cargoroute)),
  formatCargoRoute(KV).
formatCargoRoute([{_K, V}]) ->
  % dispose of our temp-cargoroute table used to determine shortest route
  ets:delete(cargoroute),
  {ok, V}.

%% 147> manager:cargo("Lublin").
%% {ok,"Kraków"}
%% 148> manager:cargo("Wrocław").
%% {ok,[321,243,100,378]}
%% 149> manager:cargo("Białystok").
%% {ok,"Warszawa"}


lookup(Ref) ->
  {ok, ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$1', Ref}], ['$$']}]), vehiclePid, ownerPid}.

%%
%% 160> manager:lookup(1415996772636361).
%% {ok,[[1415996772636361,waiting,"A","C",12]],
%% vehiclePid,ownerPid}

  loop() ->
  io:format("In loop"),
  receive
    {From, To, Kg} ->
      io:format("Message receieved ~p~n", [self()]),
      send(From, To, Kg),
      loop();
    exit ->
      io:format("Exiting"),
      ok
  end.