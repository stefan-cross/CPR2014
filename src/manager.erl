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
-export([start_link/0, deliver/1, reserve/3, loop/0, send/3]).

start_link() ->
  register(?MODULE, spawn_link(?MODULE, loop, [])),
  ets:new(manager, [set, named_table]).


uniqueref({A, B, C}) ->
  %TODO math:pow(10, 12). {1415,821017,790736} as int
  (A * 1000000000000) + (B * 1000000) + C.


send(From, To, Kg) ->
  Ref = uniqueref(now()),
  ets:insert(manager, {Ref, waiting, From, To, Kg}),
  {ok, Ref}.

% doesnt concern weight restrictions
deliver(Loc) ->
  %TODO deliveries that are going to Loc
  Pickups = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', waiting}, {'==', '$3', Loc}], [['$1', '$2', '$3']]}]),
  Length = length(Pickups),
  if
    Length > 0 -> {ok, Pickups};
    Length =< 0 -> {error, instance}
  end.

reserve(From, To, Kg) ->
  Reserved = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$2', waiting}, {'==', '$3', From}, {'==', '$4', To}], ['$$']}]),
  WeightedReserve = weightedReserve(Reserved, Kg),
  %TODO update status to reserved of selected upto weight limit, rather then delete them
  removeReserved(WeightedReserve),
  {ok, WeightedReserve}.
weightedReserve([[Ref, _Status, From, To, Kg] | T], Ac) when Ac >= Kg ->
  [[Ref, reserved, From, To, Kg] | weightedReserve(T, (Ac - Kg))];
weightedReserve([[_Ref, _Status, _From, _To, _Kg] | _T], _Ac) -> [];
weightedReserve([], _Ac) -> [].
removeReserved([[Ref, _Status, From, To, Kg] | T]) ->
  ets:delete(manager, Ref),
  % unfortuantely we cant update bags, only sets,
  ets:insert(manager, {Ref, reserved, From, To, Kg}),
  removeReserved(T);
removeReserved([]) -> ok.


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