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
-export([start_link/0, deliver/1, loop/0, send/3]).

start_link() ->
  register(?MODULE, spawn_link(?MODULE, loop, [])),
  ets:new(manager, [set, named_table]).


uniqueref({A, B, C}) ->
  ((A * 1000) * 4) + ((B * 1000) * 2) + C.

send(From, To, Kg) ->
  Ref = uniqueref(now()),
  ets:insert(manager, {Ref, waiting, From, To, Kg}),
  {ok, Ref}.

deliver(Loc) ->
  Pickups = ets:select(manager, [{{'$1', '$2', '$3', '$4', '$5'}, [{'==', '$3', Loc}], ['$$']}]),
  {ok, Pickups}.

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