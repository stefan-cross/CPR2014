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
  register(?MODULE, spawn_link(?MODULE, loop, [])).

send(From, To, Kg) ->
  manager ! {From, To, Kg}.

deliver(Loc) -> {ok, []}.

loop() ->
  io:format("In loop"),
  receive
    {_From, _To, _Kg} ->
      io:format("Message receieved ~p~n", [now()]),
      {ok, now()},
      loop();
    exit ->
      io:format("Exiting"),
      ok
  end.