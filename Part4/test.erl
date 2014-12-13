%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Dec 2014 12:04
%%%-------------------------------------------------------------------

-module(test).
-export([start/2, init/2]).

start(Name, Random) -> {ok, spawn_link(?MODULE, init, [Name, Random])}.

init(Name, Random) ->
  register(Name, self()),
  io:format("Started ~p plus second param ~p ~n",[Name, Random]),
  loop().

loop() ->
receive stop -> exit(byebye) end.