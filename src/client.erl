%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2014 15:49
%%
%% Places orders for us...
%%
%%%-------------------------------------------------------------------
-module(client).
-author("stefancross").

%% API
-export([start/2]).

% Places a number of orders and waits for specified delay, can be set to run forever
start(Num, Delay) when Num /= 0 ->
  Weight = random:uniform(1000),

  Towns = ets:select(towns, [{{'$1', '$2'}, [], ['$1']}]),
  TownsIndex = random:uniform(length(Towns)),
  % -1 + 1 is to get around random nth potentially being 0
  Town1 = lists:nth(random:uniform((TownsIndex - 1) +1), Towns),

  %TODO NOT ACCEPTABLE! Revisit this, although we can allow maunal entry of same to from, we shouldnt automate it in this client!
  % remove town1 from list so we dont have order to and from same place, but lists:delete(Town1, Towns) occationally returns and empty list! errors
  Town2 = lists:nth(random:uniform((TownsIndex -1) +1) , Towns), % allowing potentiall same to and from!

  manager:send(Town1, Town2, Weight),
  timer:sleep(Delay),

  start(Num - 1, Delay);

start(_Num, _Delay) -> ok.
