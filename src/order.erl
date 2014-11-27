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
-module(order).
-author("stefancross").

%% API
-export([place/2]).

% Places a number of orders and waits for specified delay, can be set to run forever
place(Num, Delay) when Num /= 0 ->
  Weight = random:uniform(1000),

  Towns = ets:select(towns, [{{'$1', '$2'}, [], ['$1']}]),
  TownsIndex = random:uniform(length(Towns)),
  % -1 + 1 is to get around random nth potentially being 0
  Town1 = lists:nth(random:uniform((TownsIndex - 1) +1), Towns),

  % remove town1 from list so we dont have order to and from same place, but lists:delete(Town1, Towns) occationally returns and empty list! errors
  Town2 = lists:nth(random:uniform((TownsIndex -1) +1) , Towns),

  % Sorry but i had no options as list:delete was randomly giving me an empty list
  if
    Town1 == Town2 -> place(Num, 0);
    Town1 /= Town2 ->
      manager:send(Town1, Town2, Weight),
      timer:sleep(Delay),
      place(Num - 1, Delay)
  end;

place(_Num, _Delay) -> ok.
