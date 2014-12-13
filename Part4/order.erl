%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2014 15:49
%%
%% Places a number of orders for us up to a max specified weight
%%
%%%-------------------------------------------------------------------
-module(order).
-author("stefancross").

%% API
-export([place/2]).

%%--------------------------------------------------------------------
%% @doc
%% Places a Num of orders up to a max weight
%% @spec place(NumOfOrders, MaxWeight) -> ok
%% @end
%%--------------------------------------------------------------------
place(NumOfOrders, MaxWeight) when NumOfOrders /= 0 ->
  Weight = random:uniform(MaxWeight),

  Towns = ets:select(towns, [{{'$1', '$2'}, [], ['$1']}]),
  TownsIndex = random:uniform(length(Towns)),
  % -1 + 1 is to get around random nth potentially being 0
  Town1 = lists:nth(random:uniform((TownsIndex - 1) +1), Towns),
  % remove town1 from list so we dont have order to and from same place
  Town2 = lists:nth(random:uniform((TownsIndex -1) +1) , Towns),
  % Work around as list:delete was randomly retunring an empty list
  if
    Town1 == Town2 -> place(NumOfOrders, MaxWeight);
    Town1 /= Town2 ->
      manager:send(Town1, Town2, Weight),
      place(NumOfOrders - 1, MaxWeight)
  end;

place(_NumOfOrders, _MaxWeight) -> ok.
