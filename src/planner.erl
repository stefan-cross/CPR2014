%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Oct 2014 23:18
%%%
%%%
%%% The following API should be respected when implementing the server:
%%%
%%% planner:start_link()  -> {ok, Pid}.
%%%
%%% Start a new process that is linked to the calling process.
%%%
%%% planner:route(From, ToList) -> {ok, CityList} | {error, invalid}.
%%%
%%%-------------------------------------------------------------------
-module(planner).
-author("stefancross").

%% API
-export([start_link/0, route/2]).

start_link() ->
  register(?MODULE, spawn(?MODULE, loop, [])),
  {ok, ?MODULE}.

route(From, List) ->
  Sorted = lists:usort(List),
  From, routing(From, Sorted).
routing(From, [H|T]) when From /= H ->
  % Least number of hops, distance vector based rather then link cost based for now
  [ digraph:get_short_path(lists:nth(1, ets:lookup(graph, digraph)), From, H)
  | routing(H, T) ];
routing(To, [To|_T]) -> [];
routing(_From, []) -> [].


loop()->
  receive
    {route, {From, [To]}, Pid} ->
      io:format("Routing request recieved from ~p ~n", [Pid]),
      Pid ! route(From, To)
  end.