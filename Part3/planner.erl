%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <stefancross>
%%% @doc
%%%
%%% Practical Part 1: Implementing a Journey Planner
%%%
%%% The following module implements a Journey planner, taking a config
%%% file to establish locations and their distances apart. The Planner
%%% server can provide a route from a specified starting location to a
%%% number of other locations. It is not necessarily optimised for the
%%% most efficient journey but is adequate.
%%% @end
%%%-------------------------------------------------------------------

-module(planner).
-author("stefancross").

%% API
-export([start_link/0, route/2]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start a new process that is linked to the calling process.
%% @spec start_link() -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  {ok, spawn(fun() -> init() end)}.

%%--------------------------------------------------------------------
%% @doc
%% Given the name of a starting city and a list of destinations (in no
%% particular order), this function returns a list that describes the
%% route (and order) the truck or van has to take in order to visit all
%% of the cities in ToList
%% @spec route(From, ToList) -> {ok, CityList} | {error, invalid}
%% @end
%%--------------------------------------------------------------------
route(From, List) ->
  % May not lead to the most efficient route, but lists:usort removes
  % duplicates effectively
  Sorted = lists:usort(List),
  From, routing(From, Sorted).
routing(From, [H|T]) when From /= H ->
  % Could look for route optimisation on the Route var
  % Least number of hops, distance vector rather then link cost based
  [digraph:get_short_path(lists:nth(1, ets:lookup(graph, digraph)), From, H)
  |routing(H, T) ];
routing(To, [To|_T]) -> [];
routing(_From, []) -> [].

%%%===================================================================
%%  Recieving loop pattern
%%%===================================================================
loop()->
  receive
    {route, {From, To}, Pid} ->
      io:format("Routing request received from ~p ~n", [Pid]),
      Pid ! route(From, To)
  end.

%%%===================================================================
%%% Internal function
%%%===================================================================
%% Register and enter in to recieving loop
init() ->
  register(?MODULE, self()),
  loop().
