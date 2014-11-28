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
-export([start_link/0, route/2, loop/0]).

start_link() ->
  register(?MODULE, spawn(?MODULE, loop, [])),
  {ok, ?MODULE}.

% It was decided to return nested loop of From To so that distance might be able to be included at a later date
% and allow for optimisated routing on link cost rather then hop count, however there was not enough time for this
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
    {route, {From, To}, Pid} ->
      io:format("Routing request recieved from ~p ~n", [Pid]),
      Pid ! route(From, To)
  end.


%%TODO this is more like it! will invloved heavy adaption of dispatcher to recieve messages
%% 1> c(planner).
%% {ok,planner}
%% 2> c(orchestration).
%% orchestration.erl:16: Warning: a term is constructed, but never used
%% orchestration.erl:25: Warning: function start_vehicles/0 is unused
%% orchestration.erl:94: Warning: function createVehicles/1 is unused
%% orchestration.erl:97: Warning: function createVehicles/2 is unused
%% {ok,orchestration}
%% 3> orchestration:start_simulation().
%% Config imported.
%% Digraph and ETS graph ref created as [{digraph,16407,20504,24601,true}].
%% Verticies have been created.
%% Edges have been created.
%% In loopmanager
%% 4> planner:start_link().
%% {ok,planner}
%% 5> planner:route("Wrocław", ["Katowice"]).
%% [[[87,114,111,99,197,130,97,119],"Katowice"]]
%% 6> flush().
%% ok
%% 7> planner ! {route, {"Wrocław", ["Katowice"]}, self()}.
%% Routing request recieved from <0.31.0>
%% {route,{[87,114,111,99,197,130,97,119],["Katowice"]},
%% <0.31.0>}
%% 8> flush().
%% Shell got [[[87,114,111,99,197,130,97,119],"Katowice"]]
%% ok
%% 9>