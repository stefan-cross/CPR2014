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
-export([start_link/0, init/0, route/2, loop/0]).

start_link() ->
  {ok, spawn_link(?MODULE, init, [])}.

init() ->
  register(?MODULE, self()),
  loop().

route(From, List) ->
  % May not lead to the most effcient route, but lists:usort removes duplicates effectively
  Sorted = lists:usort(List),
  From, routing(From, Sorted).
routing(From, [H|T]) when From /= H ->
  % Could look for route optimisation on the Route var
  % Least number of hops, distance vector based rather then link cost based for now
  [ digraph:get_short_path(lists:nth(1, ets:lookup(graph, digraph)), From, H) | routing(H, T) ];
routing(To, [To|_T]) -> [];
routing(_From, []) -> [].


loop()->
  receive
    {route, {From, To}, Pid} ->
      io:format("Routing request recieved from ~p ~n", [Pid]),
      Pid ! route(From, To)
  end.


%% Design Notes

% It was decided to return nested loop of From To so that distance might be able to be included at a later date
% and allow for optimisated routing on link cost rather then hop count, however there was not enough time for this

%% Although this routing technique is not efficient it is reliable and there were other areas to address
%% so during testing we can see inefficiencies such as:

%% 6> planner:route("Szczecin", ["Wrocław", "Radom"]).
%% [["Szczecin",
%% [87,114,111,99,197,130,97,119],
%% [197,129,195,179,100,197,186],
%% "Radom"],
%% ["Radom",
%% [197,129,195,179,100,197,186],
%% [87,114,111,99,197,130,97,119]]]

%% Potentially this could be remedied by removing such duplicates but is also an particularly unfortunate result
%% due to the lists:usort and that these locations are not alphebetaically contigious and I have opted
%% simplicity over complexity with random efficiency. However the aim of this exercise is to flex the
%% power of Erlangs actor concurrency model rather then weigh up hop and distance vector based routing algorithms.
%% We can make gains by can simply test package elibibility to be dropped when
%% in each city and calling manager:reserve whever the vehicle is empty.

%% After running simulations with random weighted packages it also appears that the capacity of vans is
%% such that they can not take significant load and likely to only have one destination, and equally
%% larger loads in the trucks are often more efficient when we select deliveries by location and time
%% so that we have a higher quantity to drop in just one location, thus rendering an optimised routing
%% function less neccessary.


%% The loop functionality was added later to permit for message passing to ascertain a route
%% this change in invocation invloved subsequent adaption of dispatcher to recieve messages
%% but it allows for more dexterity when deciding how to call the routing function.

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