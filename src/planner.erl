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
-export([]).
