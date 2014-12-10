%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2014 21:57

%% http://learnyousomeerlang.com/eunit

%%%-------------------------------------------------------------------
-module(manager_test).
-author("stefancross").

-include_lib("eunit/include/eunit.hrl").


%% manager_start_test_() ->
%%  ?_assert( manager:start_link() =:= {ok, manager} ).

manager_send_test_() ->
  {setup,
    fun manager_start/0,
    fun manager_send/0
  }.

manager_start() ->
  {ok, _Pid} = manager:start_link().

manager_send() ->
  ?assertMatch({ok, _}, manager:send("A", "B", 10) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%