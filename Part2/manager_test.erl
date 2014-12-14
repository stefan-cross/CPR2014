%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <stefancross>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2014 21:57
%%
%%  For more examples of eunit see the following:
%% http://learnyousomeerlang.com/eunit
%% http://courses.cms.caltech.edu/cs11/material/erlang/lectures/cs11-erl-lec4.pdf
%%
%% Run with: c(manager_test), manager_test:test().
%%
%%%-------------------------------------------------------------------

-module(manager_test).
-author("stefancross").

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS
%%
%% A basic test for the manager module to see if this might be worth
%% submitting as more evidence as to the correctness of the implmentation
%% and interpretation of the specification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager_send_test_() ->
  {setup,
    fun manager_start/0,
    fun manager_send/0
  }.

manager_start() ->
  {ok, _Pid} = manager:start_link().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager_send() ->
  ?assertMatch({ok, _}, manager:send("A", "B", 10) ).