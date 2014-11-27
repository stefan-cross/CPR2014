%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Nov 2014 18:23
%%%-------------------------------------------------------------------
-module(setupData).
-author("stefancross").

%% API
-export([init/0]).

init() ->
%%   planner:start_link(),
%%   ets:match(towns, '$1'),
%%   ets:match(distances, '$1'),
%%   planner:route("Szczecin", ["Bydgoszcz", "Toruń"]),

  manager:start_link(),
  % these are orders from, to and weight
  manager:send("aaa","bbb",10),
  manager:send("aaa","bbb",12),
  manager:send("aaa","ccc",12),
  manager:send("aaa","ddd",14),
  manager:send("ddd","ccc",8),
  manager:send("eee","aaa",6),
  manager:send("eee","bbb",13),
  manager:send("eee","ccc",11),
  manager:send("fff","aaa",18),
  manager:send("fff","bbb",19),

  % show us what we've got
  %ets:match(manager, '$1').

  manager:reserve("aaa", "bbb", 12).