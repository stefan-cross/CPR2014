%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2014 17:38
%%%
%%% At a vehilce level it suffices to have a one_for_one supervision tree structure,
%%% we roll our own here as opposed to using the OTP sup. We will look to make use of
%%% OTP sups at the top level perhaps.
%%%
%%%-------------------------------------------------------------------
-module(test_sup).
-author("stefancross").

-export([start/2, init/1, stop/1]).


start_link() ->
  proc_lib:start_link(?MODULE, init, [self()]).


init(Pid) ->
  {ok, Pid} = Pid,
  register(?MODULE, Pid),
  proc_lib:init_ack(Pid, {ok, self()}),
  loop().

start_link() ->
  ChildSpecList = [],
  spawn_link(?MODULE, init, [ChildSpecList]),
  register(?MODULE, self()),
  {ok, ?MODULE}.

start(Name, ChildSpecList) ->
  register(Name, spawn(?MODULE, init, [ChildSpecList])).

stop(Name) -> Name ! stop.

init(ChildSpecList) ->
  process_flag(trap_exit, true),
  loop(start_children(ChildSpecList, [])).

start_children([], Acc) -> Acc;
start_children([{M, F, A} | ChildSpecList], Acc) ->
  {ok, Pid} = apply(M,F,A),
  start_children(ChildSpecList, [{Pid, {M,F,A}} | Acc]).

loop(ChildList) ->
  receive
    {'EXIT', Pid, _Reason} ->
      NewChildList = restart_child(Pid, ChildList),
      loop(NewChildList);
    stop ->
      terminate(ChildList)
  end.

restart_child(Pid, ChildList) ->
  {value, {Pid, {M,F,A}}} =
    lists:keysearch(Pid, 1, ChildList),
  {ok, NewPid} = apply(M,F,A),
  [{NewPid, {M,F,A}}|lists:keydelete(Pid,1,ChildList)].

terminate([]) -> ok;
terminate([{Pid, _} | ChildList]) ->
  exit(Pid, kill),
terminate(ChildList).