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

%% API
%-export([stop/0, echo/1, handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0, init/1]).
%%


% HOw to register name!>!?!?!
start_link() ->
  proc_lib:start_link(?MODULE, init, [self()]).


init(Pid) ->
  io:format("START LINK PID = ~p ~n", [Pid]),
  proc_lib:init_ack(Pid, {ok, self()}),
  loop().

loop() ->
  io:format("Looping ~n"),
  receive
    Anything ->
      io:format("Received ~p ~n", [Anything]),
      loop()
  end.

%% Starts a new empty vehicle supervisor that is linked to the calling process.
%% start_link() ->
%% %%   process_flag(trap_exit, true),
%%   gen_server:start_link({local, ?MODULE}, ?MODULE, init, []).
%%
%%
%% %% Public API
%% stop() ->
%%   gen_server:cast(?MODULE, stop).
%%
%% echo(Req) ->
%%   gen_server:cast(?MODULE, {echo, Req}).
%%
%% %% behaviours
%% init(_Arg) ->
%%   {ok, none}.
%%
%% handle_call({echo, Req}, _From, State) ->
%%   io:format("Recieved!, ~p", [Req]),
%%   {reply, Req, State}.
%%
%% handle_cast(_, State) ->
%%   {noreply, State}.
%%
%%
%% handle_info({'EXIT', _Pid, normal}, LoopData) ->
%%   {noreply, LoopData};
%% handle_info({'EXIT', Pid, Reason}, LoopData) ->
%%   io:format("Process: ~p exited with reason: ~p~n",[Pid, Reason]),
%%   {noreply, LoopData};
%% handle_info(_Msg, LoopData) ->
%%   io:format("This is my united states of WHATEVER!, ~p", [LoopData]),
%%   {noreply, LoopData}.