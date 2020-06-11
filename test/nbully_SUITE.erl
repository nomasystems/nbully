%%% Copyright (c) 2009 Nomasystems, S.L., All Rights Reserved
%%%
%%% This file contains Original Code and/or Modifications of Original Code as
%%% defined in and that are subject to the Nomasystems Public License version
%%% 1.0 (the 'License'). You may not use this file except in compliance with
%%% the License. BY USING THIS FILE YOU AGREE TO ALL TERMS AND CONDITIONS OF
%%% THE LICENSE. A copy of the License is provided with the Original Code and
%%% Modifications, and is also available at www.nomasystems.com/license.txt.
%%%
%%% The Original Code and all software distributed under the License are
%%% distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
%%% EXPRESS OR IMPLIED, AND NOMASYSTEMS AND ALL CONTRIBUTORS HEREBY DISCLAIM
%%% ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR
%%% NON-INFRINGEMENT. Please see the License for the specific language
%%% governing rights and limitations under the License.
-module('nbully_SUITE').

%%% EXTERNAL EXPORTS
-compile(export_all).
-compile(nowarn_export_all).

%%% MACROS
-define(MATCH_SPEC, [{'_', [], [{message, {return_trace}}]}]).
-define(MAX_TIME, 10000).
-define(MAX_TIMETRAP, 429496729).   % Don't ask me why.
-define(STUBS_DIR, "../../stubs").  % Tests run in log/ct_run.*

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
  [api, api_neg, fault_tolerance, performance].


sequences() ->
  [].


suite() ->
  [{timetrap, ?MAX_TIMETRAP}].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
  lists:foreach(fun(X) -> code:add_path(X) end, ct:get_config(paths, [])),
  dbg:tracer(),
  dbg:p(all, [c, sos, sol]),
  Apps = ct:get_config(apps, []),
  Env = ct:get_config(env, []),
  [ok = application:load(App) || App <- Apps],
  [ok = application:set_env(App, K, V) || {App, KeyVal} <- Env, {K,V} <- KeyVal],
  [ok = application:start(App) || App <- Apps],
  Conf.

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(_Conf) ->
  Apps = ct:get_config(apps, []),
  [ok = application:stop(App) || App <- Apps],
  [ok = application:unload(App) || App <- Apps],
  ok.

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
  ct:print("Starting test case ~p", [Case]),
  init_stubs(Case),
  init_traces(Case),
  Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
  end_traces(Case),
  end_stubs(Case),
  ct:print("Test case ~p completed", [Case]),
  Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
api() ->
  [{userdata, [{doc, "Tests the public API."}]}].

api(_Conf) ->
  start_nodes(9),
  subscriber_srv:start_link(),
  Nodes = all_nodes(),
  Leader = lists:max(Nodes),
  timer:sleep(500),
  Leader = nbully:leader(),
  Leader = wait_consensus(),
  Leader = subscriber_srv:leader(),
  0 = subscriber_srv:repeated_lead_msgs(),
  subscriber_srv:stop(),
  stop_nodes(9),
  ok.


api_neg() ->
  [{userdata, [{doc, "Tests the public API with invalid input data."}]}].

api_neg(_Conf) ->
  start_nodes(9),
  {ok, Subscriber} = subscriber_srv:start(),
  Nodes = all_nodes(),
  Leader = lists:max(Nodes),
  rpc:call(Leader, application, stop, [nbully]),
  
  timer:sleep(500),
  NewLeader = nbully:leader(),
  true = NewLeader /= Leader,
  NewLeader = wait_consensus(),
  NewLeader = subscriber_srv:leader(),
  0 = subscriber_srv:repeated_lead_msgs(),
  erlang:exit(Subscriber, kill),
  stop_nodes(9),
  ok.


fault_tolerance() ->
  [{userdata, [{doc, "Tests the public API with invalid input data."}]}].

fault_tolerance(_Conf) ->
  subscriber_srv:start_link(),
  start_nodes(9),
  timer:sleep(500),
  Leader = lists:max(all_nodes()),
  Leader = wait_consensus(),

  stop_node(9),
  NewLeader = lists:max(all_nodes()),
  NewLeader = wait_consensus(),

  stop_node(6),
  NewLeader = lists:max(all_nodes()),
  NewLeader = wait_consensus(),

  stop_node(7),
  stop_node(8),
  NewLeader2 = lists:max(all_nodes()),
  NewLeader2 = wait_consensus(),

  start_node(9, true),
  Leader = lists:max(all_nodes()),
  Leader = wait_consensus(),
 
  stop_nodes(9),

  0 = subscriber_srv:repeated_lead_msgs(),
  subscriber_srv:stop(),
  ok.



performance() ->
  [{userdata, [{doc, "Test performace restrictions."}]}].

performance(Conf) ->
  MaxTime = ct:get_config(max_time, ?MAX_TIME),
  {T, _} = timer:tc(?MODULE, performance_test, [Conf]),
  if
    trunc(T/1000) > MaxTime ->
      exit(too_slow);
    true ->
      ok
  end.

performance_test(_Conf) ->
  ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
all_nodes() ->
  nodes().

wait_consensus() ->
  Nodes = all_nodes(),
  Leaders = [rpc:call(Node, nbully, leader, []) || Node <- Nodes],
  ct:print("Check consensus~nNodes: ~p~nLeaders: ~p~n~n", [Nodes, Leaders]),
  Check = fun(X, {false, _Node}) ->
              {false, X};
             (X, {true, undefined}) ->
              {true, X};
             (X, {true, Node}) ->
              {X == Node, X}
          end,
  case lists:foldl(Check, {true, undefined}, Leaders) of
    {true, Node} ->
      Node;
    {false, _} ->
      timer:sleep(100),
      wait_consensus()
  end.


start_node(N, true) ->
  ErlFlags =  "-pa ../../lib/*/ebin",
  {ok, Node} = ct_slave:start(list_to_atom("node"++integer_to_list(N)),
                              [{monitor_master, true}, {erl_flags, ErlFlags}]),
  wait_for_node(Node),
  rpc:call(Node, net_adm, world, []),
  lists:foreach(fun(X) -> rpc:call(Node, code, add_path, [X]) end, ct:get_config(paths, [])),
  rpc:call(Node, application, start, [nbully]),
  Node;
start_node(N, false) ->
  {ok, Node} = ct_slave:start(list_to_atom("node"++integer_to_list(N))),
  wait_for_node(Node),
  Node.


start_nodes(0) ->
  ok;
start_nodes(N) ->
  _ = start_node(N, true),
  start_nodes(N-1).

stop_node(N) ->
  {ok, Host} = inet:gethostname(),
  Node = list_to_atom("node"++integer_to_list(N)++"@"++Host),
  rpc:call(Node, application, stop, [nbully]),
  ct_slave:stop(list_to_atom("node"++integer_to_list(N))).


stop_nodes(0) ->
  ok;
stop_nodes(N) ->
  _ = stop_node(N),
  stop_nodes(N-1).


wait_for_node(Node) ->
  case lists:member(Node, nodes()) of
    true ->
      Node;
    false ->
      timer:sleep(100),
      wait_for_node(Node)
  end.


%%%-----------------------------------------------------------------------------
%%% TRACING UTIL FUNCTIONS
%%%-----------------------------------------------------------------------------
add_trace(TpFun, {Mod, Fun, Spec}) ->
  dbg:TpFun(Mod, Fun, Spec);
add_trace(TpFun, {Mod, Fun}) ->
  dbg:TpFun(Mod, Fun, ?MATCH_SPEC);
add_trace(TpFun, Mod) ->
  dbg:TpFun(Mod, ?MATCH_SPEC).


del_trace(CtpFun, {Mod, Fun, _Spec}) ->
  dbg:CtpFun(Mod, Fun);
del_trace(CtpFun, {Mod, Fun}) ->
  dbg:CtpFun(Mod, Fun);
del_trace(CtpFun, Mod) ->
  dbg:CtpFun(Mod).


end_traces(Case) ->
  TpCases = ct:get_config(tp_cases, []),
  Tps = proplists:get_value(Case, TpCases, []),
  lists:foreach(fun(Tp) -> del_trace(ctp, Tp) end, Tps),
  TplCases = ct:get_config(tpl_cases, []),
  Tpls = proplists:get_value(Case, TplCases, []),
  lists:foreach(fun(Tpl) -> del_trace(ctpl, Tpl) end, Tpls).


init_traces(Case) ->
  TpCases = ct:get_config(tp_cases, []),
  Tps = proplists:get_value(Case, TpCases, []),
  lists:foreach(fun(Tp) -> add_trace(tp, Tp) end, Tps),
  TplCases = ct:get_config(tpl_cases, []),
  Tpls = proplists:get_value(Case, TplCases, []),
  lists:foreach(fun(Tpl) -> add_trace(tpl, Tpl) end, Tpls).

%%%-----------------------------------------------------------------------------
%%% STUB UTIL FUNCTIONS
%%%-----------------------------------------------------------------------------
end_stubs(Case) ->
  NegCases = ct:get_config(neg_cases, []),
  Stubs = proplists:get_value(Case, NegCases, []),
  lists:foreach(fun purge_stub/1, Stubs).


init_stubs(Case) ->
  NegCases = ct:get_config(neg_cases, []),
  Stubs = proplists:get_value(Case, NegCases, []),
  lists:foreach(fun(Stub) -> load_stub(Stub, true) end, Stubs).


load_stub(Stub, NegTest) ->
  Opts = if NegTest -> [binary, {d, neg_case}]; true ->  [binary] end,
  Erl = atom_to_list(Stub) ++ ".erl",
  ct:print("Compiling ~s with options ~p", [Erl, Opts]),
  {ok, Mod, Bin} = compile:file(filename:join(?STUBS_DIR, Erl), Opts),
  ct:print("Purge default ~p stub", [Mod]),
  code:purge(Mod),
  code:delete(Mod),
  ct:print("Loading new ~p stub", [Mod]),
  Beam = atom_to_list(Mod) ++ code:objfile_extension(),
  {module, Mod} = code:load_binary(Mod, Beam, Bin).


purge_stub(Stub) ->
  ct:print("Purge ~p stub", [Stub]),
  code:purge(Stub),
  code:delete(Stub),
  ct:print("Reloading default ~p stub", [Stub]),
  {module, Stub} = code:load_file(Stub).
