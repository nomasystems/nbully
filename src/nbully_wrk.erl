%%% Copyright (c) 2018 [Nomasystems, S.L. http://www.nomasystems.com]
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
-module(nbully_wrk).

%%% INCLUDE FILES

%%% START/STOP EXPORTS
-export([start_link/0, stop/0]).

%%% INIT/TERMINATE EXPORTS
-export([init/1]).

%%% SYS EXPORTS
-export([system_continue/3, system_terminate/4, system_code_change/4, write_debug/3]).


%%% EXTERNAL EXPORTS
-export([leader/0]).

%%% MACROS
-define(ADVERTISE_MSG, '$nbully_advertise').
-define(ELECTION_MSG, '$nbully_election').
-define(IM_LEADER_MSG, '$nbully_im_leader').

-define(LEADER_MSG, '$nbully_leader').

-define(RESPONSE_TIMEOUT, 100).

%%% RECORDS
-record(st, {leader = undefined,
             timeout = infinity,
             nodes = [] :: {node(), reference(), term()}}).


%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link() ->
  proc_lib:start_link(?MODULE, init, [self()]).

stop() ->
  proc_lib:stop(?MODULE).


%%%-----------------------------------------------------------------------------
%%% INIT EXPORTS
%%%-----------------------------------------------------------------------------
init(Parent) ->
  register(?MODULE, self()),
  Debug = sys:debug_options([]),
  net_kernel:monitor_nodes(true),
  Nodes = nodes(),
  St = advertise(Nodes, #st{}),
  ok = proc_lib:init_ack(Parent, {ok, self()}),
  loop(Parent, Debug, St).

%%%-----------------------------------------------------------------------------
%%% SYS EXPORTS
%%%-----------------------------------------------------------------------------
system_continue(Parent, Debug, St) ->
  loop(Parent, Debug, St).
  
system_terminate(Reason, _, _, _) ->
  net_kernel:monitor_nodes(false),
  exit(Reason).
  
system_code_change(Misc, _, _, _) ->
  {ok, Misc}.

write_debug(Dev, Event, Name) ->
  io:format(Dev, "~p event -> ~p~n", [Name, Event]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
leader() ->
  case whereis(?MODULE) of
    undefined ->
      undefined;
    Pid when is_pid(Pid) ->
      Ref = make_ref(),
      Pid ! {?LEADER_MSG, self(), Ref},
      receive
        {Ref, Leader} ->
          Leader
      end
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
loop(Parent, Debug, St) ->
  Leader = St#st.leader,
  receive
    {system, From, Request} ->
      sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, St);
    {?ADVERTISE_MSG, Node, Pid} ->
      NewSt = monitor_process(St, Node, Pid),
      loop(Parent, Debug, elect(NewSt));
    {?ELECTION_MSG, Node, Pid} ->
      NewSt = monitor_process(St, Node, Pid),
      loop(Parent, Debug, become_leader(NewSt));
    {?IM_LEADER_MSG, Node, Pid} ->
      NewSt = monitor_process(St, Node, Pid),
      loop(Parent, Debug, set_leader(NewSt, Node));
    {?LEADER_MSG, From, Ref} ->
      From ! {Ref, Leader},
      loop(Parent, Debug, St);
    {nodedown, _} ->
      loop(Parent, Debug, St);
    {nodeup, Node} ->
      loop(Parent, Debug, advertise([Node], St));
    {'DOWN', Ref, _Type, _Object, _Info} ->
      NewSt = handle_down(Ref, St),
      loop(Parent, Debug, NewSt)
  after
    St#st.timeout ->
      loop(Parent, Debug, become_leader(St))
  end.

handle_down(Ref, St) ->
  case lists:keytake(Ref, 3, St#st.nodes) of
    false ->
      St;
    {value, {N, _P, _R}, Nodes} when N == St#st.leader ->
      set_leader(St, undefined),
      elect(St#st{nodes = Nodes});
    {value, {_N, _P, _R}, Nodes} ->
      St#st{nodes = Nodes}
  end.

monitor_process(St, Node, Pid) ->
  case lists:keyfind(Pid, 2, St#st.nodes) of
    false ->
      Ref = erlang:monitor(process, Pid),
      St#st{nodes = [{Node, Pid, Ref} | St#st.nodes]};
    _ ->
      St
  end.


%%%-----------------------------------------------------------------------------
%%% INTERNAL ST FUNCTIONS
%%%-----------------------------------------------------------------------------
advertise(Nodes, St) ->
  lists:foreach(fun send_advertise/1, Nodes),
  St#st{timeout = ?RESPONSE_TIMEOUT}.

elect(St) ->
  Node = highest_id(St#st.nodes),
  send_election(Node),
  St#st{timeout = ?RESPONSE_TIMEOUT}.

become_leader(St) ->
  case highest_id(St#st.nodes) of
    Node when Node > node() ->
      St;
    _Node ->
      NewSt = set_leader(St, node()),
      lists:foreach(fun send_im_leader/1, lower_ids(NewSt#st.nodes)),
      NewSt#st{timeout = infinity}
  end.

set_leader(St, Node) ->
  St#st{leader = Node, timeout = infinity}.


%%%-----------------------------------------------------------------------------
%%% INTERNAL MSG FUNCTIONS
%%%-----------------------------------------------------------------------------
send_advertise(Node) ->
  send_to_node(Node, ?ADVERTISE_MSG).

send_election(Node) ->
  send_to_node(Node, ?ELECTION_MSG).

send_im_leader(Node) ->
  send_to_node(Node, ?IM_LEADER_MSG).

send_to_node(Node, Message) ->
  {?MODULE, Node} ! {Message, node(), self()}.


%%%-----------------------------------------------------------------------------
%%% INTERNAL UTIL FUNCTIONS
%%%-----------------------------------------------------------------------------
highest_id(Nodes) ->
  lists:foldl(fun({Node, _Pid, _Ref}, AccNode) ->
                  if
                    Node > AccNode -> Node;
                    true -> AccNode
                  end
              end,
              node(),
              Nodes).


lower_ids(Nodes) ->
  SelfNode = node(),
  Lower = lists:filter(fun({Node, _Pid, _Ref}) -> Node =< SelfNode end, Nodes),
  [L || {L, _P, _R} <- Lower].
