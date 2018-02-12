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
-export([init/1, terminate/0]).

%%% SYS EXPORTS
-export([system_continue/3, system_terminate/4, system_code_change/4, write_debug/3]).


%%% EXTERNAL EXPORTS
-export([leader/0]).

%%% MACROS
-define(ELECTION_MESSAGE, '$nbully_election').
-define(IM_LEADER_MESSAGE, '$nbully_im_leader').
-define(LEADER_MESSAGE, '$nbully_leader').
-define(RESPONSE_TIMEOUT, 100).

%%% RECORDS
-record(st, {timeout = infinity, known_nodes = [], leader = undefined}).


%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link() ->
  proc_lib:start_link(?MODULE, init, [self()]).

stop() ->
  proc_lib:stop(?MODULE).


%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init(Parent) ->
  Nodes = all_nodes(),
  register(?MODULE, self()),
  Debug = sys:debug_options([]),
  net_kernel:monitor_nodes(true),
  St = start_election(#st{known_nodes = Nodes}, Nodes),
  ok = proc_lib:init_ack(Parent, {ok, self()}),
  loop(Parent, Debug, St).


terminate() ->
  net_kernel:monitor_nodes(false),
  proc_lib:stop(self()).


%%%-----------------------------------------------------------------------------
%%% SYS EXPORTS
%%%-----------------------------------------------------------------------------
system_continue(Parent, Debug, St) ->
  loop(Parent, Debug, St).
  
system_terminate(Reason, _, _, _) ->
  exit(Reason).
  
system_code_change(Misc, _, _, _) ->
  {ok, Misc}.

write_debug(Dev, Event, Name) ->
  io:format(Dev, "~p event -> ~p~n", [Name, Event]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
leader() ->
  Ref = make_ref(),
  ?MODULE ! {?LEADER_MESSAGE, self(), Ref},
  receive
    {Ref, Leader} ->
      Leader
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
loop(Parent, Debug, St) ->
  Leader = St#st.leader,
  receive
    {system, From, Request} ->
      sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, St);
    {?ELECTION_MESSAGE, _Node} ->
      loop(Parent, Debug, become_leader(St));
    {?IM_LEADER_MESSAGE, Node} ->
      loop(Parent, Debug, set_leader(St, Node));
    {?LEADER_MESSAGE, From, Ref} ->
      From ! {Ref, Leader},
      loop(Parent, Debug, St);
    {nodedown, Leader} ->
      set_leader(St, undefined),
      Nodes = all_nodes(),
      loop(Parent, Debug, start_election(St#st{known_nodes = Nodes}, Nodes));
    {nodedown, _} ->
      loop(Parent, Debug, St#st{known_nodes = all_nodes()});
    {nodeup, _} ->
      Nodes = all_nodes(),
      loop(Parent, Debug, start_election(St#st{known_nodes = Nodes}, Nodes))
  after
    St#st.timeout ->
      loop(Parent, Debug, become_leader(St))
  end.


all_nodes() ->
  [node() | nodes()].


start_election(St, Nodes) ->
  %lists:foreach(fun send_election_message/1, higher_ids(Nodes)),
  send_election(highest_id(Nodes)),
  St#st{timeout = ?RESPONSE_TIMEOUT}.


send_election(Node) ->
  send_message_to_node(Node, ?ELECTION_MESSAGE).


set_leader(St, Node) ->
  St#st{leader = Node, timeout = infinity}.


become_leader(St) ->
  set_leader(St, node()),
  broadcast_leader(St),
  St#st{timeout = infinity}.


broadcast_leader(St) ->
  lists:foreach(fun send_leader/1, lower_ids(St#st.known_nodes)).


send_leader(Node) ->
  send_message_to_node(Node, ?IM_LEADER_MESSAGE).


highest_id(Nodes) ->
  lists:max(Nodes).


lower_ids(Nodes) ->
  lists:filter(fun(Node) -> Node =< node() end, Nodes).


send_message_to_node(Node, Message) ->
  {?MODULE, Node} ! {Message, node()}.

