%% Copyright 2024 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License
-module(nbully_wrk).

%%% START/STOP EXPORTS
-export([start_link/0, stop/0]).

%%% INIT/TERMINATE EXPORTS
-export([init/1]).

%%% SYS EXPORTS
-export([system_continue/3, system_terminate/4, system_code_change/4]).

%%% EXTERNAL EXPORTS
-export([leader/0, subscribe/0, unsubscribe/0]).

%%% MACROS
-define(ADVERTISE_MSG, '$nbully_advertise').
-define(ELECTION_MSG, '$nbully_election').
-define(IM_LEADER_MSG, '$nbully_im_leader').

-define(LEADER_MSG, '$nbully_leader').
-define(SUBSCRIBE_MSG, '$nbully_subscribe').
-define(UNSUBSCRIBE_MSG, '$nbully_unsubscribe').

-define(UPDATE_MSG(Node), {nbully_leader_updated, Node}).

-define(RESPONSE_TIMEOUT, 100).

%%% RECORDS
-record(st, {
    leader = undefined,
    timeout = infinity,
    nodes = [] :: [{node(), reference(), term()}],
    subscribers = [] :: [{pid(), reference()}]
}).

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

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
leader() ->
    call(?LEADER_MSG).

subscribe() ->
    call(?SUBSCRIBE_MSG).

unsubscribe() ->
    call(?UNSUBSCRIBE_MSG).

call(Msg) ->
    case whereis(?MODULE) of
        undefined ->
            undefined;
        Pid when is_pid(Pid) ->
            Ref = make_ref(),
            Pid ! {Msg, self(), Ref},
            receive
                {Ref, Reply} ->
                    Reply
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
            NewSt = monitor_bully_process(St, Node, Pid),
            loop(Parent, Debug, elect(NewSt));
        {?ELECTION_MSG, Node, Pid} ->
            NewSt = monitor_bully_process(St, Node, Pid),
            loop(Parent, Debug, become_leader(NewSt));
        {?IM_LEADER_MSG, Node, Pid} ->
            NewSt = monitor_bully_process(St, Node, Pid),
            loop(Parent, Debug, set_leader(NewSt, Node));
        {?LEADER_MSG, From, Ref} ->
            From ! {Ref, Leader},
            loop(Parent, Debug, St);
        {?SUBSCRIBE_MSG, From, Ref} ->
            NewSt = subscribe(From, St),
            From ! {Ref, ok},
            loop(Parent, Debug, NewSt);
        {?UNSUBSCRIBE_MSG, From, Ref} ->
            NewSt = unsubscribe(From, St),
            From ! {Ref, ok},
            loop(Parent, Debug, NewSt);
        {nodedown, _} ->
            loop(Parent, Debug, St);
        {nodeup, Node} ->
            loop(Parent, Debug, advertise([Node], St));
        {'DOWN', Ref, _Type, _Object, _Info} ->
            NewSt = handle_down(Ref, St),
            loop(Parent, Debug, NewSt)
    after St#st.timeout ->
        loop(Parent, Debug, become_leader(St))
    end.

handle_down(Ref, St) ->
    case lists:keytake(Ref, 3, St#st.nodes) of
        false ->
            handle_subscriber_down(Ref, St);
        {value, {N, _P, _R}, Nodes} when N == St#st.leader ->
            set_leader(St, undefined),
            elect(St#st{nodes = Nodes});
        {value, {_N, _P, _R}, Nodes} ->
            St#st{nodes = Nodes}
    end.

handle_subscriber_down(Ref, St) ->
    Subscribers = lists:keydelete(Ref, 2, St#st.subscribers),
    St#st{subscribers = Subscribers}.

monitor_bully_process(St, Node, Pid) ->
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

set_leader(#st{leader = Node} = St, Node) ->
    St;
set_leader(St, Node) ->
    lists:foreach(fun({Pid, _R}) -> Pid ! ?UPDATE_MSG(Node) end, St#st.subscribers),
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
%%% INTERNAL SUBSCRIBER FUNCTIONS
%%%-----------------------------------------------------------------------------
subscribe(From, St) ->
    case lists:keyfind(From, 1, St#st.subscribers) of
        false ->
            Ref = erlang:monitor(process, From),
            St#st{subscribers = [{From, Ref} | St#st.subscribers]};
        {From, _R} ->
            St
    end.

unsubscribe(From, St) ->
    case lists:keytake(From, 1, St#st.subscribers) of
        false ->
            St;
        {value, {From, Ref}, Subscribers} ->
            true = erlang:demonitor(Ref),
            St#st{subscribers = Subscribers}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL UTIL FUNCTIONS
%%%-----------------------------------------------------------------------------
highest_id(Nodes) ->
    lists:foldl(
        fun({Node, _Pid, _Ref}, AccNode) ->
            if
                Node > AccNode -> Node;
                true -> AccNode
            end
        end,
        node(),
        Nodes
    ).

lower_ids(Nodes) ->
    SelfNode = node(),
    Lower = lists:filter(fun({Node, _Pid, _Ref}) -> Node < SelfNode end, Nodes),
    [L || {L, _P, _R} <- Lower].
