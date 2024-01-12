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
-module(nbully_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%% INCLUDES
-include_lib("common_test/include/ct.hrl").

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [api, api_neg, fault_tolerance, multiple_repeated_subscriptions].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
api() ->
    [{userdata, [{doc, "Tests the public API."}]}].

api(_Conf) ->
    Peers = start_peers(9),
    subscriber_srv:start_link(),
    {Leader, _} = lists:max(Peers),
    timer:sleep(500),
    Leader = nbully:leader(),
    Leader = wait_consensus(),
    Leader = subscriber_srv:leader(),
    0 = subscriber_srv:repeated_lead_msgs(),
    subscriber_srv:stop(),
    stop_peers(Peers),
    ok.

api_neg() ->
    [{userdata, [{doc, "Tests the public API with invalid input data."}]}].

api_neg(_Conf) ->
    Peers = start_peers(9),
    {ok, Subscriber} = subscriber_srv:start(),
    {Leader, _} = lists:max(Peers),
    ok = rpc:call(Leader, application, stop, [nbully]),

    timer:sleep(500),
    NewLeader = nbully:leader(),
    true = NewLeader /= Leader,
    NewLeader = wait_consensus(),
    NewLeader = subscriber_srv:leader(),
    0 = subscriber_srv:repeated_lead_msgs(),
    erlang:exit(Subscriber, kill),
    stop_peers(Peers),
    ok.

fault_tolerance() ->
    [{userdata, [{doc, "Tests the public API with invalid input data."}]}].

fault_tolerance(_Conf) ->
    subscriber_srv:start_link(),
    Peers = start_peers(9),
    timer:sleep(500),
    MaxPeer = {Leader, _} = lists:max(Peers),
    Leader = wait_consensus(),

    Peers2 = stop_peer(MaxPeer, Peers),
    NewLeader = lists:max(all_nodes()),
    NewLeader = wait_consensus(),
    true = NewLeader /= Leader,

    Peers3 = stop_peer(lists:min(Peers2), Peers2),
    NewLeader = lists:max(all_nodes()),
    NewLeader = wait_consensus(),

    Peers4 = stop_peer(lists:max(Peers3), Peers3),
    NewLeader2 = lists:max(all_nodes()),
    NewLeader2 = wait_consensus(),
    true = NewLeader2 /= NewLeader,

    CreatePeersUntilNewLeader = fun Create(CurrentPeers) ->
        [Peer] = start_peers(1),
        case Peer > lists:max(CurrentPeers) of
            true -> [Peer | CurrentPeers];
            false -> Create([Peer | CurrentPeers])
        end
    end,
    NewPeers = CreatePeersUntilNewLeader(Peers4),
    NewLeader3 = lists:max(all_nodes()),
    NewLeader3 = wait_consensus(),
    true = NewLeader3 /= NewLeader2,

    stop_peers(NewPeers),

    0 = subscriber_srv:repeated_lead_msgs(),
    subscriber_srv:stop(),
    ok.

multiple_repeated_subscriptions() ->
    [{userdata, [{doc, "Tests that multiple subscriptions are idempotent."}]}].

multiple_repeated_subscriptions(_Conf) ->
    Peers = start_peers(1),
    subscriber_srv:start_link(),
    ok = subscriber_srv:resubscribe(),
    _Leader = wait_consensus(),
    ok = subscriber_srv:unsubscribe(),
    0 = subscriber_srv:repeated_lead_msgs(),
    subscriber_srv:stop(),
    stop_peers(Peers),
    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
all_nodes() ->
    nodes().

wait_consensus() ->
    {Leaders, _} = rpc:multicall(nbully, leader, []),
    Check = fun
        (_X, {false, _Node}) ->
            {false, undefined};
        (undefined, {true, X}) ->
            {true, X};
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

start_peers(N) ->
    Seq = lists:seq(1, N),
    lists:foreach(
        fun(_) -> ?CT_PEER(#{wait_boot => {self(), tag}, args => ["-pa" | code:get_path()]}) end,
        Seq
    ),
    Peers = lists:map(
        fun(_) ->
            receive
                {tag, {started, Node, Pid}} -> {Node, Pid}
            end
        end,
        Seq
    ),
    rpc:multicall(nodes(), application, start, [nbully]),
    Peers.

stop_peer(Peer = {_Node, Pid}, Peers) ->
    peer:stop(Pid),
    lists:delete(Peer, Peers).

stop_peers(Peers) ->
    lists:foreach(fun(Peer) -> stop_peer(Peer, Peers) end, Peers).

wait_for_node(Node) ->
    case lists:member(Node, nodes()) of
        true ->
            Node;
        false ->
            timer:sleep(100),
            wait_for_node(Node)
    end.
