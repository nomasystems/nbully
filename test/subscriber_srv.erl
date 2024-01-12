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
-module(subscriber_srv).
-behaviour(gen_server).

%%% START/STOP EXPORTS
-export([start/0, start_link/0, stop/0]).

%%% EXTERNAL EXPORTS
-export([leader/0, repeated_lead_msgs/0, resubscribe/0, unsubscribe/0]).

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/2]).

%%% HANDLE MESSAGES EXPORTS
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%% RECORDS
-record(st, {
    leader = undefined :: atom(),
    repeated_lead_msgs = 0 :: non_neg_integer()
}).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
leader() ->
    gen_server:call(?MODULE, leader).

repeated_lead_msgs() ->
    gen_server:call(?MODULE, repeated_lead_msgs).

resubscribe() ->
    gen_server:call(?MODULE, resubscribe).

unsubscribe() ->
    gen_server:call(?MODULE, unsubscribe).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([]) ->
    nbully:subscribe(),
    {ok, #st{leader = nbully:leader()}}.

terminate(_Reason, _St) ->
    nbully:unsubscribe(),
    ok.

%%%-----------------------------------------------------------------------------
%%% HANDLE MESSAGES EXPORTS
%%%-----------------------------------------------------------------------------
handle_call(leader, _From, St) ->
    {reply, St#st.leader, St};
handle_call(repeated_lead_msgs, _From, St) ->
    {reply, St#st.repeated_lead_msgs, St};
handle_call(resubscribe, _From, St) ->
    nbully:subscribe(),
    {reply, ok, St};
handle_call(unsubscribe, _From, St) ->
    nbully:unsubscribe(),
    {reply, ok, St};
handle_call(stop, _From, St) ->
    {stop, normal, ok, St}.

handle_cast(Req, St) ->
    erlang:error(function_clause, [Req, St]).

handle_info({nbully_leader_updated, Node}, St) ->
    case St#st.leader == Node of
        true ->
            {noreply, St#st{repeated_lead_msgs = St#st.repeated_lead_msgs + 1}};
        false ->
            {noreply, St#st{leader = Node, repeated_lead_msgs = 0}}
    end.
