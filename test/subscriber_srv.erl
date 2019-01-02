%%% Copyright (c) 2017 [Nomasystems, S.L. http://www.nomasystems.com]
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
-module(subscriber_srv).

-behaviour(gen_server).

%%% INCLUDE FILES

%%% START/STOP EXPORTS
-export([start/0, start_link/0, stop/0]).

%%% EXTERNAL EXPORTS
-export([leader/0, unsubscribe/0]).

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/2]).

%%% HANDLE MESSAGES EXPORTS
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%% CODE UPDATE EXPORTS
-export([code_change/3]).

%%% MACROS

%%% RECORDS
-record(st, {leader = undefined :: atom()}).

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

unsubscribe() ->
  gen_server:call(?MODULE, unsusbscribe).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([]) ->
  nbully:subscribe(),
  {ok, #st{}}.


terminate(_Reason, _St) ->
  nbully:unsubscribe(),
  ok.

%%%-----------------------------------------------------------------------------
%%% HANDLE MESSAGES EXPORTS
%%%-----------------------------------------------------------------------------
handle_call(leader, _From, St) ->
  {reply, St#st.leader, St};
handle_call(unsubscribe, _From, St) ->
  nbully:unsubscribe(),
  {reply, St, St};
handle_call(stop, _From, St) ->
  {stop, normal, ok, St}.

handle_cast(Req, St) ->
  erlang:error(function_clause, [Req, St]).

handle_info({nbully_leader_updated, Node}, St) ->
  {noreply, St#st{leader = Node}}.

%%%-----------------------------------------------------------------------------
%%% CODE UPDATE EXPORTS
%%%-----------------------------------------------------------------------------
code_change(_OldVsn, St, _Extra) ->
  {ok, St}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------


