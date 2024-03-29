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
-module(nbully).
-behaviour(application).

%%% APPLICATION EXPORTS
-export([
    start/2,
    stop/1
]).

%%% API EXPORTS
-export([leader/0, subscribe/0, unsubscribe/0]).

%%%-----------------------------------------------------------------------------
%%% APPLICATION EXPORTS
%%%-----------------------------------------------------------------------------
start(_, _) ->
    nbully_sup:start_link().

stop(_) ->
    nbully_sup:stop().

%%%-----------------------------------------------------------------------------
%%% API EXPORTS
%%%-----------------------------------------------------------------------------
-spec leader() -> node().
leader() ->
    nbully_wrk:leader().

-spec subscribe() -> ok.
subscribe() ->
    nbully_wrk:subscribe().

-spec unsubscribe() -> ok.
unsubscribe() ->
    nbully_wrk:unsubscribe().
