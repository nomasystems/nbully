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
-module(nbully_sup).
-behaviour(supervisor).

%%% START/STOP EXPORTS
-export([start_link/0, stop/0]).

%%% SUPERVISOR EXPORTS
-export([init/1]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    supervisor:terminate_child(?MODULE, nbully_wkr).

%%%-----------------------------------------------------------------------------
%%% SUPERVISOR EXPORTS
%%%-----------------------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, 1, 60}, [child(nbully_wrk, nbully_wrk, start_link, [], transient)]}}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
child(Id, Mod, Fun, Args, Restart, Type) ->
    {Id, {Mod, Fun, Args}, Restart, 5000, Type, [Mod]}.

child(Id, Mod, Fun, Args, Restart) ->
    child(Id, Mod, Fun, Args, Restart, worker).
