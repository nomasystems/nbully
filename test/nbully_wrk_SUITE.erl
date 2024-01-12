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
-module(nbully_wrk_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [sys_calls, undefined_if_not_started].

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
sys_calls() ->
    [{userdata, [{doc, "Tests the sys API."}]}].

sys_calls(_Conf) ->
    {ok, Pid} = nbully_wrk:start_link(),
    ok = sys:suspend(Pid),
    ok = sys:change_code(Pid, nbully_wrk, undefined, undefined),
    ok = sys:resume(Pid),
    unlink(Pid),
    ok = nbully_wrk:stop().

undefined_if_not_started() ->
    [{userdata, [{doc, "Tests not crashing if called with a non-started worker."}]}].

undefined_if_not_started(_Conf) ->
    undefined = nbully_wrk:leader().
