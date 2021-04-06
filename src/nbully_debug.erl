%%% Copyright (c) 2016 [Nomasystems, S.L. http://www.nomasystems.com]
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
-module(nbully_debug).

%%% INCLUDE FILES

%%% EXTERNAL EXPORTS
-export([start_n/1, start_n/2]).
-export([start_link_n/1, start_link_n/2]).

%%% MACROS

%%% RECORDS

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
start_n(Count) ->
    start_n(1, Count).

start_n(Start, End) ->
    start_n_impl(Start, End, start).

start_link_n(Count) ->
    start_link_n(1, Count).

start_link_n(Start, End) ->
    start_n_impl(Start, End, start_link).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
start_n_impl(Start, End, StartFun) ->
    {ok, Host} = inet:gethostname(),
    ok = lists:foreach(
        fun(I) ->
            case slave:StartFun(Host, integer_to_list(I)) of
                {ok, _} ->
                    ok;
                {error, {already_running, _}} ->
                    ok
            end
        end,
        lists:seq(Start, End)
    ),
    _ = rpc:eval_everywhere(code, add_pathsa, [code:get_path()]),
    ok.
