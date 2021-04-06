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
-module(nbully_sup).
-behaviour(supervisor).

%%% INCLUDE FILES

%%% START/STOP EXPORTS
-export([start_link/0, stop/0]).

%%% INTERNAL EXPORTS
-export([init/1]).

%%% MACROS

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    ok = nbully_wrk:stop(),
    ok = supervisor:delete_child(?MODULE, nbully_wkr).

%%%-----------------------------------------------------------------------------
%%% INTERNAL EXPORTS
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
