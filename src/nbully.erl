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
-module(nbully).
-behaviour(application).


%%% START/STOP EXPORTS
-export ([start/2,
          stop/1]).

%%% API EXPORTS
-export([leader/0, subscribe/0, unsubscribe/0]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start(_, _) ->
  nbully_sup:start_link().


stop(_) ->
  nbully_sup:stop().


%%%-----------------------------------------------------------------------------
%%% API EXPORTS
%%%-----------------------------------------------------------------------------
leader() ->
  nbully_wrk:leader().


subscribe() ->
  nbully_wrk:subscribe().


unsubscribe() ->
  nbully_wrk:unsubscribe().
