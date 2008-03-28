%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(httpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(HttpcServices) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [HttpcServices]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([]) ->
    init([[]]);
init([HttpcServices]) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = child_spec(HttpcServices, []),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

child_spec([], []) ->
    [httpc_child_spec(default, only_session_cookies)];
child_spec([], Acc) ->
    Acc;
child_spec([{httpc, {Profile, Dir}} | Rest], Acc) ->
    case httpc_child_spec(Profile, Dir) of
	{} ->
	    child_spec(Rest, Acc);
	Spec ->
	    child_spec(Rest, [Spec | Acc])
    end.

%% Note currently only one profile is supported e.i. the default profile
httpc_child_spec(default, Dir) ->
    Name = httpc_manager,  
    StartFunc = {httpc_manager, start_link, [{default, Dir}]},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [httpc_manager],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules};
httpc_child_spec(_,_) ->
    {}.


