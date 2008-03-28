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
%%----------------------------------------------------------------------
%% Purpose: The top supervisor for tftp hangs under inets_sup.
%%----------------------------------------------------------------------

-module(tftp_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_child/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

start_link(TftpServices) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [TftpServices]).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).
    
%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================

init([Services]) when is_list(Services) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    KillAfter = timer:seconds(3),
    Children = [worker_spec(KillAfter, Options) || {tftpd, Options} <- Services],
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================

worker_spec(KillAfter, Options) ->
    Modules = [proc_lib, tftp, tftp_engine],
    KA = supervisor_timeout(KillAfter),
    Name = unique_name(Options),
    {Name, {tftp, start, [Options]}, permanent, KA, worker, Modules}.

unique_name(Options) ->
    case lists:keysearch(port, 1, Options) of
	{value, {_, Port}} when is_integer(Port), Port > 0 -> 
	    {tftpd, Port};
	_ ->
	    {tftpd, erlang:now()}
    end.

%% supervisor_spec(Name) ->
%%     {Name, {Name, start, []}, permanent, infinity, supervisor,
%%      [Name, supervisor]}.
    
-ifdef(debug_shutdown).
supervisor_timeout(_KillAfter) -> timer:hours(24).
-else.
supervisor_timeout(KillAfter) -> KillAfter.
-endif.    
