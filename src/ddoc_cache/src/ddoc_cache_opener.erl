% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(ddoc_cache_opener).
-behaviour(gen_server).
-vsn(1).


-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    {ok, nil}.

terminate(_Reason, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.


% The do_evict clauses are upgrades while we're
% in a rolling reboot.
handle_cast({do_evict, _} = Msg, St) ->
    gen_server:cast(ddoc_cache_lru, Msg),
    {noreply, St};

handle_cast({do_evict, DbName, DDocIds}, St) ->
    gen_server:cast(ddoc_cache_lru, {do_refresh, DbName, DDocIds}),
    {noreply, St};

handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
