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

-module(couch_changes_mon).

-behaviour(gen_server).

-export([decrement_clients_requesting_changes_on_exit/0]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

decrement_clients_requesting_changes_on_exit() ->
    gen_server:cast(?MODULE, {monitor_me, self()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, nil}.

handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast({monitor_me, Pid}, State) ->
    monitor(process, Pid),
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, State) ->
    couch_stats:decrement_counter([couchdb, httpd, clients_requesting_changes]),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.
