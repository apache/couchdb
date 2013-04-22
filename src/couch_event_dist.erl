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

-module(couch_event_dist).
-behavior(gen_server).


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


-include("couch_event_int.hrl").


-record(st, {
    batch_size
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).


init(_) ->
    {ok, #st{batch_size=25}}.


terminate(_Reason, _St) ->
    ok.


handle_call(Msg, From, St) ->
    couch_log:notice("~s ignoring call ~w from ~w", [?MODULE, Msg, From]),
    {reply, ignored, St}.


handle_cast({DbName, Event}, #st{batch_size=BS}=St) when is_binary(DbName) ->
    P1 = #client{dbname=DbName, _='_'},
    notify_clients(ets:select(?REGISTRY_TABLE, P1, BS), DbName, Event),
    P2 = #client{dbname=all_dbs, _='_'},
    notify_clients(ets:select(?REGISTRY_TABLE, P2, BS), DbName, Event),
    {noreply, St};

handle_cast(Msg, St) ->
    couch_log:notice("~s ignoring cast ~w", [?MODULE, Msg]),
    {noreply, St}.


handle_info(Msg, St) ->
    couch_log:notice("~s ignoring info ~w", [?MODULE, Msg]),
    {noreply, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


notify_clients('$end_of_table', _DbName, _Event) ->
    ok;
notify_clients({Clients, Cont}, DbName, Event) ->
    lists:foreach(fun(#client{pid=Pid}) ->
        Pid ! {'$couch_event', DbName, Event}
    end, Clients),
    notify_clients(ets:select(Cont), DbName, Event).
