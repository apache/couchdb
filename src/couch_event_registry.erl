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

-module(couch_event_registry).
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


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).


init(_) ->
    RegistryOpts = [
        protected,
        named_table,
        bag,
        {keypos, #client.dbname}
    ],
    MonitorOpts = [
        protected,
        named_table,
        set
    ],
    ets:new(?REGISTRY_TABLE, RegistryOpts),
    ets:new(?MONITOR_TABLE, MonitorOpts),
    {ok, nil}.


terminate(_Reason, _St) ->
    ok.


handle_call({register, Pid, DbNames}, _From, St) ->
    ToAdd = [#client{dbname=DBN, pid=Pid} || DBN <- DbNames],
    ets:insert(?REGISTRY_TABLE, ToAdd),
    case ets:member(?MONITOR_TABLE, Pid) of
        true ->
            ok;
        false ->
            Ref = erlang:monitor(process, Pid),
            ets:insert(?MONITOR_TABLE, {Pid, Ref})
    end,
    {reply, ok, St};

handle_call({unregister, Pid, DbNames}, _From, St) ->
    % TODO: Check into a multi-pattern matchspec and the
    % use of select_delete to see if that's faster.
    lists:foreach(fun(DbName) ->
        ets:match_delete(?REGISTRY_TABLE, pattern(DbName, Pid))
    end, DbNames),
    maybe_drop_monitor(Pid),
    {reply, ok, St};

handle_call({unregister_all, Pid}, _From, St) ->
    ets:match_delete(?REGISTRY_TABLE, pattern(Pid)),
    drop_monitor(Pid),
    {reply, ok, St};

handle_call(Msg, From, St) ->
    couch_log:notice("~s ignoring call ~w from ~w", [?MODULE, Msg, From]),
    {reply, ignored, St}.


handle_cast(Msg, St) ->
    couch_log:notice("~s ignoring cast ~w", [?MODULE, Msg]),
    {noreply, St}.


handle_info({'DOWN', _Ref, process, Pid, _Reason}, St) ->
    ets:match_delete(?REGISTRY_TABLE, pattern(Pid)),
    ets:delete(?REGISTRY_TABLE, Pid),
    {norepy, St};

handle_info(Msg, St) ->
    couch_log:notice("~s ignoring info ~w", [?MODULE, Msg]),
    {noreply, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


maybe_drop_monitor(Pid) ->
    case ets:select_count(?REGISTRY_TABLE, mspec(Pid)) of
        0 -> drop_monitor(Pid);
        _ -> ok
    end.


drop_monitor(Pid) ->
    case ets:lookup(?MONITOR_TABLE, Pid) of
        [{Pid, Ref}] ->
            ets:delete(?MONITOR_TABLE, Pid),
            erlang:demonitor(Ref, [flush]);
        [] ->
            ok
    end.


-compile({inline, [
    mspec/1,
    pattern/1,
    pattern/2
]}).


mspec(Pid) ->
    [{pattern(Pid), [], ['$_']}].


pattern(Pid) ->
    #client{pid=Pid, _='_'}.


pattern(DbName, Pid) ->
    #client{dbname=DbName, pid=Pid, _='_'}.
