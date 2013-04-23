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
    lists:foreach(fun(DbName) ->
        ets:insert(?REGISTRY_TABLE, #client{dbname=DbName, pid=Pid})
    end, DbNames),
    case ets:lookup(?MONITOR_TABLE, Pid) of
        [] ->
            Ref = erlang:monitor(process, Pid),
            ets:insert(?MONITOR_TABLE, {Pid, Ref});
        [{Pid, _}] ->
            ok
    end,
    {reply, ok, St};

handle_call({unregister, Pid, DbNames}, _From, St) ->
    lists:foreach(fun(DbName) ->
        unregister_pattern(#client{dbname=DbName, pid=Pid, _='_'})
    end, DbNames),
    {reply, ok, St};

handle_call({unregister_all, Pid}, _From, St) ->
    unregister_pattern(#client{pid=Pid, _='_'}),
    {reply, ok, St};

handle_call(Msg, From, St) ->
    couch_log:notice("~s ignoring call ~w from ~w", [?MODULE, Msg, From]),
    {reply, ignored, St, 0}.


handle_cast(Msg, St) ->
    couch_log:notice("~s ignoring cast ~w", [?MODULE, Msg]),
    {noreply, St, 0}.


handle_info({'DOWN', _Ref, process, Pid, _Reason}, St) ->
    unregister_pattern(#client{pid=Pid, _='_'}),
    {noreply, St};

handle_info(Msg, St) ->
    couch_log:notice("~s ignoring info ~w", [?MODULE, Msg]),
    {noreply, St, 0}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


unregister_pattern(Pattern) ->
    Clients = ets:match_object(?REGISTRY_TABLE, Pattern),
    Refs = lists:foldl(fun(#client{pid=Pid}=Cli, Acc) ->
        ets:delete_object(?REGISTRY_TABLE, Cli),
        case ets:lookup(?MONITOR_TABLE, Pid) of
            [{Pid, Ref}] ->
                ets:delete(?MONITOR_TABLE, Pid),
                [Ref | Acc];
            [] ->
                Acc
        end
    end, [], Clients),
    lists:foreach(fun(Ref) ->
        erlang:demonitor(Ref, [flush])
    end, Refs).
