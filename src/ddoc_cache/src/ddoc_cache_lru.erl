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

-module(ddoc_cache_lru).
-behaviour(gen_server).

-export([
    start_link/0,
    open/1,
    insert/2,
    refresh/2
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([
    handle_db_event/3
]).

-include("ddoc_cache.hrl").

-record(st, {
    % pid -> key
    pids,
    % dbname -> docid -> key -> pid
    dbs,
    evictor
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

open(Key) ->
    try ets:lookup(?CACHE, Key) of
        [] ->
            lru_start(Key, true);
        [#entry{pid = undefined}] ->
            lru_start(Key, false);
        [#entry{val = undefined, pid = Pid}] ->
            couch_stats:increment_counter([ddoc_cache, miss]),
            ddoc_cache_entry:open(Pid, Key);
        [#entry{val = Val, pid = Pid}] ->
            couch_stats:increment_counter([ddoc_cache, hit]),
            ddoc_cache_entry:accessed(Pid),
            {ok, Val}
    catch
        _:_ ->
            couch_stats:increment_counter([ddoc_cache, recovery]),
            ddoc_cache_entry:recover(Key)
    end.

insert(Key, Value) ->
    case ets:lookup(?CACHE, Key) of
        [] ->
            Wrapped = ddoc_cache_value:wrap(Value),
            gen_server:call(?MODULE, {start, Key, Wrapped}, infinity);
        [#entry{}] ->
            ok
    end.

refresh(DbName, DDocIds) ->
    gen_server:cast(?MODULE, {refresh, DbName, DDocIds}).

init(_) ->
    erlang:process_flag(message_queue_data, off_heap),
    process_flag(trap_exit, true),
    BaseOpts = [public, named_table],
    CacheOpts =
        [
            set,
            {read_concurrency, true},
            {keypos, #entry.key}
        ] ++ BaseOpts,
    ets:new(?CACHE, CacheOpts),
    ets:new(?LRU, [ordered_set, {write_concurrency, true}] ++ BaseOpts),
    {ok, Evictor} = couch_event:link_listener(
        ?MODULE, handle_db_event, nil, [all_dbs]
    ),
    ?EVENT(lru_init, nil),
    {ok, #st{
        pids = #{},
        dbs = #{},
        evictor = Evictor
    }}.

terminate(_Reason, St) ->
    case is_pid(St#st.evictor) of
        true -> exit(St#st.evictor, kill);
        false -> ok
    end,
    ok.

handle_call({start, Key, Default}, _From, #st{} = St) ->
    case ets:lookup(?CACHE, Key) of
        [] ->
            MaxSize = config:get_integer("ddoc_cache", "max_size", 104857600),
            case trim(St, max(0, MaxSize)) of
                {ok, #st{pids = Pids, dbs = Dbs} = St1} ->
                    true = ets:insert_new(?CACHE, #entry{key = Key}),
                    {ok, Pid} = ddoc_cache_entry:start_link(Key, Default),
                    true = ets:update_element(?CACHE, Key, {#entry.pid, Pid}),
                    Pids1 = Pids#{Pid => Key},
                    Dbs1 = store_key(Dbs, Key, Pid),
                    {reply, {ok, Pid}, St1#st{dbs = Dbs1, pids = Pids1}};
                {full, #st{} = St1} ->
                    ?EVENT(full, Key),
                    {reply, full, St1}
            end;
        [#entry{pid = Pid}] ->
            {reply, {ok, Pid}, St}
    end;
handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.

handle_cast({evict, DbName}, St) ->
    gen_server:abcast(mem3:nodes(), ?MODULE, {do_evict, DbName}),
    {noreply, St};
handle_cast({refresh, DbName, DDocIds}, St) ->
    gen_server:abcast(mem3:nodes(), ?MODULE, {do_refresh, DbName, DDocIds}),
    {noreply, St};
handle_cast({do_evict, DbName}, #st{dbs = Dbs} = St) ->
    ToRem = get_entries(DbName, Dbs),
    case ToRem of
        [] -> ?EVENT(evict_noop, DbName);
        [_ | _] -> ?EVENT(evicted, DbName)
    end,
    St1 = lists:foldl(fun remove_entry/2, St, ToRem),
    {noreply, St1#st{dbs = maps:remove(DbName, Dbs)}};
handle_cast({do_refresh, DbName, DDocIdList}, #st{dbs = Dbs} = St) ->
    % We prepend no_ddocid to the DDocIdList below
    % so that we refresh all custom and validation
    % function entries which load data from all
    % design documents.
    case Dbs of
        #{DbName := DDocIds} -> do_refresh(DDocIds, [no_ddocid | DDocIdList]);
        _ -> ok
    end,
    {noreply, St};
handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.

handle_info({'EXIT', Pid, Reason}, #st{evictor = Pid} = St) ->
    {stop, Reason, St};
handle_info({'EXIT', Pid, normal}, #st{pids = Pids, dbs = Dbs} = St) ->
    % This clause handles when an entry starts
    % up but encounters an error or uncacheable
    % response from its recover call.
    {Key, Pids1} = maps:take(Pid, Pids),
    {noreply, St#st{pids = Pids1, dbs = remove_key(Dbs, Key)}};
handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.

handle_db_event(ShardDbName, created, St) ->
    gen_server:cast(?MODULE, {evict, mem3:dbname(ShardDbName)}),
    {ok, St};
handle_db_event(ShardDbName, deleted, St) ->
    gen_server:cast(?MODULE, {evict, mem3:dbname(ShardDbName)}),
    {ok, St};
handle_db_event(_DbName, _Event, St) ->
    {ok, St}.

lru_start(Key, DoInsert) ->
    case gen_server:call(?MODULE, {start, Key, undefined}, infinity) of
        {ok, Pid} ->
            couch_stats:increment_counter([ddoc_cache, miss]),
            Resp = ddoc_cache_entry:open(Pid, Key),
            if
                not DoInsert -> ok;
                true -> ddoc_cache_entry:insert(Key, Resp)
            end,
            Resp;
        full ->
            couch_stats:increment_counter([ddoc_cache, recovery]),
            ddoc_cache_entry:recover(Key)
    end.

trim(#st{} = St, 0) ->
    {full, St};
trim(#st{} = St, MaxSize) ->
    CurSize = ets:info(?CACHE, memory) * erlang:system_info(wordsize),
    if
        CurSize =< MaxSize ->
            {ok, St};
        true ->
            case ets:first(?LRU) of
                {_Ts, Key, Pid} ->
                    St1 = remove_entry({Key, Pid}, St),
                    trim(St1, MaxSize);
                '$end_of_table' ->
                    {full, St}
            end
    end.

get_entries(DbName, #{} = Dbs) ->
    case Dbs of
        #{DbName := DDocIds} ->
            Fun = fun(_, Keys, Acc1) -> maps:to_list(Keys) ++ Acc1 end,
            maps:fold(Fun, [], DDocIds);
        _ ->
            []
    end.

do_refresh(#{} = DDocIdsMap, [_ | _] = DDocIdList) ->
    Fun = fun(DDocId) ->
        case DDocIdsMap of
            #{DDocId := Keys} ->
                maps:foreach(fun(_, Pid) -> ddoc_cache_entry:refresh(Pid) end, Keys);
            _ ->
                ok
        end
    end,
    lists:foreach(Fun, [no_ddocid | DDocIdList]).

remove_entry({Key, Pid}, #st{pids = Pids, dbs = Dbs} = St) ->
    unlink_and_flush(Pid),
    ddoc_cache_entry:shutdown(Pid),
    St#st{pids = maps:remove(Pid, Pids), dbs = remove_key(Dbs, Key)}.

store_key(#{} = Dbs, Key, Pid) ->
    DbName = ddoc_cache_entry:dbname(Key),
    DDocId = ddoc_cache_entry:ddocid(Key),
    case Dbs of
        #{DbName := DDocIds} ->
            case DDocIds of
                #{DDocId := Keys} ->
                    Dbs#{DbName := DDocIds#{DDocId := Keys#{Key => Pid}}};
                _ ->
                    Dbs#{DbName := DDocIds#{DDocId => #{Key => Pid}}}
            end;
        _ ->
            Dbs#{DbName => #{DDocId => #{Key => Pid}}}
    end.

remove_key(#{} = Dbs, Key) ->
    DbName = ddoc_cache_entry:dbname(Key),
    DDocId = ddoc_cache_entry:ddocid(Key),

    % For non-existent ddocs, a new ddoc_cache_entry is spawned for
    % each call to ddoc_cache:open. Multiple calls to open the same
    % non-existent ddoc will create multiple cache entries with the
    % same Key but different PIDs. This can result in the following
    % map lookups not finding results, so handle those corner cases.
    case Dbs of
        #{DbName := DDocIds = #{DDocId := Keys = #{Key := _Pid}}} ->
            Keys1 = maps:remove(Key, Keys),
            case map_size(Keys1) of
                0 ->
                    DDocIds1 = maps:remove(DDocId, DDocIds),
                    case map_size(DDocIds1) of
                        0 -> maps:remove(DbName, Dbs);
                        _ -> Dbs#{DbName := DDocIds1}
                    end;
                _ ->
                    Dbs#{DbName := DDocIds#{DDocId := Keys1}}
            end;
        _ ->
            Dbs
    end.

unlink_and_flush(Pid) ->
    unlink(Pid),
    % Its possible that the entry process has already exited before
    % we unlink it so we have to flush out a possible 'EXIT'
    % message sitting in our message queue. Notice that we're
    % maintaining the assertion that all entry processes only
    % ever exit normally.
    receive
        {'EXIT', Pid, normal} ->
            ok
    after 0 ->
        ok
    end.
