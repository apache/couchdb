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

-module(ken_server).

% gen_server boilerplate
-behaviour(gen_server).
-vsn(1).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% Public interface
-export([start_link/0]).
-export([add/1]).
-export([remove/1]).
-export([add_all_shards/1]).
-export([set_batch_size/1]).
-export([set_delay/1]).
-export([set_limit/1]).
-export([set_prune_interval/1]).

% exports for spawn
-export([update_db_indexes/2]).

-record(job, {
    % {DbName, GroupId} for view. {DbName, DDocId, IndexId} for search.
    name,
    % Pid of either view group or search index
    server,
    worker_pid = nil,
    seq = 0,
    lru = erlang:monotonic_time()
}).

-record(state, {
    q = queue:new(),
    dbworker = nil,
    limit = 20,
    delay = 5000,
    batch_size = 1,
    prune_interval = 60000,
    pruned_last
}).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-ifdef(HAVE_DREYFUS).
-include_lib("dreyfus/include/dreyfus.hrl").
-endif.

-ifdef(HAVE_HASTINGS).
-include_lib("hastings/src/hastings.hrl").
-endif.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Adds a database shard to be indexed
-spec add(binary()) -> ok.
add(DbName) ->
    gen_server:cast(?MODULE, {add, DbName}).

%% @doc Removes all the pending jobs for a database shard.
-spec remove(binary()) -> ok.
remove(DbName) ->
    gen_server:cast(?MODULE, {remove, DbName}).

%% @doc Adds all the shards for a database to be indexed.
-spec add_all_shards(binary()) -> ok.
add_all_shards(DbName) ->
    try
        Shards = mem3:shards(mem3:dbname(DbName)),
        lists:map(
            fun(Shard) ->
                rexi:cast(Shard#shard.node, {ken_server, add, [Shard#shard.name]})
            end,
            Shards
        )
    catch
        error:database_does_not_exist ->
            ok
    end.

%% @doc Changes the configured value for a batch size.
%% Returns previous value.
-spec set_batch_size(pos_integer()) -> pos_integer().
set_batch_size(BS) when is_integer(BS), BS > 0 ->
    gen_server:call(?MODULE, {set_batch_size, BS}).

%% @doc Changes the configured value for a delay between batches.
%% Returns previous value.
-spec set_delay(non_neg_integer()) -> non_neg_integer().
set_delay(Delay) when is_integer(Delay), Delay >= 0 ->
    gen_server:call(?MODULE, {set_delay, Delay}).

%% @doc Changes the configured value for a limit.
%% Returns previous value.
-spec set_limit(pos_integer()) -> pos_integer().
set_limit(Limit) when is_integer(Limit), Limit > 0 ->
    gen_server:call(?MODULE, {set_limit, Limit}).

%% @doc Changes the configured value for a prune interval.
%% Returns previous value.
-spec set_prune_interval(pos_integer()) -> pos_integer().
set_prune_interval(Interval) when is_integer(Interval), Interval > 1000 ->
    gen_server:call(?MODULE, {set_prune_interval, Interval}).

%% gen_server callbacks

init(_) ->
    erlang:send(self(), start_event_handler),
    ets:new(ken_pending, [named_table]),
    ets:new(ken_resubmit, [named_table]),
    ets:new(ken_workers, [named_table, public, {keypos, #job.name}]),
    Limit = list_to_integer(config("limit", "20")),
    {ok, #state{pruned_last = erlang:monotonic_time(), limit = Limit}}.

terminate(_Reason, _State) ->
    ok.

handle_call({set_batch_size, BS}, _From, #state{batch_size = Old} = State) ->
    {reply, Old, State#state{batch_size = BS}, 0};
handle_call({set_delay, Delay}, _From, #state{delay = Old} = State) ->
    {reply, Old, State#state{delay = Delay}, 0};
handle_call({set_limit, Limit}, _From, #state{limit = Old} = State) ->
    {reply, Old, State#state{limit = Limit}, 0};
handle_call({set_prune_interval, Interval}, _From, State) ->
    Old = State#state.prune_interval,
    {reply, Old, State#state{prune_interval = Interval}, 0};
handle_call(Msg, From, State) ->
    {stop, {unknown_call, Msg, From}, State}.

% Queues a DB to (maybe) have indexing jobs spawned.
handle_cast({add, DbName}, State) ->
    case ets:insert_new(ken_pending, {DbName}) of
        true ->
            {noreply, State#state{q = queue:in(DbName, State#state.q)}, 0};
        false ->
            {noreply, State, 0}
    end;
handle_cast({remove, DbName}, State) ->
    Q2 = queue:filter(fun(X) -> X =/= DbName end, State#state.q),
    ets:delete(ken_pending, DbName),
    % Delete search index workers
    ets:match_delete(ken_workers, #job{name = {DbName, '_', '_'}, _ = '_'}),
    % Delete view index workers
    ets:match_delete(ken_workers, #job{name = {DbName, '_'}, _ = '_'}),
    % TODO kill off active jobs for this DB as well
    {noreply, State#state{q = Q2}, 0};
handle_cast({resubmit, DbName}, State) ->
    ets:delete(ken_resubmit, DbName),
    handle_cast({add, DbName}, State);
% st index job names have 3 elements, 3rd being 'hastings'. See job record definition.
handle_cast({trigger_update, #job{name = {_, _, hastings}, server = GPid, seq = Seq} = Job}, State) ->
    % hastings_index:await will trigger a hastings index update
    {Pid, _} = erlang:spawn_monitor(
        hastings_index,
        await,
        [GPid, Seq]
    ),
    Now = erlang:monotonic_time(),
    ets:insert(ken_workers, Job#job{worker_pid = Pid, lru = Now}),
    {noreply, State, 0};
% search index job names have 3 elements. See job record definition.
handle_cast({trigger_update, #job{name = {_, _, _}, server = GPid, seq = Seq} = Job}, State) ->
    % dreyfus_index:await will trigger a search index update.
    {Pid, _} = erlang:spawn_monitor(
        dreyfus_index,
        await,
        [GPid, Seq]
    ),
    Now = erlang:monotonic_time(),
    ets:insert(ken_workers, Job#job{worker_pid = Pid, lru = Now}),
    {noreply, State, 0};
handle_cast({trigger_update, #job{name = {_, _}, server = SrvPid, seq = Seq} = Job}, State) ->
    % couch_index:get_state/2 will trigger a view group index update.
    {Pid, _} = erlang:spawn_monitor(couch_index, get_state, [SrvPid, Seq]),
    Now = erlang:monotonic_time(),
    ets:insert(ken_workers, Job#job{worker_pid = Pid, lru = Now}),
    {noreply, State, 0};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info({gen_event_EXIT, ken_event_handler, Reason}, State) ->
    couch_log:error("ken_event_handler terminated: ~w", [Reason]),
    erlang:send_after(5000, self(), start_event_handler),
    {ok, State, 0};
handle_info(start_event_handler, State) ->
    case ken_event_handler:start_link() of
        {ok, _Pid} ->
            ok;
        Error ->
            couch_log:error("ken_event_handler init: ~w", [Error]),
            erlang:send_after(5000, self(), start_event_handler)
    end,
    {noreply, State, 0};
handle_info(timeout, #state{prune_interval = I, pruned_last = Last} = State) ->
    Now = erlang:monotonic_time(),
    Interval = erlang:convert_time_unit(
        State#state.delay, millisecond, native
    ),
    case Now - Last > Interval of
        true ->
            NewState = prune_worker_table(State);
        _ ->
            NewState = State
    end,
    {noreply, maybe_start_next_queued_job(NewState), I};
handle_info({'DOWN', _, _, Pid, Reason}, #state{dbworker = {Name, Pid}} = St) ->
    maybe_resubmit(Name, Reason),
    {noreply, St#state{dbworker = nil}, 0};
handle_info({'DOWN', _, _, Pid, Reason}, State) ->
    debrief_worker(Pid, Reason, State),
    {noreply, State, 0};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private functions

maybe_start_next_queued_job(#state{dbworker = {_, _}} = State) ->
    State;
maybe_start_next_queued_job(#state{q = Q} = State) ->
    IncrementalChannels = list_to_integer(config("incremental_channels", "80")),
    BatchChannels = list_to_integer(config("batch_channels", "20")),
    TotalChannels = IncrementalChannels + BatchChannels,
    case queue:out(Q) of
        {{value, DbName}, Q2} ->
            case skip_job(DbName) of
                true ->
                    % job is either being resubmitted or ignored, skip it
                    ets:delete(ken_pending, DbName),
                    maybe_start_next_queued_job(State#state{q = Q2});
                false ->
                    case get_active_count() of
                        A when A < TotalChannels ->
                            Args = [DbName, State],
                            {Pid, _} = spawn_monitor(?MODULE, update_db_indexes, Args),
                            ets:delete(ken_pending, DbName),
                            State#state{dbworker = {DbName, Pid}, q = Q2};
                        _ ->
                            State#state{q = queue:in_r(DbName, Q2)}
                    end
            end;
        {empty, Q} ->
            State
    end.

skip_job(DbName) ->
    ets:member(ken_resubmit, DbName) orelse ignore_db(DbName).

ignore_db(DbName) ->
    case config:get("ken.ignore", ?b2l(DbName), false) of
        "true" ->
            true;
        _ ->
            false
    end.

get_active_count() ->
    MatchSpec = [{#job{worker_pid = '$1', _ = '_'}, [{is_pid, '$1'}], [true]}],
    ets:select_count(ken_workers, MatchSpec).

% If any indexing job fails, resubmit requests for all indexes.
update_db_indexes(Name, State) ->
    {ok, DDocs} = design_docs(Name),
    RandomSorted = lists:sort([{rand:uniform(), D} || D <- DDocs]),
    Resubmit = lists:foldl(
        fun({_, DDoc}, Acc) ->
            JsonDDoc = couch_doc:from_json_obj(DDoc),
            case update_ddoc_indexes(Name, JsonDDoc, State) of
                ok -> Acc;
                _ -> true
            end
        end,
        false,
        RandomSorted
    ),
    if
        Resubmit -> exit(resubmit);
        true -> ok
    end.

design_docs(Name) ->
    try
        case fabric:design_docs(mem3:dbname(Name)) of
            {error, {maintenance_mode, _, _Node}} ->
                {ok, []};
            Else ->
                Else
        end
    catch
        error:database_does_not_exist ->
            {ok, []}
    end.

% Returns an error if any job creation fails.
update_ddoc_indexes(Name, #doc{} = Doc, State) ->
    {ok, Db} =
        case couch_db:open_int(Name, []) of
            {ok, _} = Resp -> Resp;
            Else -> exit(Else)
        end,
    Seq = couch_db:get_update_seq(Db),
    couch_db:close(Db),
    ViewUpdated =
        case should_update(Doc, <<"views">>) of
            true ->
                try couch_mrview_util:ddoc_to_mrst(Name, Doc) of
                    {ok, MRSt} -> update_ddoc_views(Name, MRSt, Seq, State)
                catch
                    _:_ ->
                        ok
                end;
            false ->
                ok
        end,
    SearchUpdated = search_updated(Name, Doc, Seq, State),
    STUpdated = st_updated(Name, Doc, Seq, State),
    case {ViewUpdated, SearchUpdated, STUpdated} of
        {ok, ok, ok} -> ok;
        _ -> resubmit
    end.

-ifdef(HAVE_DREYFUS).
search_updated(Name, Doc, Seq, State) ->
    case should_update(Doc, <<"indexes">>) of
        true ->
            try dreyfus_index:design_doc_to_indexes(Doc) of
                SIndexes -> update_ddoc_search_indexes(Name, SIndexes, Seq, State)
            catch
                _:_ ->
                    ok
            end;
        false ->
            ok
    end.
-else.
search_updated(_Name, _Doc, _Seq, _State) ->
    ok.
-endif.

-ifdef(HAVE_HASTINGS).
st_updated(Name, Doc, Seq, State) ->
    case should_update(Doc, <<"st_indexes">>) of
        true ->
            try hastings_index:design_doc_to_indexes(Doc) of
                STIndexes -> update_ddoc_st_indexes(Name, STIndexes, Seq, State)
            catch
                _:_ ->
                    ok
            end;
        false ->
            ok
    end.
-else.
st_updated(_Name, _Doc, _Seq, _State) ->
    ok.
-endif.

should_update(#doc{body = {Props}}, IndexType) ->
    case couch_util:get_value(<<"autoupdate">>, Props) of
        false ->
            false;
        {AUProps} ->
            case couch_util:get_value(IndexType, AUProps) of
                false ->
                    false;
                _ ->
                    true
            end;
        _ ->
            true
    end.

update_ddoc_views(Name, MRSt, Seq, State) ->
    Language = couch_mrview_index:get(language, MRSt),
    Allowed = lists:member(Language, allowed_languages()),
    Views = couch_mrview_index:get(views, MRSt),
    if
        Allowed andalso Views =/= [] ->
            {ok, Pid} = couch_index_server:get_index(couch_mrview_index, MRSt),
            GroupName = couch_mrview_index:get(idx_name, MRSt),
            maybe_start_job({Name, GroupName}, Pid, Seq, State);
        true ->
            ok
    end.

-ifdef(HAVE_DREYFUS).
update_ddoc_search_indexes(DbName, Indexes, Seq, State) ->
    if
        Indexes =/= [] ->
            % Spawn a job for each search index in the ddoc
            lists:foldl(
                fun(#index{name = IName, ddoc_id = DDocName} = Index, Acc) ->
                    case dreyfus_index_manager:get_index(DbName, Index) of
                        {ok, Pid} ->
                            case maybe_start_job({DbName, DDocName, IName}, Pid, Seq, State) of
                                resubmit -> resubmit;
                                _ -> Acc
                            end;
                        _ ->
                            % If any job fails, retry the db.
                            resubmit
                    end
                end,
                ok,
                Indexes
            );
        true ->
            ok
    end.
-endif.

-ifdef(HAVE_HASTINGS).
update_ddoc_st_indexes(DbName, Indexes, Seq, State) ->
    if
        Indexes =/= [] ->
            % The record name in hastings is #h_idx rather than #index as it is for dreyfus
            % Spawn a job for each spatial index in the ddoc
            lists:foldl(
                fun(#h_idx{ddoc_id = DDocName} = Index, Acc) ->
                    case hastings_index_manager:get_index(DbName, Index) of
                        {ok, Pid} ->
                            case maybe_start_job({DbName, DDocName, hastings}, Pid, Seq, State) of
                                resubmit -> resubmit;
                                _ -> Acc
                            end;
                        _ ->
                            % If any job fails, retry the db.
                            resubmit
                    end
                end,
                ok,
                Indexes
            );
        true ->
            ok
    end.
-endif.

should_start_job(#job{name = Name, seq = Seq, server = Pid}, State) ->
    Threshold = list_to_integer(config("max_incremental_updates", "1000")),
    IncrementalChannels = list_to_integer(config("incremental_channels", "80")),
    BatchChannels = list_to_integer(config("batch_channels", "20")),
    TotalChannels = IncrementalChannels + BatchChannels,
    A = get_active_count(),
    #state{delay = Delay, batch_size = BS} = State,
    case ets:lookup(ken_workers, Name) of
        [] ->
            if
                A < BatchChannels ->
                    true;
                A < TotalChannels ->
                    case Name of
                        % st_index name has three elements
                        {_, _, hastings} ->
                            {ok, CurrentSeq} = hastings_index:await(Pid, 0),
                            (Seq - CurrentSeq) < Threshold;
                        % View name has two elements.
                        {_, _} ->
                            % Since seq is 0, couch_index:get_state/2 won't
                            % spawn an index update.
                            {ok, MRSt} = couch_index:get_state(Pid, 0),
                            CurrentSeq = couch_mrview_index:get(update_seq, MRSt),
                            (Seq - CurrentSeq) < Threshold;
                        % Search name has three elements.
                        {_, _, _} ->
                            {ok, _IndexPid, CurrentSeq} = dreyfus_index:await(Pid, 0),
                            (Seq - CurrentSeq) < Threshold;
                        % Should never happen, but if it does, ignore.
                        _ ->
                            false
                    end;
                true ->
                    false
            end;
        [#job{worker_pid = nil, lru = LRU, seq = OldSeq}] ->
            Now = erlang:monotonic_time(),
            DeltaT = erlang:convert_time_unit(Now - LRU, native, millisecond),
            if
                A < BatchChannels, (Seq - OldSeq) >= BS ->
                    true;
                A < BatchChannels, DeltaT > Delay ->
                    true;
                A < TotalChannels, (Seq - OldSeq) < Threshold, DeltaT > Delay ->
                    true;
                true ->
                    false
            end;
        _ ->
            false
    end.

maybe_start_job(JobName, IndexPid, Seq, State) ->
    Job = #job{
        name = JobName,
        server = IndexPid,
        seq = Seq
    },
    case should_start_job(Job, State) of
        true ->
            gen_server:cast(?MODULE, {trigger_update, Job});
        false ->
            resubmit
    end.

debrief_worker(Pid, Reason, _State) ->
    case ets:match_object(ken_workers, #job{worker_pid = Pid, _ = '_'}) of
        [#job{name = Name} = Job] ->
            case Name of
                {DbName, _} ->
                    maybe_resubmit(DbName, Reason);
                {DbName, _, _} ->
                    maybe_resubmit(DbName, Reason)
            end,
            ets:insert(ken_workers, Job#job{worker_pid = nil});
        % should never happen, but if it does, ignore
        [] ->
            ok
    end.

maybe_resubmit(_DbName, normal) ->
    ok;
maybe_resubmit(_DbName, {database_does_not_exist, _}) ->
    ok;
maybe_resubmit(_DbName, {not_found, no_db_file}) ->
    ok;
maybe_resubmit(DbName, resubmit) ->
    resubmit(60000, DbName);
maybe_resubmit(DbName, _) ->
    resubmit(5000, DbName).

resubmit(Delay, DbName) ->
    case ets:insert_new(ken_resubmit, {DbName}) of
        true ->
            erlang:send_after(Delay, ?MODULE, {'$gen_cast', {resubmit, DbName}});
        false ->
            ok
    end.

prune_worker_table(State) ->
    % remove all entries older than specified `delay` in milliseconds
    Delay = erlang:convert_time_unit(State#state.delay, millisecond, native),
    C = erlang:monotonic_time() - Delay,
    %% fun(#job{worker_pid=nil, lru=A) when A < C -> true end
    MatchHead = #job{worker_pid = nil, lru = '$1', _ = '_'},
    Guard = {'<', '$1', C},
    ets:select_delete(ken_workers, [{MatchHead, [Guard], [true]}]),
    State#state{pruned_last = erlang:monotonic_time()}.

allowed_languages() ->
    Config =
        couch_proc_manager:get_servers_from_env("COUCHDB_QUERY_SERVER_") ++
            couch_proc_manager:get_servers_from_env("COUCHDB_NATIVE_QUERY_SERVER_"),
    Allowed = [list_to_binary(string:to_lower(Lang)) || {Lang, _Cmd} <- Config],
    [<<"query">> | Allowed].

config(Key, Default) ->
    config:get("ken", Key, Default).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prune_old_entries_test() ->
    {
        setup,
        fun() ->
            ets:new(ken_workers, [named_table, public, {keypos, #job.name}])
        end,
        fun(_) ->
            catch ets:delete(ken_workers)
        end,
        ?_test(begin
            lists:foreach(
                fun(Idx) ->
                    ets:insert(ken_workers, #job{name = Idx}),
                    timer:sleep(100)
                end,
                lists:seq(1, 3)
            ),
            prune_worker_table(#state{delay = 250}),
            ?assertEqual(
                [2, 3],
                lists:usort(
                    [N || #job{name = N} <- ets:tab2list(ken_workers)]
                )
            ),
            ok
        end)
    }.

-endif.
