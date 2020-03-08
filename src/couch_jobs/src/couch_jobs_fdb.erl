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

-module(couch_jobs_fdb).


-export([
    add/5,
    remove/2,
    get_job_state_and_data/2,
    get_jobs/2,
    get_jobs/3,

    accept/4,
    finish/3,
    resubmit/3,
    update/3,

    set_type_timeout/3,
    clear_type_timeout/2,
    get_type_timeout/2,
    get_types/1,

    get_activity_vs/2,
    get_activity_vs_and_watch/2,
    get_active_since/3,
    get_inactive_since/3,
    re_enqueue_inactive/3,

    init_cache/0,

    encode_data/1,
    decode_data/1,

    get_jtx/0,
    get_jtx/1,
    tx/2,

    get_job/2,
    get_jobs/0,

    bump_metadata_version/0,
    bump_metadata_version/1
]).


-include("couch_jobs.hrl").


-record(jv, {
    seq,
    jlock,
    stime,
    resubmit,
    data
}).


-define(JOBS_ETS_KEY, jobs).
-define(MD_TIMESTAMP_ETS_KEY, md_timestamp).
-define(MD_VERSION_MAX_AGE_SEC, 10).
-define(PENDING_SEQ, 0).


% Data model
%
% (?JOBS, ?DATA, Type, JobId) = (Sequence, Lock, SchedTime, Resubmit, JobData)
% (?JOBS, ?PENDING, Type, ScheduledTime, JobId) = ""
% (?JOBS, ?WATCHES_PENDING, Type) = Counter
% (?JOBS, ?WATCHES_ACTIVITY, Type) = Sequence
% (?JOBS, ?ACTIVITY_TIMEOUT, Type) = ActivityTimeout
% (?JOBS, ?ACTIVITY, Type, Sequence) = JobId
%
% In the ?DATA row Sequence can have these values:
%  0 - when the job is pending
%  null - when the job is finished
%  Versionstamp - when the job is running


% Job creation API

add(#{jtx := true} = JTx0, Type, JobId, Data, STime) ->
    #{tx := Tx} = JTx = get_jtx(JTx0),
    Job = #{job => true, type => Type, id => JobId},
    case get_type_timeout(JTx, Type) of
        not_found ->
            {error, no_type_timeout};
        Int when is_integer(Int) ->
            Key = job_key(JTx, Job),
            case erlfdb:wait(erlfdb:get(Tx, Key)) of
                <<_/binary>> ->
                    {ok, Job1} = resubmit(JTx, Job, STime),
                    #{seq := Seq, state := State, data := Data1} = Job1,
                    {ok, State, Seq, Data1};
                not_found ->
                    try
                        maybe_enqueue(JTx, Type, JobId, STime, true, Data),
                        {ok, pending, ?PENDING_SEQ, Data}
                    catch
                        error:{json_encoding_error, Error} ->
                            {error, {json_encoding_error, Error}}
                    end
            end
    end.


remove(#{jtx := true} = JTx0, #{job := true} = Job) ->
    #{tx := Tx} = JTx = get_jtx(JTx0),
    #{type := Type, id := JobId} = Job,
    Key = job_key(JTx, Job),
    case get_job_val(Tx, Key) of
        #jv{stime = STime} ->
            couch_jobs_pending:remove(JTx, Type, JobId, STime),
            erlfdb:clear(Tx, Key),
            ok;
        not_found ->
            {error, not_found}
    end.


get_job_state_and_data(#{jtx := true} = JTx, #{job := true} = Job) ->
    case get_job_val(get_jtx(JTx), Job) of
        #jv{seq = Seq, jlock = JLock, data = Data} ->
            {ok, Seq, job_state(JLock, Seq), Data};
        not_found ->
            {error, not_found}
    end.


get_jobs(JTx, Type) ->
    get_jobs(JTx, Type, fun(_) -> true end).


get_jobs(#{jtx := true} = JTx, Type, Filter) when is_function(Filter, 1) ->
    #{tx := Tx, jobs_path := Jobs} = get_jtx(JTx),
    Prefix = erlfdb_tuple:pack({?DATA, Type}, Jobs),
    Opts = [{streaming_mode, want_all}],
    Result = erlfdb:wait(erlfdb:get_range_startswith(Tx, Prefix, Opts)),
    lists:foldl(fun({K, V}, #{} = Acc) ->
        {JobId} = erlfdb_tuple:unpack(K, Prefix),
        case Filter(JobId) of
            true ->
                {Seq, JLock, _, _, Data} = erlfdb_tuple:unpack(V),
                Acc#{JobId => {Seq, job_state(JLock, Seq), Data}};
            false ->
                Acc
        end
    end, #{}, Result).


% Job processor API

accept(#{jtx := true} = JTx0, Type, MaxSTime, NoSched)
        when is_integer(MaxSTime), is_boolean(NoSched) ->
    #{jtx := true, tx := Tx} = JTx = get_jtx(JTx0),
    case couch_jobs_pending:dequeue(JTx, Type, MaxSTime, NoSched) of
        {not_found, PendingWatch} ->
            {not_found, PendingWatch};
        {ok, JobId} ->
            JLock = fabric2_util:uuid(),
            Key = job_key(JTx, Type, JobId),
            JV0 = get_job_val(Tx, Key),
            #jv{jlock = null, data = Data} = JV0,
            JV = JV0#jv{seq = ?UNSET_VS, jlock = JLock, resubmit = false},
            set_job_val(Tx, Key, JV),
            update_activity(JTx, Type, JobId, null, Data),
            Job = #{
                job => true,
                type => Type,
                id => JobId,
                jlock => JLock
            },
            {ok, Job, decode_data(Data)}
    end.


finish(#{jtx := true} = JTx0, #{jlock := <<_/binary>>} = Job, Data) when
        is_map(Data) orelse Data =:= undefined ->
    #{tx := Tx} = JTx = get_jtx(JTx0),
    #{type := Type, jlock := JLock, id := JobId} = Job,
    case get_job_or_halt(Tx, job_key(JTx, Job), JLock) of
        #jv{seq = Seq, stime = STime, resubmit = Resubmit, data = OldData} ->
            NewData = case Data =:= undefined of
                true -> OldData;
                false -> Data
            end,
            try maybe_enqueue(JTx, Type, JobId, STime, Resubmit, NewData) of
                ok ->
                    clear_activity(JTx, Type, Seq),
                    update_watch(JTx, Type)
            catch
                error:{json_encoding_error, Error} ->
                    {error, {json_encoding_error, Error}}
            end;
        halt ->
            {error, halt}
    end.


resubmit(#{jtx := true} = JTx0, #{job := true} = Job, NewSTime) ->
    #{tx := Tx} = JTx = get_jtx(JTx0),
    #{type := Type, id := JobId} = Job,
    Key = job_key(JTx, Job),
    case get_job_val(Tx, Key) of
        #jv{seq = Seq, jlock = JLock, stime = OldSTime, data = Data} = JV ->
            STime = case NewSTime =:= undefined of
                true -> OldSTime;
                false -> NewSTime
            end,
            case job_state(JLock, Seq) of
                finished ->
                    ok = maybe_enqueue(JTx, Type, JobId, STime, true, Data),
                    Job1 = Job#{
                        seq => ?PENDING_SEQ,
                        state => pending,
                        data => Data
                    },
                    {ok, Job1};
                pending when STime == OldSTime ->
                    % If pending and scheduled time doesn't change avoid generating
                    % un-necessary writes by removing and re-adding the jobs into the
                    % pending queue.
                    Job1 = Job#{
                        stime => STime,
                        seq => ?PENDING_SEQ,
                        state => pending,
                        data => Data
                    },
                    {ok, Job1};
                pending ->
                    JV1 = JV#jv{seq = ?PENDING_SEQ, stime = STime},
                    set_job_val(Tx, Key, JV1),
                    couch_jobs_pending:remove(JTx, Type, JobId, OldSTime),
                    couch_jobs_pending:enqueue(JTx, Type, STime, JobId),
                    Job1 = Job#{
                        stime => STime,
                        seq => ?PENDING_SEQ,
                        state => pending,
                        data => Data
                    },
                    {ok, Job1};
                running ->
                    JV1 = JV#jv{stime = STime, resubmit = true},
                    set_job_val(Tx, Key, JV1),
                    {ok, Job#{resubmit => true, stime => STime,
                        state => running, seq => Seq, data => Data}}
            end;
        not_found ->
            {error, not_found}
    end.


update(#{jtx := true} = JTx0, #{jlock := <<_/binary>>} = Job, Data0) when
        is_map(Data0) orelse Data0 =:= undefined ->
    #{tx := Tx} = JTx = get_jtx(JTx0),
    #{jlock := JLock, type := Type, id := JobId} = Job,
    Key = job_key(JTx, Job),
    case get_job_or_halt(Tx, Key, JLock) of
        #jv{seq = Seq, stime = STime, resubmit = Resubmit} = JV0 ->
            Data = case Data0 =:= undefined of
                true -> JV0#jv.data;
                false -> Data0
            end,
            JV = JV0#jv{seq = ?UNSET_VS, data = Data},
            try set_job_val(Tx, Key, JV) of
                ok ->
                    update_activity(JTx, Type, JobId, Seq, Data),
                    {ok, Job#{resubmit => Resubmit, stime => STime}}
            catch
                error:{json_encoding_error, Error} ->
                    {error, {json_encoding_error, Error}}
            end;
        halt ->
            {error, halt}
    end.


% Type and activity monitoring API

set_type_timeout(#{jtx := true} = JTx, Type, Timeout) ->
    #{tx := Tx, jobs_path := Jobs} = get_jtx(JTx),
    Key = erlfdb_tuple:pack({?ACTIVITY_TIMEOUT, Type}, Jobs),
    Val = erlfdb_tuple:pack({Timeout}),
    erlfdb:set(Tx, Key, Val).


clear_type_timeout(#{jtx := true} = JTx, Type) ->
    #{tx := Tx, jobs_path := Jobs} = get_jtx(JTx),
    Key = erlfdb_tuple:pack({?ACTIVITY_TIMEOUT, Type}, Jobs),
    erlfdb:clear(Tx, Key).


get_type_timeout(#{jtx := true} = JTx, Type) ->
    #{tx := Tx, jobs_path := Jobs} = get_jtx(JTx),
    Key = erlfdb_tuple:pack({?ACTIVITY_TIMEOUT, Type}, Jobs),
    case erlfdb:wait(erlfdb:get_ss(Tx, Key)) of
        not_found ->
            not_found;
        Val ->
            {Timeout} = erlfdb_tuple:unpack(Val),
            Timeout
    end.


get_types(#{jtx := true} = JTx) ->
    #{tx := Tx, jobs_path := Jobs} = get_jtx(JTx),
    Prefix = erlfdb_tuple:pack({?ACTIVITY_TIMEOUT}, Jobs),
    Opts = [{streaming_mode, want_all}],
    Result = erlfdb:wait(erlfdb:get_range_startswith(Tx, Prefix, Opts)),
    lists:map(fun({K, _V}) ->
        {Type} = erlfdb_tuple:unpack(K, Prefix),
        Type
    end, Result).


get_activity_vs(#{jtx := true} = JTx, Type) ->
    #{tx := Tx, jobs_path := Jobs} = get_jtx(JTx),
    Key = erlfdb_tuple:pack({?WATCHES_ACTIVITY, Type}, Jobs),
    case erlfdb:wait(erlfdb:get(Tx, Key)) of
        not_found ->
            not_found;
        Val ->
            {VS} = erlfdb_tuple:unpack(Val),
            VS
    end.


get_activity_vs_and_watch(#{jtx := true} = JTx, Type) ->
    #{tx := Tx, jobs_path := Jobs} = get_jtx(JTx),
    Key = erlfdb_tuple:pack({?WATCHES_ACTIVITY, Type}, Jobs),
    Future = erlfdb:get(Tx, Key),
    Watch = erlfdb:watch(Tx, Key),
    case erlfdb:wait(Future) of
        not_found ->
            {not_found, Watch};
        Val ->
            {VS} = erlfdb_tuple:unpack(Val),
            {VS, Watch}
    end.


get_active_since(#{jtx := true} = JTx, Type, Versionstamp) ->
    #{tx := Tx, jobs_path := Jobs} = get_jtx(JTx),
    Prefix = erlfdb_tuple:pack({?ACTIVITY}, Jobs),
    StartKey = erlfdb_tuple:pack({Type, Versionstamp}, Prefix),
    StartKeySel = erlfdb_key:first_greater_or_equal(StartKey),
    {_, EndKey} = erlfdb_tuple:range({Type}, Prefix),
    Opts = [{streaming_mode, want_all}],
    Future = erlfdb:get_range(Tx, StartKeySel, EndKey, Opts),
    maps:from_list(lists:map(fun({_K, V}) ->
        erlfdb_tuple:unpack(V)
    end, erlfdb:wait(Future))).


get_inactive_since(#{jtx := true} = JTx, Type, Versionstamp) ->
    #{tx := Tx, jobs_path := Jobs} = get_jtx(JTx),
    Prefix = erlfdb_tuple:pack({?ACTIVITY}, Jobs),
    {StartKey, _} = erlfdb_tuple:range({Type}, Prefix),
    EndKey = erlfdb_tuple:pack({Type, Versionstamp}, Prefix),
    EndKeySel = erlfdb_key:first_greater_than(EndKey),
    Opts = [{streaming_mode, want_all}],
    Future = erlfdb:get_range(Tx, StartKey, EndKeySel, Opts),
    lists:map(fun({_K, V}) ->
        {JobId, _} = erlfdb_tuple:unpack(V),
        JobId
    end, erlfdb:wait(Future)).


re_enqueue_inactive(#{jtx := true} = JTx, Type, JobIds) when is_list(JobIds) ->
    #{tx := Tx} = get_jtx(JTx),
    lists:foreach(fun(JobId) ->
        case get_job_val(Tx, job_key(JTx, Type, JobId)) of
            #jv{seq = Seq, stime = STime, data = Data} ->
                clear_activity(JTx, Type, Seq),
                maybe_enqueue(JTx, Type, JobId, STime, true, Data);
            not_found ->
                ok
        end
    end, JobIds),
    case length(JobIds) > 0 of
        true -> update_watch(JTx, Type);
        false -> ok
    end.


% Cache initialization API. Called from the supervisor just to create the ETS
% table. It returns `ignore` to tell supervisor it won't actually start any
% process, which is what we want here.
%
init_cache() ->
    ConcurrencyOpts = [{read_concurrency, true}, {write_concurrency, true}],
    ets:new(?MODULE, [public, named_table] ++ ConcurrencyOpts),
    ignore.


% Functions to encode / decode JobData
%
encode_data(#{} = JobData) ->
    try
        jiffy:encode(JobData)
    catch
        throw:{error, Error} ->
            % legacy clause since new versions of jiffy raise error instead
            error({json_encoding_error, Error});
        error:Error ->
            error({json_encoding_error, Error})
    end.


decode_data(#{} = JobData) ->
    JobData;

decode_data(<<_/binary>> = JobData) ->
    jiffy:decode(JobData, [return_maps]).


% Cached job transaction object. This object wraps a transaction, caches the
% directory lookup path, and the metadata version. The function can be used
% from inside or outside the transaction. When used from a transaction it will
% verify if the metadata was changed, and will refresh automatically.
%
get_jtx() ->
    get_jtx(undefined).


get_jtx(#{tx := Tx} = _TxDb) ->
    get_jtx(Tx);

get_jtx(undefined = _Tx) ->
    case ets:lookup(?MODULE, ?JOBS_ETS_KEY) of
        [{_, #{} = JTx}] ->
            JTx;
        [] ->
            JTx = update_jtx_cache(init_jtx(undefined)),
            JTx#{tx := undefined}
    end;

get_jtx({erlfdb_transaction, _} = Tx) ->
    case ets:lookup(?MODULE, ?JOBS_ETS_KEY) of
        [{_, #{} = JTx}] ->
            ensure_current(JTx#{tx := Tx});
        [] ->
            update_jtx_cache(init_jtx(Tx))
    end.


% Transaction processing to be used with couch jobs' specific transaction
% contexts
%
tx(#{jtx := true} = JTx, Fun) when is_function(Fun, 1) ->
    fabric2_fdb:transactional(JTx, Fun).


% Debug and testing API

get_job(Type, JobId) ->
    fabric2_fdb:transactional(fun(Tx) ->
        JTx = init_jtx(Tx),
        case get_job_val(Tx, job_key(JTx, Type, JobId)) of
            #jv{seq = Seq, jlock = JLock} = JV ->
                #{
                    job => true,
                    type => Type,
                    id => JobId,
                    seq => Seq,
                    jlock => JLock,
                    stime => JV#jv.stime,
                    resubmit => JV#jv.resubmit,
                    data => decode_data(JV#jv.data),
                    state => job_state(JLock, Seq)
                };
            not_found ->
                not_found
        end
    end).


get_jobs() ->
    fabric2_fdb:transactional(fun(Tx) ->
        #{jobs_path := Jobs} = init_jtx(Tx),
        Prefix = erlfdb_tuple:pack({?DATA}, Jobs),
        Opts = [{streaming_mode, want_all}],
        Result = erlfdb:wait(erlfdb:get_range_startswith(Tx, Prefix, Opts)),
        lists:map(fun({K, V}) ->
            {Type, JobId} = erlfdb_tuple:unpack(K, Prefix),
            {Seq, JLock, _, _, Data} = erlfdb_tuple:unpack(V),
            JobState = job_state(JLock, Seq),
            {Type, JobId, JobState, decode_data(Data)}
        end, Result)
    end).


% Call this function if the top level "couchdb" FDB directory layer
% changes.
%
bump_metadata_version() ->
    fabric2_fdb:transactional(fun(Tx) ->
        bump_metadata_version(Tx)
    end).


bump_metadata_version(Tx) ->
    erlfdb:set_versionstamped_value(Tx, ?COUCH_JOBS_MD_VERSION, <<0:112>>).


% Private helper functions

maybe_enqueue(#{jtx := true} = JTx, Type, JobId, STime, Resubmit, Data) ->
    #{tx := Tx} = JTx,
    Key = job_key(JTx, Type, JobId),
    JV = #jv{
        seq = null,
        jlock = null,
        stime = STime,
        resubmit = false,
        data = Data
    },
    case Resubmit of
        true ->
            set_job_val(Tx, Key, JV#jv{seq = ?PENDING_SEQ}),
            couch_jobs_pending:enqueue(JTx, Type, STime, JobId);
        false ->
            set_job_val(Tx, Key, JV)
    end,
    ok.


job_key(#{jtx := true, jobs_path := Jobs}, Type, JobId) ->
    erlfdb_tuple:pack({?DATA, Type, JobId}, Jobs).


job_key(JTx, #{type := Type, id := JobId}) ->
    job_key(JTx, Type, JobId).


get_job_val(#{jtx := true, tx := Tx} = JTx, #{job := true} = Job) ->
    get_job_val(Tx, job_key(JTx, Job));

get_job_val(Tx = {erlfdb_transaction, _}, Key) ->
    case erlfdb:wait(erlfdb:get(Tx, Key)) of
        <<_/binary>> = Val ->
            {Seq, JLock, STime, Resubmit, Data} = erlfdb_tuple:unpack(Val),
            #jv{
                seq = Seq,
                jlock = JLock,
                stime = STime,
                resubmit = Resubmit,
                data = Data
            };
        not_found ->
            not_found
    end.


set_job_val(Tx = {erlfdb_transaction, _}, Key, #jv{} = JV) ->
    #jv{
        seq = Seq,
        jlock = JLock,
        stime = STime,
        resubmit = Resubmit,
        data = Data0
    } = JV,
    Data = case Data0 of
        #{} -> encode_data(Data0);
        <<_/binary>> -> Data0
    end,
    case Seq of
        ?UNSET_VS ->
            Val = erlfdb_tuple:pack_vs({Seq, JLock, STime, Resubmit, Data}),
            erlfdb:set_versionstamped_value(Tx, Key, Val);
        _Other ->
            Val = erlfdb_tuple:pack({Seq, JLock, STime, Resubmit, Data}),
            erlfdb:set(Tx, Key, Val)
    end,
    ok.


get_job_or_halt(Tx, Key, JLock) ->
    case get_job_val(Tx, Key) of
        #jv{jlock = CurJLock} when CurJLock =/= JLock ->
            halt;
        #jv{} = Res ->
            Res;
        not_found ->
            halt
    end.


update_activity(#{jtx := true} = JTx, Type, JobId, Seq, Data0) ->
    #{tx := Tx, jobs_path :=  Jobs} = JTx,
    case Seq =/= null of
        true -> clear_activity(JTx, Type, Seq);
        false -> ok
    end,
    Key = erlfdb_tuple:pack_vs({?ACTIVITY, Type, ?UNSET_VS}, Jobs),
    Data = case Data0 of
        #{} -> encode_data(Data0);
        <<_/binary>> -> Data0
    end,
    Val = erlfdb_tuple:pack({JobId, Data}),
    erlfdb:set_versionstamped_key(Tx, Key, Val),
    update_watch(JTx, Type).


clear_activity(#{jtx := true} = JTx, Type, Seq) ->
    #{tx := Tx, jobs_path :=  Jobs} = JTx,
    Key = erlfdb_tuple:pack({?ACTIVITY, Type, Seq}, Jobs),
    erlfdb:clear(Tx, Key).


update_watch(#{jtx := true} = JTx, Type) ->
    #{tx := Tx, jobs_path :=  Jobs} = JTx,
    Key = erlfdb_tuple:pack({?WATCHES_ACTIVITY, Type}, Jobs),
    Val = erlfdb_tuple:pack_vs({?UNSET_VS}),
    erlfdb:set_versionstamped_value(Tx, Key, Val),
    ok.


job_state(JLock, Seq) ->
    case {JLock, Seq} of
        {null, null} -> finished;
        {JLock, _} when JLock =/= null -> running;
        {null, Seq} when Seq =/= null -> pending
    end.


% This a transaction context object similar to the Db = #{} one from
% fabric2_fdb. It's is used to cache the jobs path directory (to avoid extra
% lookups on every operation) and to check for metadata changes (in case
% directory changes).
%
init_jtx(undefined) ->
    fabric2_fdb:transactional(fun(Tx) -> init_jtx(Tx) end);

init_jtx({erlfdb_transaction, _} = Tx) ->
    LayerPrefix = fabric2_fdb:get_dir(Tx),
    Jobs = erlfdb_tuple:pack({?JOBS}, LayerPrefix),
    % layer_prefix, md_version and tx here match db map fields in fabric2_fdb
    % but we also assert that this is a job transaction using the jtx => true
    % field
    #{
        jtx => true,
        tx => Tx,
        layer_prefix => LayerPrefix,
        jobs_path => Jobs,
        md_version => get_metadata_version(Tx)
    }.


ensure_current(#{jtx := true, tx := Tx} = JTx) ->
    case get(?COUCH_JOBS_CURRENT) of
        Tx ->
            JTx;
        _ ->
            JTx1 = update_current(JTx),
            put(?COUCH_JOBS_CURRENT, Tx),
            JTx1
    end.


get_metadata_version({erlfdb_transaction, _} = Tx) ->
    erlfdb:wait(erlfdb:get_ss(Tx, ?COUCH_JOBS_MD_VERSION)).


update_current(#{tx := Tx, md_version := Version} = JTx) ->
    case get_md_version_age(Version) of
        Age when Age =< ?MD_VERSION_MAX_AGE_SEC ->
            % Looked it up not too long ago. Avoid looking it up to frequently
            JTx;
        _ ->
            case get_metadata_version(Tx) of
                Version ->
                    update_md_version_timestamp(Version),
                    JTx;
                _NewVersion ->
                    update_jtx_cache(init_jtx(Tx))
            end
    end.


update_jtx_cache(#{jtx := true, md_version := Version} = JTx) ->
    CachedJTx = JTx#{tx := undefined},
    ets:insert(?MODULE, {?JOBS_ETS_KEY, CachedJTx}),
    update_md_version_timestamp(Version),
    JTx.


get_md_version_age(Version) ->
    Timestamp = case ets:lookup(?MODULE, ?MD_TIMESTAMP_ETS_KEY) of
        [{_, Version, Ts}] -> Ts;
        _ -> 0
    end,
    erlang:system_time(second) - Timestamp.


update_md_version_timestamp(Version) ->
    Ts = erlang:system_time(second),
    ets:insert(?MODULE, {?MD_TIMESTAMP_ETS_KEY, Version, Ts}).
