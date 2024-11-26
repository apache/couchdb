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

% Scanner plugin runner process
%
% This is the process which is spawned and run for each enabled plugin.
%
% A number of these processes are managed by the couch_scanner_server via
% start_link/1 and complete/1 functions. After a plugin runner is spawned, the only
% thing couch_scanner_server does is wait for it to exit.
%
% The plugin runner process may exit normally, crash, or exit with {shutdown,
% {reschedule, TSec}} if they want to reschedule to run again at some point the
% future (next day, a week later, etc).
%
% After the process starts, it will load and validate the plugin module. Then,
% it will start scanning all the dbs and docs on the local node. Shard ranges
% will be scanned only on one of the cluster nodes to avoid duplicating work.
% For instance, if there are 2 shard ranges, 0-7, 8-f, with copies on nodes n1,
% n2, n3. Then, 0-7 might be scanned on n1 only, and 8-f on n3.
%
% The plugin API defined in the behavior definition section.
%
% The start/2 function is called when the plugin starts running. It returns
% some context (St), which can be any Erlang term. All subsequent function
% calls will be called with the same St object, and may return an updated
% version of it.
%
% If the plugin hasn't completed runing and has resumed running after the node
% was restarted or an error happened, the resume/2 function will be called.
% That's the difference between start and resume: start/2 is called when the
% scan starts from the beginning (first db, first shard, ...), and resume/2 is
% called when the scanning hasn't finished and has to continue.
%
% If start/2 or resume/2 returns `reset` then the checkpoint will be reset and
% the plugin will be restarted. This may be useful in cases when the plugin
% detects configuration changes since last scanning session had already
% started, or when the plugin module was updated and the checkpoint version is
% stale.
%
% The checkpoint/1 callback is periodically called to checkpoint the scanning
% progress. start/2 and resume/2 function will be called with the last saved
% checkpoint map value.
%
% The complete/1 callback is called when the scan has finished. The complete
% callback should return final checkpoint map object. The last checkoint will
% be written and then it will be passed to the start/2 callback if the plugin
% runs again.
%
% As the cluster dbs, shards, ddocs and individual docs are discovered during
% scanning, the appropriate callbacks will be called. Most callbacks, besides
% the updated St object, can reply with ok, skip or complete tags. The meaning of
% those are:
%
%   * ok  - continue to the next object
%
%   * skip - skip the current object and don't scan its internal (ex: skip a db
%     and don't scan its ddocs, but continue with the next db)
%
%   * stop - stop scanning any remaining objects of that type (ex: don't scan
%     any more dbs)
%
%   * reset - stop, reset the checkpoint data and restart, this may be useful
%     if the configuration changes and it's best to just restart with the new
%     settings

-module(couch_scanner_plugin).

-export([
    % Main plugin process API
    spawn_link/1,
    stop/1,
    % Internal export
    run/1
]).

-include_lib("couch_scanner/include/couch_scanner_plugin.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

% Behaviour callback definitions

-callback start(ScanId :: binary(), EJson :: #{}) ->
    {ok, St :: term()} | skip | reset.

-callback resume(ScanId :: binary(), EJson :: #{}) ->
    {ok, St :: term()} | skip | reset.

% Optional
-callback complete(St :: term()) ->
    {ok, EJson :: #{}}.

% Optional
-callback checkpoint(St :: term()) ->
    {ok, EJson :: #{}}.

-callback db(St :: term(), DbName :: binary()) ->
    {ok | skip | stop, St1 :: term()}.

% Optional
-callback ddoc(St :: term(), DbName :: binary(), #doc{}) ->
    {ok | stop, St1 :: term()}.

% Optional. If no subsequent callbacks are defined, then the default function
% returns [] (don't open any shards). If any subsequent callbacks are defined,
% the default action is to return all the shards in the list.
-callback shards(St :: term(), [#shard{}]) ->
    {[#shard{}], St1 :: term()}.

% Optional
-callback db_opened(St :: term(), Db :: term()) ->
    {ok, St :: term()}.

% Optional. If doc is not defined, then ddoc_id default action is {skip, St}.
% If it is defined, the default action is {ok, St}.
-callback doc_id(St :: term(), DocId :: binary(), Db :: term()) ->
    {ok | skip | stop, St1 :: term()}.

% Optional.
-callback doc(St :: term(), Db :: term(), #doc{}) ->
    {ok | stop, St1 :: term()}.

% Optional.
-callback db_closing(St :: term(), Db :: term()) ->
    {ok, St1 :: term()}.

-optional_callbacks([
    complete/1,
    checkpoint/1,
    ddoc/3,
    shards/2,
    db_opened/2,
    doc_id/3,
    doc/3,
    db_closing/2
]).

-define(CALLBACKS, [
    {start, 2, fun required_callback/3},
    {resume, 2, fun required_callback/3},
    {complete, 1, fun default_complete/3},
    {checkpoint, 1, fun default_checkpoint/3},
    {db, 2, fun required_callback/3},
    {ddoc, 3, fun default_ddoc/3},
    {shards, 2, fun default_shards/3},
    {db_opened, 2, fun default_db_opened/3},
    {doc_id, 3, fun default_doc_id/3},
    {doc, 3, fun default_doc/3},
    {db_closing, 2, fun default_db_closing/3}
]).

-define(CHECKPOINT_INTERVAL_SEC, 10).
-define(STOP_TIMEOUT_SEC, 5).

-record(st, {
    id,
    rlimiter,
    scan_id,
    mod,
    callbacks = #{},
    pst,
    dbname,
    cursor,
    shards_db,
    db,
    checkpoint_sec = 0,
    start_sec = 0,
    skip_dbs,
    skip_ddocs,
    skip_docs
}).

spawn_link(Id) ->
    proc_lib:spawn_link(?MODULE, run, [Id]).

stop(Pid) when is_pid(Pid) ->
    unlink(Pid),
    Ref = erlang:monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after ?STOP_TIMEOUT_SEC * 1000 ->
        exit(Pid, kill),
        receive
            {'DOWN', Ref, _, _, _} -> ok
        end
    end,
    ok.

% Main run function

run(Id) ->
    RLimiter = couch_scanner_rate_limiter:get(),
    {Mod, Callbacks} = plugin_mod(Id),
    St = #st{
        id = Id,
        mod = Mod,
        callbacks = Callbacks,
        rlimiter = RLimiter
    },
    St1 = init_config(St),
    St2 = init_from_checkpoint(St1),
    St3 = scan_dbs(St2),
    finalize(St3).

% Private functions

init_config(#st{mod = Mod} = St) ->
    St#st{
        skip_dbs = config_match_patterns(Mod, "skip_dbs"),
        skip_ddocs = config_match_patterns(Mod, "skip_ddocs"),
        skip_docs = config_match_patterns(Mod, "skip_docs")
    }.

init_from_checkpoint(#st{} = St) ->
    #st{id = Id, mod = Mod, callbacks = Cbks} = St,
    case couch_scanner_checkpoint:read(Id) of
        #{
            <<"state">> := <<"running">>,
            <<"cursor">> := Cur,
            <<"scan_id">> := SId,
            <<"pst">> := EJsonPSt,
            <<"start_sec">> := StartSec
        } ->
            Now = tsec(),
            PSt = resume_callback(Cbks, SId, EJsonPSt),
            St#st{
                pst = PSt,
                cursor = Cur,
                checkpoint_sec = Now,
                start_sec = StartSec,
                scan_id = SId
            };
        not_found ->
            SId = couch_scanner_util:new_scan_id(),
            Now = tsec(),
            LastStartSec = 0,
            Cur = <<>>,
            PSt = start_callback(Mod, Cbks, Now, SId, LastStartSec, #{}),
            ok = start_checkpoint(Id, Cbks, Now, SId, Cur, PSt),
            St#st{
                pst = PSt,
                cursor = Cur,
                checkpoint_sec = 0,
                start_sec = Now,
                scan_id = SId
            };
        #{
            <<"state">> := <<"finished">>,
            <<"pst">> := EJson,
            <<"start_sec">> := LastStartSec
        } ->
            SId = couch_scanner_util:new_scan_id(),
            Now = tsec(),
            Cur = <<>>,
            PSt = start_callback(Mod, Cbks, Now, SId, LastStartSec, EJson),
            ok = start_checkpoint(Id, Cbks, Now, SId, Cur, PSt),
            St#st{
                pst = PSt,
                cursor = Cur,
                checkpoint_sec = Now,
                start_sec = Now,
                scan_id = SId
            }
    end.

scan_dbs(#st{cursor = Cursor} = St) ->
    ShardsDbName = mem3_sync:shards_db(),
    ioq:set_io_priority({system, ShardsDbName}),
    {ok, Db} = mem3_util:ensure_exists(ShardsDbName),
    St1 = St#st{shards_db = Db},
    Opts = [{start_key, Cursor}],
    try
        {ok, St2} = couch_db:fold_docs(Db, fun scan_dbs_fold/2, St1, Opts),
        St2#st{shards_db = undefined}
    after
        couch_db:close(Db)
    end.

scan_dbs_fold(#full_doc_info{} = FDI, #st{shards_db = Db} = Acc) ->
    Acc1 = Acc#st{cursor = FDI#full_doc_info.id},
    Acc2 = maybe_checkpoint(Acc1),
    case couch_db:open_doc(Db, FDI, [ejson_body]) of
        {ok, #doc{deleted = true}} ->
            {ok, Acc2};
        {ok, #doc{id = DbName, body = Body}} ->
            Shards = shards(DbName, Body),
            Acc3 = Acc2#st{dbname = DbName},
            {Go, Acc4} = scan_db(Shards, Acc3),
            {Go, Acc4#st{dbname = undefined}};
        {not_found, _} ->
            {ok, Acc2}
    end.

scan_db([], #st{} = St) ->
    {ok, St};
scan_db([_ | _] = Shards, #st{} = St) ->
    #st{dbname = DbName, callbacks = Cbks, pst = PSt, skip_dbs = Skip} = St,
    #{db := DbCbk} = Cbks,
    case match_skip_pat(DbName, Skip) of
        false ->
            {Go, PSt1} = DbCbk(PSt, DbName),
            St1 = St#st{pst = PSt1},
            case Go of
                ok ->
                    St2 = rate_limit(St1, db),
                    St3 = fold_ddocs(fun scan_ddocs_fold/2, St2),
                    {Shards1, St4} = shards_callback(St3, Shards),
                    St5 = scan_shards(Shards1, St4),
                    {ok, St5};
                skip ->
                    {ok, St1};
                stop ->
                    {stop, St1}
            end;
        true ->
            {ok, St}
    end.

scan_ddocs_fold({meta, _}, #st{} = Acc) ->
    {ok, Acc};
scan_ddocs_fold({row, RowProps}, #st{} = Acc) ->
    DDoc = couch_util:get_value(doc, RowProps),
    scan_ddoc(ejson_to_doc(DDoc), Acc);
scan_ddocs_fold(complete, #st{} = Acc) ->
    {ok, Acc};
scan_ddocs_fold({error, Error}, _Acc) ->
    exit({shutdown, {scan_ddocs_fold, Error}}).

scan_shards([], #st{} = St) ->
    St;
scan_shards([#shard{} = Shard | Rest], #st{} = St) ->
    St1 = scan_docs(St, Shard),
    scan_shards(Rest, St1).

scan_ddoc(#doc{id = DDocId} = DDoc, #st{} = St) ->
    #st{dbname = DbName, callbacks = Cbks, pst = PSt, skip_ddocs = Skip} = St,
    #{ddoc := DDocCbk} = Cbks,
    case match_skip_pat(DDocId, Skip) of
        false ->
            {Go, PSt1} = DDocCbk(PSt, DbName, DDoc),
            St1 = St#st{pst = PSt1},
            case Go of
                ok -> {ok, St1};
                stop -> {stop, St1}
            end;
        true ->
            {ok, St}
    end.

scan_docs(#st{} = St, #shard{name = ShardDbName}) ->
    St1 = rate_limit(St, shard),
    case couch_db:open_int(ShardDbName, [?ADMIN_CTX]) of
        {ok, Db} ->
            try
                St2 = St1#st{db = Db},
                St3 = db_opened_callback(St2),
                {ok, St4} = couch_db:fold_docs(Db, fun scan_docs_fold/2, St3, []),
                St5 = db_closing_callback(St4),
                erlang:garbage_collect(),
                St5#st{db = undefined}
            after
                couch_db:close(Db)
            end;
        {not_found, _} ->
            St1
    end.

scan_docs_fold(#full_doc_info{id = Id} = FDI, #st{} = St) ->
    #st{db = Db, callbacks = Cbks, pst = PSt, skip_docs = Skip} = St,
    #{doc_id := DocIdCbk} = Cbks,
    case match_skip_pat(Id, Skip) of
        false ->
            {Go, PSt1} = DocIdCbk(PSt, Id, Db),
            St1 = St#st{pst = PSt1},
            case Go of
                ok -> scan_doc(FDI, St1);
                skip -> {ok, St1};
                stop -> {stop, St1}
            end;
        true ->
            {ok, St}
    end.

scan_doc(#full_doc_info{} = FDI, #st{} = St) ->
    #st{db = Db, callbacks = Cbks, pst = PSt} = St,
    St1 = rate_limit(St, doc),
    case couch_db:open_doc(Db, FDI, [ejson_body]) of
        {ok, #doc{} = Doc} ->
            #{doc := DocCbk} = Cbks,
            {Go, PSt1} = DocCbk(PSt, Db, Doc),
            case Go of
                ok -> {ok, St1#st{pst = PSt1}};
                stop -> {stop, St1#st{pst = PSt1}}
            end;
        {not_found, _} ->
            {ok, St1}
    end.

maybe_checkpoint(#st{checkpoint_sec = LastCheckpointTSec} = St) ->
    receive
        stop -> exit({shutdown, stop})
    after 0 -> ok
    end,
    erlang:garbage_collect(),
    case tsec() - LastCheckpointTSec > ?CHECKPOINT_INTERVAL_SEC of
        true -> checkpoint(St);
        false -> St
    end.

rate_limit(#st{rlimiter = RLimiter} = St, Type) ->
    {WaitMSec, RLimiter1} = couch_scanner_rate_limiter:update(RLimiter, Type),
    receive
        stop -> exit({shutdown, stop})
    after WaitMSec -> St#st{rlimiter = RLimiter1}
    end.

checkpoint(#st{} = St) ->
    #st{
        id = Id,
        callbacks = Cbks,
        pst = PSt,
        cursor = Cursor,
        start_sec = StartSec,
        scan_id = SId
    } = St,
    EJsonPSt = checkpoint_callback(Cbks, PSt),
    EJson = #{
        <<"cursor">> => Cursor,
        <<"pst">> => EJsonPSt,
        <<"state">> => <<"running">>,
        <<"scan_id">> => SId,
        <<"start_sec">> => StartSec
    },
    ok = couch_scanner_checkpoint:write(Id, EJson),
    St#st{checkpoint_sec = tsec()}.

finalize(#st{} = St) ->
    #st{
        id = Id,
        mod = Mod,
        callbacks = Cbks,
        pst = PSt,
        start_sec = StartSec,
        scan_id = SId
    } = St,
    #{complete := CompleteCbk} = Cbks,
    {ok, #{} = PStEJson} = CompleteCbk(PSt),
    EJson = #{
        <<"cursor">> => <<>>,
        <<"pst">> => couch_scanner_util:ejson_map(PStEJson),
        <<"state">> => <<"finished">>,
        <<"scan_id">> => SId,
        <<"start_sec">> => StartSec
    },
    ok = couch_scanner_checkpoint:write(Id, EJson),
    case schedule_time(Mod, StartSec, tsec()) of
        infinity -> ok;
        TSec when is_integer(TSec) -> exit_resched(TSec)
    end.

exit_resched(reset) ->
    % Reset the checkpoint and restart with a fresh config
    exit({shutdown, reset});
exit_resched(infinity) ->
    exit({shutdown, {reschedule, infinity}});
exit_resched(TimeSec) when is_integer(TimeSec) ->
    exit({shutdown, {reschedule, TimeSec}}).

% Call plugin module API functions

start_callback(Mod, Cbks, Now, ScanId, LastStartSec, #{} = EJson) when
    is_atom(Mod), is_binary(ScanId), is_integer(LastStartSec)
->
    case schedule_time(Mod, LastStartSec, Now) of
        infinity ->
            exit_resched(infinity);
        TSec when is_integer(TSec), TSec =< Now ->
            #{start := StartCbk} = Cbks,
            case StartCbk(ScanId, EJson) of
                {ok, PSt} -> PSt;
                skip -> exit_resched(infinity);
                reset -> exit_resched(reset)
            end;
        TSec when is_integer(TSec), TSec > Now ->
            exit_resched(TSec)
    end.

resume_callback(#{} = Cbks, SId, #{} = EJsonPSt) when is_binary(SId) ->
    #{resume := ResumeCbk} = Cbks,
    case ResumeCbk(SId, EJsonPSt) of
        {ok, PSt} -> PSt;
        skip -> exit_resched(infinity);
        reset -> exit_resched(reset)
    end.

db_opened_callback(#st{pst = PSt, callbacks = Cbks, db = Db} = St) ->
    #{db_opened := DbOpenedCbk} = Cbks,
    {ok, PSt1} = DbOpenedCbk(PSt, Db),
    St#st{pst = PSt1}.

db_closing_callback(#st{pst = PSt, callbacks = Cbks, db = Db} = St) ->
    #{db_closing := DbClosingCbk} = Cbks,
    {ok, PSt1} = DbClosingCbk(PSt, Db),
    St#st{pst = PSt1}.

shards_callback(#st{pst = PSt, callbacks = Cbks} = St, Shards) ->
    #{shards := ShardsCbk} = Cbks,
    {Shards1, PSt1} = ShardsCbk(PSt, Shards),
    {Shards1, St#st{pst = PSt1}}.

start_checkpoint(Id, #{} = Cbks, StartSec, ScanId, Cur, PSt) when
    is_binary(Id), is_binary(ScanId), is_integer(StartSec)
->
    EJsonPSt = checkpoint_callback(Cbks, PSt),
    EJson = #{
        <<"cursor">> => Cur,
        <<"pst">> => EJsonPSt,
        <<"state">> => <<"running">>,
        <<"scan_id">> => ScanId,
        <<"start_sec">> => StartSec
    },
    ok = couch_scanner_checkpoint:write(Id, EJson).

checkpoint_callback(#{} = Cbks, PSt) ->
    #{checkpoint := CheckpointCbk} = Cbks,
    case CheckpointCbk(PSt) of
        {ok, #{} = EJsonPSt} -> couch_scanner_util:ejson_map(EJsonPSt);
        reset -> exit_resched(reset)
    end.

% Plugin discovery, loading and validation

plugin_mod(<<Plugin/binary>>) ->
    Mod = binary_to_atom(Plugin),
    case code:ensure_loaded(Mod) of
        {module, _} ->
            lists:foldl(fun callback_fold/2, {Mod, #{}}, ?CALLBACKS);
        {error, Error} ->
            error({?MODULE, {missing_plugin_module, Mod, Error}})
    end.

callback_fold({F, A, Spec}, {Mod, Acc}) ->
    case is_exported(Mod, F, A) of
        true -> {Mod, Acc#{F => fun Mod:F/A}};
        false -> {Mod, Acc#{F => Spec(Mod, F, A)}}
    end.

required_callback(Mod, F, A) ->
    error({?MODULE, {undefined_plugin_fun, Mod, F, A}}).

default_complete(Mod, _F, _A) when is_atom(Mod) ->
    fun(_St) -> {ok, #{}} end.

default_checkpoint(Mod, _F, _A) when is_atom(Mod) ->
    fun(_St) -> {ok, #{}} end.

default_ddoc(Mod, _F, _A) when is_atom(Mod) ->
    fun(St, _DbName, _Doc) -> {stop, St} end.

default_shards(Mod, _F, _A) when is_atom(Mod) ->
    % If any subsequent callbacks are
    % defined, then the default function is to
    % traverse all shards, otherwise if they are
    % not defined, the default action is to skip
    % opening shard files
    case
        is_exported(Mod, db_opened, 2) orelse
            is_exported(Mod, doc_id, 3) orelse
            is_exported(Mod, doc, 3) orelse
            is_exported(Mod, db_closing, 2)
    of
        true -> fun(St, Shards) -> {Shards, St} end;
        false -> fun(St, _Shards) -> {[], St} end
    end.

default_db_opened(Mod, _F, _A) when is_atom(Mod) ->
    fun(St, _Db) -> {ok, St} end.

default_doc_id(Mod, _F, _A) when is_atom(Mod) ->
    case is_exported(Mod, doc, 3) of
        true -> fun(St, _DocId, _Db) -> {ok, St} end;
        false -> fun(St, _DocId, _Db) -> {skip, St} end
    end.

default_doc(Mod, _F, _A) when is_atom(Mod) ->
    fun(St, _Db, _Doc) -> {ok, St} end.

default_db_closing(Mod, _F, _A) when is_atom(Mod) ->
    fun(St, _Db) -> {ok, St} end.

is_exported(Mod, F, A) ->
    erlang:function_exported(Mod, F, A).

% Shard selection

shards(<<?DESIGN_DOC_PREFIX, _/binary>>, {Props}) when is_list(Props) ->
    % In case the shard map has a design document in it
    [];
shards(DbName, {Props = [_ | _]}) ->
    Shards = lists:sort(mem3_util:build_shards(DbName, Props)),
    Fun = fun({R, SList}) ->
        case mem3_util:rotate_list({DbName, R}, SList) of
            [#shard{node = N} = S | _] when N =:= node() ->
                {true, S};
            [_ | _] ->
                false
        end
    end,
    lists:filtermap(Fun, shards_by_range(lists:sort(Shards))).

shards_by_range(Shards) ->
    Fun = fun(#shard{range = R} = S, Acc) -> orddict:append(R, S, Acc) end,
    Dict = lists:foldl(Fun, orddict:new(), Shards),
    orddict:to_list(Dict).

% Design doc fetching helper

fold_ddocs(Fun, #st{dbname = DbName} = Acc) ->
    QArgs = #mrargs{
        include_docs = true,
        extra = [{namespace, <<"_design">>}]
    },
    try
        {ok, Acc1} = fabric:all_docs(DbName, [?ADMIN_CTX], Fun, Acc, QArgs),
        Acc1
    catch
        error:database_does_not_exist ->
            Acc
    end.

% Simple ejson to #doc{} function to avoid all the extra validation in from_json_obj/1.
% We just got these docs from the cluster, they are already saved on disk.
ejson_to_doc({[_ | _] = Props}) ->
    {value, {_, DocId}, Props1} = lists:keytake(<<"_id">>, 1, Props),
    Props2 = [{K, V} || {K, V} <- Props1, K =:= <<>> orelse binary:first(K) =/= $_],
    #doc{id = DocId, body = {Props2}}.

% Skip patterns

% Build the skip patterns from a config section
config_match_patterns(Module, Type) ->
    Section = atom_to_list(Module) ++ "." ++ Type,
    RegexKVs = config:get(Section),
    Regexes = couch_scanner_util:load_regexes(RegexKVs),
    couch_scanner_util:compile_regexes(Regexes).

% Match skip patterns. If no pattern specified we don't skip
% i.e. skip returns `false`
match_skip_pat(<<_/binary>>, #{} = Pats) when map_size(Pats) == 0 ->
    false;
match_skip_pat(<<_/binary>> = Bin, #{} = Pats) ->
    case couch_scanner_util:match_regexes(Bin, Pats) of
        {match, _} -> true;
        nomatch -> false
    end.

cfg(Mod, Key, Default) when is_list(Key) ->
    Section = atom_to_list(Mod),
    config:get(Section, Key, Default).

schedule_time(Mod, LastSec, NowSec) ->
    After = cfg(Mod, "after", "restart"),
    Repeat = cfg(Mod, "repeat", "restart"),
    Restart = couch_scanner_util:restart_tsec(),
    couch_scanner_util:schedule_time(NowSec, LastSec, Restart, After, Repeat).

tsec() ->
    erlang:system_time(second).
