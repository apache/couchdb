% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under1
% the License.

% Scanner plugin runner process
%
% This is the process which is spawned and run for each enabled plugin.
%
% A number of these processes are managed by the couch_scanner_server via
% start_link/1 and stop/1 functions. After a plugin runner is spawned, the only
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
% The start/1 function is called when the plugin starts running. It returns
% some context (St), which can be any Erlang term. All subsequent function
% calls will be called with the same St object, and may return an updated
% version of it.
%
% If the plugin hasn't finished runing and has resumed running after the node
% was restarted or an error happened, the resume/1 function will be called.
% That's the difference between start and resume: start/1 is called when the
% scan starts from the beginning (first db, first shard, ...), and resume/1 is
% called when the scanning hasn't finished and has to continue.
%
% The checkpoint/1 callback is periodically called to checkpoint the scanning
% progress. start/1 and resume/1 function will be called with the last saved
% checkpoint map value.
%
% The stop/1 callback is called when the scan has finished. The stop callback
% should a final checkpoint map object. The last checkoint will be written and
% then.
%
% As the cluster dbs, shards, ddocs and individual docs are discovered during
% scanning, the appropriate callbacks will be called. Most callbacks, besides
% the updated St object, can reply with ok, skip or stop tags. The meaning of
% those are:
%
%   * ok  - continue to the next object
%
%   * skip - skip the current object and don't scan its internal (ex: skip a db and
%     don't scan its ddocs, but continue with the next db)
%
%   * stop - stop scanning any remaining objects of that type (ex: don't scan any more dbs)
%
% Plugins may use a few helper functions:
%
%   * log/5 - Used with macros from couch_scanner_plugin.hrl for simple logging
%      with a bit of context from running plugin.
%
%   * ejson_map/1 - Normalize ejson objects as maps
%
%   * schedule_time/4 - Given a current time, last start and configured
%     scheduled times, calculate when the next run should happen as an absolute
%     Posix/Unix time in seconds.

-module(couch_scanner_plugin).

-export([
    % Main plugin process API
    spawn_link/1,
    stop/1,
    % Utility functions
    log/5,
    ejson_map/1,
    % Internal export
    run/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

% Behaviour callback definitions

-callback start(ScanId :: binary(), EJson :: #{}) ->
    {ok, St :: term()} | skip.

-callback resume(ScanId :: binary(), EJson :: #{}) ->
    {ok, St :: term()} | skip.

% Optional
-callback stop(St :: term()) ->
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
    stop/1,
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
    {stop, 1, fun default_stop/3},
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
-define(STOP_TIMEOUT_MSEC, 5000).

-define(MINUTE, 60).
-define(HOUR, 3600).
-define(DAY, 24 * 3600).
-define(WEEK, 7 * 24 * 3600).
-define(MONTH, 30 * 24 * 3600).

-record(st, {
    id,
    scan_id,
    mod,
    callbacks = #{},
    pst,
    cursor,
    shards_db,
    db,
    checkpoint_sec = 0,
    start_sec = 0,
    skip_dbs,
    skip_ddocs,
    skip_docs
}).

spawn_link(<<Id/binary>>) ->
    proc_lib:spawn_link(?MODULE, run, [Id]).

stop(Pid) when is_pid(Pid) ->
    unlink(Pid),
    Ref = erlang:monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after ?STOP_TIMEOUT_MSEC ->
        exit(Pid, kill),
        receive
            {'DOWN', Ref, _, _, _} -> ok
        end
    end,
    ok.

% Utilities used by plugins

log(Level, Mod, Fmt, Args, #{} = Meta) when
    is_atom(Level), is_atom(Mod), is_list(Fmt), is_list(Args)
->
    {MetaFmt, MetaArgs} = log_format_meta(Mod, Meta),
    couch_log:Level(lists:flatten([MetaFmt, Fmt]), MetaArgs ++ Args).

ejson_map(Obj) ->
    jiffy:decode(jiffy:encode(Obj), [return_maps]).

% Main run function

run(<<Id/binary>>) ->
    process_flag(priority, low),
    {Mod, Callbacks} = plugin_mod(Id),
    St = #st{id = Id, mod = Mod, callbacks = Callbacks},
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
            <<"pst">> := EJson,
            <<"start_sec">> := StartSec
        } ->
            Now = tsec(),
            PSt1 = resume_callback(Cbks, SId, EJson),
            St#st{
                pst = PSt1,
                cursor = Cur,
                checkpoint_sec = Now,
                start_sec = StartSec,
                scan_id = SId
            };
        not_found ->
            SId = new_scan_id(),
            Now = tsec(),
            LastStartSec = 0,
            Cur = <<>>,
            PSt1 = start_callback(Mod, Cbks, Now, SId, LastStartSec, #{}),
            ok = start_checkpoint(Id, Cbks, Now, SId, Cur, PSt1),
            St#st{
                pst = PSt1,
                cursor = Cur,
                checkpoint_sec = Now,
                start_sec = Now,
                scan_id = SId
            };
        #{
            <<"state">> := <<"finished">>,
            <<"pst">> := EJson,
            <<"start_sec">> := LastStartSec
        } ->
            SId = new_scan_id(),
            Now = tsec(),
            Cur = <<>>,
            PSt1 = start_callback(Mod, Cbks, Now, SId, LastStartSec, EJson),
            ok = start_checkpoint(Id, Cbks, Now, SId, Cur, PSt1),
            St#st{
                pst = PSt1,
                cursor = Cur,
                checkpoint_sec = Now,
                start_sec = Now,
                scan_id = SId
            }
    end.

new_scan_id() ->
    TSec = integer_to_binary(erlang:system_time(second)),
    Rand = string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(6))),
    <<TSec/binary, "-", Rand/binary>>.

scan_dbs(#st{cursor = Cursor} = St) ->
    DbsDbName = mem3_sync:shards_db(),
    ioq:set_io_priority({system, DbsDbName}),
    {ok, Db} = mem3_util:ensure_exists(DbsDbName),
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
        {ok, #doc{id = <<"_design/", _/binary>>}} ->
            {ok, Acc2};
        {ok, #doc{id = DbName, body = Body}} ->
            scan_db(shards(DbName, Body), Acc2);
        {not_found, _} ->
            {ok, Acc2}
    end.

scan_db([], #st{} = St) ->
    {ok, St};
scan_db([_ | _] = Shards, #st{} = St) ->
    #st{cursor = DbName, callbacks = Cbks, pst = PSt, skip_dbs = Skip} = St,
    #{db := DbCbk} = Cbks,
    case match_skip_pat(DbName, Skip) of
        false ->
            {Go, PSt1} = DbCbk(PSt, DbName),
            St1 = St#st{pst = PSt1},
            case Go of
                ok ->
                    St2 = fold_ddocs(fun scan_ddocs_fold/2, St1),
                    {Shards1, St3} = shards_callback(St2, Shards),
                    St4 = scan_shards(Shards1, St3),
                    {ok, St4};
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
    scan_ddoc(couch_doc:from_json_obj(DDoc), Acc);
scan_ddocs_fold(complete, #st{} = Acc) ->
    {ok, Acc};
scan_ddocs_fold({error, Error}, _Acc) ->
    exit({shutdown, {scan_ddocs_fold, Error}}).

scan_shards([], #st{} = St) ->
    St;
scan_shards([#shard{} = Shard | Rest], #st{} = St) ->
    St1 = maybe_stop(St),
    St2 = scan_docs(St1, Shard),
    scan_shards(Rest, St2).

scan_ddoc(#doc{id = DDocId} = DDoc, #st{} = St) ->
    #st{cursor = DbName, callbacks = Cbks, pst = PSt, skip_ddocs = Skip} = St,
    #{ddoc := DDocCbk} = Cbks,
    case match_skip_pat(DDocId, Skip) of
        false ->
            {Go, PSt1} = DDocCbk(PSt, DbName, DDoc),
            St1 = St#st{pst = PSt1},
            case Go of
                ok -> {ok, St1};
                skip -> {ok, St1};
                stop -> {stop, St1}
            end;
        true ->
            {ok, St}
    end.

scan_docs(#st{} = St, #shard{name = ShardDbName}) ->
    case couch_db:open_int(ShardDbName, [?ADMIN_CTX]) of
        {ok, Db} ->
            St1 = St#st{db = Db},
            St2 = db_opened_callback(St1),
            {ok, St3} = couch_db:fold_docs(Db, fun scan_docs_fold/2, St2, []),
            St4 = db_closing_callback(St3),
            couch_db:close(Db),
            erlang:garbage_collect(),
            St4#st{db = undefined};
        {not_found, _} ->
            St
    end.

scan_docs_fold(#full_doc_info{id = Id} = FDI, #st{} = St) ->
    #st{db = Db, callbacks = Cbks, pst = PSt, skip_dbs = Skip} = St,
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
    St1 = maybe_stop(St),
    #st{db = Db, callbacks = Cbks, pst = PSt} = St1,
    #{doc := DocCbk} = Cbks,
    {ok, #doc{} = Doc} = couch_db:open_doc(Db, FDI, [ejson_body]),
    {Go, PSt1} = DocCbk(PSt, Db, Doc),
    case Go of
        ok -> {ok, St1#st{pst = PSt1}};
        stop -> {stop, St1#st{pst = PSt1}}
    end.

maybe_checkpoint(#st{checkpoint_sec = LastCheckpointTSec} = St) ->
    St1 = maybe_stop(St),
    erlang:garbage_collect(),
    case tsec() - LastCheckpointTSec > ?CHECKPOINT_INTERVAL_SEC of
        true -> checkpoint(St1);
        false -> St1
    end.

maybe_stop(#st{} = St) ->
    receive
        stop -> exit({shutdown, stop})
    after 0 -> St
    end.

checkpoint(#st{} = St) ->
    #st{
        id = Id,
        mod = Mod,
        pst = PSt,
        cursor = Cursor,
        start_sec = StartSec,
        scan_id = SId
    } = St,
    JsonPSt = checkpoint_callback(Mod, PSt),
    EJson = #{
        <<"cursor">> => Cursor,
        <<"pst">> => JsonPSt,
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
    #{stop := StopCbk} = Cbks,
    {ok, #{} = PStEJson} = StopCbk(PSt),
    EJson = #{
        <<"cursor">> => <<>>,
        <<"pst">> => ejson_map(PStEJson),
        <<"state">> => <<"finished">>,
        <<"scan_id">> => SId,
        <<"start_sec">> => StartSec
    },
    ok = couch_scanner_checkpoint:write(Id, EJson),
    case schedule_time(Mod, StartSec, tsec()) of
        infinity -> ok;
        TSec when is_integer(TSec) -> exit_resched(TSec)
    end.

exit_resched(TimeSec) when is_integer(TimeSec) orelse TimeSec == infinity ->
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
                skip -> exit_resched(infinity)
            end;
        TSec when is_integer(TSec), TSec > Now ->
            exit_resched(TSec)
    end.

resume_callback(#{} = Cbks, ScanId, #{} = EJson) when
    is_binary(ScanId)
->
    #{resume := ResumeCbk} = Cbks,
    case ResumeCbk(ScanId, EJson) of
        {ok, PSt} -> PSt;
        skip -> exit_resched(infinity)
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

start_checkpoint(Id, #{} = Cbks, StartSec, ScanId, Cur, PSt1) when
    is_binary(Id), is_binary(ScanId), is_integer(StartSec)
->
    PStEJson = checkpoint_callback(Cbks, PSt1),
    EJson = #{
        <<"cursor">> => Cur,
        <<"pst">> => PStEJson,
        <<"state">> => <<"running">>,
        <<"scan_id">> => ScanId,
        <<"start_sec">> => StartSec
    },
    ok = couch_scanner_checkpoint:write(Id, EJson).

checkpoint_callback(#{} = Cbks, PSt) ->
    #{checkpoint := CheckpointCbk} = Cbks,
    {ok, #{} = PStEJson} = CheckpointCbk(PSt),
    ejson_map(PStEJson).

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

default_stop(Mod, _F, _A) when is_atom(Mod) ->
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

fold_ddocs(Fun, #st{cursor = DbName} = Acc) ->
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

% Skip patterns

% Build the skip patterns from a config section
config_match_patterns(Module, Type) ->
    Section = atom_to_list(Module) ++ "." ++ Type,
    Fun = fun
        ({K, "true"}, Acc) -> [list_to_binary(K) | Acc];
        ({_, _}, Acc) -> Acc
    end,
    Items = lists:foldl(Fun, [], config:get(Section)),
    case Items of
        [] -> undefined;
        [<<_/binary>> | _] -> binary:compile_pattern(Items)
    end.

% Match skip patterns. If no pattern specified we don't skip
% i.e. skip returns `false`
match_skip_pat(<<_/binary>>, undefined) ->
    false;
match_skip_pat(<<_/binary>> = Bin, Pat) ->
    binary:match(Bin, Pat) /= nomatch.

% Time parsing functions

cfg(Mod, Key, Default) when is_list(Key) ->
    Section = atom_to_list(Mod),
    config:get(Section, Key, Default).

schedule_time(Mod, LastSec, NowSec) ->
    After = cfg(Mod, "after", "restart"),
    Repeat = cfg(Mod, "repeat", "restart"),
    schedule_time(NowSec, LastSec, After, Repeat).

schedule_time(Now, Last, AfterCfg, RepeatCfg) when
    is_integer(Now), is_integer(Last), is_list(AfterCfg), is_list(RepeatCfg)
->
    RepeatPeriod = repeat_period(Now, Last, parse_repeat(RepeatCfg)),
    Restart = restart_tsec(),
    case {parse_after(AfterCfg), RepeatPeriod} of
        {undefined, undefined} when Last >= Restart ->
            % Run after restart, and already ran
            infinity;
        {undefined, undefined} when Last < Restart ->
            % Run after restart, but haven't run yet
            Now;
        {After, undefined} when is_integer(After), Last > After ->
            % Run after an absolute timestamp, and already ran
            infinity;
        {After, undefined} when is_integer(After), Last =< After ->
            % Run once, haven't run yet, schedule to run
            max(Now, After);
        {undefined, Period} ->
            % No after time, just period. Either need to wait
            % since last time it ran, or is actually ready to run
            max(Now, Last + Period);
        {After, Period} ->
            % Both after time set and a period. Wait for whichever
            % takes the longest
            lists:max([Now, After, Last + Period])
    end.

tsec() ->
    erlang:system_time(second).

parse_after(Time) when is_list(Time), length(Time) >= 7 ->
    case string:uppercase(Time) of
        "RESTART" ->
            undefined;
        [_, _, _, _, $-, _, _, $-, _, _] = T ->
            parse_rfc3339(T ++ "T00:00:00Z");
        [_, _, _, _, $-, _, _, $-, _, _, $T, _, _] = T ->
            parse_rfc3339(T ++ ":00:00Z");
        [_, _, _, _, $-, _, _, $-, _, _, $T, _, _, $:, _, _] = T ->
            parse_rfc3339(T ++ ":00Z");
        [_, _, _, _, $-, _, _, $-, _, _, $T, _, _, $:, _, _, $:, _, _] = T ->
            parse_rfc3339(T ++ "Z");
        [_, _, _, _, $-, _, _, $-, _, _, $T, _, _, $:, _, _, $:, _, _ | _] = T ->
            parse_rfc3339(T);
        [_ | _] = T ->
            parse_unix(T)
    end.

parse_rfc3339(Time) ->
    try calendar:rfc3339_to_system_time(Time) of
        Sec when is_integer(Sec), Sec > 0 ->
            Sec;
        Sec when is_integer(Sec) ->
            undefined
    catch
        error:_ ->
            undefined
    end.

parse_unix(Time) ->
    try list_to_integer(Time) of
        Sec when is_integer(Sec), Sec > 0 ->
            Sec;
        Sec when is_integer(Sec) ->
            undefined
    catch
        error:badarg ->
            undefined
    end.

repeat_period(_Now, _Last, undefined) ->
    undefined;
repeat_period(Now, Last, {weekday, WeekdayNum}) ->
    {NowDate, DayTime} = calendar:system_time_to_universal_time(Now, second),
    case abs(calendar:day_of_the_week(NowDate) - WeekdayNum) rem 7 of
        0 ->
            % It's today. Run only if it hasn't started yet
            DayStartSec = calendar:datetime_to_gregorian_seconds({{0, 0, 0}, DayTime}),
            DayStartUnixTSec = Now - DayStartSec,
            case Last > DayStartUnixTSec of
                true -> ?WEEK;
                false -> 0
            end;
        Days ->
            ?DAY * Days
    end;
repeat_period(_Now, _Last, Period) when is_integer(Period), Period > 0 ->
    Period.

parse_repeat(Repeat) when is_list(Repeat) ->
    % Numbering follows https://www.erlang.org/doc/man/calendar#day_of_the_week-1
    case string:lowercase(Repeat) of
        "restart" -> undefined;
        "mon" ++ _ -> {weekday, 1};
        "tue" ++ _ -> {weekday, 2};
        "wed" ++ _ -> {weekday, 3};
        "thu" ++ _ -> {weekday, 4};
        "fri" ++ _ -> {weekday, 5};
        "sat" ++ _ -> {weekday, 6};
        "sun" ++ _ -> {weekday, 7};
        Val -> parse_non_weekday_period(Val)
    end.

parse_non_weekday_period(Period) ->
    case string:split(Period, "_") of
        [UnitStr] ->
            % "hour" -> ?HOUR
            parse_period_unit(UnitStr);
        [NumStr, UnitStr] ->
            Unit = parse_period_unit(UnitStr),
            % 10_hours -> ["10", "hours"] -> 10 * ?HOUR
            try list_to_integer(NumStr) of
                Num when is_integer(Num), Num > 0 ->
                    Num * Unit;
                Num when is_integer(Num) ->
                    undefined
            catch
                error:badarg ->
                    undefined
            end
    end.

parse_period_unit(Period) when is_list(Period) ->
    case Period of
        "min" ++ _ -> ?MINUTE;
        "hour" ++ _ -> ?HOUR;
        "day" ++ _ -> ?DAY;
        "week" ++ _ -> ?WEEK;
        "month" ++ _ -> ?MONTH;
        _ -> 1
    end.

restart_tsec() ->
    Native = erlang:system_info(start_time) + erlang:time_offset(),
    erlang:convert_time_unit(Native, native, second).

% Logging bits

log_format_meta(Mod, #{} = Meta) ->
    SId = {"s:~s ", maps:get(sid, Meta, undefined)},
    Fun = {"f:~s ", maps:get(fn, Meta, undefined)},
    Db = {"db:~s ", format_db(maps:get(db, Meta, undefined))},
    DDocId = {"ddoc:~s ", maps:get(ddoc, Meta, undefined)},
    DocId = {"doc:~s ", maps:get(doc, Meta, undefined)},
    FmtArgs = [{"~s ", Mod}, SId, Fun, Db, DDocId, DocId],
    lists:unzip([{Fmt, Arg} || {Fmt, Arg} <- FmtArgs, Arg /= undefined]).

format_db(undefined) ->
    undefined;
format_db(Db) when is_list(Db) ->
    format_db(list_to_binary(Db));
format_db(#shard{dbname = Db, range = [B, E]}) ->
    {BStr, EStr} = {hex(B), hex(E)},
    <<Db/binary, "/", BStr/binary, "-", EStr/binary>>;
format_db(Db) when is_tuple(Db) ->
    format_db(couch_db:name(Db));
format_db(<<"shards/", B:8/binary, "-", E:8/binary, "/", Rest/binary>>) ->
    [Db, _] = binary:split(Rest, <<".">>),
    <<Db/binary, "/", B/binary, "-", E/binary>>;
format_db(<<Db/binary>>) ->
    Db.

hex(Val) when is_integer(Val) ->
    string:lowercase(erlang:integer_to_binary(Val, 16)).
