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

-module(couch_replicator_doc_processor).

-behaviour(gen_server).
-behaviour(couch_multidb_changes).

-export([
    start_link/0
]).

-export([
    init/1,
    handle_call/3,
    handle_info/2,
    handle_cast/2
]).

-export([
    db_deleted/2,
    db_found/2,
    db_change/3
]).

-export([
    docs/0,
    docs/1,
    doc/2,
    doc_lookup/3,
    update_docs/0,
    get_worker_ref/1
]).

-include("couch_replicator.hrl").

-import(couch_replicator_utils, [
    get_json_value/2,
    get_json_value/3
]).

-define(DEFAULT_UPDATE_DOCS, false).
% ~ 1 day on average
-define(ERROR_MAX_BACKOFF_EXPONENT, 12).
-define(TS_DAY_SEC, 86400).
-define(INITIAL_BACKOFF_EXPONENT, 64).
-define(MIN_FILTER_DELAY_SEC, 60).
-define(DEFAULT_START_DELAY_MSEC, 5000).
-define(MIN_START_DELAY_MSEC, 500).

-type filter_type() :: nil | view | user | docids | mango.
-type repstate() :: initializing | error | scheduled | not_owner.

-record(st, {
    % Timer reference
    tref :: reference(),
    mdb_changes_pid :: pid()
}).

-record(rdoc, {
    id :: db_doc_id() | '_' | {any(), '_'},
    state :: repstate() | '_',
    rep :: #rep{} | '_',
    rid :: rep_id() | nil | '_',
    filter :: filter_type() | '_',
    info :: binary() | nil | '_',
    errcnt :: non_neg_integer() | '_',
    worker :: reference() | nil | '_',
    last_updated :: erlang:timestamp() | '_'
}).

% couch_multidb_changes API callbacks

db_deleted(DbName, Server) ->
    couch_stats:increment_counter([couch_replicator, docs, dbs_deleted]),
    ok = gen_server:call(?MODULE, {clean_up_replications, DbName}, infinity),
    Server.

db_found(DbName, Server) ->
    couch_stats:increment_counter([couch_replicator, docs, dbs_found]),
    couch_replicator_docs:delete_old_rep_ddoc(DbName),
    Server.

db_change(DbName, {ChangeProps} = Change, Server) ->
    couch_stats:increment_counter([couch_replicator, docs, db_changes]),
    try
        ok = process_change(DbName, Change)
    catch
        throw:{bad_rep_doc, Error} ->
            {RepProps} = get_json_value(doc, ChangeProps),
            DocId = get_json_value(<<"_id">>, RepProps),
            couch_replicator_docs:update_failed(DbName, DocId, Error)
    end,
    Server.

-spec get_worker_ref(db_doc_id()) -> reference() | nil.
get_worker_ref({DbName, DocId}) when is_binary(DbName), is_binary(DocId) ->
    case ets:lookup(?MODULE, {DbName, DocId}) of
        [#rdoc{worker = WRef}] when is_reference(WRef) ->
            WRef;
        [#rdoc{worker = nil}] ->
            nil;
        [] ->
            nil
    end.

process_change(DbName, {Change}) ->
    {RepProps} = JsonRepDoc = get_json_value(doc, Change),
    DocId = get_json_value(<<"_id">>, RepProps),
    Owner = couch_replicator_utils:owner(DbName, DocId),
    Id = {DbName, DocId},
    case {Owner, get_json_value(deleted, Change, false)} of
        {_, true} ->
            ok = gen_server:call(?MODULE, {removed, Id}, infinity);
        {Node, false} when Node =:= node() ->
            case get_json_value(<<"_replication_state">>, RepProps) of
                undefined ->
                    ok = process_updated(Id, JsonRepDoc, true);
                <<"triggered">> ->
                    maybe_remove_state_fields(DbName, DocId),
                    ok = process_updated(Id, JsonRepDoc, true);
                <<"completed">> ->
                    ok = gen_server:call(?MODULE, {completed, Id}, infinity);
                <<"error">> ->
                    % Handle replications started from older versions of replicator
                    % which wrote transient errors to replication docs
                    maybe_remove_state_fields(DbName, DocId),
                    ok = process_updated(Id, JsonRepDoc, true);
                <<"failed">> ->
                    ok
            end;
        {Node, false} when Node =/= node() ->
            % When we're not the owner node, do not update the doc fields
            case get_json_value(<<"_replication_state">>, RepProps) of
                undefined ->
                    ok = process_updated(Id, JsonRepDoc, false);
                <<"triggered">> ->
                    ok = process_updated(Id, JsonRepDoc, false);
                <<"completed">> ->
                    ok = gen_server:call(?MODULE, {completed, Id}, infinity);
                <<"error">> ->
                    ok = process_updated(Id, JsonRepDoc, false);
                <<"failed">> ->
                    ok
            end
    end,
    ok.

maybe_remove_state_fields(DbName, DocId) ->
    case update_docs() of
        true ->
            ok;
        false ->
            couch_replicator_docs:remove_state_fields(DbName, DocId)
    end.

process_updated({DbName, _DocId} = Id, JsonRepDoc, Owner = true) ->
    % Parsing replication doc (but not calculating the id) could throw an
    % exception which would indicate this document is malformed. This exception
    % should propagate to db_change function and will be recorded as permanent
    % failure in the document. User will have to update the documet to fix the
    % problem.
    Rep0 = couch_replicator_parse:parse_rep_doc_without_id(JsonRepDoc),
    Rep = Rep0#rep{db_name = DbName, start_time = os:timestamp()},
    ok = couch_replicator_utils:valid_endpoint_protocols_log(Rep),
    Filter = parse_filter_type(Rep),
    gen_server:call(?MODULE, {updated, Id, Rep, Filter, Owner}, infinity);
process_updated({DbName, DocId} = Id, JsonRepDoc, Owner = false) ->
    % We're not the owner. We'll keep the replication record updated
    % in case we have to take over. Since we're not the owner, we avoid
    % updating the doc with failures if we can't parse it.
    try
        Rep0 = couch_replicator_parse:parse_rep_doc_without_id(JsonRepDoc),
        Rep = Rep0#rep{db_name = DbName, start_time = os:timestamp()},
        Filter = parse_filter_type(Rep),
        gen_server:call(?MODULE, {updated, Id, Rep, Filter, Owner}, infinity)
    catch
        Tag:Err ->
            LogMsg = "~p : Failure parsing replication doc ~s:~s as non-owner ~p:~p",
            couch_log:warning(LogMsg, [?MODULE, DbName, DocId, Tag, Err]),
            ok
    end.

parse_filter_type(#rep{} = Rep) ->
    case couch_replicator_filters:parse(Rep#rep.options) of
        {ok, nil} ->
            nil;
        {ok, {user, _FName, _QP}} ->
            user;
        {ok, {view, _FName, _QP}} ->
            view;
        {ok, {docids, _DocIds}} ->
            docids;
        {ok, {mango, _Selector}} ->
            mango;
        {error, FilterError} ->
            throw(FilterError)
    end.

% Doc processor gen_server API and callbacks

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?MODULE = ets:new(?MODULE, [
        named_table,
        {keypos, #rdoc.id},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    erlang:send_after(start_delay_msec(), self(), delayed_init),
    {ok, schedule_membership_timer(#st{})}.

handle_call({updated, Id, Rep, Filter, Owner}, _From, #st{} = St) ->
    ok = updated_doc(Id, Rep, Filter, Owner),
    {reply, ok, St};
handle_call({removed, Id}, _From, #st{} = St) ->
    ok = removed_doc(Id),
    {reply, ok, St};
handle_call({completed, Id}, _From, #st{} = St) ->
    true = ets:delete(?MODULE, Id),
    {reply, ok, St};
handle_call({clean_up_replications, DbName}, _From, #st{} = St) ->
    ok = removed_db(DbName),
    {reply, ok, St};
handle_call(get_docs, _From, #st{} = St) ->
    {reply, ets:foldl(fun docs_foldl/2, [], ?MODULE), St}.

handle_cast(Msg, #st{} = St) ->
    {stop, {error, unexpected_message, Msg}, St}.

handle_info(delayed_init, #st{} = St) ->
    {noreply, start_scanner(St)};
handle_info({'DOWN', _, _, _, #doc_worker_result{} = WRes}, #st{} = St) ->
    #doc_worker_result{id = Id, wref = Ref, result = Res} = WRes,
    ok = worker_returned(Ref, Id, Res),
    {noreply, St};
handle_info(check_membership, #st{} = St) ->
    St1 = cancel_membership_timer(St),
    nil = ets:foldl(fun cluster_membership_foldl/2, nil, ?MODULE),
    {noreply, schedule_membership_timer(St1)};
handle_info(_Msg, #st{} = St) ->
    {noreply, St}.

% Doc processor gen_server private helper functions

% Handle doc update as owner -- add to ets, then start a worker to try to turn
% it into a replication job. In most cases it will succeed quickly but for
% filtered replications or if there are duplicates, it could take longer
% (theoretically indefinitely) until a replication could be started. Before
% adding replication job, make sure to delete all old jobs associated with same
% document.
-spec updated_doc(db_doc_id(), #rep{}, filter_type(), boolean()) -> ok.
updated_doc(Id, #rep{} = Rep, Filter, Owner) ->
    NormCurRep = couch_replicator_utils:normalize_rep(current_rep(Id)),
    NormNewRep = couch_replicator_utils:normalize_rep(Rep),
    case NormCurRep == NormNewRep of
        false ->
            removed_doc(Id),
            State =
                case Owner of
                    true -> initializing;
                    false -> not_owner
                end,
            Row = #rdoc{
                id = Id,
                state = State,
                rep = Rep,
                rid = nil,
                filter = Filter,
                info = nil,
                errcnt = 0,
                worker = nil,
                last_updated = os:timestamp()
            },
            true = ets:insert(?MODULE, Row),
            ok = maybe_start_worker(Id);
        true ->
            ok
    end.

% Return current #rep{} record if any. If replication hasn't been submitted
% to the scheduler yet, #rep{} record will be in the document processor's
% ETS table, otherwise query scheduler for the #rep{} record.
-spec current_rep({binary(), binary()}) -> #rep{} | nil.
current_rep({DbName, DocId}) when is_binary(DbName), is_binary(DocId) ->
    case ets:lookup(?MODULE, {DbName, DocId}) of
        [] ->
            nil;
        [#rdoc{state = scheduled, rep = Rep, rid = JobId}] ->
            % Try to get the rep record from the scheduler first
            case couch_replicator_scheduler:rep_state(JobId) of
                #rep{} = SchedRep -> SchedRep;
                nil -> Rep
            end;
        [#rdoc{rep = Rep}] ->
            Rep
    end.

-spec worker_returned(reference(), db_doc_id(), rep_start_result()) -> ok.
worker_returned(Ref, Id, {ok, RepId}) ->
    case ets:lookup(?MODULE, Id) of
        [#rdoc{state = not_owner}] ->
            % Was usurped as it was just starting, ensure to to stop it.
            ok = couch_replicator_scheduler:remove_job(RepId);
        [#rdoc{worker = Ref} = Row] ->
            Row0 = Row#rdoc{
                state = scheduled,
                errcnt = 0,
                worker = nil,
                last_updated = os:timestamp()
            },
            NewRow =
                case Row0 of
                    #rdoc{rid = RepId, filter = user} ->
                        % Filtered replication id didn't change.
                        Row0;
                    #rdoc{rid = nil, filter = user} ->
                        % Calculated new replication id for a filtered replication. Make
                        % sure to schedule another check as filter code could change.
                        % Replication starts could have been failing, so also clear
                        % error count.
                        Row0#rdoc{rid = RepId};
                    #rdoc{rid = OldRepId, filter = user} ->
                        % Replication id of existing replication job with filter has
                        % changed. Remove old replication job from scheduler and
                        % schedule check to check for future changes.
                        ok = couch_replicator_scheduler:remove_job(OldRepId),
                        Msg = io_lib:format("Replication id changed: ~p -> ~p", [
                            OldRepId, RepId
                        ]),
                        Row0#rdoc{rid = RepId, info = couch_util:to_binary(Msg)};
                    #rdoc{rid = nil} ->
                        % Calculated new replication id for non-filtered replication.
                        Row0#rdoc{rid = RepId, info = nil}
                end,
            true = ets:insert(?MODULE, NewRow),
            ok = maybe_update_doc_triggered(Row#rdoc.rep, RepId),
            ok = maybe_start_worker(Id);
        _ ->
            % doc could have been deleted, ignore
            ok
    end,
    ok;
worker_returned(_Ref, _Id, ignore) ->
    ok;
worker_returned(Ref, Id, {temporary_error, Reason}) ->
    case ets:lookup(?MODULE, Id) of
        [#rdoc{worker = Ref, errcnt = ErrCnt} = Row] ->
            NewRow = Row#rdoc{
                rid = nil,
                state = error,
                info = Reason,
                errcnt = ErrCnt + 1,
                worker = nil,
                last_updated = os:timestamp()
            },
            true = ets:insert(?MODULE, NewRow),
            ok = maybe_update_doc_error(NewRow#rdoc.rep, Reason),
            ok = maybe_start_worker(Id);
        _ ->
            % doc could have been deleted, ignore
            ok
    end,
    ok;
worker_returned(Ref, Id, {permanent_failure, _Reason}) ->
    case ets:lookup(?MODULE, Id) of
        [#rdoc{worker = Ref}] ->
            true = ets:delete(?MODULE, Id);
        _ ->
            % doc could have been deleted, ignore
            ok
    end,
    ok.

-spec maybe_update_doc_error(#rep{}, any()) -> ok.
maybe_update_doc_error(Rep, Reason) ->
    case update_docs() of
        true ->
            couch_replicator_docs:update_error(Rep, Reason);
        false ->
            ok
    end.

-spec maybe_update_doc_triggered(#rep{}, rep_id()) -> ok.
maybe_update_doc_triggered(Rep, RepId) ->
    case update_docs() of
        true ->
            couch_replicator_docs:update_triggered(Rep, RepId);
        false ->
            ok
    end.

-spec error_backoff(non_neg_integer()) -> seconds().
error_backoff(ErrCnt) ->
    Exp = min(ErrCnt, ?ERROR_MAX_BACKOFF_EXPONENT),
    % ErrCnt is the exponent here. The reason 64 is used is to start at
    % 64 (about a minute) max range. Then first backoff would be 30 sec
    % on average. Then 1 minute and so on.
    rand:uniform(?INITIAL_BACKOFF_EXPONENT bsl Exp).

-spec filter_backoff() -> seconds().
filter_backoff() ->
    Total = ets:info(?MODULE, size),
    % This value scaled by the number of replications. If the are a lot of them
    % wait is longer, but not more than a day (?TS_DAY_SEC). If there are just
    % few, wait is shorter, starting at about 30 seconds. `2 *` is used since
    % the expected wait would then be 0.5 * Range so it is easier to see the
    % average wait. `1 +` is used because rand:uniform only
    % accepts >= 1 values and crashes otherwise.
    Range = 1 + min(2 * (Total / 10), ?TS_DAY_SEC),
    ?MIN_FILTER_DELAY_SEC + rand:uniform(round(Range)).

% Replication was usurped - move it to the not_owner state
% and try to kill running jobs if there are any.
stop_usurped(#rdoc{id = {DbName, DocId}} = RDoc) ->
    RDoc1 = RDoc#rdoc{
        state = not_owner,
        rid = nil,
        errcnt = 0,
        worker = nil,
        last_updated = os:timestamp()
    },
    true = ets:insert(?MODULE, RDoc1),
    RepIds = couch_replicator_scheduler:find_jobs_by_doc(DbName, DocId),
    lists:foreach(fun couch_replicator_scheduler:remove_job/1, RepIds).

% We are the new owner (i.e. the usurper) of this replication.
start_as_usurper(#rdoc{id = Id, rep = #rep{} = Rep} = RDoc) ->
    Wait = get_worker_wait(RDoc),
    Ref = make_ref(),
    RDoc1 = RDoc#rdoc{
        state = initializing,
        rid = nil,
        info = nil,
        errcnt = 0,
        worker = Ref,
        last_updated = os:timestamp()
    },
    true = ets:insert(?MODULE, RDoc1),
    couch_replicator_doc_processor_worker:spawn_worker(Id, Rep, Wait, Ref),
    ok.

% Document removed from db -- clear ets table and remove all scheduled jobs
-spec removed_doc(db_doc_id()) -> ok.
removed_doc({DbName, DocId} = Id) ->
    ets:delete(?MODULE, Id),
    RepIds = couch_replicator_scheduler:find_jobs_by_doc(DbName, DocId),
    lists:foreach(fun couch_replicator_scheduler:remove_job/1, RepIds).

% Whole db shard is gone -- remove all its ets rows and stop jobs
-spec removed_db(binary()) -> ok.
removed_db(DbName) ->
    EtsPat = #rdoc{id = {DbName, '_'}, _ = '_'},
    ets:match_delete(?MODULE, EtsPat),
    RepIds = couch_replicator_scheduler:find_jobs_by_dbname(DbName),
    lists:foreach(fun couch_replicator_scheduler:remove_job/1, RepIds).

% Spawn a worker process which will attempt to calculate a replication id, then
% start a replication. Returns a process monitor reference. The worker is
% guaranteed to exit with rep_start_result() type only.
-spec maybe_start_worker(db_doc_id()) -> ok.
maybe_start_worker(Id) ->
    case ets:lookup(?MODULE, Id) of
        [] ->
            ok;
        [#rdoc{state = not_owner}] ->
            ok;
        [#rdoc{state = scheduled, filter = Filter}] when Filter =/= user ->
            ok;
        [#rdoc{rep = #rep{} = Rep} = Doc] ->
            % For any replication with a user created filter function, periodically
            % (every `filter_backoff/0` seconds) to try to see if the user filter
            % has changed by using a worker to check for changes. When the worker
            % returns check if replication ID has changed. If it hasn't keep
            % checking (spawn another worker and so on). If it has stop the job
            % with the old ID and continue checking.
            Wait = get_worker_wait(Doc),
            Ref = make_ref(),
            true = ets:insert(?MODULE, Doc#rdoc{worker = Ref}),
            couch_replicator_doc_processor_worker:spawn_worker(Id, Rep, Wait, Ref),
            ok
    end.

-spec get_worker_wait(#rdoc{}) -> seconds().
get_worker_wait(#rdoc{state = scheduled, filter = user}) ->
    filter_backoff();
get_worker_wait(#rdoc{state = error, errcnt = ErrCnt}) ->
    error_backoff(ErrCnt);
get_worker_wait(#rdoc{state = initializing}) ->
    0;
get_worker_wait(#rdoc{state = not_owner}) ->
    % Wait a whole interval before starting usurper to give a chance to all the
    % usurped jobs to stop, to avoid having dueling duplicate jobs errors.
    couch_replicator_scheduler:get_interval_msec() div 1000.

-spec update_docs() -> boolean().
update_docs() ->
    config:get_boolean("replicator", "update_docs", ?DEFAULT_UPDATE_DOCS).

% Debug/test function to get a summary of all the docs and
% their states as a table.
docs() ->
    lists:sort(gen_server:call(?MODULE, get_docs, infinity)).

% _scheduler/docs HTTP endpoint helpers

-spec docs([atom()]) -> [{[_]}] | [].
docs(States) ->
    HealthThreshold = couch_replicator_scheduler:health_threshold(),
    ets:foldl(
        fun(RDoc, Acc) ->
            case ejson_doc(RDoc, HealthThreshold) of
                nil ->
                    % Could have been deleted if job just completed
                    Acc;
                {Props} = EJson ->
                    {state, DocState} = lists:keyfind(state, 1, Props),
                    case ejson_doc_state_filter(DocState, States) of
                        true ->
                            [EJson | Acc];
                        false ->
                            Acc
                    end
            end
        end,
        [],
        ?MODULE
    ).

-spec doc(binary(), binary()) -> {ok, {[_]}} | {error, not_found}.
doc(Db, DocId) ->
    HealthThreshold = couch_replicator_scheduler:health_threshold(),
    Res =
        (catch ets:foldl(
            fun(RDoc, nil) ->
                {Shard, RDocId} = RDoc#rdoc.id,
                case {mem3:dbname(Shard), RDocId} of
                    {Db, DocId} when RDoc#rdoc.state =/= not_owner ->
                        throw({found, ejson_doc(RDoc, HealthThreshold)});
                    {_OtherDb, _OtherDocId} ->
                        nil
                end
            end,
            nil,
            ?MODULE
        )),
    case Res of
        {found, DocInfo} ->
            {ok, DocInfo};
        nil ->
            {error, not_found}
    end.

-spec doc_lookup(binary(), binary(), integer()) ->
    {ok, {[_]}} | {error, not_found}.
doc_lookup(Db, DocId, HealthThreshold) ->
    case ets:lookup(?MODULE, {Db, DocId}) of
        [#rdoc{} = RDoc] ->
            {ok, ejson_doc(RDoc, HealthThreshold)};
        [] ->
            {error, not_found}
    end.

-spec ejson_rep_id(rep_id() | nil) -> binary() | null.
ejson_rep_id(nil) ->
    null;
ejson_rep_id({BaseId, Ext}) ->
    iolist_to_binary([BaseId, Ext]).

-spec ejson_doc(#rdoc{}, non_neg_integer()) -> {[_]} | nil.
ejson_doc(#rdoc{state = scheduled} = RDoc, HealthThreshold) ->
    #rdoc{id = {DbName, DocId}, rid = RepId} = RDoc,
    JobProps = couch_replicator_scheduler:job_summary(RepId, HealthThreshold),
    case JobProps of
        nil ->
            nil;
        [{_, _} | _] ->
            {[
                {doc_id, DocId},
                {database, DbName},
                {id, ejson_rep_id(RepId)},
                {node, node()}
                | JobProps
            ]}
    end;
ejson_doc(#rdoc{state = not_owner}, _HealthThreshold) ->
    % Only let the owner respond
    nil;
ejson_doc(#rdoc{state = RepState} = RDoc, _HealthThreshold) ->
    #rdoc{
        id = {DbName, DocId},
        info = StateInfo,
        rid = RepId,
        errcnt = ErrorCount,
        last_updated = StateTime,
        rep = Rep
    } = RDoc,
    {[
        {doc_id, DocId},
        {database, DbName},
        {id, ejson_rep_id(RepId)},
        {state, RepState},
        {info, couch_replicator_utils:ejson_state_info(StateInfo)},
        {error_count, ErrorCount},
        {node, node()},
        {last_updated, couch_replicator_utils:iso8601(StateTime)},
        {start_time, couch_replicator_utils:iso8601(Rep#rep.start_time)}
    ]}.

-spec ejson_doc_state_filter(atom(), [atom()]) -> boolean().
ejson_doc_state_filter(_DocState, []) ->
    true;
ejson_doc_state_filter(State, States) when is_list(States), is_atom(State) ->
    lists:member(State, States).

-spec cluster_membership_foldl(#rdoc{}, nil) -> nil.
cluster_membership_foldl(#rdoc{} = RDoc, nil) ->
    #rdoc{id = {DbName, DocId}, rid = RepId, state = State} = RDoc,
    Owner = couch_replicator_utils:owner(DbName, DocId),
    case {State, Owner} of
        {not_owner, Node} when Node =:= node() ->
            Msg = "Node ~p is the usurper for doc ~s:~s with id ~p",
            couch_log:notice(Msg, [Node, DbName, DocId, RepId]),
            start_as_usurper(RDoc),
            nil;
        {_, Node} when is_atom(Node), Node =:= node() ->
            nil;
        {not_owner, Node} when Node =/= node() ->
            nil;
        {_, Node} when Node =/= node() ->
            Msg = "Replication doc ~s:~s with id ~p usurped by node ~p",
            couch_log:notice(Msg, [DbName, DocId, RepId, Node]),
            stop_usurped(RDoc),
            nil
    end.

schedule_membership_timer(#st{tref = undefined} = St) ->
    Interval = couch_replicator_scheduler:get_interval_msec() div 2,
    TRef = erlang:send_after(Interval, self(), check_membership),
    St#st{tref = TRef}.

cancel_membership_timer(#st{tref = TRef} = St) when is_reference(TRef) ->
    erlang:cancel_timer(TRef),
    St#st{tref = undefined}.

docs_foldl(#rdoc{} = RDoc, Acc) ->
    #rdoc{id = Id, state = State, rid = RId, errcnt = ErrCnt, info = Info} = RDoc,
    [[Id, State, RId, ErrCnt, Info] | Acc].

start_scanner(#st{mdb_changes_pid = undefined} = St) ->
    Suffix = <<"_replicator">>,
    Interval = couch_replicator_scheduler:get_interval_msec(),
    Opts = [skip_ddocs, {shards_db_check_msec, Interval div 2}],
    {ok, Pid} = couch_multidb_changes:start_link(Suffix, ?MODULE, nil, Opts),
    couch_stats:increment_counter([couch_replicator, db_scans]),
    couch_log:notice("Started replicator db changes listener ~p", [Pid]),
    St#st{mdb_changes_pid = Pid}.

start_delay_msec() ->
    DefaultSec = ?DEFAULT_START_DELAY_MSEC div 1000,
    % We're using a compatiblity config setting (cluster_start_period) to avoid
    % introducting a new config value.
    MSec = 1000 * config:get_integer("replicator", "cluster_start_period", DefaultSec),
    max(MSec, ?MIN_START_DELAY_MSEC).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

-define(DB, <<"db">>).
-define(EXIT_DB, <<"exit_db">>).
-define(DOC1, <<"doc1">>).
-define(DOC2, <<"doc2">>).
-define(R1, {"1", ""}).
-define(R2, {"2", ""}).

doc_processor_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_bad_change),
                ?TDEF_FE(t_regular_change),
                ?TDEF_FE(t_change_with_doc_processor_crash),
                ?TDEF_FE(t_change_with_existing_job),
                ?TDEF_FE(t_deleted_change),
                ?TDEF_FE(t_triggered_change),
                ?TDEF_FE(t_completed_change),
                ?TDEF_FE(t_active_replication_completed),
                ?TDEF_FE(t_error_change),
                ?TDEF_FE(t_failed_change),
                ?TDEF_FE(t_change_for_different_node),
                ?TDEF_FE(t_ejson_docs),
                ?TDEF_FE(t_cluster_ownership, 15),
                ?TDEF_FE(t_processing_failure_when_not_owner),
                ?TDEF_FE(t_processing_error_when_not_owner),
                ?TDEF_FE(t_worker_return_basic),
                ?TDEF_FE(t_worker_return_not_owner),
                ?TDEF_FE(t_worker_return_ignore),
                ?TDEF_FE(t_worker_return_error),
                ?TDEF_FE(t_worker_return_failure)
            ]
        }
    }.

% Can't parse replication doc, so should write failure state to document.
t_bad_change(_) ->
    ?assertEqual(acc, db_change(?DB, bad_change(), acc)),
    ?assert(updated_doc_with_failed_state()).

% Regular change, parse to a #rep{} and then add job.
t_regular_change(_) ->
    mock_existing_jobs_lookup([]),
    ?assertEqual(ok, process_change(?DB, change())),
    ?assert(ets:member(?MODULE, {?DB, ?DOC1})),
    ?assert(started_worker({?DB, ?DOC1})).

% Handle cases where doc processor exits or crashes while processing a change
t_change_with_doc_processor_crash(_) ->
    mock_existing_jobs_lookup([]),
    try
        db_change(?EXIT_DB, change(), acc)
    catch
        Tag:Err ->
            ?assertMatch({exit, {kapow, _}}, {Tag, Err})
    end,
    ?assert(failed_state_not_updated()).

% Regular change, parse to a #rep{} and then add job but there is already
% a running job with same Id found.
t_change_with_existing_job(_) ->
    mock_existing_jobs_lookup([?R2]),
    ?assertEqual(ok, process_change(?DB, change())),
    ?assert(ets:member(?MODULE, {?DB, ?DOC1})),
    ?assert(started_worker({?DB, ?DOC1})).

% Change is a deletion, and job is running, so remove job.
t_deleted_change(_) ->
    mock_existing_jobs_lookup([?R2]),
    ?assertEqual(ok, process_change(?DB, deleted_change())),
    ?assert(removed_job(?R2)).

% Change is in `triggered` state. Remove legacy state and add job.
t_triggered_change(_) ->
    mock_existing_jobs_lookup([]),
    ?assertEqual(ok, process_change(?DB, change(<<"triggered">>))),
    ?assert(removed_state_fields()),
    ?assert(ets:member(?MODULE, {?DB, ?DOC1})),
    ?assert(started_worker({?DB, ?DOC1})).

% Change is in `completed` state, so skip over it.
t_completed_change(_) ->
    ?assertEqual(ok, process_change(?DB, change(<<"completed">>))),
    ?assert(did_not_remove_state_fields()),
    ?assertNot(ets:member(?MODULE, {?DB, ?DOC1})),
    ?assert(did_not_spawn_worker()).

% Completed change comes for what used to be an active job. In this case
% remove entry from doc_processor's ets (because there is no linkage or
% callback mechanism for scheduler to tell doc_processsor a replication just
% completed).
t_active_replication_completed(_) ->
    mock_existing_jobs_lookup([]),
    ?assertEqual(ok, process_change(?DB, change())),
    ?assert(ets:member(?MODULE, {?DB, ?DOC1})),
    ?assertEqual(ok, process_change(?DB, change(<<"completed">>))),
    ?assert(did_not_remove_state_fields()),
    ?assertNot(ets:member(?MODULE, {?DB, ?DOC1})).

% Change is in `error` state. Remove legacy state and retry
% running the job. This state was used for transient erorrs which are not
% written to the document anymore.
t_error_change(_) ->
    mock_existing_jobs_lookup([]),
    ?assertEqual(ok, process_change(?DB, change(<<"error">>))),
    ?assert(removed_state_fields()),
    ?assert(ets:member(?MODULE, {?DB, ?DOC1})),
    ?assert(started_worker({?DB, ?DOC1})).

% Change is in `failed` state. This is a terminal state and it will not
% be tried again, so skip over it.
t_failed_change(_) ->
    ?assertEqual(ok, process_change(?DB, change(<<"failed">>))),
    ?assert(did_not_remove_state_fields()),
    ?assertNot(ets:member(?MODULE, {?DB, ?DOC1})),
    ?assert(did_not_spawn_worker()).

% Normal change, but according to cluster ownership algorithm, replication
% belongs to a different node, so this node should skip it.
t_change_for_different_node(_) ->
    meck:expect(couch_replicator_utils, owner, 2, different_node),
    ?assertEqual(ok, process_change(?DB, change())),
    ?assert(did_not_spawn_worker()).

% Check if docs/0 function produces expected ejson after adding a job
t_ejson_docs(_) ->
    mock_existing_jobs_lookup([]),
    ?assertEqual(ok, process_change(?DB, change())),
    ?assert(ets:member(?MODULE, {?DB, ?DOC1})),
    EJsonDocs = docs([]),
    ?assertMatch([{[_ | _]}], EJsonDocs),
    [{DocProps}] = EJsonDocs,
    {value, StateTime, DocProps1} = lists:keytake(
        last_updated,
        1,
        DocProps
    ),
    ?assertMatch(
        {last_updated, BinVal1} when is_binary(BinVal1),
        StateTime
    ),
    {value, StartTime, DocProps2} = lists:keytake(start_time, 1, DocProps1),
    ?assertMatch({start_time, BinVal2} when is_binary(BinVal2), StartTime),
    ExpectedProps = [
        {database, ?DB},
        {doc_id, ?DOC1},
        {error_count, 0},
        {id, null},
        {info, null},
        {node, node()},
        {state, initializing}
    ],
    ?assertEqual(ExpectedProps, lists:usort(DocProps2)).

% Check that when cluster membership changes records from doc processor when
% cluster relicator doc ownership changes
t_cluster_ownership(_) ->
    mock_existing_jobs_lookup([?R1]),
    ?assertEqual(ok, process_change(?DB, change())),
    Id = {?DB, ?DOC1},

    % After a membership we're still the owner
    ?assertMatch([[Id, initializing | _]], docs()),
    meck:reset(couch_replicator_doc_processor_worker),
    meck:reset(couch_replicator_scheduler),
    whereis(?MODULE) ! check_membership,
    ?assertMatch([[Id, initializing | _]], docs()),
    ?assert(did_not_spawn_worker()),
    ?assertNot(removed_job(?R1)),

    % During the next check we find out we're not the owner
    meck:expect(couch_replicator_utils, owner, 2, different_node),
    meck:reset(couch_replicator_doc_processor_worker),
    meck:reset(couch_replicator_scheduler),
    whereis(?MODULE) ! check_membership,
    meck:wait(1, couch_replicator_scheduler, find_jobs_by_doc, 2, 2000),
    ?assertMatch([[Id, not_owner | _]], docs()),
    ?assert(removed_job(?R1)),

    % Another check, in this instance we're still not the owner
    meck:reset(couch_replicator_doc_processor_worker),
    meck:reset(couch_replicator_scheduler),
    whereis(?MODULE) ! check_membership,
    ?assertMatch([[Id, not_owner | _]], docs()),
    ?assert(did_not_spawn_worker()),
    ?assertNot(removed_job(?R1)),

    % We're the owner again.
    ThisNode = node(),
    meck:expect(couch_replicator_utils, owner, 2, ThisNode),
    meck:reset(couch_replicator_doc_processor_worker),
    meck:reset(couch_replicator_scheduler),
    whereis(?MODULE) ! check_membership,
    meck:wait(1, couch_replicator_doc_processor_worker, spawn_worker, 4, 2000),
    ?assertMatch([[Id, initializing | _]], docs()).

t_processing_failure_when_not_owner(_) ->
    meck:expect(couch_replicator_utils, owner, 2, different_node),
    meck:reset(couch_log),
    ?assertEqual(ok, process_change(?DB, change(<<"failed">>))),
    ?assert(did_not_remove_state_fields()),
    ?assertNot(ets:member(?MODULE, {?DB, ?DOC1})),
    ?assert(did_not_spawn_worker()).

t_processing_error_when_not_owner(_) ->
    meck:expect(couch_replicator_utils, owner, 2, different_node),
    meck:reset(couch_log),
    ?assertEqual(ok, process_change(?DB, bad_change())),
    ?assert(did_not_remove_state_fields()),
    ?assertNot(ets:member(?MODULE, {?DB, ?DOC1})),
    ?assert(did_not_spawn_worker()).

t_worker_return_basic(_) ->
    ?assertEqual(ok, process_change(?DB, change())),
    Id = {?DB, ?DOC1},
    ?assertMatch([[Id, initializing, nil, 0, nil]], docs()),
    [#rdoc{worker = Ref}] = ets:lookup(?MODULE, Id),
    WRes = #doc_worker_result{id = Id, wref = Ref, result = {ok, ?R1}},
    whereis(?MODULE) ! {'DOWN', ref, process, pid, WRes},
    ?assertMatch([[Id, scheduled, ?R1, 0, nil]], docs()).

t_worker_return_not_owner(_) ->
    ?assertEqual(ok, process_change(?DB, change())),
    Id = {?DB, ?DOC1},
    ?assertMatch([[Id, initializing, nil, 0, nil]], docs()),
    [#rdoc{worker = Ref}] = ets:lookup(?MODULE, Id),

    % While worker is supposedly running, usurp the doc
    meck:expect(couch_replicator_utils, owner, 2, different_node),
    whereis(?MODULE) ! check_membership,
    meck:wait(2, couch_replicator_scheduler, find_jobs_by_doc, 2, 2000),
    ?assertMatch([[Id, not_owner | _]], docs()),

    % Make the worker return after usurping
    meck:reset(couch_replicator_scheduler),
    WRes = #doc_worker_result{id = Id, wref = Ref, result = {ok, ?R1}},
    whereis(?MODULE) ! {'DOWN', ref, process, pid, WRes},
    ?assertMatch([[Id, not_owner, nil, 0, nil]], docs()),
    ?assert(removed_job(?R1)).

t_worker_return_ignore(_) ->
    ?assertEqual(ok, process_change(?DB, change())),
    Id = {?DB, ?DOC1},
    ?assertMatch([[Id, initializing, nil, 0, nil]], docs()),
    WRes = #doc_worker_result{id = Id, wref = ref, result = ignore},
    whereis(?MODULE) ! {'DOWN', ref, process, pid, WRes},
    ?assertMatch([[Id, initializing, nil, 0, nil]], docs()).

t_worker_return_error(_) ->
    ?assertEqual(ok, process_change(?DB, change())),
    Id = {?DB, ?DOC1},
    ?assertMatch([[Id, initializing, nil, 0, nil]], docs()),
    [#rdoc{worker = Ref}] = ets:lookup(?MODULE, Id),
    Res = {temporary_error, foo},
    WRes = #doc_worker_result{id = Id, wref = Ref, result = Res},
    whereis(?MODULE) ! {'DOWN', ref, process, pid, WRes},
    ?assertMatch([[Id, error, nil, 1, foo]], docs()).

t_worker_return_failure(_) ->
    ?assertEqual(ok, process_change(?DB, change())),
    Id = {?DB, ?DOC1},
    ?assertMatch([[Id, initializing, nil, 0, nil]], docs()),
    [#rdoc{worker = Ref}] = ets:lookup(?MODULE, Id),
    Res = {permanent_failure, bar},
    WRes = #doc_worker_result{id = Id, wref = Ref, result = Res},
    whereis(?MODULE) ! {'DOWN', ref, process, pid, WRes},
    ?assertEqual([], docs()).

get_worker_ref_test_() ->
    {
        foreach,
        fun() ->
            ets:new(?MODULE, [named_table, public, {keypos, #rdoc.id}])
        end,
        fun(_) -> ets:delete(?MODULE) end,
        [
            ?TDEF_FE(t_worker_ref)
        ]
    }.

t_worker_ref(_) ->
    Id = {<<"db">>, <<"doc">>},
    ?assertEqual(nil, get_worker_ref(Id)),
    ets:insert(?MODULE, #rdoc{id = Id, worker = nil}),
    ?assertEqual(nil, get_worker_ref(Id)),
    Ref = make_ref(),
    ets:insert(?MODULE, #rdoc{id = Id, worker = Ref}),
    ?assertEqual(Ref, get_worker_ref(Id)).

% Test helper functions

setup_all() ->
    meck:expect(couch_log, info, 2, ok),
    meck:expect(couch_log, notice, 2, ok),
    meck:expect(couch_log, warning, 2, ok),
    meck:expect(couch_log, error, 2, ok),
    meck:expect(config, get, fun(_, _, Default) -> Default end),
    meck:expect(config, listen_for_changes, 2, ok),
    meck:expect(couch_replicator_utils, owner, 2, node()),
    meck:expect(couch_multidb_changes, start_link, 4, {ok, mdb_changes_pid}),
    meck:expect(couch_replicator_doc_processor_worker, spawn_worker, fun
        ({?EXIT_DB, _}, _, _, _) -> exit(kapow);
        (_, _, _, _) -> pid
    end),
    meck:expect(couch_replicator_scheduler, remove_job, 1, ok),
    meck:expect(couch_replicator_scheduler, get_interval_msec, 0, 1000),
    meck:expect(couch_replicator_docs, remove_state_fields, 2, ok),
    meck:expect(couch_replicator_docs, update_failed, 3, ok).

teardown_all(_) ->
    meck:unload().

setup() ->
    meck:reset([
        config,
        couch_log,
        couch_replicator_utils,
        couch_replicator_doc_processor_worker,
        couch_replicator_docs,
        couch_replicator_scheduler
    ]),
    % Set this expectation back to the default for
    % each test since some tests change it
    meck:expect(couch_replicator_utils, owner, 2, node()),
    {ok, Pid} = start_link(),
    unlink(Pid),
    Pid.

teardown(Pid) ->
    % 1s wait should suffice
    test_util:stop_sync(Pid, kill, 1000).

removed_state_fields() ->
    meck:called(couch_replicator_docs, remove_state_fields, [?DB, ?DOC1]).

started_worker(_Id) ->
    1 == meck:num_calls(couch_replicator_doc_processor_worker, spawn_worker, 4).

removed_job(Id) ->
    meck:called(couch_replicator_scheduler, remove_job, [Id]).

did_not_remove_state_fields() ->
    0 == meck:num_calls(couch_replicator_docs, remove_state_fields, '_').

did_not_spawn_worker() ->
    0 ==
        meck:num_calls(
            couch_replicator_doc_processor_worker,
            spawn_worker,
            '_'
        ).

updated_doc_with_failed_state() ->
    1 == meck:num_calls(couch_replicator_docs, update_failed, '_').

failed_state_not_updated() ->
    0 == meck:num_calls(couch_replicator_docs, update_failed, '_').

mock_existing_jobs_lookup(ExistingJobs) ->
    meck:expect(couch_replicator_scheduler, find_jobs_by_doc, fun
        (?EXIT_DB, ?DOC1) -> [];
        (?DB, ?DOC1) -> ExistingJobs
    end).

change() ->
    {[
        {<<"id">>, ?DOC1},
        {doc,
            {[
                {<<"_id">>, ?DOC1},
                {<<"source">>, <<"http://srchost.local/src">>},
                {<<"target">>, <<"http://tgthost.local/tgt">>}
            ]}}
    ]}.

change(State) ->
    {[
        {<<"id">>, ?DOC1},
        {doc,
            {[
                {<<"_id">>, ?DOC1},
                {<<"source">>, <<"http://srchost.local/src">>},
                {<<"target">>, <<"http://tgthost.local/tgt">>},
                {<<"_replication_state">>, State}
            ]}}
    ]}.

deleted_change() ->
    {[
        {<<"id">>, ?DOC1},
        {<<"deleted">>, true},
        {doc,
            {[
                {<<"_id">>, ?DOC1},
                {<<"source">>, <<"http://srchost.local/src">>},
                {<<"target">>, <<"http://tgthost.local/tgt">>}
            ]}}
    ]}.

bad_change() ->
    {[
        {<<"id">>, ?DOC2},
        {doc,
            {[
                {<<"_id">>, ?DOC2},
                {<<"source">>, <<"src">>}
            ]}}
    ]}.

-endif.
