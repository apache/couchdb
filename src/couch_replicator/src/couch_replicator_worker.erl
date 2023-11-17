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

-module(couch_replicator_worker).
-behaviour(gen_server).

% public API
-export([start_link/5]).

% gen_server callbacks
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([format_status/2]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").

-define(DOC_BUFFER_BYTE_SIZE, 512 * 1024).
-define(STATS_DELAY_SEC, 10).
-define(MISSING_DOC_RETRY_MSEC, 2000).

% Coefficients for the bulk_get and revs_diffs stats processing.
%
% Threshold is the ratio limit. Valid ranges are from 0.0 to 1.0. When
% the estimated ratio is greater than the threshold, the "action" is
% performed (_bulk_get or _revs_diff API are used). If it's below the
% limit, the action is not performed.
%
% Decay is the "alpha" of the exponential moving average. Valid ranges
% are from 0.0 to 1.0. The closer to 1.0, the more the latest update
% takes effect; the closer to 0.0, the more historical values persist.
%
% Retries are the forced periodic retry intervals. Even if the
% estimated ratio indicates not to perform the action, periodically
% try to use the API anyway to update the stats with the latest
% information. Both values are at 37 seconds, which is a bit longer
% than the default checkpoint interval.
%
-define(BULK_GET_RATIO_THRESHOLD, 0.5).
-define(BULK_GET_RATIO_DECAY, 0.25).
-define(BULK_GET_RETRY_SEC, 37).
-define(REVS_DIFF_RATIO_THRESHOLD, 0.95).
-define(REVS_DIFF_RATIO_DECAY, 0.4).
-define(REVS_DIFF_RETRY_SEC, 37).

-import(couch_util, [
    to_binary/1,
    get_value/3
]).

-record(batch, {
    docs = [],
    size = 0
}).

-record(state, {
    cp,
    loop,
    max_parallel_conns,
    source,
    target,
    readers = [],
    writer = nil,
    pending_fetch = nil,
    flush_waiter = nil,
    stats = couch_replicator_stats:new(),
    last_stats_report_sec = 0,
    batch = #batch{}
}).

-record(fetch_stats, {
    ratio = 0,
    tsec = 0
}).

-record(fetch_st, {
    source,
    target,
    parent,
    cp,
    changes_manager,
    use_bulk_get,
    bulk_get_stats = #fetch_stats{},
    revs_diff_stats = #fetch_stats{}
}).

start_link(Cp, #httpdb{} = Source, Target, ChangesManager, [_ | _] = Options) ->
    gen_server:start_link(
        ?MODULE, {Cp, Source, Target, ChangesManager, Options}, []
    ).

init({Cp, Source, Target, ChangesManager, Options}) ->
    process_flag(trap_exit, true),
    NowSec = erlang:monotonic_time(second),
    MaxConns = couch_util:get_value(http_connections, Options),
    UseBulkGet = couch_util:get_value(use_bulk_get, Options),
    FetchSt = #fetch_st{
        cp = Cp,
        source = Source,
        target = Target,
        parent = self(),
        changes_manager = ChangesManager,
        use_bulk_get = UseBulkGet,
        bulk_get_stats = #fetch_stats{tsec = NowSec},
        revs_diff_stats = #fetch_stats{tsec = NowSec}
    },
    State = #state{
        cp = Cp,
        max_parallel_conns = MaxConns,
        loop = spawn_link(fun() -> queue_fetch_loop(FetchSt) end),
        source = Source,
        target = Target,
        last_stats_report_sec = NowSec
    },
    {ok, State}.

handle_call(
    {fetch_doc, {_Id, _Revs, _PAs} = Params},
    {Pid, _} = From,
    #state{
        loop = Pid,
        readers = Readers,
        pending_fetch = nil,
        source = Src,
        target = Tgt,
        max_parallel_conns = MaxConns
    } = State
) ->
    case length(Readers) of
        Size when Size < MaxConns ->
            Reader = spawn_doc_reader(Src, Tgt, Params),
            NewState = State#state{
                readers = [Reader | Readers]
            },
            {reply, ok, NewState};
        _ ->
            NewState = State#state{
                pending_fetch = {From, Params}
            },
            {noreply, NewState}
    end;
handle_call({batch_doc, Doc}, From, State) ->
    gen_server:reply(From, ok),
    {noreply, maybe_flush_docs(Doc, State)};
handle_call(
    flush,
    {Pid, _} = From,
    #state{
        loop = Pid,
        writer = nil,
        flush_waiter = nil,
        target = Target,
        batch = Batch
    } = State
) ->
    State2 =
        case State#state.readers of
            [] ->
                State#state{writer = spawn_writer(Target, Batch)};
            _ ->
                State
        end,
    {noreply, State2#state{flush_waiter = From}}.

handle_cast({sum_stats, IncStats}, #state{stats = Stats} = State) ->
    SumStats = couch_replicator_utils:sum_stats(Stats, IncStats),
    {noreply, maybe_report_stats(State#state{stats = SumStats})};
handle_cast(Msg, State) ->
    {stop, {unexpected_cast, Msg}, State}.

handle_info({'EXIT', Pid, normal}, #state{loop = Pid} = State) ->
    #state{
        batch = #batch{docs = []},
        readers = [],
        writer = nil,
        pending_fetch = nil,
        flush_waiter = nil
    } = State,
    {stop, normal, State};
handle_info({'EXIT', Pid, normal}, #state{writer = Pid} = State) ->
    {noreply, after_full_flush(State)};
handle_info({'EXIT', Pid, normal}, #state{writer = nil} = State) ->
    #state{
        readers = Readers,
        writer = Writer,
        batch = Batch,
        source = Source,
        target = Target,
        pending_fetch = Fetch,
        flush_waiter = FlushWaiter
    } = State,
    case Readers -- [Pid] of
        Readers ->
            {noreply, State};
        Readers2 ->
            State2 =
                case Fetch of
                    nil ->
                        case
                            (FlushWaiter =/= nil) andalso (Writer =:= nil) andalso
                                (Readers2 =:= [])
                        of
                            true ->
                                State#state{
                                    readers = Readers2,
                                    writer = spawn_writer(Target, Batch)
                                };
                            false ->
                                State#state{readers = Readers2}
                        end;
                    {From, FetchParams} ->
                        Reader = spawn_doc_reader(Source, Target, FetchParams),
                        gen_server:reply(From, ok),
                        State#state{
                            readers = [Reader | Readers2],
                            pending_fetch = nil
                        }
                end,
            {noreply, State2}
    end;
handle_info({'EXIT', _Pid, max_backoff}, State) ->
    {stop, {shutdown, max_backoff}, State};
handle_info({'EXIT', _Pid, {bulk_docs_failed, _, _} = Err}, State) ->
    {stop, {shutdown, Err}, State};
handle_info({'EXIT', _Pid, {bulk_get_failed, _, _} = Err}, State) ->
    {stop, {shutdown, Err}, State};
handle_info({'EXIT', _Pid, {revs_diff_failed, _, _} = Err}, State) ->
    {stop, {shutdown, Err}, State};
handle_info({'EXIT', _Pid, {http_request_failed, _, _, _} = Err}, State) ->
    {stop, {shutdown, Err}, State};
handle_info({'EXIT', _Pid, {doc_write_failed, _} = Err}, State) ->
    {stop, {shutdown, Err}, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    {stop, {process_died, Pid, Reason}, State}.

format_status(_Opt, [_PDict, State]) ->
    #state{
        cp = MainJobPid,
        loop = LoopPid,
        source = Source,
        target = Target,
        readers = Readers,
        pending_fetch = PendingFetch,
        batch = #batch{size = BatchSize}
    } = State,
    [
        {main_pid, MainJobPid},
        {loop, LoopPid},
        {source, couch_replicator_api_wrap:db_uri(Source)},
        {target, couch_replicator_api_wrap:db_uri(Target)},
        {num_readers, length(Readers)},
        {pending_fetch, PendingFetch},
        {batch_size, BatchSize}
    ].

sum_stats(Pid, Stats) when is_pid(Pid) ->
    ok = gen_server:cast(Pid, {sum_stats, Stats}).

report_seq_done(Cp, Seq) ->
    ok = report_seq_done(Cp, Seq, couch_replicator_stats:new()).

report_seq_done(Cp, Seq, Stats) ->
    ok = couch_replicator_scheduler_job:report_seq_done(Cp, Seq, Stats).

queue_fetch_loop(#fetch_st{} = St) ->
    #fetch_st{
        cp = Cp,
        source = Source,
        target = Target,
        parent = Parent,
        changes_manager = ChangesManager,
        use_bulk_get = UseBulkGet,
        bulk_get_stats = BgSt,
        revs_diff_stats = RdSt
    } = St,
    ChangesManager ! {get_changes, self()},
    receive
        {closed, ChangesManager} ->
            ok;
        {changes, ChangesManager, [], ReportSeq} ->
            ok = report_seq_done(Cp, ReportSeq),
            queue_fetch_loop(St);
        {changes, ChangesManager, Changes, ReportSeq} ->
            % Find missing revisions (POST to _revs_diff)
            {IdRevs, RdSt1} = find_missing(Changes, Target, Parent, RdSt),
            % Filter out and handle design docs individually
            DDocFilter = fun
                ({<<?DESIGN_DOC_PREFIX, _/binary>>, _Rev}, _PAs) -> true;
                ({_Id, _Rev}, _PAs) -> false
            end,
            DDocIdRevs = maps:filter(DDocFilter, IdRevs),
            FetchFun = fun({Id, Rev}, PAs) ->
                ok = gen_server:call(Parent, {fetch_doc, {Id, [Rev], PAs}}, infinity)
            end,
            maps:map(FetchFun, DDocIdRevs),
            % IdRevs1 is all the docs without design docs. Bulk get those.
            IdRevs1 = maps:without(maps:keys(DDocIdRevs), IdRevs),
            {Docs, BgSt1} = bulk_get(UseBulkGet, Source, IdRevs1, Parent, BgSt),
            BatchFun = fun({_, #doc{} = Doc}) ->
                ok = gen_server:call(Parent, {batch_doc, Doc}, infinity)
            end,
            lists:foreach(BatchFun, lists:sort(maps:to_list(Docs))),
            % Invidually upload docs with attachments.
            maps:map(FetchFun, maps:without(maps:keys(Docs), IdRevs1)),
            {ok, Stats} = gen_server:call(Parent, flush, infinity),
            ok = report_seq_done(Cp, ReportSeq, Stats),
            couch_log:debug("Worker reported completion of seq ~p", [ReportSeq]),
            St1 = St#fetch_st{
                bulk_get_stats = BgSt1,
                revs_diff_stats = RdSt1
            },
            queue_fetch_loop(St1)
    end.

% Return revisions without attachments. Maintain an exponential moving failure
% ratio. When the ratio becomes greater than the threshold, skip calling
% bulk_get altogether. To avoid getting permanently stuck with a high failure
% ratio after replicating lots of attachments, periodically attempt to use
% _bulk_get. After a few successful attempts that should lower the failure rate
% enough to start allow using _bulk_get again.
%
bulk_get(false, _Source, _IdRevs, _Parent, #fetch_stats{} = St) ->
    {#{}, St};
bulk_get(true, Source, IdRevs, Parent, #fetch_stats{} = St) ->
    NowSec = erlang:monotonic_time(second),
    case attempt_bulk_get(St, NowSec) of
        true ->
            Docs = bulk_get(Source, IdRevs),
            Attempts = map_size(IdRevs),
            Successes = map_size(Docs),
            Stats = couch_replicator_stats:new([
                {bulk_get_docs, Successes},
                {bulk_get_attempts, Attempts}
            ]),
            ok = sum_stats(Parent, Stats),
            {Docs, update_bulk_get_ratio(St, Successes, Attempts, NowSec)};
        false ->
            {#{}, St}
    end.

bulk_get(#httpdb{} = Source, #{} = IdRevs) ->
    Opts = [latest, revs, {attachments, false}],
    case couch_replicator_api_wrap:bulk_get(Source, IdRevs, Opts) of
        {ok, #{} = Docs} ->
            FilterFun = fun
                (_, #doc{atts = []}) -> true;
                (_, #doc{atts = [_ | _]}) -> false;
                (_, {error, _}) -> false
            end,
            maps:filter(FilterFun, Docs);
        {error, Error} ->
            couch_log:debug("_bulk_get failed ~p", [Error]),
            #{}
    end.

attempt_fetch(#fetch_stats{} = St, NowSec, RetryLimit, RatioLimit) ->
    #fetch_stats{tsec = TSec, ratio = Ratio} = St,
    TimeThreshold = (NowSec - TSec) > RetryLimit,
    RatioThreshold = Ratio =< RatioLimit,
    TimeThreshold orelse RatioThreshold.

attempt_bulk_get(#fetch_stats{} = St, NowSec) ->
    attempt_fetch(St, NowSec, ?BULK_GET_RETRY_SEC, ?BULK_GET_RATIO_THRESHOLD).

attempt_revs_diff(#fetch_stats{} = St, NowSec) ->
    attempt_fetch(St, NowSec, ?REVS_DIFF_RETRY_SEC, ?REVS_DIFF_RATIO_THRESHOLD).

% Update fail ratio. Use the basic exponential moving average formula to smooth
% over minor bumps in case we encounter a few % attachments and then get back
% to replicationg documents without attachments.
%
update_fetch_stats(#fetch_stats{} = St, Successes, Attempts, Decay, NowSec) ->
    #fetch_stats{ratio = Avg} = St,
    Ratio =
        case Attempts > 0 of
            true -> (Attempts - Successes) / Attempts;
            false -> 0
        end,
    St#fetch_stats{ratio = Decay * (Ratio - Avg) + Avg, tsec = NowSec}.

update_bulk_get_ratio(#fetch_stats{} = St, Successes, Attempts, NowSec) ->
    update_fetch_stats(St, Successes, Attempts, ?BULK_GET_RATIO_DECAY, NowSec).

update_revs_diff_ratio(#fetch_stats{} = St, Successes, Attempts, NowSec) ->
    update_fetch_stats(St, Successes, Attempts, ?REVS_DIFF_RATIO_DECAY, NowSec).

-spec spawn_doc_reader(#httpdb{}, #httpdb{}, {list(), list(), list()}) -> no_return().
spawn_doc_reader(Source, Target, FetchParams) ->
    Parent = self(),
    spawn_link(fun() ->
        fetch_doc(
            Source, FetchParams, fun remote_doc_handler/2, {Parent, Target}
        )
    end).

-spec fetch_doc(#httpd{}, {list(), list(), list()}, function(), any()) -> no_return().
fetch_doc(Source, {Id, Revs, PAs}, DocHandler, Acc) ->
    try
        couch_replicator_api_wrap:open_doc_revs(
            Source, Id, Revs, [{atts_since, PAs}, latest], DocHandler, Acc
        )
    catch
        throw:missing_doc ->
            couch_log:error(
                "Retrying fetch and update of document `~s` as it is "
                "unexpectedly missing. Missing revisions are: ~s",
                [Id, couch_doc:revs_to_strs(Revs)]
            ),
            WaitMSec = config:get_integer(
                "replicator",
                "missing_doc_retry_msec",
                ?MISSING_DOC_RETRY_MSEC
            ),
            timer:sleep(WaitMSec),
            couch_replicator_api_wrap:open_doc_revs(Source, Id, Revs, [latest], DocHandler, Acc);
        throw:{missing_stub, _} ->
            couch_log:error(
                "Retrying fetch and update of document `~s` due to out of "
                "sync attachment stubs. Missing revisions are: ~s",
                [Id, couch_doc:revs_to_strs(Revs)]
            ),
            WaitMSec = config:get_integer(
                "replicator",
                "missing_doc_retry_msec",
                ?MISSING_DOC_RETRY_MSEC
            ),
            timer:sleep(WaitMSec),
            couch_replicator_api_wrap:open_doc_revs(Source, Id, Revs, [latest], DocHandler, Acc)
    end.

remote_doc_handler(
    {ok, #doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc},
    Acc
) ->
    % Flush design docs in their own PUT requests to correctly process
    % authorization failures for design doc updates.
    couch_log:debug("Worker flushing design doc", []),
    doc_handler_flush_doc(Doc, Acc);
remote_doc_handler({ok, #doc{atts = [_ | _]} = Doc}, Acc) ->
    % Immediately flush documents with attachments received from a remote
    % source. The data property of each attachment is a function that starts
    % streaming the attachment data from the remote source, therefore it's
    % convenient to call it ASAP to avoid ibrowse inactivity timeouts.
    couch_log:debug("Worker flushing doc with attachments", []),
    doc_handler_flush_doc(Doc, Acc);
remote_doc_handler({ok, #doc{atts = []} = Doc}, {Parent, _} = Acc) ->
    ok = gen_server:call(Parent, {batch_doc, Doc}, infinity),
    {ok, Acc};
remote_doc_handler({{not_found, missing}, _}, _Acc) ->
    throw(missing_doc).

doc_handler_flush_doc(#doc{} = Doc, {Parent, Target} = Acc) ->
    Stats = couch_replicator_stats:new([{docs_read, 1}]),
    Success = (flush_doc(Target, Doc) =:= ok),
    {Result, Stats2} =
        case Success of
            true ->
                {{ok, Acc}, couch_replicator_stats:increment(docs_written, Stats)};
            false ->
                {{skip, Acc}, couch_replicator_stats:increment(doc_write_failures, Stats)}
        end,
    ok = sum_stats(Parent, Stats2),
    Result.

spawn_writer(Target, #batch{docs = DocList, size = Size}) ->
    case {Target, Size > 0} of
        {#httpdb{}, true} ->
            couch_log:debug("Worker flushing doc batch of size ~p bytes", [Size]);
        _ ->
            ok
    end,
    Parent = self(),
    spawn_link(
        fun() ->
            Stats = flush_docs(Target, DocList),
            ok = sum_stats(Parent, Stats)
        end
    ).

after_full_flush(#state{stats = Stats, flush_waiter = Waiter} = State) ->
    gen_server:reply(Waiter, {ok, Stats}),
    State#state{
        stats = couch_replicator_stats:new(),
        flush_waiter = nil,
        writer = nil,
        batch = #batch{},
        last_stats_report_sec = erlang:monotonic_time(second)
    }.

maybe_flush_docs(Doc, State) ->
    #state{
        target = Target,
        batch = Batch,
        stats = Stats
    } = State,
    {Batch2, WStats} = maybe_flush_docs(Target, Batch, Doc),
    Stats2 = couch_replicator_stats:sum_stats(Stats, WStats),
    Stats3 = couch_replicator_stats:increment(docs_read, Stats2),
    maybe_report_stats(State#state{stats = Stats3, batch = Batch2}).

maybe_flush_docs(#httpdb{} = Target, Batch, Doc) ->
    #batch{docs = DocAcc, size = SizeAcc} = Batch,
    JsonDoc = ?JSON_ENCODE(couch_doc:to_json_obj(Doc, [revs, attachments])),
    case SizeAcc + iolist_size(JsonDoc) of
        SizeAcc2 when SizeAcc2 > ?DOC_BUFFER_BYTE_SIZE ->
            couch_log:debug("Worker flushing doc batch of size ~p bytes", [SizeAcc2]),
            Stats = flush_docs(Target, [JsonDoc | DocAcc]),
            {#batch{}, Stats};
        SizeAcc2 ->
            Stats = couch_replicator_stats:new(),
            {#batch{docs = [JsonDoc | DocAcc], size = SizeAcc2}, Stats}
    end.

flush_docs(_Target, []) ->
    couch_replicator_stats:new();
flush_docs(Target, DocList) ->
    FlushResult = couch_replicator_api_wrap:update_docs(
        Target,
        DocList,
        [delay_commit],
        ?REPLICATED_CHANGES
    ),
    handle_flush_docs_result(FlushResult, Target, DocList).

handle_flush_docs_result({error, request_body_too_large}, _Target, [Doc]) ->
    couch_log:error("Replicator: failed to write doc ~p. Too large", [Doc]),
    couch_replicator_stats:new([{doc_write_failures, 1}]);
handle_flush_docs_result({error, request_body_too_large}, Target, DocList) ->
    Len = length(DocList),
    {DocList1, DocList2} = lists:split(Len div 2, DocList),
    couch_log:notice(
        "Replicator: couldn't write batch of size ~p to ~p because"
        " request body is too large. Splitting batch into 2 separate batches of"
        " sizes ~p and ~p",
        [
            Len,
            couch_replicator_api_wrap:db_uri(Target),
            length(DocList1),
            length(DocList2)
        ]
    ),
    Stats1 = flush_docs(Target, DocList1),
    Stats2 = flush_docs(Target, DocList2),
    couch_replicator_stats:sum_stats(Stats1, Stats2);
handle_flush_docs_result({ok, Errors}, Target, DocList) ->
    DbUri = couch_replicator_api_wrap:db_uri(Target),
    lists:foreach(
        fun({Props}) ->
            couch_log:error(
                "Replicator: couldn't write document `~s`, revision"
                " `~s`, to target database `~s`. Error: `~s`, reason: `~s`.",
                [
                    get_value(id, Props, ""),
                    get_value(rev, Props, ""),
                    DbUri,
                    get_value(error, Props, ""),
                    get_value(reason, Props, "")
                ]
            )
        end,
        Errors
    ),
    couch_replicator_stats:new([
        {docs_written, length(DocList) - length(Errors)},
        {doc_write_failures, length(Errors)}
    ]);
handle_flush_docs_result({error, {bulk_docs_failed, _, _} = Err}, _, _) ->
    exit(Err).

flush_doc(Target, #doc{id = Id, revs = {Pos, [RevId | _]}} = Doc) ->
    try couch_replicator_api_wrap:update_doc(Target, Doc, [], ?REPLICATED_CHANGES) of
        {ok, _} -> ok;
        Error -> handle_doc_write_error(Error, Target, Id, Pos, RevId)
    catch
        throw:{Error, Reason} ->
            handle_doc_write_error({Error, Reason}, Target, Id, Pos, RevId)
    end.

% In most cases we fail the replication job by re-throwing the error.
% The only exceptions are expected validation and VDU failures
%  401 : unauthorized
%  403 : forbidden
%  413 : request_body_too_large
%  415 : unsupported_media_type
%  400 : bad_request where reason starts with "Attachment name "
%
handle_doc_write_error({missing_stub, _} = MissingStub, _, _, _, _) ->
    throw(MissingStub);
handle_doc_write_error({error, Reason} = Error, Target, Id, _, _) when
    Reason == request_body_too_large orelse Reason == unsupported_media_type
->
    couch_log:error(
        "Replicator: skipping writing document `~s` to `~s` : ~s.",
        [Id, couch_replicator_api_wrap:db_uri(Target), Reason]
    ),
    Error;
handle_doc_write_error({error, {invalid_attachment_name, Reason}} = Error, Target, Id, _, _) ->
    couch_log:error(
        "Replicator: skipping writing document `~s` to `~s` : invalid_attachment_name ~s",
        [Id, couch_replicator_api_wrap:db_uri(Target), Reason]
    ),
    Error;
handle_doc_write_error({Error, Reason}, Target, Id, Pos, RevId) when
    Error == unauthorized orelse Error == forbidden
->
    couch_log:error(
        "Replicator: skipping writing document `~s`, revision `~s`,"
        " to target database `~s`. Error: `~s`, reason: `~s`.",
        [
            Id,
            couch_doc:rev_to_str({Pos, RevId}),
            couch_replicator_api_wrap:db_uri(Target),
            to_binary(Error),
            to_binary(Reason)
        ]
    ),
    {error, Error};
handle_doc_write_error({error, {Error, Reason}}, Target, Id, Pos, RevId) ->
    couch_log:error(
        "Replicator: error writing document `~s`, revision `~s`,"
        " to target database `~s`. Error: `~s`, reason: `~s`.",
        [
            Id,
            couch_doc:rev_to_str({Pos, RevId}),
            couch_replicator_api_wrap:db_uri(Target),
            to_binary(Error),
            to_binary(Reason)
        ]
    ),
    exit({doc_write_failed, {Error, Reason}});
handle_doc_write_error(Error, Target, Id, Pos, RevId) ->
    couch_log:error(
        "Replicator: error writing document `~s`, revision `~s`,"
        " to target database `~s`. Error: `~s`.",
        [
            Id,
            couch_doc:rev_to_str({Pos, RevId}),
            couch_replicator_api_wrap:db_uri(Target),
            to_binary(Error)
        ]
    ),
    exit({doc_write_failed, Error}).

find_missing(DocInfos, Target, Parent, #fetch_stats{} = St) ->
    {IdRevs, AllCount} = lists:foldr(
        fun
            (#doc_info{revs = []}, {IdRevAcc, CountAcc}) ->
                {IdRevAcc, CountAcc};
            (#doc_info{id = Id, revs = RevsInfo}, {IdRevAcc, CountAcc}) ->
                Revs = [Rev || #rev_info{rev = Rev} <- RevsInfo],
                {[{Id, Revs} | IdRevAcc], CountAcc + length(Revs)}
        end,
        {[], 0},
        DocInfos
    ),
    NowSec = erlang:monotonic_time(second),
    {MissingRes, St1} =
        case attempt_revs_diff(St, NowSec) of
            true ->
                Missing = find_missing(IdRevs, Target),
                % The target might have some of the revisions and those might have
                % attachments associated with them, so only consider missing
                % revisions with an empty "possible_ancestors" list.
                FoldFun = fun
                    (_IdRev, _PAs = [_ | _], Acc) -> Acc;
                    (_IdRev, _PAs = [], Acc) -> Acc + 1
                end,
                MissingWithoutPAs = maps:fold(FoldFun, 0, Missing),
                % The "success" metric of the update algorithm is the number of
                % revisions which are already on target. The higher the number -
                % the higher the chance of calling _revs_diff. If it gets lower
                % than a threshold, it's worth avoiding calling revs_diff since the
                % target seems to be missing the majority of the revisions.
                OnTarget = AllCount - MissingWithoutPAs,
                {Missing, update_revs_diff_ratio(St, OnTarget, AllCount, NowSec)};
            false ->
                % Construct the result to look as if _revs_diff returned with
                % all missing revs. To reuse the existing id_revs_map/1
                % function, add the empty PAs list to IdRevs input.
                MapFun = fun({Id, Revs}) -> {Id, Revs, []} end,
                {id_rev_map(lists:map(MapFun, IdRevs)), St}
        end,
    Stats = couch_replicator_stats:new([
        {missing_checked, AllCount},
        {missing_found, map_size(MissingRes)}
    ]),
    ok = sum_stats(Parent, Stats),
    {MissingRes, St1}.

find_missing(Revs, Target) ->
    case couch_replicator_api_wrap:get_missing_revs(Target, Revs) of
        {ok, Missing} ->
            % Turn {Id, [Rev1, Rev2, ...], PAs} into a map:
            % #{{Id, Rev1} => PAs, {Id, Rev2} => PAs, ...}
            id_rev_map(Missing);
        {error, Error} ->
            exit(Error)
    end.

id_rev_map(IdRevs) ->
    id_rev_map(IdRevs, #{}).

id_rev_map([], #{} = Acc) ->
    Acc;
id_rev_map([{_, [], _} | Docs], #{} = Acc) ->
    id_rev_map(Docs, Acc);
id_rev_map([{Id, [Rev | Revs], PAs} | Docs], #{} = Acc) ->
    id_rev_map([{Id, Revs, PAs} | Docs], Acc#{{Id, Rev} => PAs}).

maybe_report_stats(#state{} = State) ->
    #state{cp = Cp, stats = Stats, last_stats_report_sec = LastReport} = State,
    Now = erlang:monotonic_time(second),
    case Now - LastReport >= ?STATS_DELAY_SEC of
        true ->
            ok = couch_replicator_scheduler_job:sum_stats(Cp, Stats),
            NewStats = couch_replicator_stats:new(),
            State#state{stats = NewStats, last_stats_report_sec = Now};
        false ->
            State
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

replication_worker_format_status_test() ->
    State = #state{
        cp = self(),
        loop = self(),
        source = #httpdb{url = "http://u:p@h/d1"},
        target = #httpdb{url = "http://u:p@h/d2"},
        readers = [r1, r2, r3],
        pending_fetch = nil,
        batch = #batch{size = 5}
    },
    Format = format_status(opts_ignored, [pdict, State]),
    ?assertEqual(self(), proplists:get_value(main_pid, Format)),
    ?assertEqual(self(), proplists:get_value(loop, Format)),
    ?assertEqual("http://u:*****@h/d1", proplists:get_value(source, Format)),
    ?assertEqual("http://u:*****@h/d2", proplists:get_value(target, Format)),
    ?assertEqual(3, proplists:get_value(num_readers, Format)),
    ?assertEqual(nil, proplists:get_value(pending_fetch, Format)),
    ?assertEqual(5, proplists:get_value(batch_size, Format)).

bulk_get_attempt_test() ->
    Now = erlang:monotonic_time(second),
    St = #fetch_stats{ratio = 0, tsec = Now},
    ?assert(attempt_bulk_get(St#fetch_stats{ratio = 0.1}, Now)),
    ?assertNot(attempt_bulk_get(St#fetch_stats{ratio = 0.9}, Now)),
    RetryTime = Now + ?BULK_GET_RETRY_SEC + 1,
    ?assert(attempt_bulk_get(St#fetch_stats{ratio = 0.9}, RetryTime)).

update_bulk_get_ratio_test() ->
    Init = #fetch_stats{ratio = 0, tsec = 0},
    Update = fun(St, Successes, Attempts) ->
        update_bulk_get_ratio(St, Successes, Attempts, 0)
    end,
    Seq = lists:seq(1, 100),

    % Almost all failures
    Fail = lists:foldl(fun(_, Acc) -> Update(Acc, 1, 1000) end, Init, Seq),
    ?assert(Fail#fetch_stats.ratio > 0.9),

    % Almost all successes
    Success = lists:foldl(fun(_, Acc) -> Update(Acc, 900, 1000) end, Init, Seq),
    ?assert(Success#fetch_stats.ratio < 0.1),

    % Half and half
    Half = lists:foldl(fun(_, Acc) -> Update(Acc, 500, 1000) end, Init, Seq),
    ?assert(Half#fetch_stats.ratio > 0.49),
    ?assert(Half#fetch_stats.ratio < 0.51),

    % Successes after failures
    FailSuccess = lists:foldl(fun(_, Acc) -> Update(Acc, 1000, 1000) end, Fail, Seq),
    ?assert(FailSuccess#fetch_stats.ratio < 0.1),

    % Failures after success
    SuccessFailure = lists:foldl(fun(_, Acc) -> Update(Acc, 0, 1000) end, Success, Seq),
    ?assert(SuccessFailure#fetch_stats.ratio > 0.9),

    % 0 attempts doesn't crash with a division by 0
    ZeroAttempts = Update(Init, 0, 0),
    ?assertEqual(0.0, ZeroAttempts#fetch_stats.ratio).

-endif.
