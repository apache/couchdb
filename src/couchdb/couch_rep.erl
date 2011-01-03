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

-module(couch_rep).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([replicate/2, checkpoint/1]).

-include("couch_db.hrl").

-record(state, {
    changes_feed,
    missing_revs,
    reader,
    writer,

    source,
    target,
    continuous,
    create_target,
    init_args,
    checkpoint_scheduled = nil,

    start_seq,
    history,
    session_id,
    source_log,
    target_log,
    rep_starttime,
    src_starttime,
    tgt_starttime,
    checkpoint_history = nil,

    listeners = [],
    complete = false,
    committed_seq = 0,

    stats = nil,
    doc_ids = nil,
    source_db_update_notifier = nil,
    target_db_update_notifier = nil
}).

%% convenience function to do a simple replication from the shell
replicate(Source, Target) when is_list(Source) ->
    replicate(?l2b(Source), Target);
replicate(Source, Target) when is_binary(Source), is_list(Target) ->
    replicate(Source, ?l2b(Target));
replicate(Source, Target) when is_binary(Source), is_binary(Target) ->
    replicate({[{<<"source">>, Source}, {<<"target">>, Target}]}, #user_ctx{});

%% function handling POST to _replicate
replicate({Props}=PostBody, UserCtx) ->
    {BaseId, Extension} = make_replication_id(PostBody, UserCtx),
    Replicator = {BaseId ++ Extension,
        {gen_server, start_link, [?MODULE, [BaseId, PostBody, UserCtx], []]},
        temporary,
        1,
        worker,
        [?MODULE]
    },

    case couch_util:get_value(<<"cancel">>, Props, false) of
    true ->
 case supervisor:terminate_child(couch_rep_sup, BaseId ++ Extension) of
        {error, not_found} ->
     {error, not_found};
        ok ->
     ok = supervisor:delete_child(couch_rep_sup, BaseId ++ Extension),
            {ok, {cancelled, ?l2b(BaseId)}}
 end;
    false ->
        Server = start_replication_server(Replicator),

        case couch_util:get_value(<<"continuous">>, Props, false) of
        true ->
            {ok, {continuous, ?l2b(BaseId)}};
        false ->
            get_result(Server, PostBody, UserCtx)
        end
    end.

checkpoint(Server) ->
    gen_server:cast(Server, do_checkpoint).

get_result(Server, PostBody, UserCtx) ->
    try gen_server:call(Server, get_result, infinity) of
    retry -> replicate(PostBody, UserCtx);
    Else -> Else
    catch
    exit:{noproc, {gen_server, call, [Server, get_result , infinity]}} ->
        %% oops, this replication just finished -- restart it.
        replicate(PostBody, UserCtx);
    exit:{normal, {gen_server, call, [Server, get_result , infinity]}} ->
        %% we made the call during terminate
        replicate(PostBody, UserCtx)
    end.

init(InitArgs) ->
    try
        do_init(InitArgs)
    catch
    throw:Error ->
        {stop, Error}
    end.

do_init([RepId, {PostProps}, UserCtx] = InitArgs) ->
    process_flag(trap_exit, true),

    SourceProps = couch_util:get_value(<<"source">>, PostProps),
    TargetProps = couch_util:get_value(<<"target">>, PostProps),

    DocIds = couch_util:get_value(<<"doc_ids">>, PostProps, nil),
    Continuous = couch_util:get_value(<<"continuous">>, PostProps, false),
    CreateTarget = couch_util:get_value(<<"create_target">>, PostProps, false),

    ProxyParams = parse_proxy_params(
        couch_util:get_value(<<"proxy">>, PostProps, [])),
    Source = open_db(SourceProps, UserCtx, ProxyParams),
    Target = open_db(TargetProps, UserCtx, ProxyParams, CreateTarget),

    SourceInfo = dbinfo(Source),
    TargetInfo = dbinfo(Target),

    case DocIds of
    List when is_list(List) ->
        % Fast replication using only a list of doc IDs to replicate.
        % Replication sessions, checkpoints and logs are not created
        % since the update sequence number of the source DB is not used
        % for determining which documents are copied into the target DB.
        SourceLog = nil,
        TargetLog = nil,

        StartSeq = nil,
        History = nil,

        ChangesFeed = nil,
        MissingRevs = nil,

        {ok, Reader} =
        couch_rep_reader:start_link(self(), Source, DocIds, PostProps);

    _ ->
        % Replication using the _changes API (DB sequence update numbers).
        SourceLog = open_replication_log(Source, RepId),
        TargetLog = open_replication_log(Target, RepId),
    
        {StartSeq, History} = compare_replication_logs(SourceLog, TargetLog),

        {ok, ChangesFeed} =
        couch_rep_changes_feed:start_link(self(), Source, StartSeq, PostProps),
        {ok, MissingRevs} =
        couch_rep_missing_revs:start_link(self(), Target, ChangesFeed, PostProps),
        {ok, Reader} =
        couch_rep_reader:start_link(self(), Source, MissingRevs, PostProps)
    end,

    {ok, Writer} =
    couch_rep_writer:start_link(self(), Target, Reader, PostProps),

    Stats = ets:new(replication_stats, [set, private]),
    ets:insert(Stats, {total_revs,0}),
    ets:insert(Stats, {missing_revs, 0}),
    ets:insert(Stats, {docs_read, 0}),
    ets:insert(Stats, {docs_written, 0}),
    ets:insert(Stats, {doc_write_failures, 0}),

    {ShortId, _} = lists:split(6, RepId),
    couch_task_status:add_task("Replication", io_lib:format("~s: ~s -> ~s",
        [ShortId, dbname(Source), dbname(Target)]), "Starting"),

    State = #state{
        changes_feed = ChangesFeed,
        missing_revs = MissingRevs,
        reader = Reader,
        writer = Writer,

        source = Source,
        target = Target,
        continuous = Continuous,
        create_target = CreateTarget,
        init_args = InitArgs,
        stats = Stats,
        checkpoint_scheduled = nil,

        start_seq = StartSeq,
        history = History,
        session_id = couch_uuids:random(),
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = httpd_util:rfc1123_date(),
        src_starttime = couch_util:get_value(instance_start_time, SourceInfo),
        tgt_starttime = couch_util:get_value(instance_start_time, TargetInfo),
        doc_ids = DocIds,
        source_db_update_notifier = source_db_update_notifier(Source),
        target_db_update_notifier = target_db_update_notifier(Target)
    },
    {ok, State}.

handle_call(get_result, From, #state{complete=true, listeners=[]} = State) ->
    {stop, normal, State#state{listeners=[From]}};
handle_call(get_result, From, State) ->
    Listeners = State#state.listeners,
    {noreply, State#state{listeners=[From|Listeners]}};

handle_call(get_source_db, _From, #state{source = Source} = State) ->
    {reply, {ok, Source}, State};

handle_call(get_target_db, _From, #state{target = Target} = State) ->
    {reply, {ok, Target}, State}.

handle_cast(reopen_source_db, #state{source = Source} = State) ->
    {ok, NewSource} = couch_db:reopen(Source),
    {noreply, State#state{source = NewSource}};

handle_cast(reopen_target_db, #state{target = Target} = State) ->
    {ok, NewTarget} = couch_db:reopen(Target),
    {noreply, State#state{target = NewTarget}};

handle_cast(do_checkpoint, State) ->
    {noreply, do_checkpoint(State)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({missing_revs_checkpoint, SourceSeq}, State) ->
    couch_task_status:update("MR Processed source update #~p", [SourceSeq]),
    {noreply, schedule_checkpoint(State#state{committed_seq = SourceSeq})};

handle_info({writer_checkpoint, SourceSeq}, #state{committed_seq=N} = State)
        when SourceSeq > N ->
    MissingRevs = State#state.missing_revs,
    ok = gen_server:cast(MissingRevs, {update_committed_seq, SourceSeq}),
    couch_task_status:update("W Processed source update #~p", [SourceSeq]),
    {noreply, schedule_checkpoint(State#state{committed_seq = SourceSeq})};
handle_info({writer_checkpoint, _}, State) ->
    {noreply, State};

handle_info({update_stats, Key, N}, State) ->
    ets:update_counter(State#state.stats, Key, N),
    {noreply, State};

handle_info({'DOWN', _, _, _, _}, State) ->
    ?LOG_INFO("replication terminating because local DB is shutting down", []),
    timer:cancel(State#state.checkpoint_scheduled),
    {stop, shutdown, State};

handle_info({'EXIT', Writer, normal}, #state{writer=Writer} = State) ->
    case State#state.listeners of
    [] ->
        {noreply, State#state{complete = true}};
    _Else ->
        {stop, normal, State}
    end;

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, {Err, Reason}}, State) when Err == source_error;
        Err == target_error ->
    ?LOG_INFO("replication terminating due to ~p: ~p", [Err, Reason]),
    timer:cancel(State#state.checkpoint_scheduled),
    {stop, shutdown, State};
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

terminate(normal, #state{checkpoint_scheduled=nil} = State) ->
    do_terminate(State);
    
terminate(normal, State) ->
    timer:cancel(State#state.checkpoint_scheduled),
    do_terminate(do_checkpoint(State));

terminate(Reason, State) ->
    #state{
        listeners = Listeners,
        source = Source,
        target = Target,
        stats = Stats
    } = State,
    [gen_server:reply(L, {error, Reason}) || L <- Listeners],
    ets:delete(Stats),
    close_db(Target),
    close_db(Source).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal funs

start_replication_server(Replicator) ->
    RepId = element(1, Replicator),
    case supervisor:start_child(couch_rep_sup, Replicator) of
    {ok, Pid} ->
        ?LOG_INFO("starting new replication ~p at ~p", [RepId, Pid]),
        Pid;
    {error, already_present} ->
        case supervisor:restart_child(couch_rep_sup, RepId) of
        {ok, Pid} ->
            ?LOG_INFO("starting replication ~p at ~p", [RepId, Pid]),
            Pid;
        {error, running} ->
            %% this error occurs if multiple replicators are racing
            %% each other to start and somebody else won.  Just grab
            %% the Pid by calling start_child again.
            {error, {already_started, Pid}} =
                supervisor:start_child(couch_rep_sup, Replicator),
            ?LOG_DEBUG("replication ~p already running at ~p", [RepId, Pid]),
            Pid;
        {error, {db_not_found, DbUrl}} ->
            throw({db_not_found, <<"could not open ", DbUrl/binary>>});
        {error, {unauthorized, DbUrl}} ->
            throw({unauthorized,
                <<"unauthorized to access database ", DbUrl/binary>>})
        end;
    {error, {already_started, Pid}} ->
        ?LOG_DEBUG("replication ~p already running at ~p", [RepId, Pid]),
        Pid;
    {error, {{db_not_found, DbUrl}, _}} ->
        throw({db_not_found, <<"could not open ", DbUrl/binary>>});
    {error, {{unauthorized, DbUrl}, _}} ->
        throw({unauthorized,
            <<"unauthorized to access database ", DbUrl/binary>>})
    end.

compare_replication_logs(SrcDoc, TgtDoc) ->
    #doc{body={RepRecProps}} = SrcDoc,
    #doc{body={RepRecPropsTgt}} = TgtDoc,
    case couch_util:get_value(<<"session_id">>, RepRecProps) ==
            couch_util:get_value(<<"session_id">>, RepRecPropsTgt) of
    true ->
        % if the records have the same session id,
        % then we have a valid replication history
        OldSeqNum = couch_util:get_value(<<"source_last_seq">>, RepRecProps, 0),
        OldHistory = couch_util:get_value(<<"history">>, RepRecProps, []),
        {OldSeqNum, OldHistory};
    false ->
        SourceHistory = couch_util:get_value(<<"history">>, RepRecProps, []),
        TargetHistory = couch_util:get_value(<<"history">>, RepRecPropsTgt, []),
        ?LOG_INFO("Replication records differ. "
                "Scanning histories to find a common ancestor.", []),
        ?LOG_DEBUG("Record on source:~p~nRecord on target:~p~n",
                [RepRecProps, RepRecPropsTgt]),
        compare_rep_history(SourceHistory, TargetHistory)
    end.

compare_rep_history(S, T) when S =:= [] orelse T =:= [] ->
    ?LOG_INFO("no common ancestry -- performing full replication", []),
    {0, []};
compare_rep_history([{S}|SourceRest], [{T}|TargetRest]=Target) ->
    SourceId = couch_util:get_value(<<"session_id">>, S),
    case has_session_id(SourceId, Target) of
    true ->
        RecordSeqNum = couch_util:get_value(<<"recorded_seq">>, S, 0),
        ?LOG_INFO("found a common replication record with source_seq ~p",
            [RecordSeqNum]),
        {RecordSeqNum, SourceRest};
    false ->
        TargetId = couch_util:get_value(<<"session_id">>, T),
        case has_session_id(TargetId, SourceRest) of
        true ->
            RecordSeqNum = couch_util:get_value(<<"recorded_seq">>, T, 0),
            ?LOG_INFO("found a common replication record with source_seq ~p",
                [RecordSeqNum]),
            {RecordSeqNum, TargetRest};
        false ->
            compare_rep_history(SourceRest, TargetRest)
        end
    end.

close_db(#http_db{}) ->
    ok;
close_db(Db) ->
    couch_db:close(Db).

dbname(#http_db{url = Url}) ->
    couch_util:url_strip_password(Url);
dbname(#db{name = Name}) ->
    Name.

dbinfo(#http_db{} = Db) ->
    {DbProps} = couch_rep_httpc:request(Db),
    [{list_to_existing_atom(?b2l(K)), V} || {K,V} <- DbProps];
dbinfo(Db) ->
    {ok, Info} = couch_db:get_db_info(Db),
    Info.

do_terminate(#state{doc_ids=DocIds} = State) when is_list(DocIds) ->
    #state{
        listeners = Listeners,
        rep_starttime = ReplicationStartTime,
        stats = Stats
    } = State,

    RepByDocsJson = {[
        {<<"start_time">>, ?l2b(ReplicationStartTime)},
        {<<"end_time">>, ?l2b(httpd_util:rfc1123_date())},
        {<<"docs_read">>, ets:lookup_element(Stats, docs_read, 2)},
        {<<"docs_written">>, ets:lookup_element(Stats, docs_written, 2)},
        {<<"doc_write_failures">>,
            ets:lookup_element(Stats, doc_write_failures, 2)}
    ]},

    terminate_cleanup(State),
    [gen_server:reply(L, {ok, RepByDocsJson}) || L <- lists:reverse(Listeners)];

do_terminate(State) ->
    #state{
        checkpoint_history = CheckpointHistory,
        committed_seq = NewSeq,
        listeners = Listeners,
        source = Source,
        continuous = Continuous,
        source_log = #doc{body={OldHistory}}
    } = State,
    
    NewRepHistory = case CheckpointHistory of
    nil ->
        {[{<<"no_changes">>, true} | OldHistory]};
    _Else ->
        CheckpointHistory
    end,

    %% reply to original requester
    OtherListeners = case Continuous of
    true ->
        []; % continuous replications have no listeners
    _ ->
        [Original|Rest] = lists:reverse(Listeners),
        gen_server:reply(Original, {ok, NewRepHistory}),
        Rest
    end,

    %% maybe trigger another replication. If this replicator uses a local
    %% source Db, changes to that Db since we started will not be included in
    %% this pass.
    case up_to_date(Source, NewSeq) of
        true ->
            [gen_server:reply(R, {ok, NewRepHistory}) || R <- OtherListeners];
        false ->
            [gen_server:reply(R, retry) || R <- OtherListeners]
    end,
    couch_task_status:update("Finishing"),
    terminate_cleanup(State).

terminate_cleanup(State) ->
    close_db(State#state.source),
    close_db(State#state.target),
    stop_db_update_notifier(State#state.source_db_update_notifier),
    stop_db_update_notifier(State#state.target_db_update_notifier),
    ets:delete(State#state.stats).

stop_db_update_notifier(nil) ->
    ok;
stop_db_update_notifier(Notifier) ->
    couch_db_update_notifier:stop(Notifier).

has_session_id(_SessionId, []) ->
    false;
has_session_id(SessionId, [{Props} | Rest]) ->
    case couch_util:get_value(<<"session_id">>, Props, nil) of
    SessionId ->
        true;
    _Else ->
        has_session_id(SessionId, Rest)
    end.

maybe_append_options(Options, Props) ->
    lists:foldl(fun(Option, Acc) ->
        Acc ++
        case couch_util:get_value(Option, Props, false) of
        true ->
            "+" ++ ?b2l(Option);
        false ->
            ""
        end
    end, [], Options).

make_replication_id({Props}, UserCtx) ->
    %% funky algorithm to preserve backwards compatibility
    {ok, HostName} = inet:gethostname(),
    % Port = mochiweb_socket_server:get(couch_httpd, port),
    Src = get_rep_endpoint(UserCtx, couch_util:get_value(<<"source">>, Props)),
    Tgt = get_rep_endpoint(UserCtx, couch_util:get_value(<<"target">>, Props)),
    Base = [HostName, Src, Tgt] ++
        case couch_util:get_value(<<"filter">>, Props) of
        undefined ->
            case couch_util:get_value(<<"doc_ids">>, Props) of
            undefined ->
                [];
            DocIds ->
                [DocIds]
            end;
        Filter ->
            [Filter, couch_util:get_value(<<"query_params">>, Props, {[]})]
        end,
    Extension = maybe_append_options(
        [<<"continuous">>, <<"create_target">>], Props),
    {couch_util:to_hex(couch_util:md5(term_to_binary(Base))), Extension}.

maybe_add_trailing_slash(Url) ->
    re:replace(Url, "[^/]$", "&/", [{return, list}]).

get_rep_endpoint(_UserCtx, {Props}) ->
    Url = maybe_add_trailing_slash(couch_util:get_value(<<"url">>, Props)),
    {BinHeaders} = couch_util:get_value(<<"headers">>, Props, {[]}),
    {Auth} = couch_util:get_value(<<"auth">>, Props, {[]}),
    case couch_util:get_value(<<"oauth">>, Auth) of
    undefined ->
        {remote, Url, [{?b2l(K),?b2l(V)} || {K,V} <- BinHeaders]};
    {OAuth} ->
        {remote, Url, [{?b2l(K),?b2l(V)} || {K,V} <- BinHeaders], OAuth}
    end;
get_rep_endpoint(_UserCtx, <<"http://",_/binary>>=Url) ->
    {remote, maybe_add_trailing_slash(Url), []};
get_rep_endpoint(_UserCtx, <<"https://",_/binary>>=Url) ->
    {remote, maybe_add_trailing_slash(Url), []};
get_rep_endpoint(UserCtx, <<DbName/binary>>) ->
    {local, DbName, UserCtx}.

open_replication_log(#http_db{}=Db, RepId) ->
    DocId = ?LOCAL_DOC_PREFIX ++ RepId,
    Req = Db#http_db{resource=couch_util:url_encode(DocId)},
    case couch_rep_httpc:request(Req) of
    {[{<<"error">>, _}, {<<"reason">>, _}]} ->
        ?LOG_DEBUG("didn't find a replication log for ~s", [Db#http_db.url]),
        #doc{id=?l2b(DocId)};
    Doc ->
        ?LOG_DEBUG("found a replication log for ~s", [Db#http_db.url]),
        couch_doc:from_json_obj(Doc)
    end;
open_replication_log(Db, RepId) ->
    DocId = ?l2b(?LOCAL_DOC_PREFIX ++ RepId),
    case couch_db:open_doc(Db, DocId, []) of
    {ok, Doc} ->
        ?LOG_DEBUG("found a replication log for ~s", [Db#db.name]),
        Doc;
    _ ->
        ?LOG_DEBUG("didn't find a replication log for ~s", [Db#db.name]),
        #doc{id=DocId}
    end.

open_db(Props, UserCtx, ProxyParams) ->
    open_db(Props, UserCtx, ProxyParams, false).

open_db({Props}, _UserCtx, ProxyParams, CreateTarget) ->
    Url = maybe_add_trailing_slash(couch_util:get_value(<<"url">>, Props)),
    {AuthProps} = couch_util:get_value(<<"auth">>, Props, {[]}),
    {BinHeaders} = couch_util:get_value(<<"headers">>, Props, {[]}),
    Headers = [{?b2l(K),?b2l(V)} || {K,V} <- BinHeaders],
    DefaultHeaders = (#http_db{})#http_db.headers,
    Db1 = #http_db{
        url = Url,
        auth = AuthProps,
        headers = lists:ukeymerge(1, Headers, DefaultHeaders)
    },
    Db = Db1#http_db{
        options = Db1#http_db.options ++ ProxyParams ++
            couch_rep_httpc:ssl_options(Db1)
    },
    couch_rep_httpc:db_exists(Db, CreateTarget);
open_db(<<"http://",_/binary>>=Url, _, ProxyParams, CreateTarget) ->
    open_db({[{<<"url">>,Url}]}, [], ProxyParams, CreateTarget);
open_db(<<"https://",_/binary>>=Url, _, ProxyParams, CreateTarget) ->
    open_db({[{<<"url">>,Url}]}, [], ProxyParams, CreateTarget);
open_db(<<DbName/binary>>, UserCtx, _ProxyParams, CreateTarget) ->
    try
        case CreateTarget of
        true ->
            ok = couch_httpd:verify_is_server_admin(UserCtx),
            couch_server:create(DbName, [{user_ctx, UserCtx}]);
        false ->
            ok
        end,

        case couch_db:open(DbName, [{user_ctx, UserCtx}]) of
        {ok, Db} ->
            couch_db:monitor(Db),
            Db;
        {not_found, no_db_file} ->
            throw({db_not_found, DbName})
        end
    catch throw:{unauthorized, _} ->
        throw({unauthorized, DbName})
    end.

schedule_checkpoint(#state{checkpoint_scheduled = nil} = State) ->
    Server = self(),
    case timer:apply_after(5000, couch_rep, checkpoint, [Server]) of
    {ok, TRef} ->
        State#state{checkpoint_scheduled = TRef};
    Error ->
        ?LOG_ERROR("tried to schedule a checkpoint but got ~p", [Error]),
        State
    end;
schedule_checkpoint(State) ->
    State.

do_checkpoint(State) ->
    #state{
        source = Source,
        target = Target,
        committed_seq = NewSeqNum,
        start_seq = StartSeqNum,
        history = OldHistory,
        session_id = SessionId,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = ReplicationStartTime,
        src_starttime = SrcInstanceStartTime,
        tgt_starttime = TgtInstanceStartTime,
        stats = Stats
    } = State,
    case commit_to_both(Source, Target, NewSeqNum) of
    {SrcInstanceStartTime, TgtInstanceStartTime} ->
        ?LOG_INFO("recording a checkpoint for ~s -> ~s at source update_seq ~p",
            [dbname(Source), dbname(Target), NewSeqNum]),
        NewHistoryEntry = {[
            {<<"session_id">>, SessionId},
            {<<"start_time">>, list_to_binary(ReplicationStartTime)},
            {<<"end_time">>, list_to_binary(httpd_util:rfc1123_date())},
            {<<"start_last_seq">>, StartSeqNum},
            {<<"end_last_seq">>, NewSeqNum},
            {<<"recorded_seq">>, NewSeqNum},
            {<<"missing_checked">>, ets:lookup_element(Stats, total_revs, 2)},
            {<<"missing_found">>, ets:lookup_element(Stats, missing_revs, 2)},
            {<<"docs_read">>, ets:lookup_element(Stats, docs_read, 2)},
            {<<"docs_written">>, ets:lookup_element(Stats, docs_written, 2)},
            {<<"doc_write_failures">>,
                ets:lookup_element(Stats, doc_write_failures, 2)}
        ]},
        % limit history to 50 entries
        NewRepHistory = {[
            {<<"session_id">>, SessionId},
            {<<"source_last_seq">>, NewSeqNum},
            {<<"history">>, lists:sublist([NewHistoryEntry | OldHistory], 50)}
        ]},

        try
        {SrcRevPos,SrcRevId} =
            update_local_doc(Source, SourceLog#doc{body=NewRepHistory}),
        {TgtRevPos,TgtRevId} =
            update_local_doc(Target, TargetLog#doc{body=NewRepHistory}),
        State#state{
            checkpoint_scheduled = nil,
            checkpoint_history = NewRepHistory,
            source_log = SourceLog#doc{revs={SrcRevPos, [SrcRevId]}},
            target_log = TargetLog#doc{revs={TgtRevPos, [TgtRevId]}}
        }
        catch throw:conflict ->
        ?LOG_ERROR("checkpoint failure: conflict (are you replicating to "
            "yourself?)", []),
        State
        end;
    _Else ->
        ?LOG_INFO("rebooting ~s -> ~s from last known replication checkpoint",
            [dbname(Source), dbname(Target)]),
        #state{
            changes_feed = CF,
            missing_revs = MR,
            reader = Reader,
            writer = Writer
        } = State,
        Pids = [Writer, Reader, MR, CF],
        [unlink(Pid) || Pid <- Pids],
        [exit(Pid, shutdown) || Pid <- Pids],
        close_db(Target),
        close_db(Source),
        {ok, NewState} = init(State#state.init_args),
        NewState#state{listeners=State#state.listeners}
    end.

commit_to_both(Source, Target, RequiredSeq) ->
    % commit the src async
    ParentPid = self(),
    SrcCommitPid = spawn_link(fun() ->
            ParentPid ! {self(), ensure_full_commit(Source, RequiredSeq)} end),

    % commit tgt sync
    TargetStartTime = ensure_full_commit(Target),

    SourceStartTime =
    receive
    {SrcCommitPid, Timestamp} ->
        Timestamp;
    {'EXIT', SrcCommitPid, {http_request_failed, _}} ->
        exit(replication_link_failure)
    end,
    {SourceStartTime, TargetStartTime}.
    
ensure_full_commit(#http_db{headers = Headers} = Target) ->
    Headers1 = [
        {"Content-Length", 0} |
        couch_util:proplist_apply_field(
            {"Content-Type", "application/json"}, Headers)
    ],
    Req = Target#http_db{
        resource = "_ensure_full_commit",
        method = post,
        headers = Headers1
    },
    {ResultProps} = couch_rep_httpc:request(Req),
    true = couch_util:get_value(<<"ok">>, ResultProps),
    couch_util:get_value(<<"instance_start_time">>, ResultProps);
ensure_full_commit(Target) ->
    {ok, NewDb} = couch_db:open_int(Target#db.name, []),
    UpdateSeq = couch_db:get_update_seq(Target),
    CommitSeq = couch_db:get_committed_update_seq(NewDb),
    InstanceStartTime = NewDb#db.instance_start_time,
    couch_db:close(NewDb),
    if UpdateSeq > CommitSeq ->
        ?LOG_DEBUG("target needs a full commit: update ~p commit ~p",
            [UpdateSeq, CommitSeq]),
        {ok, DbStartTime} = couch_db:ensure_full_commit(Target),
        DbStartTime;
    true ->
        ?LOG_DEBUG("target doesn't need a full commit", []),
        InstanceStartTime
    end.

ensure_full_commit(#http_db{headers = Headers} = Source, RequiredSeq) ->
    Headers1 = [
        {"Content-Length", 0} |
        couch_util:proplist_apply_field(
            {"Content-Type", "application/json"}, Headers)
    ],
    Req = Source#http_db{
        resource = "_ensure_full_commit",
        method = post,
        qs = [{seq, RequiredSeq}],
        headers = Headers1
    },
    {ResultProps} = couch_rep_httpc:request(Req),
    case couch_util:get_value(<<"ok">>, ResultProps) of
    true ->
        couch_util:get_value(<<"instance_start_time">>, ResultProps);
    undefined -> nil end;
ensure_full_commit(Source, RequiredSeq) ->
    {ok, NewDb} = couch_db:open_int(Source#db.name, []),
    CommitSeq = couch_db:get_committed_update_seq(NewDb),
    InstanceStartTime = NewDb#db.instance_start_time,
    couch_db:close(NewDb),
    if RequiredSeq > CommitSeq ->
        ?LOG_DEBUG("source needs a full commit: required ~p committed ~p",
            [RequiredSeq, CommitSeq]),
        {ok, DbStartTime} = couch_db:ensure_full_commit(Source),
        DbStartTime;
    true ->
        ?LOG_DEBUG("source doesn't need a full commit", []),
        InstanceStartTime
    end.

update_local_doc(#http_db{} = Db, #doc{id=DocId} = Doc) ->
    Req = Db#http_db{
        resource = couch_util:url_encode(DocId),
        method = put,
        body = couch_doc:to_json_obj(Doc, [attachments]),
        headers = [{"x-couch-full-commit", "false"} | Db#http_db.headers]
    },
    {ResponseMembers} = couch_rep_httpc:request(Req),
    Rev = couch_util:get_value(<<"rev">>, ResponseMembers),
    couch_doc:parse_rev(Rev);
update_local_doc(Db, Doc) ->
    {ok, Result} = couch_db:update_doc(Db, Doc, [delay_commit]),
    Result.

up_to_date(#http_db{}, _Seq) ->
    true;
up_to_date(Source, Seq) ->
    {ok, NewDb} = couch_db:open_int(Source#db.name, []),
    T = NewDb#db.update_seq == Seq,
    couch_db:close(NewDb),
    T.

parse_proxy_params(ProxyUrl) when is_binary(ProxyUrl) ->
    parse_proxy_params(?b2l(ProxyUrl));
parse_proxy_params([]) ->
    [];
parse_proxy_params(ProxyUrl) ->
    {url, _, Base, Port, User, Passwd, _Path, _Proto} =
        ibrowse_lib:parse_url(ProxyUrl),
    [{proxy_host, Base}, {proxy_port, Port}] ++
        case is_list(User) andalso is_list(Passwd) of
        false ->
            [];
        true ->
            [{proxy_user, User}, {proxy_password, Passwd}]
        end.

source_db_update_notifier(#db{name = DbName}) ->
    Server = self(),
    {ok, Notifier} = couch_db_update_notifier:start_link(
        fun({compacted, DbName1}) when DbName1 =:= DbName ->
                ok = gen_server:cast(Server, reopen_source_db);
            (_) ->
                ok
        end),
    Notifier;
source_db_update_notifier(_) ->
    nil.

target_db_update_notifier(#db{name = DbName}) ->
    Server = self(),
    {ok, Notifier} = couch_db_update_notifier:start_link(
        fun({compacted, DbName1}) when DbName1 =:= DbName ->
                ok = gen_server:cast(Server, reopen_target_db);
            (_) ->
                ok
        end),
    Notifier;
target_db_update_notifier(_) ->
    nil.
