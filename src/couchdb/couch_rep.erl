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

-export([replicate/2]).

-include("couch_db.hrl").

-record(state, {
    changes_feed,
    missing_revs,
    reader,
    writer,

    source,
    target,
    init_args,

    start_seq,
    history,
    source_log,
    target_log,
    rep_starttime,
    src_starttime,
    tgt_starttime,
    checkpoint_history = nil,

    listeners = [],
    complete = false,
    committed_seq = 0,

    stats = nil
}).

%% convenience function to do a simple replication from the shell
replicate(Source, Target) when is_list(Source) ->
    replicate(?l2b(Source), Target);
replicate(Source, Target) when is_binary(Source), is_list(Target) ->
    replicate(Source, ?l2b(Target));
replicate(Source, Target) when is_binary(Source), is_binary(Target) ->
    replicate({[{<<"source">>, Source}, {<<"target">>, Target}]}, #user_ctx{});

%% function handling POST to _replicate
replicate(PostBody, UserCtx) ->
    RepId = make_replication_id(PostBody, UserCtx),
    Replicator = {RepId,
        {gen_server, start_link, [?MODULE, [RepId, PostBody, UserCtx], []]},
        transient,
        1,
        worker,
        [?MODULE]
    },

    Server = start_replication_server(Replicator),

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
    try do_init(InitArgs)
    catch throw:{db_not_found, DbUrl} -> {stop, {db_not_found, DbUrl}} end.

do_init([RepId, {PostProps}, UserCtx] = InitArgs) ->
    process_flag(trap_exit, true),

    SourceProps = proplists:get_value(<<"source">>, PostProps),
    TargetProps = proplists:get_value(<<"target">>, PostProps),

    Source = open_db(SourceProps, UserCtx),
    Target = open_db(TargetProps, UserCtx),

    SourceLog = open_replication_log(Source, RepId),
    TargetLog = open_replication_log(Target, RepId),

    SourceInfo = dbinfo(Source),
    TargetInfo = dbinfo(Target),
    
    {StartSeq, History} = compare_replication_logs(SourceLog, TargetLog),

    {ok, ChangesFeed} =
    couch_rep_changes_feed:start_link(self(), Source, StartSeq, PostProps),
    {ok, MissingRevs} =
    couch_rep_missing_revs:start_link(self(), Target, ChangesFeed, PostProps),
    {ok, Reader} =
    couch_rep_reader:start_link(self(), Source, MissingRevs, PostProps),
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
        init_args = InitArgs,
        stats = Stats,

        start_seq = StartSeq,
        history = History,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = httpd_util:rfc1123_date(),
        src_starttime = proplists:get_value(instance_start_time, SourceInfo),
        tgt_starttime = proplists:get_value(instance_start_time, TargetInfo)
    },
    {ok, State}.

handle_call(get_result, From, #state{complete=true, listeners=[]} = State) ->
    {stop, normal, State#state{listeners=[From]}};
handle_call(get_result, From, State) ->
    Listeners = State#state.listeners,
    {noreply, State#state{listeners=[From|Listeners]}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({missing_revs_checkpoint, SourceSeq}, State) ->
    couch_task_status:update("MR Processed source update #~p", [SourceSeq]),
    {noreply, do_checkpoint(State#state{committed_seq = SourceSeq})};

handle_info({writer_checkpoint, SourceSeq}, #state{committed_seq=N} = State)
        when SourceSeq > N ->
    MissingRevs = State#state.missing_revs,
    ok = gen_server:cast(MissingRevs, {update_committed_seq, SourceSeq}),
    couch_task_status:update("W Processed source update #~p", [SourceSeq]),
    {noreply, do_checkpoint(State#state{committed_seq = SourceSeq})};
handle_info({writer_checkpoint, _}, State) ->
    {noreply, State};

handle_info({update_stats, Key, N}, State) ->
    ets:update_counter(State#state.stats, Key, N),
    {noreply, State};

handle_info({'EXIT', Writer, normal}, #state{writer=Writer} = State) ->
    case State#state.listeners of
    [] ->
        {noreply, State#state{complete = true}};
    _Else ->
        {stop, normal, State}
    end;

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    ?LOG_ERROR("exit of linked Pid ~p with reason ~p", [Pid, Reason]),
    {stop, Reason, State}.

terminate(normal, State) ->
    #state{
        checkpoint_history = CheckpointHistory,
        committed_seq = NewSeq,
        listeners = Listeners,
        source = Source,
        target = Target,
        stats = Stats,
        source_log = #doc{body={OldHistory}}
    } = State,
    couch_task_status:update("Finishing"),
    ets:delete(Stats),
    close_db(Target),
    
    NewRepHistory = case CheckpointHistory of
    nil ->
        {[{<<"no_changes">>, true} | OldHistory]};
    _Else ->
        CheckpointHistory
    end,

    %% reply to original requester
    [Original|OtherListeners] = lists:reverse(Listeners),
    gen_server:reply(Original, {ok, NewRepHistory}),

    %% maybe trigger another replication. If this replicator uses a local
    %% source Db, changes to that Db since we started will not be included in
    %% this pass.
    case up_to_date(Source, NewSeq) of
        true ->
            [gen_server:reply(R, {ok, NewRepHistory}) || R <- OtherListeners];
        false ->
            [gen_server:reply(R, retry) || R <- OtherListeners]
    end,
    close_db(Source);

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
            Pid
        end;
    {error, {already_started, Pid}} ->
        ?LOG_DEBUG("replication ~p already running at ~p", [RepId, Pid]),
        Pid;
    {error, {{db_not_found, DbUrl}, _}} ->
        throw({db_not_found, <<"could not open ", DbUrl/binary>>})
    end.

compare_replication_logs(SrcDoc, TgtDoc) ->
    #doc{body={RepRecProps}} = SrcDoc,
    #doc{body={RepRecPropsTgt}} = TgtDoc,
    case proplists:get_value(<<"session_id">>, RepRecProps) ==
            proplists:get_value(<<"session_id">>, RepRecPropsTgt) of
    true ->
        % if the records have the same session id,
        % then we have a valid replication history
        OldSeqNum = proplists:get_value(<<"source_last_seq">>, RepRecProps, 0),
        OldHistory = proplists:get_value(<<"history">>, RepRecProps, []),
        {OldSeqNum, OldHistory};
    false ->
        SourceHistory = proplists:get_value(<<"history">>, RepRecProps, []),
        TargetHistory = proplists:get_value(<<"history">>, RepRecPropsTgt, []),
        ?LOG_INFO("Replication records differ. "
                "Scanning histories to find a common ancestor.", []),
        ?LOG_DEBUG("Record on source:~p~nRecord on target:~p~n",
                [RepRecProps, RepRecPropsTgt]),
        compare_rep_history(SourceHistory, TargetHistory)
    end.

compare_rep_history(S, T) when length(S) =:= 0 orelse length(T) =:= 0 ->
    ?LOG_INFO("no common ancestry -- performing full replication", []),
    {0, []};
compare_rep_history([{S}|SourceRest], [{T}|TargetRest]=Target) ->
    SourceId = proplists:get_value(<<"session_id">>, S),
    case has_session_id(SourceId, Target) of
    true ->
        RecordSeqNum = proplists:get_value(<<"recorded_seq">>, S, 0),
        ?LOG_INFO("found a common replication record with source_seq ~p",
            [RecordSeqNum]),
        {RecordSeqNum, SourceRest};
    false ->
        TargetId = proplists:get_value(<<"session_id">>, T),
        case has_session_id(TargetId, SourceRest) of
        true ->
            RecordSeqNum = proplists:get_value(<<"recorded_seq">>, T, 0),
            ?LOG_INFO("found a common replication record with source_seq ~p",
                [RecordSeqNum]),
            {RecordSeqNum, TargetRest};
        false ->
            compare_rep_history(SourceRest, TargetRest)
        end
    end.

close_db(#http_db{})->
    ok;
close_db(Db)->
    couch_db:close(Db).

dbname(#http_db{} = Db) ->
    Db#http_db.url;
dbname(Db) ->
    Db#db.name.

dbinfo(#http_db{} = Db) ->
    {DbProps} = couch_rep_httpc:request(Db),
    [{list_to_atom(?b2l(K)), V} || {K,V} <- DbProps];
dbinfo(Db) ->
    {ok, Info} = couch_db:get_db_info(Db),
    Info.

has_session_id(_SessionId, []) ->
    false;
has_session_id(SessionId, [{Props} | Rest]) ->
    case proplists:get_value(<<"session_id">>, Props, nil) of
    SessionId ->
        true;
    _Else ->
        has_session_id(SessionId, Rest)
    end.

make_replication_id({Props}, UserCtx) ->
    %% funky algorithm to preserve backwards compatibility
    {ok, HostName} = inet:gethostname(),
    Src = get_rep_endpoint(UserCtx, proplists:get_value(<<"source">>, Props)),
    Tgt = get_rep_endpoint(UserCtx, proplists:get_value(<<"target">>, Props)),    
    couch_util:to_hex(erlang:md5(term_to_binary([HostName, Src, Tgt]))).

maybe_add_trailing_slash(Url) ->
    re:replace(Url, "[^/]$", "&/", [{return, list}]).

get_rep_endpoint(_UserCtx, {Props}) ->
    Url = maybe_add_trailing_slash(proplists:get_value(<<"url">>, Props)),
    {BinHeaders} = proplists:get_value(<<"headers">>, Props, {[]}),
    {Auth} = proplists:get_value(<<"auth">>, Props, {[]}),
    case proplists:get_value(<<"oauth">>, Auth) of
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

open_db({Props}, _UserCtx) ->
    Url = maybe_add_trailing_slash(proplists:get_value(<<"url">>, Props)),
    {AuthProps} = proplists:get_value(<<"auth">>, Props, {[]}),
    {BinHeaders} = proplists:get_value(<<"headers">>, Props, {[]}),
    Headers = [{?b2l(K),?b2l(V)} || {K,V} <- BinHeaders],
    DefaultHeaders = (#http_db{})#http_db.headers,
    Db = #http_db{
        url = Url,
        auth = AuthProps,
        headers = lists:ukeymerge(1, Headers, DefaultHeaders)
    },
    case couch_rep_httpc:db_exists(Db) of
    true -> Db;
    false -> throw({db_not_found, ?l2b(Url)})
    end;
open_db(<<"http://",_/binary>>=Url, _) ->
    open_db({[{<<"url">>,Url}]}, []);
open_db(<<"https://",_/binary>>=Url, _) ->
    open_db({[{<<"url">>,Url}]}, []);
open_db(<<DbName/binary>>, UserCtx) ->
    case couch_db:open(DbName, [{user_ctx, UserCtx}]) of
    {ok, Db} -> Db;
    {not_found, no_db_file} -> throw({db_not_found, DbName})
    end.

do_checkpoint(State) ->
    #state{
        source = Source,
        target = Target,
        committed_seq = NewSeqNum,
        start_seq = StartSeqNum,
        history = OldHistory,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = ReplicationStartTime,
        src_starttime = SrcInstanceStartTime,
        tgt_starttime = TgtInstanceStartTime,
        stats = Stats
    } = State,
    ?LOG_INFO("recording a checkpoint at source update_seq ~p", [NewSeqNum]),
    RecordSeqNum = case commit_to_both(Source, Target) of
    {SrcInstanceStartTime, TgtInstanceStartTime} ->
        NewSeqNum;
    _Else ->
        ?LOG_INFO("A server has restarted sinced replication start. "
            "Not recording the new sequence number to ensure the "
            "replication is redone and documents reexamined.", []),
        StartSeqNum
    end,
    SessionId = couch_util:new_uuid(),
    NewHistoryEntry = {[
        {<<"session_id">>, SessionId},
        {<<"start_time">>, list_to_binary(ReplicationStartTime)},
        {<<"end_time">>, list_to_binary(httpd_util:rfc1123_date())},
        {<<"start_last_seq">>, StartSeqNum},
        {<<"end_last_seq">>, NewSeqNum},
        {<<"recorded_seq">>, RecordSeqNum},
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
        {<<"source_last_seq">>, RecordSeqNum},
        {<<"history">>, lists:sublist([NewHistoryEntry | OldHistory], 50)}
    ]},

    try
    {SrcRevPos,SrcRevId} =
        update_doc(Source, SourceLog#doc{body=NewRepHistory}, []),
    {TgtRevPos,TgtRevId} =
        update_doc(Target, TargetLog#doc{body=NewRepHistory}, []),
    State#state{
        checkpoint_history = NewRepHistory,
        source_log = SourceLog#doc{revs={SrcRevPos, [SrcRevId]}},
        target_log = TargetLog#doc{revs={TgtRevPos, [TgtRevId]}}
    }
    catch throw:conflict ->
    ?LOG_ERROR("checkpoint failure: conflict (are you replicating to yourself?)",
        []),
    State
    end.

commit_to_both(Source, Target) ->
    % commit the src async
    ParentPid = self(),
    SrcCommitPid = spawn_link(fun() ->
            ParentPid ! {self(), ensure_full_commit(Source)} end),

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
    
ensure_full_commit(#http_db{} = Db) ->
    Req = Db#http_db{
        resource = "_ensure_full_commit",
        method = post,
        body = true
    },
    {ResultProps} = couch_rep_httpc:request(Req),
    true = proplists:get_value(<<"ok">>, ResultProps),
    proplists:get_value(<<"instance_start_time">>, ResultProps);
ensure_full_commit(Db) ->
    {ok, StartTime} = couch_db:ensure_full_commit(Db),
    StartTime.

update_doc(#http_db{} = Db, #doc{id=DocId} = Doc, []) ->
    Req = Db#http_db{
        resource = couch_util:url_encode(DocId),
        method = put,
        body = couch_doc:to_json_obj(Doc, [attachments])
    },
    {ResponseMembers} = couch_rep_httpc:request(Req),
    Rev = proplists:get_value(<<"rev">>, ResponseMembers),
    couch_doc:parse_rev(Rev);
update_doc(Db, Doc, Options) ->
    {ok, Result} = couch_db:update_doc(Db, Doc, Options),
    Result.

up_to_date(#http_db{}, _Seq) ->
    true;
up_to_date(Source, Seq) ->
    {ok, NewDb} = couch_db:open(Source#db.name, []),
    T = NewDb#db.update_seq == Seq,
    couch_db:close(NewDb),
    T.
