% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_rep).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
    code_change/3]).

-export([replicate/2]).

-include_lib("couch_db.hrl").

%% @spec replicate(Source::binary(), Target::binary()) -> 
%%      {ok, Stats} | {error, Reason}
%% @doc Triggers a replication.  Stats is a JSON Object with the following 
%%      keys: session_id (UUID), source_last_seq (integer), and history (array).
%%      Each element of the history is an Object with keys start_time, end_time,
%%      start_last_seq, end_last_seq, missing_checked, missing_found, docs_read,
%%      and docs_written.
%%
%%      The supervisor will try to restart the replication in case of any error
%%      other than shutdown.  Just call this function again to listen for the 
%%      result of the retry.
replicate(Source, Target) ->
    
    {ok, HostName} = inet:gethostname(),
    RepId = couch_util:to_hex(
            erlang:md5(term_to_binary([HostName, Source, Target]))),
    Args = [?MODULE, [RepId, Source,Target], []],
    
    Replicator = {RepId,
        {gen_server, start_link, Args},
        transient,
        1,
        worker,
        [?MODULE]
    },
    
    Server = case supervisor:start_child(couch_rep_sup, Replicator) of
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
                    ?LOG_INFO("replication ~p already running at ~p", [RepId, Pid]),
                    Pid
            end;
        {error, {already_started, Pid}} -> 
            ?LOG_INFO("replication ~p already running at ~p", [RepId, Pid]),
            Pid
    end,
    
    case gen_server:call(Server, get_result, infinity) of 
        retry -> replicate(Source, Target);
        Else -> Else
    end.

%%=============================================================================
%% gen_server callbacks
%%=============================================================================

-record(http_db, {
    uri,
    headers
}).

    
-record(state, {
    context,
    current_seq,
    source,
    target,
    stats,
    enum_pid,
    docs_buffer = [],
    listeners = []
}).


init([RepId, Source, Target]) ->
    process_flag(trap_exit, true),
    
    {ok, DbSrc, SrcName} = open_db(Source),
    {ok, DbTgt, TgtName} =  open_db(Target),
    
    DocKey = ?l2b(?LOCAL_DOC_PREFIX ++ RepId),
    
    {ok, InfoSrc} = get_db_info(DbSrc),
    {ok, InfoTgt} = get_db_info(DbTgt),
    
    ReplicationStartTime = httpd_util:rfc1123_date(),
    SrcInstanceStartTime = proplists:get_value(instance_start_time, InfoSrc),
    TgtInstanceStartTime = proplists:get_value(instance_start_time, InfoTgt),
    
    RepRecDocSrc =
    case open_doc(DbSrc, DocKey, []) of
    {ok, SrcDoc} ->
        ?LOG_DEBUG("Found existing replication record on source", []),
        SrcDoc;
    _ -> #doc{id=DocKey}
    end,
    
    RepRecDocTgt =
    case open_doc(DbTgt, DocKey, []) of
    {ok, TgtDoc} ->
        ?LOG_DEBUG("Found existing replication record on target", []),
        TgtDoc;
    _ -> #doc{id=DocKey}
    end,
    
    #doc{body={RepRecProps}} = RepRecDocSrc,
    #doc{body={RepRecPropsTgt}} = RepRecDocTgt,
    
    case proplists:get_value(<<"session_id">>, RepRecProps) == 
            proplists:get_value(<<"session_id">>, RepRecPropsTgt) of
    true ->
        % if the records have the same session id,
        % then we have a valid replication history
        OldSeqNum = proplists:get_value(<<"source_last_seq">>, RepRecProps, 0),
        OldHistory = proplists:get_value(<<"history">>, RepRecProps, []);
    false ->
        ?LOG_INFO("Replication records differ. "
                "Performing full replication instead of incremental.", []),
        ?LOG_DEBUG("Record on source:~p~nRecord on target:~p~n",
                [RepRecProps, RepRecPropsTgt]),
        OldSeqNum = 0,
        OldHistory = []
    end,
    
    Context = [
        {start_seq, OldSeqNum},
        {history, OldHistory},
        {rep_starttime, ReplicationStartTime},
        {src_starttime, SrcInstanceStartTime},
        {tgt_starttime, TgtInstanceStartTime},
        {src_record, RepRecDocSrc},
        {tgt_record, RepRecDocTgt}
    ],
    
    Stats = ets:new(replication_stats, [set, private]),
    ets:insert(Stats, {total_revs,0}),
    ets:insert(Stats, {missing_revs, 0}),
    ets:insert(Stats, {docs_read, 0}),
    ets:insert(Stats, {docs_written, 0}),
    ets:insert(Stats, {doc_write_failures, 0}),
    
    couch_task_status:add_task("Replication", <<SrcName/binary, " -> ",
        TgtName/binary>>, "Starting"),
    
    Parent = self(),
    Pid = spawn_link(fun() -> enum_docs_since(Parent,DbSrc,DbTgt,{OldSeqNum,0}) end),
    
    State = #state{
        context = Context,
        current_seq = OldSeqNum,
        enum_pid = Pid,
        source = DbSrc,
        target = DbTgt,
        stats = Stats
    },
    
    {ok, State}.

handle_call(get_result, From, #state{listeners=L} = State) ->
    {noreply, State#state{listeners=[From|L]}};

handle_call({replicate_doc, {Id, Revs}}, {Pid,_}, #state{enum_pid=Pid} = State) ->
    #state{
        context = Context,
        current_seq = Seq,
        docs_buffer = Buffer,
        source = Source,
        target = Target,
        stats = Stats
    } = State,

    ets:update_counter(Stats, missing_revs, length(Revs)),
    
    %% get document(s)
    {ok, DocResults} = open_doc_revs(Source, Id, Revs, [latest]),
    Docs = [RevDoc || {ok, RevDoc} <- DocResults],
    ets:update_counter(Stats, docs_read, length(Docs)),
    
    %% save them (maybe in a buffer)
    {NewBuffer, NewContext} = case couch_util:should_flush() of
        true ->
            Docs2 = lists:flatten([Docs|Buffer]),
            {ok, Errors} = update_docs(Target, Docs2, [], replicated_changes),
            dump_update_errors(Errors),
            ets:update_counter(Stats, doc_write_failures, length(Errors)),
            ets:update_counter(Stats, docs_written, length(Docs2) -
                    length(Errors)),
            {ok, _, Ctxt} = do_checkpoint(Source, Target, Context, Seq, Stats),
            {[], Ctxt};
        false ->
            {[Docs | Buffer], Context}
    end,
    
    {reply, ok, State#state{context=NewContext, docs_buffer=NewBuffer}};

handle_call({fin, {LastSeq, RevsCount}}, {Pid,_}, #state{enum_pid=Pid} = State) ->
    ets:update_counter(State#state.stats, total_revs, RevsCount),
    {stop, normal, ok, State#state{current_seq=LastSeq}}.

handle_cast({increment_update_seq, Seq}, State) ->
    couch_task_status:update("Processed source update #~p", [Seq]),
    {noreply, State#state{current_seq=Seq}}.

handle_info({'EXIT', Pid, Reason}, #state{enum_pid=Pid} = State) ->
    ?LOG_ERROR("replication enumerator exited with ~p .. respawning", [Reason]),
    #state{
        current_seq = Seq,
        source = Src,
        target = Tgt,
        enum_pid = Pid
    } = State,
    Parent = self(),
    NewPid = spawn_link(fun() -> enum_docs_since(Parent,Src,Tgt,{Seq,0}) end),
    {noreply, State#state{enum_pid=NewPid}};
    
%% if any linked process dies, respawn the enumerator to get things going again
handle_info({'EXIT', _From, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', From, Reason}, #state{enum_pid=EnumPid} = State) ->
    ?LOG_ERROR("replicator-linked pid ~p exited with ~p", [From, Reason]),
    exit(EnumPid, pls_restart_kthxbye),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, State) ->
    #state{
        context = Context,
        current_seq = Seq,
        docs_buffer = Buffer,
        listeners = Listeners,
        source = Source,
        target = Target,
        stats = Stats
    } = State,
    
    {ok, Errors} = update_docs(Target, lists:flatten(Buffer), [], replicated_changes),
    dump_update_errors(Errors),
    ets:update_counter(Stats, doc_write_failures, length(Errors)),
    ets:update_counter(Stats, docs_written, lists:flatlength(Buffer) -
            length(Errors)),
    
    couch_task_status:update("Finishing"),
    
    {ok, NewRepHistory, _} = do_checkpoint(Source, Target, Context, Seq, Stats),
    ets:delete(Stats),
    close_db(Target),
    
    case Listeners of
    [Original|Rest] ->
        %% reply to original requester
        gen_server:reply(Original, {ok, NewRepHistory});
    Rest -> ok
    end,
    
    %% maybe trigger another replication. If this replicator uses a local 
    %% source Db, changes to that Db since we started will not be included in 
    %% this pass.
    case up_to_date(Source, Seq) of
        true ->
            [gen_server:reply(R, {ok, NewRepHistory}) || R <- Rest];
        false ->
            [gen_server:reply(R, retry) || R <- Rest]
    end,
    close_db(Source);
terminate(Reason, State) ->
    ?LOG_ERROR("replicator terminating with reason ~p", [Reason]),
    #state{
        context = Context,
        current_seq = Seq,
        listeners = Listeners,
        source = Source,
        target = Target,
        stats = Stats
    } = State,
    
    [gen_server:reply(L, {error, Reason}) || L <- Listeners],
    
    {ok, _, _} = do_checkpoint(Source, Target, Context, Seq, Stats),
    
    ets:delete(Stats),
    close_db(Target),
    close_db(Source).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%=============================================================================
%% internal functions
%%=============================================================================


% we should probably write these to a special replication log
% or have a callback where the caller decides what to do with replication
% errors.
dump_update_errors([]) -> ok;
dump_update_errors([{{Id, Rev}, Error}|Rest]) ->
    ?LOG_INFO("error replicating document \"~s\" rev \"~s\":~p",
        [Id, couch_doc:rev_to_str(Rev), Error]),
    dump_update_errors(Rest).

attachment_loop(ReqId) ->
    couch_util:should_flush(),
    receive 
        {From, {set_req_id, NewId}} ->
            From ! {self(), {ok, NewId}},
            attachment_loop(NewId);
        {ibrowse_async_headers, ReqId, _Status, _Headers} ->
            attachment_loop(ReqId);
        {ibrowse_async_response, ReqId, {chunk_start,_}} ->
            attachment_loop(ReqId);
        {ibrowse_async_response, ReqId, chunk_end} ->
            attachment_loop(ReqId);
        {ibrowse_async_response, ReqId, {error, Err}} ->
            ?LOG_ERROR("streaming attachment failed with ~p", [Err]),
            exit(attachment_request_failed);
        {ibrowse_async_response, ReqId, Data} -> 
            receive {From, gimme_data} -> From ! {self(), Data} end,
            attachment_loop(ReqId);
        {ibrowse_async_response_end, ReqId} -> ok
    end.

attachment_stub_converter(DbS, Id, {Name, {stub, Type, Length}}) ->
    #http_db{uri=DbUrl, headers=Headers} = DbS,
    % TODO worry about revisions
    Url = DbUrl ++ url_encode(Id) ++ "/" ++ ?b2l(Name),
    ?LOG_DEBUG("Attachment URL ~p", [Url]),
    
    %% start the process that receives attachment data from ibrowse
    Pid = spawn_link(fun() -> attachment_loop(nil) end),
    
    %% make the async request
    Options = [{stream_to, Pid}, {response_format, binary}],
    ReqId = case ibrowse:send_req(Url, Headers, get, [], Options, infinity) of
        {ibrowse_req_id, X} -> X;
        {error, _Reason} -> exit(attachment_request_failed)
    end,
    
    %% tell our receiver about the ReqId it needs to look for
    Pid ! {self(), {set_req_id, ReqId}},
    receive {Pid, {ok, ReqId}} -> ok end,
    
    %% this is the function that goes into the streaming attachment code.
    %% It gets executed by the replication gen_server, so it can't
    %% be the one to actually receive the ibrowse data.
    RcvFun = fun() -> 
        Pid ! {self(), gimme_data}, 
        receive {Pid, Data} -> Data end
    end,
    {Name, {Type, {RcvFun, Length}}}.


open_db({remote, Url, Headers})->
    {ok, #http_db{uri=?b2l(Url), headers=Headers}, Url};
open_db({local, DbName, UserCtx})->
    case couch_db:open(DbName, [{user_ctx, UserCtx}]) of
    {ok, Db} -> {ok, Db, DbName};
    Error -> Error
    end.


close_db(#http_db{})->
    ok;
close_db(Db)->
    couch_db:close(Db).

do_checkpoint(Source, Target, Context, NewSeqNum, Stats) ->
    ?LOG_INFO("recording a checkpoint at source update_seq ~p", [NewSeqNum]),
    [
        {start_seq, StartSeqNum},
        {history, OldHistory},
        {rep_starttime, ReplicationStartTime},
        {src_starttime, SrcInstanceStartTime},
        {tgt_starttime, TgtInstanceStartTime},
        {src_record, #doc{body={LastRepRecord}}=RepRecDocSrc},
        {tgt_record, RepRecDocTgt}
    ] = Context,
    
    case NewSeqNum == StartSeqNum andalso OldHistory /= [] of
    true ->
        % nothing changed, don't record results
        {ok, {[{<<"no_changes">>, true} | LastRepRecord]}, Context};
    false ->
        % something changed, record results for incremental replication,
        
        % commit changes to both src and tgt. The src because if changes
        % we replicated are lost, we'll record the a seq number ahead 
        % of what was committed. If those changes are lost and the seq number
        % reverts to a previous committed value, we will skip future changes
        % when new doc updates are given our already replicated seq nums.
        
        % commit the src async
        ParentPid = self(),
        SrcCommitPid = spawn_link(fun() -> 
                ParentPid ! {self(), ensure_full_commit(Source)} end),
                
        % commit tgt sync
        {ok, TgtInstanceStartTime2} = ensure_full_commit(Target),
        
        receive {SrcCommitPid, {ok, SrcInstanceStartTime2}} -> ok end,
        
        RecordSeqNum =
        if SrcInstanceStartTime2 == SrcInstanceStartTime andalso
                TgtInstanceStartTime2 == TgtInstanceStartTime ->
            NewSeqNum;
        true ->
            ?LOG_INFO("A server has restarted sinced replication start. "
                "Not recording the new sequence number to ensure the "
                "replication is redone and documents reexamined.", []),
            StartSeqNum
        end,
        
        NewHistoryEntry = {
            [{<<"start_time">>, list_to_binary(ReplicationStartTime)},
            {<<"end_time">>, list_to_binary(httpd_util:rfc1123_date())},
            {<<"start_last_seq">>, StartSeqNum},
            {<<"end_last_seq">>, NewSeqNum},
            {<<"missing_checked">>, ets:lookup_element(Stats, total_revs, 2)},
            {<<"missing_found">>, ets:lookup_element(Stats, missing_revs, 2)},
            {<<"docs_read">>, ets:lookup_element(Stats, docs_read, 2)},
            {<<"docs_written">>, ets:lookup_element(Stats, docs_written, 2)},
            {<<"doc_write_failures">>, ets:lookup_element(Stats, doc_write_failures, 2)}
            ]},
        % limit history to 50 entries
        HistEntries =lists:sublist([NewHistoryEntry |  OldHistory], 50),

        NewRepHistory =
                {[{<<"session_id">>, couch_util:new_uuid()},
                  {<<"source_last_seq">>, RecordSeqNum},
                  {<<"history">>, HistEntries}]},

        {ok, {SrcRevPos,SrcRevId}} = update_doc(Source, 
                RepRecDocSrc#doc{body=NewRepHistory}, []),
        {ok, {TgtRevPos,TgtRevId}} = update_doc(Target,
                RepRecDocTgt#doc{body=NewRepHistory}, []),
    
        NewContext = [
            {start_seq, StartSeqNum},
            {history, OldHistory},
            {rep_starttime, ReplicationStartTime},
            {src_starttime, SrcInstanceStartTime},
            {tgt_starttime, TgtInstanceStartTime},
            {src_record, RepRecDocSrc#doc{revs={SrcRevPos,[SrcRevId]}}},
            {tgt_record, RepRecDocTgt#doc{revs={TgtRevPos,[TgtRevId]}}}
        ],
    
        {ok, NewRepHistory, NewContext}
    
    end.

do_http_request(Url, Action, Headers) ->
    do_http_request(Url, Action, Headers, []).

do_http_request(Url, Action, Headers, JsonBody) ->
    do_http_request(Url, Action, Headers, JsonBody, 10).

do_http_request(Url, Action, _Headers, _JsonBody, 0) ->
    ?LOG_ERROR("couch_rep HTTP ~p request failed after 10 retries: ~s", 
        [Action, Url]),
    exit({http_request_failed, Url});
do_http_request(Url, Action, Headers, JsonBody, Retries) ->
    ?LOG_DEBUG("couch_rep HTTP ~p request: ~s", [Action, Url]),
    Body =
    case JsonBody of
    [] ->
        <<>>;
    _ ->
        iolist_to_binary(?JSON_ENCODE(JsonBody))
    end,
    Options = case Action of
        get -> [];
        _ -> [{transfer_encoding, {chunked, 65535}}]
    end ++ [
        {content_type, "application/json; charset=utf-8"},
        {max_pipeline_size, 101},
        {response_format, binary}
    ],
    case ibrowse:send_req(Url, Headers, Action, Body, Options, infinity) of
    {ok, Status, ResponseHeaders, ResponseBody} ->
        ResponseCode = list_to_integer(Status),
        if
        ResponseCode >= 200, ResponseCode < 300 ->
            ?JSON_DECODE(ResponseBody);
        ResponseCode >= 300, ResponseCode < 400 ->
            RedirectUrl = mochiweb_headers:get_value("Location", 
                mochiweb_headers:make(ResponseHeaders)),
            do_http_request(RedirectUrl, Action, Headers, JsonBody, Retries-1);
        ResponseCode >= 400, ResponseCode < 500 -> 
            ?JSON_DECODE(ResponseBody);        
        ResponseCode == 500 ->
            ?LOG_INFO("retrying couch_rep HTTP ~p request due to 500 error: ~s",
                [Action, Url]),
            do_http_request(Url, Action, Headers, JsonBody, Retries - 1)
        end;
    {error, Reason} ->
        ?LOG_INFO("retrying couch_rep HTTP ~p request due to {error, ~p}: ~s", 
            [Action, Reason, Url]),
        do_http_request(Url, Action, Headers, JsonBody, Retries - 1)
    end.

ensure_full_commit(#http_db{uri=DbUrl, headers=Headers}) ->
    {ResultProps} = do_http_request(DbUrl ++ "_ensure_full_commit", post, 
        Headers, true),
    true = proplists:get_value(<<"ok">>, ResultProps),
    {ok, proplists:get_value(<<"instance_start_time">>, ResultProps)};
ensure_full_commit(Db) ->
    couch_db:ensure_full_commit(Db).

enum_docs_since(Pid, DbSource, DbTarget, {StartSeq, RevsCount}) ->
    case get_doc_info_list(DbSource, StartSeq) of
    [] ->
        gen_server:call(Pid, {fin, {StartSeq, RevsCount}}, infinity);
    DocInfoList ->
        SrcRevsList = lists:map(fun(SrcDocInfo) ->
            #doc_info{id=Id,
                rev=Rev,
                conflict_revs=Conflicts,
                deleted_conflict_revs=DelConflicts
            } = SrcDocInfo,
            SrcRevs = [Rev | Conflicts] ++ DelConflicts,
            {Id, SrcRevs}
        end, DocInfoList),        
        {ok, MissingRevs} = get_missing_revs(DbTarget, SrcRevsList),
        
        %% do we need to check for success here?
        [ gen_server:call(Pid, {replicate_doc, Info}, infinity) 
            || Info <- MissingRevs ],
        
        #doc_info{update_seq=LastSeq} = lists:last(DocInfoList),
        RevsCount2 = RevsCount + length(SrcRevsList),
        gen_server:cast(Pid, {increment_update_seq, LastSeq}),
        
        enum_docs_since(Pid, DbSource, DbTarget, {LastSeq, RevsCount2})
    end.


            
get_db_info(#http_db{uri=DbUrl, headers=Headers}) ->
    {DbProps} = do_http_request(DbUrl, get, Headers),
    {ok, [{list_to_existing_atom(?b2l(K)), V} || {K,V} <- DbProps]};
get_db_info(Db) ->
    couch_db:get_db_info(Db).

get_doc_info_list(#http_db{uri=DbUrl, headers=Headers}, StartSeq) ->
    Url = DbUrl ++ "_all_docs_by_seq?limit=100&startkey=" 
        ++ integer_to_list(StartSeq),
    {Results} = do_http_request(Url, get, Headers),
    lists:map(fun({RowInfoList}) ->
        {RowValueProps} = proplists:get_value(<<"value">>, RowInfoList),
        #doc_info{
            id=proplists:get_value(<<"id">>, RowInfoList),
            rev=couch_doc:parse_rev(proplists:get_value(<<"rev">>, RowValueProps)),
            update_seq = proplists:get_value(<<"key">>, RowInfoList),
            conflict_revs =
                couch_doc:parse_revs(proplists:get_value(<<"conflicts">>, RowValueProps, [])),
            deleted_conflict_revs =
                couch_doc:parse_revs(proplists:get_value(<<"deleted_conflicts">>, RowValueProps, [])),
            deleted = proplists:get_value(<<"deleted">>, RowValueProps, false)
        }
    end, proplists:get_value(<<"rows">>, Results));
get_doc_info_list(DbSource, StartSeq) ->
    {ok, {_Count, DocInfoList}} = couch_db:enum_docs_since(DbSource, StartSeq, 
    fun (_, _, {100, DocInfoList}) ->
            {stop, {100, DocInfoList}};
        (DocInfo, _, {Count, DocInfoList}) -> 
            {ok, {Count+1, [DocInfo|DocInfoList]}} 
    end, {0, []}),
    lists:reverse(DocInfoList).

get_missing_revs(#http_db{uri=DbUrl, headers=Headers}, DocIdRevsList) ->
    DocIdRevsList2 = [{Id, couch_doc:rev_to_strs(Revs)} || {Id, Revs} <- DocIdRevsList],
    {ResponseMembers} = do_http_request(DbUrl ++ "_missing_revs", post, Headers,
            {DocIdRevsList2}),
    {DocMissingRevsList} = proplists:get_value(<<"missing_revs">>, ResponseMembers),
    DocMissingRevsList2 = [{Id, couch_doc:parse_revs(MissingRevStrs)} || {Id, MissingRevStrs} <- DocMissingRevsList],
    {ok, DocMissingRevsList2};
get_missing_revs(Db, DocId) ->
    couch_db:get_missing_revs(Db, DocId).


open_doc(#http_db{uri=DbUrl, headers=Headers}, DocId, Options) ->
    [] = Options,
    case do_http_request(DbUrl ++ url_encode(DocId), get, Headers) of
    {[{<<"error">>, ErrId}, {<<"reason">>, Reason}]} ->
        {couch_util:to_existing_atom(ErrId), Reason};
    Doc  ->
        {ok, couch_doc:from_json_obj(Doc)}
    end;
open_doc(Db, DocId, Options) ->
    couch_db:open_doc(Db, DocId, Options).

open_doc_revs(#http_db{uri=DbUrl, headers=Headers} = DbS, DocId, Revs0, 
        [latest]) ->
    Revs = couch_doc:rev_to_strs(Revs0),
    BaseUrl = DbUrl ++ url_encode(DocId) ++ "?revs=true&latest=true",
    
    %% MochiWeb expects URLs < 8KB long, so maybe split into multiple requests
    MaxN = trunc((8192 - length(BaseUrl))/14),
    
    JsonResults = case length(Revs) > MaxN of
    false ->
        Url = BaseUrl ++ "&open_revs=" ++ lists:flatten(?JSON_ENCODE(Revs)),
        do_http_request(Url, get, Headers);
    true ->
        {_, Rest, Acc} = lists:foldl(
        fun(Rev, {Count, RevsAcc, AccResults}) when Count =:= MaxN ->
            QSRevs = lists:flatten(?JSON_ENCODE(lists:reverse(RevsAcc))),
            Url = BaseUrl ++ "&open_revs=" ++ QSRevs,
            {1, [Rev], AccResults++do_http_request(Url, get, Headers)};
        (Rev, {Count, RevsAcc, AccResults}) ->
            {Count+1, [Rev|RevsAcc], AccResults}
        end, {0, [], []}, Revs),
        Acc ++ do_http_request(BaseUrl ++ "&open_revs=" ++ 
            lists:flatten(?JSON_ENCODE(lists:reverse(Rest))), get, Headers)
    end,
    
    Results =
    lists:map(
        fun({[{<<"missing">>, Rev}]}) ->
            {{not_found, missing}, couch_doc:parse_rev(Rev)};
        ({[{<<"ok">>, JsonDoc}]}) ->
        #doc{id=Id, attachments=Attach} = Doc = couch_doc:from_json_obj(JsonDoc),
        Attach2 = [attachment_stub_converter(DbS,Id,A) || A <- Attach],
        {ok, Doc#doc{attachments=Attach2}}
        end, JsonResults),
    {ok, Results};
open_doc_revs(Db, DocId, Revs, Options) ->
    couch_db:open_doc_revs(Db, DocId, Revs, Options).


update_doc(#http_db{uri=DbUrl, headers=Headers}, #doc{id=DocId}=Doc, Options) ->
    [] = Options,
    Url = DbUrl ++ url_encode(DocId),
    {ResponseMembers} = do_http_request(Url, put, Headers,
            couch_doc:to_json_obj(Doc, [attachments])),
    Rev = proplists:get_value(<<"rev">>, ResponseMembers),
    {ok, couch_doc:parse_rev(Rev)};
update_doc(Db, Doc, Options) ->
    couch_db:update_doc(Db, Doc, Options).

update_docs(_, [], _, _) ->
    {ok, []};
update_docs(#http_db{uri=DbUrl, headers=Headers}, Docs, [], replicated_changes) ->
    JsonDocs = [couch_doc:to_json_obj(Doc, [revs,attachments]) || Doc <- Docs],
    ErrorsJson =
        do_http_request(DbUrl ++ "_bulk_docs", post, Headers,
                {[{new_edits, false}, {docs, JsonDocs}]}),
    ErrorsList =
    lists:map(
        fun({Props}) ->
            Id = proplists:get_value(<<"id">>, Props),
            Rev = couch_doc:parse_rev(proplists:get_value(<<"rev">>, Props)),
            ErrId = couch_util:to_existing_atom(
                    proplists:get_value(<<"error">>, Props)),
            Reason = proplists:get_value(<<"reason">>, Props),
            Error = {ErrId, Reason},
            {{Id, Rev}, Error}
        end, ErrorsJson),
    {ok, ErrorsList};
update_docs(Db, Docs, Options, UpdateType) ->
    couch_db:update_docs(Db, Docs, Options, UpdateType).

up_to_date(#http_db{}, _Seq) ->
    true;
up_to_date(Source, Seq) ->
    {ok, NewDb} = couch_db:open(Source#db.name, []),
    T = NewDb#db.update_seq == Seq,
    couch_db:close(NewDb),
    T.

url_encode(Bin) when is_binary(Bin) ->
    url_encode(binary_to_list(Bin));
url_encode([H|T]) ->
    if
    H >= $a, $z >= H ->
        [H|url_encode(T)];
    H >= $A, $Z >= H ->
        [H|url_encode(T)];
    H >= $0, $9 >= H ->
        [H|url_encode(T)];
    H == $_; H == $.; H == $-; H == $: ->
        [H|url_encode(T)];
    true ->
        case lists:flatten(io_lib:format("~.16.0B", [H])) of
        [X, Y] ->
            [$%, X, Y | url_encode(T)];
        [X] ->
            [$%, $0, X | url_encode(T)]
        end
    end;
url_encode([]) ->
    [].
