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

-export([replicate/3]).

-include_lib("couch_db.hrl").

%% @spec replicate(Source::binary(), Target::binary(), Options::proplist()) -> 
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
replicate(Source, Target, Options) ->
    Id = <<Source/binary, ":", Target/binary>>,
    Args = [?MODULE, [Source,Target,Options], []],
    
    Replicator = {Id,
        {gen_server, start_link, Args},
        transient,
        10000,
        worker,
        [?MODULE]
    },
    
    Server = case supervisor:start_child(couch_rep_sup, Replicator) of
        {ok, Pid} -> 
            ?LOG_INFO("starting new replication ~p at ~p", [Id, Pid]),
            Pid;
        {error, already_present} ->
            case supervisor:restart_child(couch_rep_sup, Id) of
                {ok, Pid} -> 
                    ?LOG_INFO("starting replication ~p at ~p", [Id, Pid]),
                    Pid;
                {error, running} -> 
                    %% this error occurs if multiple replicators are racing
                    %% each other to start and somebody else won.  Just grab
                    %% the Pid by calling start_child again.
                    {error, {already_started, Pid}} = 
                        supervisor:start_child(couch_rep_sup, Replicator),
                    ?LOG_INFO("replication ~p already running at ~p", [Id, Pid]),
                    Pid
            end;
        {error, {already_started, Pid}} -> 
            ?LOG_INFO("replication ~p already running at ~p", [Id, Pid]),
            Pid
    end,
    
    case gen_server:call(Server, get_result, infinity) of 
        retry -> replicate(Source, Target, Options);
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

init([Source, Target, Options]) ->
    process_flag(trap_exit, true),
    
    {ok, DbSrc} = 
        open_db(Source, proplists:get_value(source_options, Options, [])),
    {ok, DbTgt} = 
        open_db(Target, proplists:get_value(target_options, Options, [])),
    
    {ok, Host} = inet:gethostname(),
    HostBin = list_to_binary(Host),
    DocKey = <<?LOCAL_DOC_PREFIX, HostBin/binary, ":", Source/binary, ":", 
        Target/binary>>,
    
    {ok, InfoSrc} = get_db_info(DbSrc),
    {ok, InfoTgt} = get_db_info(DbTgt),
    
    ReplicationStartTime = httpd_util:rfc1123_date(),
    SrcInstanceStartTime = proplists:get_value(instance_start_time, InfoSrc),
    TgtInstanceStartTime = proplists:get_value(instance_start_time, InfoTgt),
    
    case proplists:get_value(full, Options, false)
        orelse proplists:get_value("full", Options, false) of
    true ->
        RepRecSrc = RepRecTgt = #doc{id=DocKey};
    false ->
        RepRecSrc = case open_doc(DbSrc, DocKey, []) of
            {ok, SrcDoc} ->
                ?LOG_DEBUG("Found existing replication record on source", []),
                SrcDoc;
            _ -> #doc{id=DocKey}
        end,

        RepRecTgt = case open_doc(DbTgt, DocKey, []) of
            {ok, TgtDoc} ->
                ?LOG_DEBUG("Found existing replication record on target", []),
                TgtDoc;
            _ -> #doc{id=DocKey}
        end
    end,

    #doc{body={OldRepHistoryProps}} = RepRecSrc,
    #doc{body={OldRepHistoryPropsTrg}} = RepRecTgt,

    SeqNum = case OldRepHistoryProps == OldRepHistoryPropsTrg of
        true ->
            % if the records are identical, then we have a valid replication history
            proplists:get_value(<<"source_last_seq">>, OldRepHistoryProps, 0);
        false ->
            ?LOG_INFO("Replication records differ. "
                "Performing full replication instead of incremental.", []),
            ?LOG_DEBUG("Record on source:~p~nRecord on target:~p~n", 
                [OldRepHistoryProps, OldRepHistoryPropsTrg]),
        0
    end,
    
    Context = [
        {start_seq, SeqNum},
        {history, OldRepHistoryProps},
        {rep_starttime, ReplicationStartTime},
        {src_starttime, SrcInstanceStartTime},
        {tgt_starttime, TgtInstanceStartTime},
        {src_record, RepRecSrc},
        {tgt_record, RepRecTgt}
    ],
    
    Stats = ets:new(replication_stats, [set, private]),
    ets:insert(Stats, {total_revs,0}),
    ets:insert(Stats, {missing_revs, 0}),
    ets:insert(Stats, {docs_read, 0}),
    ets:insert(Stats, {docs_written, 0}),
    
    couch_task_status:add_task("Replication", <<Source/binary, " -> ",
        Target/binary>>, "Starting"),
    
    Parent = self(),
    Pid = spawn_link(fun() -> enum_docs_since(Parent,DbSrc,DbTgt,{SeqNum,0}) end),
    
    State = #state{
        context = Context,
        current_seq = SeqNum,
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
            ok = update_docs(Target, Docs2, [], false),
            ets:update_counter(Stats, docs_written, length(Docs2)),
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
    
    ok = update_docs(Target, lists:flatten(Buffer), [], false),
    ets:update_counter(Stats, docs_written, lists:flatlength(Buffer)),
    
    couch_task_status:update("Finishing"),
    
    {ok, NewRepHistory, _} = do_checkpoint(Source, Target, Context, Seq, Stats),
    ets:delete(Stats),
    close_db(Target),
    
    %% reply to original requester
    [Original|Rest] = Listeners,
    gen_server:reply(Original, {ok, NewRepHistory}),
    
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

close_db(#http_db{})->
    ok;
close_db(Db)->
    couch_db:close(Db).

do_checkpoint(Source, Target, Context, NewSeqNum, Stats) ->
    ?LOG_INFO("recording a checkpoint at source update_seq ~p", [NewSeqNum]),
    [
        {start_seq, SeqNum},
        {history, OldRepHistoryProps},
        {rep_starttime, ReplicationStartTime},
        {src_starttime, SrcInstanceStartTime},
        {tgt_starttime, TgtInstanceStartTime},
        {src_record, RepRecSrc},
        {tgt_record, RepRecTgt}
    ] = Context,
    
    NewHistory = case NewSeqNum == SeqNum andalso OldRepHistoryProps /= [] of
    true ->
        % nothing changed, don't record results
        {OldRepHistoryProps};
    false ->
        % commit changes to both src and tgt. The src because if changes
        % we replicated are lost, we'll record the a seq number of ahead 
        % of what was committed and therefore lose future changes with the
        % same seq nums.
        {ok, SrcInstanceStartTime2} = ensure_full_commit(Source),
        {ok, TgtInstanceStartTime2} = ensure_full_commit(Target),
        
        RecordSeqNum =
        if SrcInstanceStartTime2 == SrcInstanceStartTime andalso
                TgtInstanceStartTime2 == TgtInstanceStartTime ->
            NewSeqNum;
        true ->
            ?LOG_INFO("A server has restarted sinced replication start. "
                "Not recording the new sequence number to ensure the "
                "replication is redone and documents reexamined.", []),
            SeqNum
        end,
        
        %% format replication history
        JsonStats = [
            {<<"missing_checked">>, ets:lookup_element(Stats, total_revs, 2)},
            {<<"missing_found">>, ets:lookup_element(Stats, missing_revs, 2)},
            {<<"docs_read">>, ets:lookup_element(Stats, docs_read, 2)},
            {<<"docs_written">>, ets:lookup_element(Stats, docs_written, 2)}
        ],
        
        HistEntries =[
            {
                [{<<"start_time">>, list_to_binary(ReplicationStartTime)},
                {<<"end_time">>, list_to_binary(httpd_util:rfc1123_date())},
                {<<"start_last_seq">>, SeqNum},
                {<<"end_last_seq">>, NewSeqNum} | JsonStats]}
            | proplists:get_value(<<"history">>, OldRepHistoryProps, [])],
        % something changed, record results
        {[
            {<<"session_id">>, couch_util:new_uuid()},
            {<<"source_last_seq">>, RecordSeqNum},
            {<<"history">>, lists:sublist(HistEntries, 50)}
        ]}
    end,
    
    %% update local documents
    RepRecSrc = proplists:get_value(src_record, Context),
    RepRecTgt = proplists:get_value(tgt_record, Context),
    {ok, TgtRev} = update_local_doc(Target, RepRecTgt#doc{body=NewHistory}, []),
    {ok, SrcRev} = update_local_doc(Source, RepRecSrc#doc{body=NewHistory}, []),
    
    NewContext = [
        {start_seq, SeqNum},
        {history, OldRepHistoryProps},
        {rep_starttime, ReplicationStartTime},
        {src_starttime, SrcInstanceStartTime},
        {tgt_starttime, TgtInstanceStartTime},
        {src_record, RepRecSrc#doc{revs=[SrcRev]}},
        {tgt_record, RepRecTgt#doc{revs=[TgtRev]}}
    ],
    
    {ok, NewHistory, NewContext}.

do_http_request(Url, Action, Headers) ->
    do_http_request(Url, Action, Headers, []).

do_http_request(Url, Action, Headers, JsonBody) ->
    do_http_request(?b2l(?l2b(Url)), Action, Headers, JsonBody, 10).

do_http_request(Url, Action, _Headers, _JsonBody, 0) ->
    ?LOG_ERROR("couch_rep HTTP ~p request failed after 10 retries: ~s", 
        [Action, Url]),
    exit({http_request_failed, ?l2b(Url)});
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
        % UpdateSeqs = [D#doc_info.update_seq || D <- DocInfoList],
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

fix_url(UrlBin) ->
    Url = binary_to_list(UrlBin),
    case lists:last(Url) of
        $/ -> Url;
        _ ->  Url ++ "/"
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
            rev=proplists:get_value(<<"rev">>, RowValueProps),
            update_seq = proplists:get_value(<<"key">>, RowInfoList),
            conflict_revs =
                proplists:get_value(<<"conflicts">>, RowValueProps, []),
            deleted_conflict_revs =
                proplists:get_value(<<"deleted_conflicts">>, RowValueProps, []),
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
    {ResponseMembers} = do_http_request(DbUrl ++ "_missing_revs", post, Headers,
            {DocIdRevsList}),
    {MissingRevs} = proplists:get_value(<<"missing_revs">>, ResponseMembers),
    {ok, MissingRevs};
get_missing_revs(Db, DocId) ->
    couch_db:get_missing_revs(Db, DocId).

open_http_db(UrlBin, Options) ->
    Headers = proplists:get_value(headers, Options, {[]}),
    {ok, #http_db{uri=fix_url(UrlBin), headers=Headers}}.
            
open_db(<<"http://", _/binary>>=Url, Options)->
    open_http_db(Url, Options);
open_db(<<"https://", _/binary>>=Url, Options)->
    open_http_db(Url, Options);
open_db(DbName, Options)->
    couch_db:open(DbName, Options).

open_doc(#http_db{uri=DbUrl, headers=Headers}, DocId, []) ->
    case do_http_request(DbUrl ++ url_encode(DocId), get, Headers) of
    {[{<<"error">>, ErrId}, {<<"reason">>, Reason}]} ->
        {couch_util:to_existing_atom(ErrId), Reason};
    Doc  ->
        {ok, couch_doc:from_json_obj(Doc)}
    end;
open_doc(Db, DocId, Options) ->
    couch_db:open_doc(Db, DocId, Options).

open_doc_revs(#http_db{uri=DbUrl, headers=Headers} = DbS, DocId, Revs, _Opts) ->
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
    lists:map(fun({[{<<"missing">>, Rev}]}) ->
        {{not_found, missing}, Rev};
    ({[{<<"ok">>, JsonDoc}]}) ->
        #doc{id=Id, attachments=Attach} = Doc = couch_doc:from_json_obj(JsonDoc),
        Attach2 = [attachment_stub_converter(DbS,Id,A) || A <- Attach],
        {ok, Doc#doc{attachments=Attach2}}
    end, JsonResults),
    {ok, Results};
open_doc_revs(Db, DocId, Revs, Options) ->
    couch_db:open_doc_revs(Db, DocId, Revs, Options).

update_docs(_, [], _, _) ->
    ok;
update_docs(#http_db{uri=DbUrl, headers=Headers}, Docs, [], NewEdits) ->
    JsonDocs = [couch_doc:to_json_obj(Doc, [revs,attachments]) || Doc <- Docs],
    {Returned} =
        do_http_request(DbUrl ++ "_bulk_docs", post, Headers,
                {[{new_edits, NewEdits}, {docs, JsonDocs}]}),
    true = proplists:get_value(<<"ok">>, Returned),
    ok;
update_docs(Db, Docs, Options, NewEdits) ->
    couch_db:update_docs(Db, Docs, Options, NewEdits).

update_local_doc(#http_db{uri=DbUrl, headers=Headers}, #doc{id=DocId}=Doc, []) ->
    Url = DbUrl ++ url_encode(DocId),
    {ResponseMembers} = do_http_request(Url, put, Headers,
            couch_doc:to_json_obj(Doc, [revs,attachments])),
    RevId = proplists:get_value(<<"rev">>, ResponseMembers),
    {ok, RevId};
update_local_doc(Db, Doc, Options) ->
    couch_db:update_doc(Db, Doc, Options).

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
