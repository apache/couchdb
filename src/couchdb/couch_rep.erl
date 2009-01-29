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

-include("couch_db.hrl").

-record(http_db, {
    uri,
    headers
}).

-export([replicate/2, replicate/3]).

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


replicate(DbNameA, DbNameB) ->
    replicate(DbNameA, DbNameB, []).

replicate(Source, Target, Options) ->
    {ok, DbSrc} = open_db(Source,
            proplists:get_value(source_options, Options, [])),
    try
        {ok, DbTgt} = open_db(Target,
                proplists:get_value(target_options, Options, [])),
        try
            replicate2(Source, DbSrc, Target, DbTgt, Options)
        after
            close_db(DbTgt)
        end        
    after
        close_db(DbSrc)
    end.
    
replicate2(Source, DbSrc, Target, DbTgt, Options) ->
    {ok, HostName} = inet:gethostname(),
    HostNameBin = list_to_binary(HostName),
    RepRecKey = <<?LOCAL_DOC_PREFIX, HostNameBin/binary, 
            ":", Source/binary, ":", Target/binary>>,
    
    ReplicationStartTime = httpd_util:rfc1123_date(),
    
    {ok, SrcInstanceStartTime} = get_db_info(DbSrc),
    {ok, TgtInstanceStartTime} = get_db_info(DbTgt),
    
    case proplists:get_value(full, Options, false)
        orelse proplists:get_value("full", Options, false) of
    true ->
         RepRecSrc = RepRecTgt = #doc{id=RepRecKey};
    false ->
        RepRecSrc =
        case open_doc(DbSrc, RepRecKey, []) of
        {ok, SrcDoc} ->
            ?LOG_DEBUG("Found existing replication record on source", []),
            SrcDoc;
        _ -> #doc{id=RepRecKey}
        end,

        RepRecTgt =
        case open_doc(DbTgt, RepRecKey, []) of
        {ok, TgtDoc} ->
            ?LOG_DEBUG("Found existing replication record on target", []),
            TgtDoc;
        _ -> #doc{id=RepRecKey}
        end
    end,

    #doc{body={OldRepHistoryProps}} = RepRecSrc,
    #doc{body={OldRepHistoryPropsTrg}} = RepRecTgt,

    SeqNum =
    case OldRepHistoryProps == OldRepHistoryPropsTrg of
    true ->
        % if the records are identical, then we have a valid replication history
        proplists:get_value(<<"source_last_seq">>, OldRepHistoryProps, 0);
    false ->
        ?LOG_INFO("Replication records differ. "
                "Performing full replication instead of incremental.", []),
        ?LOG_DEBUG("Record on source:~p~nRecord on target:~p~n", [OldRepHistoryProps, OldRepHistoryPropsTrg]),
        0
    end,

    {NewSeqNum, Stats} = pull_rep(DbTgt, DbSrc, SeqNum),    
    
    case NewSeqNum == SeqNum andalso OldRepHistoryProps /= [] of
    true ->
        % nothing changed, don't record results
        {ok, {OldRepHistoryProps}};
    false ->
        % commit changes to both src and tgt. The src because if changes
        % we replicated are lost, we'll record the a seq number of ahead 
        % of what was committed and therefore lose future changes with the
        % same seq nums.
        
        {ok, SrcInstanceStartTime2} = ensure_full_commit(DbSrc),
        {ok, TgtInstanceStartTime2} = ensure_full_commit(DbTgt),
        
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
        
        HistEntries =[
            {
                [{<<"start_time">>, list_to_binary(ReplicationStartTime)},
                {<<"end_time">>, list_to_binary(httpd_util:rfc1123_date())},
                {<<"start_last_seq">>, SeqNum},
                {<<"end_last_seq">>, NewSeqNum} | Stats]}
            | proplists:get_value("history", OldRepHistoryProps, [])],
        % something changed, record results
        NewRepHistory =
            {
                [{<<"session_id">>, couch_util:new_uuid()},
                {<<"source_last_seq">>, RecordSeqNum},
                {<<"history">>, lists:sublist(HistEntries, 50)}]},

        {ok, _} = update_doc(DbSrc, RepRecSrc#doc{body=NewRepHistory}, []),
        {ok, _} = update_doc(DbTgt, RepRecTgt#doc{body=NewRepHistory}, []),
        {ok, NewRepHistory}
    end.

pull_rep(DbTarget, DbSource, SourceSeqNum) ->
    {ok, {NewSeq, Stats}} = 
        enum_docs_since(DbSource, DbTarget, SourceSeqNum, {SourceSeqNum, []}),
    {NewSeq, Stats}.

do_http_request(Url, Action, Headers) ->
    do_http_request(Url, Action, Headers, []).

do_http_request(Url, Action, Headers, JsonBody) ->
    do_http_request(Url, Action, Headers, JsonBody, 10).

do_http_request(Url, Action, _Headers, _JsonBody, 0) ->
    ?LOG_ERROR("couch_rep HTTP ~p request failed after 10 retries: ~p", 
        [Action, Url]);
do_http_request(Url, Action, Headers, JsonBody, Retries) ->
    ?LOG_DEBUG("couch_rep HTTP ~p request: ~p", [Action, Url]),
    Body =
    case JsonBody of
    [] ->
        <<>>;
    _ ->
        iolist_to_binary(?JSON_ENCODE(JsonBody))
    end,
    Options = [
        {content_type, "application/json; charset=utf-8"},
        {max_pipeline_size, 101},
        {transfer_encoding, {chunked, 65535}}
    ],
    case ibrowse:send_req(Url, Headers, Action, Body, Options) of
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
            ?LOG_INFO("retrying couch_rep HTTP ~p request due to 500 error: ~p",
                [Action, Url]),
            do_http_request(Url, Action, Headers, JsonBody, Retries - 1)
        end;
    {error, Reason} ->
        ?LOG_INFO("retrying couch_rep HTTP ~p request due to {error, ~p}: ~p", 
            [Action, Reason, Url]),
        do_http_request(Url, Action, Headers, JsonBody, Retries - 1)
    end.

save_docs_buffer(DbTarget, DocsBuffer, []) ->
    receive
    {Src, shutdown} ->
        ok = update_docs(DbTarget, lists:reverse(DocsBuffer), [], false),
        Src ! {done, self(), [{<<"docs_written">>, length(DocsBuffer)}]}
    end;
save_docs_buffer(DbTarget, DocsBuffer, UpdateSequences) ->
    [NextSeq|Rest] = UpdateSequences,
    receive
    {Src, skip, NextSeq} ->
        Src ! got_it,
        save_docs_buffer(DbTarget, DocsBuffer, Rest);
    {Src, docs, {NextSeq, Docs}} ->
        Src ! got_it,
        case couch_util:should_flush() of
            true ->
                ok = update_docs(DbTarget, lists:reverse(Docs++DocsBuffer), [], 
                    false),
                save_docs_buffer(DbTarget, [], Rest);
            false ->
                save_docs_buffer(DbTarget, Docs++DocsBuffer, Rest)
        end;
        {Src, shutdown} ->
        ?LOG_ERROR("received shutdown while waiting for more update_seqs", []),
        ok = update_docs(DbTarget, lists:reverse(DocsBuffer), [], false),
        Src ! {done, self(), [{<<"docs_written">>, length(DocsBuffer)}]}
    end.

pmap(F,List) ->
    [wait_result(Worker) || Worker <- [spawn_worker(self(),F,E) || E <- List]].

spawn_worker(Parent, F, E) ->
    erlang:spawn_monitor(fun() -> Parent ! {self(), F(E)} end).

wait_result({Pid,Ref}) ->
    receive
    {'DOWN', Ref, _, _, normal} -> receive {Pid,Result} -> Result end;
    {'DOWN', Ref, _, _, Reason} -> exit(Reason)
end.

enum_docs_parallel(DbS, DbT, InfoList) ->
    UpdateSeqs = [Seq || {_, Seq, _, _} <- InfoList],
    SaveDocsPid = spawn_link(fun() -> save_docs_buffer(DbT,[],UpdateSeqs) end),
    
    Stats = pmap(fun({Id, Seq, SrcRevs, MissingRevs}) ->
        case MissingRevs of
        [] ->
            SaveDocsPid ! {self(), skip, Seq},
            receive got_it -> ok end,
            [{missing_checked, length(SrcRevs)}];
        _ ->
            {ok, DocResults} = open_doc_revs(DbS, Id, MissingRevs, [latest]),
            
            % only save successful reads
            Docs = [RevDoc || {ok, RevDoc} <- DocResults],
            
            % include update_seq so we save docs in order
            SaveDocsPid ! {self(), docs, {Seq, Docs}},
            receive got_it -> ok end,
            [{missing_checked, length(SrcRevs)},
             {missing_found, length(MissingRevs)},
             {docs_read, length(Docs)}]
        end
    end, InfoList),
    
    SaveDocsPid ! {self(), shutdown},
    
    {MissingChecked, MissingFound, DocsRead} = lists:foldl(fun(S, {C, F, R}) ->
        C1 = C + proplists:get_value(missing_checked, S, 0),
        F1 = F + proplists:get_value(missing_found, S, 0),
        R1 = R + proplists:get_value(docs_read, S, 0),
        {C1, F1, R1}
    end, {0, 0, 0}, Stats),
    
    receive
        {done, SaveDocsPid, [{<<"docs_written">>, DocsWritten}]} -> ok
    end,
    
    [ {<<"missing_checked">>, MissingChecked},
      {<<"missing_found">>, MissingFound}, 
      {<<"docs_read">>, DocsRead},
      {<<"docs_written">>, DocsWritten} ].

fix_url(UrlBin) ->
    Url = binary_to_list(UrlBin),
    case lists:last(Url) of
    $/ ->
        Url;
    _ ->
        Url ++ "/"
    end.

open_http_db(UrlBin, Options) ->
    Headers = proplists:get_value(headers, Options, {[]}),
    {ok, #http_db{uri=fix_url(UrlBin), headers=Headers}}.
            
open_db(<<"http://", _/binary>>=Url, Options)->
    open_http_db(Url, Options);
open_db(<<"https://", _/binary>>=Url, Options)->
    open_http_db(Url, Options);
open_db(DbName, Options)->
    couch_db:open(DbName, Options).

close_db(#http_db{})->
    ok;
close_db(Db)->
    couch_db:close(Db).

get_db_info(#http_db{uri=DbUrl, headers=Headers}) ->
    {DbProps} = do_http_request(DbUrl, get, Headers),
    {ok, [{list_to_existing_atom(?b2l(K)), V} || {K,V} <- DbProps]};
get_db_info(Db) ->
    couch_db:get_db_info(Db).


ensure_full_commit(#http_db{uri=DbUrl, headers=Headers}) ->
    {ResultProps} = do_http_request(DbUrl ++ "_ensure_full_commit", post, Headers, true),
    true = proplists:get_value(<<"ok">>, ResultProps),
    {ok, proplists:get_value(<<"instance_start_time">>, ResultProps)};
ensure_full_commit(Db) ->
    couch_db:ensure_full_commit(Db).
    
    
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

enum_docs_since(DbSource, DbTarget, StartSeq, InAcc) ->
    DocInfoList = get_doc_info_list(DbSource, StartSeq),
    case DocInfoList of
    [] ->
        {ok, InAcc};
    _ ->
        UpdateSeqs = [D#doc_info.update_seq || D <- DocInfoList],
        SrcRevsList = lists:map(fun(SrcDocInfo) ->
            #doc_info{id=Id,
                rev=Rev,
                conflict_revs=Conflicts,
                deleted_conflict_revs=DelConflicts
            } = SrcDocInfo,
            SrcRevs = [Rev | Conflicts] ++ DelConflicts,
            {Id, SrcRevs}
        end, DocInfoList),        
        {ok, MissingRevsList} = get_missing_revs(DbTarget, SrcRevsList),
        InfoList = lists:map(fun({{Id, SrcRevs}, Seq}) ->
            MissingRevs = proplists:get_value(Id, MissingRevsList, []),
            {Id, Seq, SrcRevs, MissingRevs}
        end, lists:zip(SrcRevsList, UpdateSeqs)),
        Stats = enum_docs_parallel(DbSource, DbTarget, InfoList),
        OldStats = element(2, InAcc),
        TotalStats = [
            {<<"missing_checked">>, 
                proplists:get_value(<<"missing_checked">>, OldStats, 0) +
                proplists:get_value(<<"missing_checked">>, Stats, 0)},
            {<<"missing_found">>, 
                proplists:get_value(<<"missing_found">>, OldStats, 0) +
                proplists:get_value(<<"missing_found">>, Stats, 0)},
            {<<"docs_read">>, 
                proplists:get_value(<<"docs_read">>, OldStats, 0) +
                proplists:get_value(<<"docs_read">>, Stats, 0)},
            {<<"docs_written">>, 
                proplists:get_value(<<"docs_written">>, OldStats, 0) +
                proplists:get_value(<<"docs_written">>, Stats, 0)}
        ],
        
        #doc_info{update_seq=LastSeq} = lists:last(DocInfoList),
        enum_docs_since(DbSource, DbTarget, LastSeq, {LastSeq, TotalStats})
    end.

get_missing_revs(#http_db{uri=DbUrl, headers=Headers}, DocIdRevsList) ->
    {ResponseMembers} = do_http_request(DbUrl ++ "_missing_revs", post, Headers,
            {DocIdRevsList}),
    {DocMissingRevsList} = proplists:get_value(<<"missing_revs">>, ResponseMembers),
    {ok, DocMissingRevsList};
get_missing_revs(Db, DocId) ->
    couch_db:get_missing_revs(Db, DocId).


update_doc(#http_db{uri=DbUrl, headers=Headers}, #doc{id=DocId}=Doc, Options) ->
    [] = Options,
    Url = DbUrl ++ url_encode(DocId),
    {ResponseMembers} = do_http_request(Url, put, Headers,
            couch_doc:to_json_obj(Doc, [revs,attachments])),
    RevId = proplists:get_value(<<"_rev">>, ResponseMembers),
    {ok, RevId};
update_doc(Db, Doc, Options) ->
    couch_db:update_doc(Db, Doc, Options).

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


open_doc_revs(#http_db{uri=DbUrl, headers=Headers}, DocId, Revs, Options) ->
    QueryOptionStrs =
    lists:map(fun(latest) ->
            % latest is only option right now
            "latest=true"
        end, Options),
    
    BaseUrl = DbUrl ++ url_encode(DocId) ++ "?" ++ couch_util:implode(
        ["revs=true", "attachments=true"] ++ QueryOptionStrs, "&"),
    
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
            {{not_found, missing}, Rev};
        ({[{<<"ok">>, JsonDoc}]}) ->
            {ok, couch_doc:from_json_obj(JsonDoc)}
        end, JsonResults),
    {ok, Results};
open_doc_revs(Db, DocId, Revs, Options) ->
    couch_db:open_doc_revs(Db, DocId, Revs, Options).


