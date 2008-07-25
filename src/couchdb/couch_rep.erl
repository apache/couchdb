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

-export([replicate/2, replicate/3]).

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
    {ok, DbSrc} = open_db(Source),
    {ok, DbTgt} = open_db(Target),
    {ok, HostName} = inet:gethostname(),

    RepRecKey = ?LOCAL_DOC_PREFIX ++ HostName ++ ":" ++ Source ++ ":" ++ Target,
    StartTime = httpd_util:rfc1123_date(),
    
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

    #doc{body={obj,OldRepHistoryProps}} = RepRecSrc,
    #doc{body={obj,OldRepHistoryPropsTrg}} = RepRecTgt,

    SeqNum =
    case OldRepHistoryProps == OldRepHistoryPropsTrg of
    true ->
        % if the records are identical, then we have a valid replication history
        proplists:get_value("source_last_seq", OldRepHistoryProps, 0);
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
        {ok, {obj, OldRepHistoryProps}};
    false ->
        HistEntries =[
            {obj,
                [{"start_time", StartTime},
                {"end_time", httpd_util:rfc1123_date()},
                {"start_last_seq", SeqNum},
                {"end_last_seq", NewSeqNum} | Stats]}
            | tuple_to_list(proplists:get_value("history", OldRepHistoryProps, {}))],
        % something changed, record results
        NewRepHistory =
            {obj,
                [{"session_id", couch_util:new_uuid()},
                {"source_last_seq", NewSeqNum},
                {"history", list_to_tuple(lists:sublist(HistEntries, 50))}]},

        {ok, _} = update_doc(DbSrc, RepRecSrc#doc{body=NewRepHistory}, []),
        {ok, _} = update_doc(DbTgt, RepRecTgt#doc{body=NewRepHistory}, []),
        {ok, NewRepHistory}
    end.

pull_rep(DbTarget, DbSource, SourceSeqNum) ->
    SaveDocsPid = spawn_link(fun() ->
            save_docs_loop(DbTarget, 0) end),
    OpenDocsPid = spawn_link(fun() ->
            open_doc_revs_loop(DbSource, SaveDocsPid, 0) end),
    OpenDocsPid ! got_it, % prime queue with got_it
    MissingRevsPid = spawn_link(fun() ->
            get_missing_revs_loop(DbTarget, OpenDocsPid, 0, 0) end),
    MissingRevsPid ! got_it, % prime queue with got_it
    self() ! got_it,
    {ok, NewSeq} = enum_docs_since(DbSource, SourceSeqNum,
        fun(SrcDocInfo, _, _) ->
            #doc_info{id=Id,
                rev=Rev,
                conflict_revs=Conflicts,
                deleted_conflict_revs=DelConflicts,
                update_seq=Seq} = SrcDocInfo,
            SrcRevs = [Rev | Conflicts] ++ DelConflicts,
            receive got_it -> ok end,
            MissingRevsPid !  {self(), Id, SrcRevs}, % send to the missing revs process
            {ok, Seq}
        end, SourceSeqNum),
    
    receive got_it -> ok end,
    
    MissingRevsPid ! {self(), shutdown},
    receive {done, MissingRevsPid, Stats1} -> ok end,
    
    OpenDocsPid ! {self(), shutdown},
    receive {done, OpenDocsPid, Stats2} -> ok end,
    
    SaveDocsPid ! {self(), shutdown},
    receive {done, SaveDocsPid, Stats3} -> ok end,
    
    {NewSeq, Stats1 ++ Stats2 ++ Stats3}.


get_missing_revs_loop(DbTarget, OpenDocsPid, RevsChecked, MissingFound) ->
    receive got_it -> ok end,
    receive
    {Src, Id, Revs} ->
        Src ! got_it,
        MissingRevs =
        case get_missing_revs(DbTarget, [{Id, Revs}]) of
        {ok, [{Id, MissingRevs0}]} ->
            OpenDocsPid ! {self(), Id, MissingRevs0},
            MissingRevs0;
        {ok, []} ->
            % prime our message queue
            self() ! got_it,
            []
        end,
        get_missing_revs_loop(DbTarget, OpenDocsPid,
                RevsChecked + length(Revs),
                MissingFound + length(MissingRevs));
    {Src, shutdown} ->
        Src ! {done, self(), [{"missing_checked", RevsChecked},
                                 {"missing_found", MissingFound}]}
    end.
    

open_doc_revs_loop(DbSource, SaveDocsPid, DocsRead) ->
    receive got_it -> ok end,
    receive
    {Src, Id, MissingRevs} ->
        Src ! got_it,
        {ok, DocResults} = open_doc_revs(DbSource, Id, MissingRevs, [latest]),
        % only save successful reads
        Docs = [RevDoc || {ok, RevDoc} <- DocResults],
        SaveDocsPid ! {self(), docs, Docs},
        open_doc_revs_loop(DbSource, SaveDocsPid, DocsRead + length(Docs));
    {Src, shutdown} ->
        Src ! {done, self(), [{"docs_read", DocsRead}]}
    end.


        
save_docs_loop(DbTarget, DocsWritten) ->
    receive
    {Src, docs, Docs} ->
        Src ! got_it,
        ok = save_docs(DbTarget, Docs, []),
        save_docs_loop(DbTarget, DocsWritten + length(Docs));
    {Src, shutdown} ->
        Src ! {done, self(), [{"docs_written", DocsWritten}]}
    end.


do_http_request(Url, Action) ->
    do_http_request(Url, Action, []).

do_http_request(Url, Action, JsonBody) ->
    ?LOG_DEBUG("couch_rep HTTP client request:", []),
    ?LOG_DEBUG("\tAction: ~p", [Action]),
    ?LOG_DEBUG("\tUrl: ~p", [Url]),

    Request =
    case JsonBody of
    [] ->
        {Url, []};
    _ ->
        {Url, [], "application/json; charset=utf-8", lists:flatten(cjson:encode(JsonBody))}
    end,
    {ok, {{_, ResponseCode,_},_Headers, ResponseBody}} = http:request(Action, Request, [], []),
    if
    ResponseCode >= 200, ResponseCode < 500 ->
        cjson:decode(ResponseBody)
    end.

enum_docs0(_InFun, [], Acc) ->
    Acc;
enum_docs0(InFun, [DocInfo | Rest], Acc) ->
    case InFun(DocInfo, 0, Acc) of
    {ok, Acc2} -> enum_docs0(InFun, Rest, Acc2);
    {stop, Acc2} -> Acc2
    end.

open_db("http" ++ DbName)->
    case lists:last(DbName) of
    $/ ->
        {ok, "http" ++ DbName};
    _ ->
        {ok, "http" ++ DbName ++ "/"}
    end;
open_db(DbName)->
    couch_server:open(DbName).


enum_docs_since(DbUrl, StartSeq, InFun, InAcc) when is_list(DbUrl) ->
    Url = DbUrl ++ "_all_docs_by_seq?count=100&startkey=" ++ integer_to_list(StartSeq),
    {obj, Results} = do_http_request(Url, get),
    DocInfoList=
    lists:map(fun({obj, RowInfoList}) ->
        {obj, RowValueProps} = proplists:get_value("value", RowInfoList),
        #doc_info{
            id=proplists:get_value("id", RowInfoList),
            rev=proplists:get_value("rev", RowValueProps),
            update_seq = proplists:get_value("key", RowInfoList),
            conflict_revs =
                tuple_to_list(proplists:get_value("conflicts", RowValueProps, {})),
            deleted_conflict_revs =
                tuple_to_list(proplists:get_value("deleted_conflicts", RowValueProps, {})),
            deleted = proplists:get_value("deleted", RowValueProps, false)}
        end, tuple_to_list(proplists:get_value("rows", Results))),
    case DocInfoList of
    [] ->
        {ok, InAcc};
    _ ->
        Acc2 = enum_docs0(InFun, DocInfoList, InAcc),
        #doc_info{update_seq=LastSeq} = lists:last(DocInfoList),
        enum_docs_since(DbUrl, LastSeq, InFun, Acc2)
    end;
enum_docs_since(DbSource, StartSeq, Fun, Acc) ->
    couch_db:enum_docs_since(DbSource, StartSeq, Fun, Acc).

get_missing_revs(DbUrl, DocIdRevsList) when is_list(DbUrl) ->
    JsonDocIdRevsList = {obj,
        [{Id, list_to_tuple(RevList)} || {Id, RevList} <- DocIdRevsList]},
    {obj, ResponseMembers} =
    do_http_request(DbUrl ++ "_missing_revs",
        post, JsonDocIdRevsList),
    {obj, DocMissingRevsList} = proplists:get_value("missing_revs", ResponseMembers),
    {ok, [{Id, tuple_to_list(MissingRevs)} || {Id, MissingRevs} <- DocMissingRevsList]};
get_missing_revs(Db, DocId) ->
    couch_db:get_missing_revs(Db, DocId).


update_doc(DbUrl, #doc{id=DocId}=Doc, _Options) when is_list(DbUrl) ->
    Url = DbUrl ++ url_encode(DocId),
    {obj, ResponseMembers} =
        do_http_request(Url, put, couch_doc:to_json_obj(Doc, [revs,attachments])),
    RevId = proplists:get_value("_rev", ResponseMembers),
    {ok, RevId};
update_doc(Db, Doc, Options) ->
    couch_db:update_doc(Db, Doc, Options).

save_docs(_, [], _) ->
    ok;
save_docs(DbUrl, Docs, []) when is_list(DbUrl) ->
    JsonDocs = [couch_doc:to_json_obj(Doc, [revs,attachments]) || Doc <- Docs],
    {obj, Returned} =
        do_http_request(DbUrl ++ "_bulk_docs", post, {obj, [{new_edits, false}, {docs, list_to_tuple(JsonDocs)}]}),
    true = proplists:get_value("ok", Returned),
    ok;
save_docs(Db, Docs, Options) ->
    couch_db:save_docs(Db, Docs, Options).


open_doc(DbUrl, DocId, []) when is_list(DbUrl) ->
    case do_http_request(DbUrl ++ url_encode(DocId), get) of
    {obj, [{"error", ErrId}, {"reason", Reason}]} ->
        {list_to_atom(ErrId), Reason};
    Doc  ->
        {ok, couch_doc:from_json_obj(Doc)}
    end;
open_doc(Db, DocId, Options) when not is_list(Db) ->
    couch_db:open_doc(Db, DocId, Options).


open_doc_revs(DbUrl, DocId, Revs, Options) when is_list(DbUrl) ->
    QueryOptionStrs =
    lists:map(fun(latest) ->
            % latest is only option right now
            "latest=true"
        end, Options),
    RevsQueryStrs = lists:flatten(cjson:encode(list_to_tuple(Revs))),
    Url = DbUrl ++ DocId ++ "?" ++ couch_util:implode(["revs=true", "attachments=true", "open_revs=" ++ RevsQueryStrs ] ++ QueryOptionStrs, "&"),
    JsonResults = do_http_request(Url, get, []),
    Results =
    lists:map(
        fun({obj, [{"missing", Rev}]}) ->
            {{not_found, missing}, Rev};
        ({obj, [{"ok", JsonDoc}]}) ->
            {ok, couch_doc:from_json_obj(JsonDoc)}
        end, tuple_to_list(JsonResults)),
    {ok, Results};
open_doc_revs(Db, DocId, Revs, Options) ->
    couch_db:open_doc_revs(Db, DocId, Revs, Options).


