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

-module(couch_rep_writer).

-export([start_link/4]).

-include("couch_db.hrl").

start_link(Parent, _Target, Reader, _PostProps) ->
    {ok, spawn_link(fun() -> writer_loop(Parent, Reader) end)}.

writer_loop(Parent, Reader) ->
    case couch_rep_reader:next(Reader) of
    {complete, nil} ->
        ok;
    {complete, FinalSeq} ->
        Parent ! {writer_checkpoint, FinalSeq},
        ok;
    {HighSeq, Docs} ->
        DocCount = length(Docs),
        {ok, Target} = gen_server:call(Parent, get_target_db, infinity),
        try write_docs(Target, Docs) of
        {ok, []} ->
            Parent ! {update_stats, docs_written, DocCount};
        {ok, Errors} ->
            ErrorCount = length(Errors),
            Parent ! {update_stats, doc_write_failures, ErrorCount},
            Parent ! {update_stats, docs_written, DocCount - ErrorCount}
        catch
        {attachment_request_failed, Err} ->
            ?LOG_DEBUG("writer failed to write an attachment ~p", [Err]),
            exit({attachment_request_failed, Err, Docs})
        end,
        case HighSeq of
        nil ->
            ok;
        _SeqNumber ->
            Parent ! {writer_checkpoint, HighSeq}
        end,
        couch_rep_att:cleanup(),
        couch_util:should_flush(),
        writer_loop(Parent, Reader)
    end.

write_docs(#http_db{} = Db, Docs) ->
    {DocsAtts, DocsNoAtts} = lists:partition(
        fun(#doc{atts=[]}) -> false; (_) -> true end,
        Docs
    ),
    ErrorsJson0 = write_bulk_docs(Db, DocsNoAtts),
    ErrorsJson = lists:foldl(
       fun(Doc, Acc) -> write_multi_part_doc(Db, Doc) ++ Acc end,
       ErrorsJson0,
       DocsAtts
    ),
    {ok, ErrorsJson};
write_docs(Db, Docs) ->
    couch_db:update_docs(Db, Docs, [delay_commit], replicated_changes).

write_bulk_docs(_Db, []) ->
    [];
write_bulk_docs(#http_db{headers = Headers} = Db, Docs) ->
    JsonDocs = [
        couch_doc:to_json_obj(Doc, [revs, att_gzip_length]) || Doc <- Docs
    ],
    Request = Db#http_db{
        resource = "_bulk_docs",
        method = post,
        body = {[{new_edits, false}, {docs, JsonDocs}]},
        headers = couch_util:proplist_apply_field({"Content-Type", "application/json"}, [{"X-Couch-Full-Commit", "false"} | Headers])
    },
    ErrorsJson = case couch_rep_httpc:request(Request) of
    {FailProps} ->
        exit({target_error, couch_util:get_value(<<"error">>, FailProps)});
    List when is_list(List) ->
        List
    end,
    [write_docs_1(V) || V <- ErrorsJson].

write_multi_part_doc(#http_db{headers=Headers} = Db, #doc{atts=Atts} = Doc) ->
    JsonBytes = ?JSON_ENCODE(
        couch_doc:to_json_obj(
            Doc,
            [follows, att_encoding_info, attachments]
        )
    ),
    Boundary = couch_uuids:random(),
    {ContentType, Len} = couch_doc:len_doc_to_multi_part_stream(
        Boundary, JsonBytes, Atts, true
    ),
    StreamerPid = spawn_link(
        fun() -> streamer_fun(Boundary, JsonBytes, Atts) end
    ),
    BodyFun = fun(Acc) ->
        DataQueue = case Acc of
        nil ->
            StreamerPid ! {start, self()},
            receive
            {queue, Q} ->
                Q
            end;
        Queue ->
            Queue
        end,
        case couch_work_queue:dequeue(DataQueue) of
        closed ->
            eof;
        {ok, Data} ->
            {ok, iolist_to_binary(Data), DataQueue}
        end
    end,
    Request = Db#http_db{
        resource = couch_util:url_encode(Doc#doc.id),
        method = put,
        qs = [{new_edits, false}],
        body = {BodyFun, nil},
        headers = [
            {"x-couch-full-commit", "false"},
            {"Content-Type", ?b2l(ContentType)},
            {"Content-Length", Len} | Headers
        ]
    },
    Result = case couch_rep_httpc:request(Request) of
    {[{<<"error">>, Error}, {<<"reason">>, Reason}]} ->
        {Pos, [RevId | _]} = Doc#doc.revs,
        ErrId = couch_util:to_existing_atom(Error),
        [{Doc#doc.id, couch_doc:rev_to_str({Pos, RevId})}, {ErrId, Reason}];
    _ ->
        []
    end,
    StreamerPid ! stop,
    Result.

streamer_fun(Boundary, JsonBytes, Atts) ->
    receive
    stop ->
        ok;
    {start, From} ->
        % better use a brand new queue, to ensure there's no garbage from
        % a previous (failed) iteration
        {ok, DataQueue} = couch_work_queue:new(1024 * 1024, 1000),
        From ! {queue, DataQueue},
        couch_doc:doc_to_multi_part_stream(
            Boundary,
            JsonBytes,
            Atts,
            fun(Data) ->
                couch_work_queue:queue(DataQueue, Data)
            end,
            true
        ),
        couch_work_queue:close(DataQueue),
        streamer_fun(Boundary, JsonBytes, Atts)
    end.

write_docs_1({Props}) ->
    Id = couch_util:get_value(<<"id">>, Props),
    Rev = couch_doc:parse_rev(couch_util:get_value(<<"rev">>, Props)),
    ErrId = couch_util:to_existing_atom(couch_util:get_value(<<"error">>, Props)),
    Reason = couch_util:get_value(<<"reason">>, Props),
    {{Id, Rev}, {ErrId, Reason}}.
