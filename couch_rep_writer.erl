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

-define (MAX_BYTES, 10000000).

start_link(Parent, Target, Reader, _PostProps) ->
    {ok, spawn_link(fun() -> writer_loop(Parent, Reader, Target) end)}.

make_chunk(Data) when is_list(Data) ->
    make_chunk(list_to_binary(Data));
make_chunk(Data) ->
    Size = size(Data),
    {Size, [ibrowse_lib:dec2hex(8, Size), "\r\n", Data, "\r\n"]}.

upload_docs({start, W, Docs}) ->
    {Size, Chunk} = make_chunk(<<"{\"new_edits\":false, \"docs\":[">>),
    {ok, Chunk, {continue, W, Docs, "", Size}};
upload_docs({continue, W, Docs, _, ByteCount}) when ByteCount > ?MAX_BYTES ->
    W ! {docs_remaining, length(Docs)},
    {ok, "2\r\n]}\r\n", last_chunk};
upload_docs({continue, W, [Doc|Rest], Prepend, ByteCount}) ->
    JsonDoc = ?JSON_ENCODE(couch_doc:to_json_obj(Doc, [revs,attachments])),
    {Size, Chunk} = make_chunk([Prepend, JsonDoc]),
    {ok, Chunk, {continue, W, Rest, ",", ByteCount+Size}};
upload_docs({continue, W, [], _, _}) ->
    W ! {docs_remaining, 0},
    {ok, "2\r\n]}\r\n", last_chunk};
upload_docs(last_chunk) ->
    {ok, "0\r\n\r\n", finish};
upload_docs(finish) ->
    couch_util:should_flush(),
    eof.
    
writer_loop(Parent, Reader, Target) ->
    case couch_rep_reader:next(Reader) of
    {complete, FinalSeq} ->
        Parent ! {writer_checkpoint, FinalSeq},
        ok;
    {HighSeq, Docs} ->
        DocCount = length(Docs),
        try write_docs(Target, Docs, []) of
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
        Parent ! {writer_checkpoint, HighSeq},
        couch_rep_att:cleanup(),
        couch_util:should_flush(),
        writer_loop(Parent, Reader, Target)
    end.

write_docs(#http_db{} = Db, Docs, ErrorsAcc) ->
    ErrorsJson = couch_rep_httpc:request(Db#http_db{
        resource = "_bulk_docs",
        method = post,
        body = {fun upload_docs/1, {start, self(), Docs}},
        headers = [{"transfer-encoding", "chunked"} | Db#http_db.headers]
    }),
    ErrorsList =
    lists:map(
        fun({Props}) ->
            Id = proplists:get_value(<<"id">>, Props),
            Rev = couch_doc:parse_rev(proplists:get_value(<<"rev">>, Props)),
            ErrId = couch_util:to_existing_atom(
                    proplists:get_value(<<"error">>, Props)),
            Reason = proplists:get_value(<<"reason">>, Props),
            {{Id, Rev}, {ErrId, Reason}}
        end, ErrorsJson),
    receive
    {docs_remaining, 0} ->
        {ok, lists:flatten([ErrorsList|ErrorsAcc])};
    {docs_remaining, N} ->
        MoreDocs = lists:nthtail(length(Docs)-N, Docs),
        write_docs(Db, MoreDocs, [ErrorsList|ErrorsAcc])
    end;
write_docs(Db, Docs, _) ->
    couch_db:update_docs(Db, Docs, [], replicated_changes).
