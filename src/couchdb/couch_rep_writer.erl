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
-define (MAX_CHUNK_SIZE, 65535).

start_link(Parent, Target, Reader, _PostProps) ->
    {ok, spawn_link(fun() -> writer_loop(Parent, Reader, Target) end)}.

make_chunk(Data) when is_list(Data) ->
    make_chunk(list_to_binary(Data));
make_chunk(Data) when size(Data) > ?MAX_CHUNK_SIZE ->
    <<ChunkData:?MAX_CHUNK_SIZE/binary, Rest/binary>> = Data,
    Chunk = {?MAX_CHUNK_SIZE, [ibrowse_lib:dec2hex(4, ?MAX_CHUNK_SIZE), "\r\n",
        ChunkData, "\r\n"]},
    [Chunk, Rest];
make_chunk(Data) ->
    Size = size(Data),
    [{Size, [ibrowse_lib:dec2hex(4, Size), "\r\n", Data, "\r\n"]}].

upload_docs({start, Buffer}) ->
    [{Size, Chunk}] = make_chunk(<<"{\"new_edits\":false, \"docs\":[">>),
    % Buffer ! {self(), next_doc},
    {ok, Chunk, {continue, Buffer, "", Size}};

upload_docs({continue, _Buf, _Pre, ByteCount}) when ByteCount > ?MAX_BYTES ->
    {ok, "2\r\n]}\r\n0\r\n\r\n", finish};
upload_docs({continue, Buffer, Prepend, ByteCount}) ->
    Buffer ! {self(), next_doc},
    receive 
    {ok, JsonDoc} ->
        [{Size, Chunk} | MoreData] = make_chunk([Prepend, JsonDoc]),
        {ok, Chunk, {multi_chunk, MoreData, Buffer, ",", ByteCount+Size}};
    eof ->
        {ok, "2\r\n]}\r\n0\r\n\r\n", finish};
    timeout ->
        ?LOG_DEBUG("sending a heartbeat to keep the connection open", []),
        {ok, "1\r\n \r\n", {continue, Buffer, Prepend, ByteCount}}
    end;

upload_docs({multi_chunk, [], Buffer, Prepend, ByteCount}) ->
    % Buffer ! {self(), next_doc},
    upload_docs({continue, Buffer, Prepend, ByteCount});
upload_docs({multi_chunk, [Data], Buffer, Prepend, ByteCount}) ->
    [{Size, Chunk} | MoreData] = make_chunk(Data),
    {ok, Chunk, {multi_chunk, MoreData, Buffer, Prepend, ByteCount+Size}};

upload_docs(finish) ->
    couch_util:should_flush(),
    eof.

encoding_worker([]) ->
    receive {MiddleMan, next_doc} -> MiddleMan ! eof end,
    ok;
encoding_worker([Doc|Rest]) ->
    JsonDoc = ?l2b(?JSON_ENCODE(couch_doc:to_json_obj(Doc,[revs,attachments]))),
    receive {MiddleMan, next_doc} -> MiddleMan ! {ok, JsonDoc} end,
    encoding_worker(Rest).

% needed because upload_docs is inside a gen_server so it can't have a mailbox
middle_man(EncodingWorker) ->
    receive {Uploader, next_doc} ->
        receive
        {ok, JsonDoc} ->
            EncodingWorker ! {self(), next_doc},
            Uploader ! {ok, JsonDoc},
            middle_man(EncodingWorker);
        eof ->
            Uploader ! eof,
            ok
        after 5000 ->
            Uploader ! timeout,
            middle_man(EncodingWorker)
        end
    end.

request_loop(Ref, Request, Acc) ->
    receive
    {'DOWN', Ref, _, _, normal} ->
        lists:flatten(lists:reverse(Acc));
    {'DOWN', Ref, _, _, Reason} ->
        exit(Reason)
    after 0 ->
        ErrorsJson = couch_rep_httpc:request(Request),
        request_loop(Ref, Request, [ErrorsJson|Acc])
    end.

writer_loop(Parent, Reader, Target) ->
    case couch_rep_reader:next(Reader) of
    {complete, FinalSeq} ->
        Parent ! {writer_checkpoint, FinalSeq},
        ok;
    {HighSeq, Docs} ->
        DocCount = length(Docs),
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
        Parent ! {writer_checkpoint, HighSeq},
        couch_rep_att:cleanup(),
        couch_util:should_flush(),
        writer_loop(Parent, Reader, Target)
    end.

write_docs(#http_db{} = Db, Docs) ->
    {Worker, Ref} = spawn_monitor(fun() -> encoding_worker(Docs) end),
    Pid = spawn_link(fun() -> Worker ! {self(),next_doc}, middle_man(Worker) end),
    Request = Db#http_db{
        resource = "_bulk_docs",
        method = post,
        body = {fun upload_docs/1, {start, Pid}},
        headers = [{"transfer-encoding", "chunked"} | Db#http_db.headers]
    },
    ErrorsJson = request_loop(Ref, Request, []),
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
    {ok, ErrorsList};
write_docs(Db, Docs) ->
    couch_db:update_docs(Db, Docs, [], replicated_changes).
