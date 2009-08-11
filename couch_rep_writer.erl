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

start_link(Parent, Target, Reader, _PostProps) ->
    {ok, spawn_link(fun() -> writer_loop(Parent, Reader, Target) end)}.

make_chunk(Data) when is_list(Data) ->
    make_chunk(list_to_binary(Data));
make_chunk(Data) ->
    [ibrowse_lib:dec2hex(4, size(Data)), "\r\n", Data, "\r\n"].

upload_docs({start, Docs}) ->
    Data = make_chunk(<<"{\"new_edits\":false, \"docs\":[">>),
    {ok, Data, {continue, Docs, ""}};
upload_docs({continue, [Doc|Rest], Prepend}) ->
    JsonDoc = ?JSON_ENCODE(couch_doc:to_json_obj(Doc, [revs,attachments])),
    {ok, make_chunk([Prepend, JsonDoc]), {continue, Rest, ","}};
upload_docs({continue, [], _}) ->
    {ok, make_chunk(<<"]}\n">>), last_chunk};
upload_docs(last_chunk) ->
    {ok, "0\r\n\r\n", finish};
upload_docs(finish) ->
    eof.
    
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
        writer_loop(Parent, Reader, Target)
    end.

write_docs(#http_db{} = Db, Docs) ->
    ErrorsJson = couch_rep_httpc:request(Db#http_db{
        resource = "_bulk_docs",
        method = post,
        body = {fun upload_docs/1, {start, Docs}},
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
    {ok, ErrorsList};
write_docs(Db, Docs) ->
    couch_db:update_docs(Db, Docs, [], replicated_changes).
