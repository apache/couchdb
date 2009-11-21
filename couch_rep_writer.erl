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

write_docs(#http_db{headers = Headers} = Db, Docs) ->
    JsonDocs = [couch_doc:to_json_obj(Doc, [revs,attachments]) || Doc <- Docs],
    Request = Db#http_db{
        resource = "_bulk_docs",
        method = post,
        body = {[{new_edits, false}, {docs, JsonDocs}]},
        headers = [{"x-couch-full-commit", "false"} | Headers]
    },
    ErrorsJson = case couch_rep_httpc:request(Request) of
    {FailProps} ->
        exit({target_error, proplists:get_value(<<"error">>, FailProps)});
    List when is_list(List) ->
        List
    end,
    ErrorsList = [write_docs_1(V) || V <- ErrorsJson],
    {ok, ErrorsList};
write_docs(Db, Docs) ->
    couch_db:update_docs(Db, Docs, [delay_commit], replicated_changes).

write_docs_1({Props}) ->
    Id = proplists:get_value(<<"id">>, Props),
    Rev = couch_doc:parse_rev(proplists:get_value(<<"rev">>, Props)),
    ErrId = couch_util:to_existing_atom(proplists:get_value(<<"error">>, Props)),
    Reason = proplists:get_value(<<"reason">>, Props),
    {{Id, Rev}, {ErrId, Reason}}.
