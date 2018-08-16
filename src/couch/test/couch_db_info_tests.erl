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

-module(couch_db_info_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DELAY, 100).
-define(WAIT_DELAY_COUNT, 40).

setup() ->
    Ctx = test_util:start_couch(),
    config:set("couchdb", "file_compression", "none", false),
    DbName = ?tempdb(),
    DocId = couch_uuids:random(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    {DbName, DocId, Ctx}.

teardown({DbName, _, Ctx}) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    test_util:stop_couch(Ctx).

db_info_sizes_test_() ->
    {
        setup,
        fun setup/0, fun teardown/1,
        fun({DbName, DocId, _}) ->
            {
                inorder,
                [
                    ?_test(should_change_with_doc_insert(DbName, DocId)),
                    ?_test(should_change_with_doc_update(DbName, DocId)),
                    ?_test(should_change_with_att_upload(DbName, DocId)),
                    ?_test(should_change_with_att_update(DbName, DocId)),
                    ?_test(should_change_with_att_delete(DbName, DocId)),
                    ?_test(should_change_with_doc_delete(DbName, DocId)),
                    ?_test(should_change_after_compact(DbName))
                ]
            }
        end
    }.


should_change_with_doc_insert(DbName, DocId) ->
    {ok, PreSizes} = get_db_sizes(DbName),
    PreActiveSize = couch_util:get_value(active, PreSizes),
    PreExternalSize = couch_util:get_value(external, PreSizes),
    {ok, _} = update_doc(DbName, DocId, ?l2b(lists:duplicate(60, $#))),
    {ok, PostSizes} = get_db_sizes(DbName),
    PostActiveSize = couch_util:get_value(active, PostSizes),
    PostExternalSize = couch_util:get_value(external, PostSizes),
    ?assert(PostActiveSize > PreActiveSize),
    ?assert(PostExternalSize > PreExternalSize).

should_change_with_doc_update(DbName, DocId) ->
    {ok, PreSizes} = get_db_sizes(DbName),
    PreActiveSize = couch_util:get_value(active, PreSizes),
    PreExternalSize = couch_util:get_value(external, PreSizes),
    {ok, _} = update_doc(DbName, DocId, <<>>),
    {ok, PostSizes} = get_db_sizes(DbName),
    PostActiveSize = couch_util:get_value(active, PostSizes),
    PostExternalSize = couch_util:get_value(external, PostSizes),
    ?assert(PostActiveSize > PreActiveSize),
    ?assert(PostExternalSize < PreExternalSize).

should_change_with_att_upload(DbName, DocId) ->
    {ok, PreSizes} = get_db_sizes(DbName),
    PreActiveSize = couch_util:get_value(active, PreSizes),
    PreExternalSize = couch_util:get_value(external, PreSizes),
    Atts = [new_att(<<"one">>, 1024), new_att(<<"two">>, 8192)],
    {ok, _} = update_doc(DbName, DocId, <<>>, Atts),
    {ok, PostSizes} = get_db_sizes(DbName),
    PostActiveSize = couch_util:get_value(active, PostSizes),
    PostExternalSize = couch_util:get_value(external, PostSizes),
    ?assert(PostActiveSize > PreActiveSize),
    ?assert(PostExternalSize > PreExternalSize).

should_change_with_att_update(DbName, DocId) ->
    {ok, PreSizes} = get_db_sizes(DbName),
    PreActiveSize = couch_util:get_value(active, PreSizes),
    PreExternalSize = couch_util:get_value(external, PreSizes),
    Atts = [new_att(<<"one">>, 1024), new_att(<<"two">>, 4096)],
    {ok, _} = update_doc(DbName, DocId, <<>>, Atts),
    {ok, PostSizes} = get_db_sizes(DbName),
    PostActiveSize = couch_util:get_value(active, PostSizes),
    PostExternalSize = couch_util:get_value(external, PostSizes),
    ?assert(PostActiveSize > PreActiveSize),
    ?assert(PostExternalSize < PreExternalSize).

should_change_with_att_delete(DbName, DocId) ->
    {ok, PreSizes} = get_db_sizes(DbName),
    PreActiveSize = couch_util:get_value(active, PreSizes),
    PreExternalSize = couch_util:get_value(external, PreSizes),
    %% we are deleting attachment <<"one">> by excluding it from upload
    Atts = [new_att(<<"two">>, 4096)],
    {ok, _} = update_doc(DbName, DocId, <<>>, Atts),
    {ok, PostSizes} = get_db_sizes(DbName),
    PostActiveSize = couch_util:get_value(active, PostSizes),
    PostExternalSize = couch_util:get_value(external, PostSizes),
    ?assert(PostActiveSize > PreActiveSize),
    ?assert(PostExternalSize < PreExternalSize).

should_change_with_doc_delete(DbName, DocId) ->
    {ok, PreSizes} = get_db_sizes(DbName),
    PreActiveSize = couch_util:get_value(active, PreSizes),
    PreExternalSize = couch_util:get_value(external, PreSizes),
    PreFileSize = couch_util:get_value(file, PreSizes),
    {ok, _} = delete_doc(DbName, DocId),
    {ok, PostSizes} = get_db_sizes(DbName),
    PostActiveSize = couch_util:get_value(active, PostSizes),
    PostExternalSize = couch_util:get_value(external, PostSizes),
    PostFileSize = couch_util:get_value(file, PostSizes),
    ?assert(PostActiveSize > PreActiveSize),
    ?assert(PostExternalSize < PreExternalSize),
    ?assert(PostFileSize > PreFileSize).

should_change_after_compact(DbName) ->
    {ok, PreSizes} = get_db_sizes(DbName),
    PreActiveSize = couch_util:get_value(active, PreSizes),
    PreExternalSize = couch_util:get_value(external, PreSizes),
    PreFileSize = couch_util:get_value(file, PreSizes),
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _} = couch_db:start_compact(Db),
    ok = couch_db:close(Db),
    wait_compact_done(DbName, ?WAIT_DELAY_COUNT),
    {ok, PostSizes} = get_db_sizes(DbName),
    PostActiveSize = couch_util:get_value(active, PostSizes),
    PostExternalSize = couch_util:get_value(external, PostSizes),
    PostFileSize = couch_util:get_value(file, PostSizes),
    ?assert(PostActiveSize < PreActiveSize),
    ?assert(PostExternalSize == PreExternalSize),
    ?assert(PostFileSize < PreFileSize).


update_doc(DbName, DocId, DocBody) ->
    update_doc(DbName, DocId, DocBody, []).

update_doc(DbName, DocId, DocBody, Atts) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    Doc = case couch_db:get_doc_info(Db, DocId) of
        not_found ->
            couch_doc:from_json_obj({[
                {<<"_id">>, DocId},
                {<<"body">>, DocBody}
            ]});
        {ok, _} ->
            Doc0 = couch_httpd_db:couch_doc_open(Db, DocId, nil, []),
            Doc0#doc{body = {[{<<"body">>, DocBody}]}, atts = Atts}
    end,
    Result = couch_db:update_doc(Db, Doc, []),
    couch_db:close(Db),
    Result.

delete_doc(DbName, DocId) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, DI} = couch_db:get_doc_info(Db, DocId),
    RR = hd(DI#doc_info.revs),
    Rev = RR#rev_info.rev,
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, DocId},
        {<<"_rev">>, couch_doc:rev_to_str(Rev)},
        {<<"_deleted">>, true}
    ]}),
    Result = couch_db:update_doc(Db, Doc, [Rev]),
    couch_db:close(Db),
    Result.

wait_compact_done(_DbName, 0) ->
    erlang:error({assertion_failed, [
        {module, ?MODULE},
        {line, ?LINE},
        {reason, "DB compaction failed to finish"}
    ]});
wait_compact_done(DbName, N) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    ok = couch_db:close(Db),
    CompactorPid = couch_db:get_compactor_pid(Db),
    case is_pid(CompactorPid) of
        false ->
            ok;
        true ->
            ok = timer:sleep(?DELAY),
            wait_compact_done(DbName, N - 1)
    end.

get_db_info(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    ok = couch_db:close(Db),
    couch_db:get_db_info(Db).

get_db_sizes(DbName) ->
    {ok, Info} = get_db_info(DbName),
    {Sizes} = couch_util:get_nested_json_value({Info}, [sizes]),
    {ok, Sizes}.

new_att(Name, Size) ->
    couch_att:new([
        {name, Name},
        {type, <<"application/octet-stream">>},
        {data, crypto:strong_rand_bytes(Size)},
        {att_len, undefined},
        {md5, <<>>},
        {encoding, identity}
    ]).
