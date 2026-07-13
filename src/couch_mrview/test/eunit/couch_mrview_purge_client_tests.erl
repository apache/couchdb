% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_mrview_purge_client_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(DDOC_ID, <<"_design/viewddoc">>).

purge_client_verification_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_verify_matching_index),
                ?TDEF_FE(t_verify_false_on_wrong_type),
                ?TDEF_FE(t_verify_false_on_missing_ddoc),
                ?TDEF_FE(t_verify_false_on_signature_mismatch),
                ?TDEF_FE(t_verify_true_on_ddoc_read_error),
                ?TDEF_FE(t_verify_true_on_exception),
                ?TDEF_FE(t_verify_true_on_missing_db),
                ?TDEF_FE(t_verify_true_on_failed_clustered_ddoc_read),
                ?TDEF_FE(t_compaction_retains_history_on_unverifiable_checkpoint, 15),
                ?TDEF_FE(t_compaction_retains_history_for_live_checkpoint, 15),
                ?TDEF_FE(t_compaction_prunes_history_of_deleted_index, 15)
            ]
        }
    }.

setup_all() ->
    test_util:start_couch([mem3, fabric]).

teardown_all(Ctx) ->
    test_util:stop_couch(Ctx).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    {ok, _} = couch_db:update_doc(Db, couch_doc:from_json_obj(ddoc()), []),
    ok = couch_db:close(Db),
    DbName.

teardown(DbName) ->
    catch meck:unload(),
    couch_server:delete(DbName, [?ADMIN_CTX]).

ddoc() ->
    {[
        {<<"_id">>, ?DDOC_ID},
        {<<"views">>,
            {[
                {<<"v">>,
                    {[
                        {<<"map">>, <<"function(doc){emit(doc._id, 1);}">>}
                    ]}}
            ]}}
    ]}.

props(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    try
        {ok, DDoc} = couch_db:get_design_doc(Db, ?DDOC_ID),
        props_from(DbName, DDoc)
    after
        couch_db:close(Db)
    end.

props_from(DbName, DDoc) ->
    {ok, #mrst{sig = Sig}} = couch_mrview_util:ddoc_to_mrst(DbName, DDoc),
    [
        {<<"type">>, <<"mrview">>},
        {<<"ddoc_id">>, ?DDOC_ID},
        {<<"signature">>, couch_util:to_hex_bin(Sig)}
    ].

replace(Key, Val, Props) ->
    lists:keyreplace(Key, 1, Props, {Key, Val}).

t_verify_matching_index(DbName) ->
    ?assert(couch_mrview_index:verify_index_exists(DbName, props(DbName))).

t_verify_false_on_wrong_type(DbName) ->
    Props = replace(<<"type">>, <<"dreyfus">>, props(DbName)),
    ?assertNot(couch_mrview_index:verify_index_exists(DbName, Props)).

t_verify_false_on_missing_ddoc(DbName) ->
    Props = replace(<<"ddoc_id">>, <<"_design/missing">>, props(DbName)),
    ?assertNot(couch_mrview_index:verify_index_exists(DbName, Props)).

t_verify_false_on_signature_mismatch(DbName) ->
    Props = replace(<<"signature">>, <<"deadbeef">>, props(DbName)),
    ?assertNot(couch_mrview_index:verify_index_exists(DbName, Props)).

% Even on timeout we'd like to return true. We only want to return false if we've
% gotten a response that it's definitely gone.
t_verify_true_on_ddoc_read_error(DbName) ->
    Props = props(DbName),
    meck:new(couch_db, [passthrough]),
    meck:expect(couch_db, get_design_doc, fun(_, _) -> {error, timeout} end),
    ?assert(couch_mrview_index:verify_index_exists(DbName, Props)).

t_verify_true_on_exception(DbName) ->
    Props = props(DbName),
    meck:new(couch_db, [passthrough]),
    meck:expect(couch_db, get_design_doc, fun(_, _) -> meck:exception(error, boom) end),
    ?assert(couch_mrview_index:verify_index_exists(DbName, Props)).

t_verify_true_on_missing_db(DbName) ->
    ?assert(couch_mrview_index:verify_index_exists(?tempdb(), props(DbName))).

t_verify_true_on_failed_clustered_ddoc_read(_DbName) ->
    % Clustered reads will fail but we'll still return true until cleanup runs
    ShardDb = <<"shards/00000000-ffffffff/", (?tempdb())/binary, ".1234567890">>,
    {ok, Db} = couch_db:create(ShardDb, [?ADMIN_CTX]),
    {ok, _} = couch_db:update_doc(Db, couch_doc:from_json_obj(ddoc()), []),
    ok = couch_db:close(Db),
    Props = props_from(ShardDb, couch_doc:from_json_obj(ddoc())),
    try
        ?assert(couch_mrview_index:verify_index_exists(ShardDb, Props))
    after
        couch_server:delete(ShardDb, [?ADMIN_CTX])
    end.

t_compaction_retains_history_on_unverifiable_checkpoint(DbName) ->
    ok = prepare_purges(DbName),
    ok = write_checkpoint(DbName, props(DbName)),
    meck:new(couch_db, [passthrough]),
    meck:expect(couch_db, get_design_doc, fun(_, _) -> {error, timeout} end),
    ok = compact(DbName),
    % purge info kept since we couldn't verify the client
    ?assertEqual(1, oldest_purge_seq(DbName)).

t_compaction_retains_history_for_live_checkpoint(DbName) ->
    ok = prepare_purges(DbName),
    ok = write_checkpoint(DbName, props(DbName)),
    ok = compact(DbName),
    ?assertEqual(1, oldest_purge_seq(DbName)).

t_compaction_prunes_history_of_deleted_index(DbName) ->
    ok = prepare_purges(DbName),
    Props = replace(<<"ddoc_id">>, <<"_design/missing">>, props(DbName)),
    ok = write_checkpoint(DbName, Props),
    ok = compact(DbName),
    % Now we could check index is definitely gone the oldest purge seq moved up
    ?assertEqual(4, oldest_purge_seq(DbName)).

prepare_purges(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    try
        ok = couch_db:set_purge_infos_limit(Db, 2),
        lists:foreach(
            fun(I) ->
                Id = integer_to_binary(I),
                Doc = couch_doc:from_json_obj({[{<<"_id">>, Id}]}),
                {ok, Rev} = couch_db:update_doc(Db, Doc, []),
                {ok, _} = couch_db:purge_docs(Db, [{couch_uuids:random(), Id, [Rev]}])
            end,
            lists:seq(1, 5)
        )
    after
        couch_db:close(Db)
    end.

write_checkpoint(DbName, Props) ->
    Sig = couch_util:get_value(<<"signature">>, Props),
    Doc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"_local/purge-mrview-", Sig/binary>>},
            {<<"purge_seq">>, 0},
            {<<"updated_on">>, erlang:system_time(second)}
            | Props
        ]}
    ),
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    try
        {ok, _} = couch_db:update_doc(Db, Doc, []),
        ok
    after
        couch_db:close(Db)
    end.

compact(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    try
        {ok, _} = couch_db:start_compact(Db),
        ok = couch_db:wait_for_compaction(Db)
    after
        couch_db:close(Db)
    end.

oldest_purge_seq(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    try
        couch_db:get_oldest_purge_seq(Db)
    after
        couch_db:close(Db)
    end.
