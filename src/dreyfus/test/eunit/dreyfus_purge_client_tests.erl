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

-module(dreyfus_purge_client_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("dreyfus/include/dreyfus.hrl").

-define(DDOC_ID, <<"_design/searchddoc">>).
-define(INDEX_NAME, <<"searchidx">>).

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
                ?TDEF_FE(t_verify_false_on_missing_index),
                ?TDEF_FE(t_verify_false_on_signature_mismatch),
                ?TDEF_FE(t_verify_true_on_ddoc_read_error),
                ?TDEF_FE(t_verify_true_on_exception),
                ?TDEF_FE(t_verify_true_on_missing_db),
                ?TDEF_FE(t_verify_true_on_failed_clustered_ddoc_read)
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
        {<<"indexes">>,
            {[
                {?INDEX_NAME,
                    {[
                        {<<"index">>, <<"function(doc){index(\"def\", doc.val);}">>}
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
    {ok, #index{sig = Sig}} = dreyfus_index:design_doc_to_index(DbName, DDoc, ?INDEX_NAME),
    [
        {<<"type">>, <<"dreyfus">>},
        {<<"ddoc_id">>, ?DDOC_ID},
        {<<"indexname">>, ?INDEX_NAME},
        {<<"signature">>, Sig}
    ].

replace(Key, Val, Props) ->
    lists:keyreplace(Key, 1, Props, {Key, Val}).

t_verify_matching_index(DbName) ->
    ?assert(dreyfus_util:verify_index_exists(DbName, props(DbName))).

t_verify_false_on_wrong_type(DbName) ->
    Props = replace(<<"type">>, <<"mrview">>, props(DbName)),
    ?assertNot(dreyfus_util:verify_index_exists(DbName, Props)).

t_verify_false_on_missing_ddoc(DbName) ->
    Props = replace(<<"ddoc_id">>, <<"_design/missing">>, props(DbName)),
    ?assertNot(dreyfus_util:verify_index_exists(DbName, Props)).

t_verify_false_on_missing_index(DbName) ->
    Props = replace(<<"indexname">>, <<"missingidx">>, props(DbName)),
    ?assertNot(dreyfus_util:verify_index_exists(DbName, Props)).

t_verify_false_on_signature_mismatch(DbName) ->
    Props = replace(<<"signature">>, <<"deadbeef">>, props(DbName)),
    ?assertNot(dreyfus_util:verify_index_exists(DbName, Props)).

% On timeout or other random error we want to still return that index exists as
% we don't want purges to be pruned until we're sure each client has seen them
t_verify_true_on_ddoc_read_error(DbName) ->
    Props = props(DbName),
    meck:new(couch_db, [passthrough]),
    meck:expect(couch_db, get_design_doc, fun(_, _) -> {error, timeout} end),
    ?assert(dreyfus_util:verify_index_exists(DbName, Props)).

t_verify_true_on_exception(DbName) ->
    Props = props(DbName),
    meck:new(couch_db, [passthrough]),
    meck:expect(couch_db, get_design_doc, fun(_, _) -> meck:exception(error, boom) end),
    ?assert(dreyfus_util:verify_index_exists(DbName, Props)).

t_verify_true_on_missing_db(DbName) ->
    ?assert(dreyfus_util:verify_index_exists(?tempdb(), props(DbName))).

t_verify_true_on_failed_clustered_ddoc_read(_DbName) ->
    % Clustered ddoc read fails here but we'll still expect verify index to return true
    ShardDb = <<"shards/00000000-ffffffff/", (?tempdb())/binary, ".1234567890">>,
    {ok, Db} = couch_db:create(ShardDb, [?ADMIN_CTX]),
    {ok, _} = couch_db:update_doc(Db, couch_doc:from_json_obj(ddoc()), []),
    ok = couch_db:close(Db),
    Props = props_from(ShardDb, couch_doc:from_json_obj(ddoc())),
    try
        ?assert(dreyfus_util:verify_index_exists(ShardDb, Props))
    after
        couch_server:delete(ShardDb, [?ADMIN_CTX])
    end.
