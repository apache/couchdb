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

-module(cpse_test_get_set_props).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").


setup_each() ->
    cpse_util:dbname().


teardown_each(DbName) ->
    ok = couch_server:delete(DbName, []).


cpse_default_props(DbName) ->
    {ok, {_App, Engine, _Extension}} = application:get_env(couch, test_engine),
    {ok, Db} = cpse_util:create_db(DbName),
    Node = node(),

    ?assertEqual(Engine, couch_db_engine:get_engine(Db)),
    ?assertEqual(0, couch_db_engine:get_doc_count(Db)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db)),
    ?assertEqual(true, is_list(couch_db_engine:get_size_info(Db))),
    ?assertEqual(true, is_integer(couch_db_engine:get_disk_version(Db))),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db)),
    ?assertEqual(0, couch_db_engine:get_purge_seq(Db)),
    ?assertEqual(true, is_integer(couch_db_engine:get_purge_infos_limit(Db))),
    ?assertEqual(true, couch_db_engine:get_purge_infos_limit(Db) > 0),
    ?assertEqual([], couch_db_engine:get_security(Db)),
    ?assertEqual(1000, couch_db_engine:get_revs_limit(Db)),
    ?assertMatch(<<_:32/binary>>, couch_db_engine:get_uuid(Db)),
    ?assertEqual([{Node, 0}], couch_db_engine:get_epochs(Db)),
    ?assertEqual(0, couch_db_engine:get_compacted_seq(Db)).


-define(ADMIN_ONLY_SEC_PROPS, {[
    {<<"members">>, {[
        {<<"roles">>, [<<"_admin">>]}
    ]}},
    {<<"admins">>, {[
        {<<"roles">>, [<<"_admin">>]}
    ]}}
]}).


cpse_admin_only_security(DbName) ->
    Config = [{"couchdb", "default_security", "admin_only"}],
    {ok, Db1} = cpse_util:with_config(Config, fun() ->
        cpse_util:create_db(DbName)
    end),

    ?assertEqual(?ADMIN_ONLY_SEC_PROPS, couch_db:get_security(Db1)),
    cpse_util:shutdown_db(Db1),

    {ok, Db2} = couch_db:reopen(Db1),
    couch_log:error("~n~n~n~n~s -> ~s~n~n", [couch_db:name(Db1), couch_db:name(Db2)]),
    ?assertEqual(?ADMIN_ONLY_SEC_PROPS, couch_db:get_security(Db2)).


cpse_set_security(DbName) ->
    SecProps = {[{<<"foo">>, <<"bar">>}]},
    check_prop_set(DbName, get_security, set_security, {[]}, SecProps).


cpse_set_revs_limit(DbName) ->
    check_prop_set(DbName, get_revs_limit, set_revs_limit, 1000, 50).


check_prop_set(DbName, GetFun, SetFun, Default, Value) ->
    {ok, Db0} = cpse_util:create_db(DbName),

    ?assertEqual(Default, couch_db:GetFun(Db0)),
    ?assertMatch(ok, couch_db:SetFun(Db0, Value)),

    {ok, Db1} = couch_db:reopen(Db0),
    ?assertEqual(Value, couch_db:GetFun(Db1)),

    ?assertMatch({ok, _}, couch_db:ensure_full_commit(Db1)),
    cpse_util:shutdown_db(Db1),

    {ok, Db2} = couch_db:reopen(Db1),
    ?assertEqual(Value, couch_db:GetFun(Db2)).
