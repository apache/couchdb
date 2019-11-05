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

-module(fabric2_db_security_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").


security_test_() ->
    {
        "Test database security operations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {with, [
                fun is_admin_name/1,
                fun is_not_admin_name/1,
                fun is_admin_role/1,
                fun is_not_admin_role/1,
                fun check_is_admin/1,
                fun check_is_not_admin/1,
                fun check_is_member_name/1,
                fun check_is_not_member_name/1,
                fun check_is_member_role/1,
                fun check_is_not_member_role/1,
                fun check_admin_is_member/1,
                fun check_is_member_of_public_db/1,
                fun check_set_user_ctx/1,
                fun check_forbidden/1,
                fun check_fail_no_opts/1,
                fun check_fail_name_null/1
            ]}
        }
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    DbName = ?tempdb(),
    PubDbName = ?tempdb(),
    {ok, Db1} = fabric2_db:create(DbName, [?ADMIN_CTX]),
    SecProps = {[
        {<<"admins">>, {[
            {<<"names">>, [<<"admin_name1">>, <<"admin_name2">>]},
            {<<"roles">>, [<<"admin_role1">>, <<"admin_role2">>]}
        ]}},
        {<<"members">>, {[
            {<<"names">>, [<<"member_name1">>, <<"member_name2">>]},
            {<<"roles">>, [<<"member_role1">>, <<"member_role2">>]}
        ]}}
    ]},
    ok = fabric2_db:set_security(Db1, SecProps),
    {ok, Db2} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    {ok, PubDb} = fabric2_db:create(PubDbName, []),
    {Db2, PubDb, Ctx}.


cleanup({Db, PubDb, Ctx}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    ok = fabric2_db:delete(fabric2_db:name(PubDb), []),
    test_util:stop_couch(Ctx).


is_admin_name({Db, _, _}) ->
    UserCtx = #user_ctx{name = <<"admin_name1">>},
    ?assertEqual(true, fabric2_db:is_admin(Db#{user_ctx := UserCtx})).


is_not_admin_name({Db, _, _}) ->
    UserCtx = #user_ctx{name = <<"member1">>},
    ?assertEqual(false, fabric2_db:is_admin(Db#{user_ctx := UserCtx})).


is_admin_role({Db, _, _}) ->
    UserCtx = #user_ctx{roles = [<<"admin_role1">>]},
    ?assertEqual(true, fabric2_db:is_admin(Db#{user_ctx := UserCtx})).


is_not_admin_role({Db, _, _}) ->
    UserCtx = #user_ctx{roles = [<<"member_role1">>]},
    ?assertEqual(false, fabric2_db:is_admin(Db#{user_ctx := UserCtx})).


check_is_admin({Db, _, _}) ->
    UserCtx = #user_ctx{name = <<"admin_name1">>},
    ?assertEqual(ok, fabric2_db:check_is_admin(Db#{user_ctx := UserCtx})).


check_is_not_admin({Db, _, _}) ->
    UserCtx = #user_ctx{name = <<"member_name1">>},
    ?assertThrow(
        {unauthorized, <<"You are not a db or server admin.">>},
        fabric2_db:check_is_admin(Db#{user_ctx := #user_ctx{}})
    ),
    ?assertThrow(
        {forbidden, <<"You are not a db or server admin.">>},
        fabric2_db:check_is_admin(Db#{user_ctx := UserCtx})
    ).


check_is_member_name({Db, _, _}) ->
    UserCtx = #user_ctx{name = <<"member_name1">>},
    ?assertEqual(ok, fabric2_db:check_is_member(Db#{user_ctx := UserCtx})).


check_is_not_member_name({Db, _, _}) ->
    UserCtx = #user_ctx{name = <<"foo">>},
    ?assertThrow(
        {unauthorized, <<"You are not authorized", _/binary>>},
        fabric2_db:check_is_member(Db#{user_ctx := #user_ctx{}})
    ),
    ?assertThrow(
        {forbidden, <<"You are not allowed to access", _/binary>>},
        fabric2_db:check_is_member(Db#{user_ctx := UserCtx})
    ).


check_is_member_role({Db, _, _}) ->
    UserCtx = #user_ctx{name = <<"foo">>, roles = [<<"member_role1">>]},
    ?assertEqual(ok, fabric2_db:check_is_member(Db#{user_ctx := UserCtx})).


check_is_not_member_role({Db, _, _}) ->
    UserCtx = #user_ctx{name = <<"foo">>, roles = [<<"bar">>]},
    ?assertThrow(
        {forbidden, <<"You are not allowed to access", _/binary>>},
        fabric2_db:check_is_member(Db#{user_ctx := UserCtx})
    ).


check_admin_is_member({Db, _, _}) ->
    UserCtx = #user_ctx{name = <<"admin_name1">>},
    ?assertEqual(ok, fabric2_db:check_is_member(Db#{user_ctx := UserCtx})).


check_is_member_of_public_db({_, PubDb, _}) ->
    UserCtx = #user_ctx{name = <<"foo">>, roles = [<<"bar">>]},
    ?assertEqual(
        ok,
        fabric2_db:check_is_member(PubDb#{user_ctx := #user_ctx{}})
    ),
    ?assertEqual(
        ok,
        fabric2_db:check_is_member(PubDb#{user_ctx := UserCtx})
    ).


check_set_user_ctx({Db0, _, _}) ->
    DbName = fabric2_db:name(Db0),
    UserCtx = #user_ctx{name = <<"foo">>, roles = [<<"admin_role1">>]},
    {ok, Db1} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertEqual(UserCtx, fabric2_db:get_user_ctx(Db1)).


check_forbidden({Db0, _, _}) ->
    DbName = fabric2_db:name(Db0),
    UserCtx = #user_ctx{name = <<"foo">>, roles = [<<"bar">>]},
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertThrow({forbidden, _}, fabric2_db:get_db_info(Db)).


check_fail_no_opts({Db0, _, _}) ->
    DbName = fabric2_db:name(Db0),
    {ok, Db} = fabric2_db:open(DbName, []),
    ?assertThrow({unauthorized, _}, fabric2_db:get_db_info(Db)).


check_fail_name_null({Db0, _, _}) ->
    DbName = fabric2_db:name(Db0),
    UserCtx = #user_ctx{name = null},
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertThrow({unauthorized, _}, fabric2_db:get_db_info(Db)).
