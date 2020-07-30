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
-include("fabric2_test.hrl").


security_test_() ->
    {
        "Test database security operations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            with([
                ?TDEF(check_is_admin),
                ?TDEF(check_is_not_admin),
                ?TDEF(check_is_admin_role),
                ?TDEF(check_is_not_admin_role),
                ?TDEF(check_is_member_name),
                ?TDEF(check_is_not_member_name),
                ?TDEF(check_is_member_role),
                ?TDEF(check_is_not_member_role),
                ?TDEF(check_admin_is_member),
                ?TDEF(check_is_member_of_public_db),
                ?TDEF(check_set_user_ctx),
                ?TDEF(check_forbidden),
                ?TDEF(check_fail_no_opts),
                ?TDEF(check_fail_name_null),
                ?TDEF(check_forbidden_with_interactive_reopen)
            ])
        }
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    DbName = ?tempdb(),
    PubDbName = ?tempdb(),
    {ok, Db1} = fabric2_db:create(DbName, [?ADMIN_CTX]),
    ok = set_test_security(Db1),
    {ok, _} = fabric2_db:create(PubDbName, [?ADMIN_CTX]),
    {DbName, PubDbName, Ctx}.


cleanup({DbName, PubDbName, Ctx}) ->
    ok = fabric2_db:delete(DbName, []),
    ok = fabric2_db:delete(PubDbName, []),
    test_util:stop_couch(Ctx).


set_test_security(Db) ->
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
    ok = fabric2_db:set_security(Db, SecProps).


check_is_admin({DbName, _, _}) ->
    UserCtx = #user_ctx{name = <<"admin_name1">>},
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertEqual(ok, fabric2_db:check_is_admin(Db)).


check_is_not_admin({DbName, _, _}) ->
    {ok, Db1} = fabric2_db:open(DbName, [{user_ctx, #user_ctx{}}]),
    ?assertThrow(
        {unauthorized, <<"You are not authorized", _/binary>>},
        fabric2_db:check_is_admin(Db1)
    ),

    UserCtx = #user_ctx{name = <<"member_name1">>},
    {ok, Db2} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertThrow(
        {forbidden, <<"You are not a db or server admin.">>},
        fabric2_db:check_is_admin(Db2)
    ).


check_is_admin_role({DbName, _, _}) ->
    UserCtx = #user_ctx{roles = [<<"admin_role1">>]},
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertEqual(ok, fabric2_db:check_is_admin(Db)).


check_is_not_admin_role({DbName, _, _}) ->
    UserCtx = #user_ctx{
        name = <<"member_name1">>,
        roles = [<<"member_role1">>]
    },
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertThrow(
        {forbidden, <<"You are not a db or server admin.">>},
        fabric2_db:check_is_admin(Db)
    ).


check_is_member_name({DbName, _, _}) ->
    UserCtx = #user_ctx{name = <<"member_name1">>},
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertEqual(ok, fabric2_db:check_is_member(Db)).


check_is_not_member_name({DbName, _, _}) ->
    {ok, Db1} = fabric2_db:open(DbName, [{user_ctx, #user_ctx{}}]),
    ?assertThrow(
        {unauthorized, <<"You are not authorized", _/binary>>},
        fabric2_db:check_is_member(Db1)
    ),

    UserCtx = #user_ctx{name = <<"foo">>},
    {ok, Db2} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertThrow(
        {forbidden, <<"You are not allowed to access", _/binary>>},
        fabric2_db:check_is_member(Db2)
    ).


check_is_member_role({DbName, _, _}) ->
    UserCtx = #user_ctx{name = <<"foo">>, roles = [<<"member_role1">>]},
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertEqual(ok, fabric2_db:check_is_member(Db)).


check_is_not_member_role({DbName, _, _}) ->
    UserCtx = #user_ctx{name = <<"foo">>, roles = [<<"bar">>]},
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertThrow(
        {forbidden, <<"You are not allowed to access", _/binary>>},
        fabric2_db:check_is_member(Db)
    ).


check_admin_is_member({DbName, _, _}) ->
    UserCtx = #user_ctx{name = <<"admin_name1">>},
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertEqual(ok, fabric2_db:check_is_member(Db)).


check_is_member_of_public_db({_, PubDbName, _}) ->
    {ok, Db1} = fabric2_db:open(PubDbName, [{user_ctx, #user_ctx{}}]),
    ?assertEqual(ok, fabric2_db:check_is_member(Db1)),

    UserCtx = #user_ctx{name = <<"foo">>, roles = [<<"bar">>]},
    {ok, Db2} = fabric2_db:open(PubDbName, [{user_ctx, UserCtx}]),
    ?assertEqual(ok, fabric2_db:check_is_member(Db2)).


check_set_user_ctx({DbName, _, _}) ->
    UserCtx = #user_ctx{name = <<"foo">>, roles = [<<"admin_role1">>]},
    {ok, Db1} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertEqual(UserCtx, fabric2_db:get_user_ctx(Db1)).


check_forbidden({DbName, _, _}) ->
    UserCtx = #user_ctx{name = <<"foo">>, roles = [<<"bar">>]},
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertThrow({forbidden, _}, fabric2_db:get_db_info(Db)).


check_fail_no_opts({DbName, _, _}) ->
    {ok, Db} = fabric2_db:open(DbName, []),
    ?assertThrow({unauthorized, _}, fabric2_db:get_db_info(Db)).


check_fail_name_null({DbName, _, _}) ->
    UserCtx = #user_ctx{name = null},
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
    ?assertThrow({unauthorized, _}, fabric2_db:get_db_info(Db)).


check_forbidden_with_interactive_reopen({DbName, _, _}) ->
    UserCtx = #user_ctx{name = <<"foo">>},
    Options = [{user_ctx, UserCtx}, {interactive, true}],

    {ok, Db1} = fabric2_db:open(DbName, Options),

    % Verify foo is forbidden by default
    ?assertThrow({forbidden, _}, fabric2_db:get_db_info(Db1)),

    % Allow foo
    {ok, Db2} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    AllowFoo = {[
        {<<"members">>, {[
           {<<"names">>, [<<"foo">>]}
        ]}}
    ]},
    ok = fabric2_db:set_security(Db2, AllowFoo),

    ?assertMatch({ok, _}, fabric2_db:get_db_info(Db1)),

    % Recreate test db instance with the default security
    ok = fabric2_db:delete(DbName, [?ADMIN_CTX]),
    {ok, Db3} = fabric2_db:create(DbName, [?ADMIN_CTX]),
    ok = set_test_security(Db3),

    % Original handle is forbidden to again
    ?assertThrow({forbidden, _}, fabric2_db:get_db_info(Db1)).
