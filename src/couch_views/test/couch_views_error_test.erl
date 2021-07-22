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

-module(couch_views_error_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("fabric/test/fabric2_test.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

error_test_() ->
    {
        "Test views report errors",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            {
                foreach,
                fun foreach_setup/0,
                fun foreach_teardown/1,
                [
                    ?TDEF_FE(view_reports_error)
                ]
            }
        }
    }.

setup() ->
    Ctx = test_util:start_couch([
        fabric,
        chttpd,
        couch_jobs,
        js_engine,
        couch_views
    ]),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Ctx.

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

foreach_setup() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    DbName = fabric2_db:name(Db),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(DbName)]),
    {Db, Url}.

foreach_teardown({Db, _}) ->
    meck:unload(),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).

view_reports_error({Db, Url}) ->
    meck:new(couch_views_batch, [passthrough]),
    meck:expect(couch_views_batch, start, fun(_) ->
        erlang:error({erlfdb_error, 2101})
    end),

    {ok, _} = fabric2_db:update_doc(Db, ddoc(), []),

    ViewUrl = lists:concat([Url, "/_design/foo/_view/bar"]),
    {ok, Status, _Headers, Body} = test_request:get(ViewUrl, [?AUTH]),

    ?assertEqual(500, Status),
    {Props} = couch_util:json_decode(Body),
    {<<"error">>, Error} = lists:keyfind(<<"error">>, 1, Props),
    ?assertEqual(<<"foundationdb_error">>, Error).

ddoc() ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"_design/foo">>},
            {<<"language">>, <<"javascript">>},
            {<<"views">>,
                {[
                    {<<"bar">>,
                        {[
                            {<<"map">>, <<"function(doc) {emit(doc.value, doc.value);}">>}
                        ]}}
                ]}}
        ]}
    ).
