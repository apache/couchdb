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

-module(couchdb_mrview_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DDOC,
    {[
        {<<"_id">>, <<"_design/foo">>},
        {<<"shows">>,
            {[
                {<<"bar">>, <<"function(doc, req) {return '<h1>wosh</h1>';}">>}
            ]}},
        {<<"updates">>,
            {[
                {<<"report">>, <<
                    "function(doc, req) {"
                    "var data = JSON.parse(req.body); "
                    "return ['test', data];"
                    "}"
                >>}
            ]}},
        {<<"views">>,
            {[
                {<<"view1">>,
                    {[
                        {<<"map">>, <<"function(doc){emit(doc._id, doc._rev)}">>}
                    ]}}
            ]}}
    ]}
).

-define(USER, "admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).

setup_all() ->
    Ctx = test_util:start_couch([chttpd]),
    ok = meck:new(mochiweb_socket, [passthrough]),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Ctx.

teardown_all(Ctx) ->
    meck:unload(),
    ok = config:delete("admins", ?USER, _Persist = false),
    test_util:stop_couch(Ctx).

setup(PortType) ->
    meck:reset([mochiweb_socket]),
    ok = meck:expect(mochiweb_socket, recv, fun mochiweb_socket_recv/3),

    DbName = ?tempdb(),
    ok = create_db(PortType, DbName),

    Host = host_url(PortType),
    upload_ddoc(Host, ?b2l(DbName)),
    {Host, ?b2l(DbName)}.

teardown(PortType, {_Host, DbName}) ->
    delete_db(PortType, ?l2b(DbName)),
    ok.

mrview_show_test_() ->
    {
        "Check show functionality",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            [
                make_test_case(clustered, [fun should_return_invalid_request_body/2]),
                make_test_case(backdoor, [fun should_return_invalid_request_body/2])
            ]
        }
    }.

mrview_query_test_() ->
    {
        "Check view query functionality",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            [
                make_test_case(clustered, [fun should_return_400_for_wrong_order_of_keys/2]),
                make_test_case(backdoor, [fun should_return_400_for_wrong_order_of_keys/2])
            ]
        }
    }.

mrview_cleanup_index_files_test_() ->
    {
        "Check index files cleanup",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            [
                make_test_case(clustered, [fun should_cleanup_index_files/2])
            ]
        }
    }.

make_test_case(Mod, Funs) ->
    {
        lists:flatten(io_lib:format("~s", [Mod])),
        {
            foreachx,
            fun setup/1,
            fun teardown/2,
            [{Mod, Fun} || Fun <- Funs]
        }
    }.

should_return_invalid_request_body(PortType, {Host, DbName}) ->
    ?_test(begin
        ok = create_doc(PortType, ?l2b(DbName), <<"doc_id">>, {[]}),
        ReqUrl = Host ++ "/" ++ DbName ++ "/_design/foo/_update/report/doc_id",
        {ok, Status, _Headers, Body} =
            test_request:post(ReqUrl, [?AUTH], <<"{truncated}">>),
        {Props} = jiffy:decode(Body),
        ?assertEqual(
            <<"bad_request">>, couch_util:get_value(<<"error">>, Props)
        ),
        ?assertEqual(
            <<"Invalid request body">>, couch_util:get_value(<<"reason">>, Props)
        ),
        ?assertEqual(400, Status),
        ok
    end).

should_return_400_for_wrong_order_of_keys(_PortType, {Host, DbName}) ->
    Args = [{start_key, "\"bbb\""}, {end_key, "\"aaa\""}],
    ?_test(begin
        ReqUrl =
            Host ++ "/" ++ DbName ++
                "/_design/foo/_view/view1?" ++ mochiweb_util:urlencode(Args),
        {ok, Status, _Headers, Body} = test_request:get(ReqUrl, [?AUTH]),
        {Props} = jiffy:decode(Body),
        ?assertEqual(
            <<"query_parse_error">>, couch_util:get_value(<<"error">>, Props)
        ),
        ?assertEqual(
            <<"No rows can match your key range, reverse your start_key and end_key or set descending=true">>,
            couch_util:get_value(<<"reason">>, Props)
        ),
        ?assertEqual(400, Status),
        ok
    end).

should_cleanup_index_files(_PortType, {Host, DbName}) ->
    ?_test(begin
        IndexWildCard = [
            config:get("couchdb", "view_index_dir"),
            "/.shards/*/",
            DbName,
            ".[0-9]*_design/mrview/*"
        ],
        ReqUrl = Host ++ "/" ++ DbName ++ "/_design/foo/_view/view1",
        {ok, _Status0, _Headers0, _Body0} = test_request:get(ReqUrl, [?AUTH]),
        FileList0 = filelib:wildcard(IndexWildCard),
        ?assertNotEqual([], FileList0),

        % It is hard to simulate inactive view.
        % Since couch_mrview:cleanup is called on view definition change.
        % That's why we just create extra files in place
        ToDelete = lists:map(
            fun(FilePath) ->
                ViewFile = filename:join([
                    filename:dirname(FilePath),
                    "11111111111111111111111111111111.view"
                ]),
                file:write_file(ViewFile, <<>>),
                ViewFile
            end,
            FileList0
        ),
        FileList1 = filelib:wildcard(IndexWildCard),
        ?assertEqual([], lists:usort(FileList1 -- (FileList0 ++ ToDelete))),

        CleanupUrl = Host ++ "/" ++ DbName ++ "/_view_cleanup",
        {ok, _Status1, _Headers1, _Body1} = test_request:post(
            CleanupUrl, [], <<>>, [?AUTH]
        ),
        test_util:wait(fun() ->
            IndexFiles = filelib:wildcard(IndexWildCard),
            case lists:usort(FileList0) == lists:usort(IndexFiles) of
                false -> wait;
                true -> ok
            end
        end),
        ok
    end).

create_doc(backdoor, DbName, Id, Body) ->
    JsonDoc = couch_util:json_apply_field({<<"_id">>, Id}, Body),
    Doc = couch_doc:from_json_obj(JsonDoc),
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    {ok, _} = couch_db:update_docs(Db, [Doc]),
    couch_db:close(Db);
create_doc(clustered, DbName, Id, Body) ->
    JsonDoc = couch_util:json_apply_field({<<"_id">>, Id}, Body),
    Doc = couch_doc:from_json_obj(JsonDoc),
    {ok, _} = fabric:update_docs(DbName, [Doc], [?ADMIN_CTX]),
    ok.

create_db(backdoor, DbName) ->
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    couch_db:close(Db);
create_db(clustered, DbName) ->
    {ok, Status, _, _} = test_request:put(db_url(DbName), [?AUTH], ""),
    assert_success(create_db, Status),
    ok.

delete_db(backdoor, DbName) ->
    couch_server:delete(DbName, [?ADMIN_CTX]);
delete_db(clustered, DbName) ->
    {ok, Status, _, _} = test_request:delete(db_url(DbName), [?AUTH]),
    assert_success(delete_db, Status),
    ok.

assert_success(create_db, Status) ->
    ?assert(lists:member(Status, [201, 202]));
assert_success(delete_db, Status) ->
    ?assert(lists:member(Status, [200, 202])).

host_url(PortType) ->
    "http://" ++ bind_address(PortType) ++ ":" ++ port(PortType).

bind_address(PortType) ->
    config:get(section(PortType), "bind_address", "127.0.0.1").

section(backdoor) -> "http";
section(clustered) -> "chttpd".

db_url(DbName) when is_binary(DbName) ->
    db_url(binary_to_list(DbName));
db_url(DbName) when is_list(DbName) ->
    host_url(clustered) ++ "/" ++ DbName.

port(clustered) ->
    integer_to_list(mochiweb_socket_server:get(chttpd, port));
port(backdoor) ->
    integer_to_list(mochiweb_socket_server:get(couch_httpd, port)).

upload_ddoc(Host, DbName) ->
    Url = Host ++ "/" ++ DbName ++ "/_design/foo",
    Body = couch_util:json_encode(?DDOC),
    {ok, 201, _Resp, _Body} = test_request:put(Url, [?AUTH], Body),
    ok.

mochiweb_socket_recv(Sock, Len, Timeout) ->
    case meck:passthrough([Sock, Len, Timeout]) of
        {ok, <<"{truncated}">>} ->
            {error, closed};
        {ok, Data} ->
            {ok, Data};
        Else ->
            Else
    end.
