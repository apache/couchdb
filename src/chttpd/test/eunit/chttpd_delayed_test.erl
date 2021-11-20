-module(chttpd_delayed_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_view_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(DDOC,
    "{\"_id\": \"_design/bar\", \"views\": {\"baz\":\n"
    "               {\"map\": \"function(doc) {emit(doc._id, doc._id);}\"}}}"
).

-define(FIXTURE_TXT, ?ABS_PATH(?FILE)).
-define(i2l(I), integer_to_list(I)).
% seconds
-define(TIMEOUT, 60).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    ok = config:set("chttpd", "buffer_response", "true", _Persist = false),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(Url),
    Url.

teardown(Url) ->
    delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist = false).

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

all_test_() ->
    {
        "chttpd delay tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun test_buffer_response_all_docs/1,
                    fun test_buffer_response_changes/1
                ]
            }
        }
    }.

test_buffer_response_all_docs(Url) ->
    assert_successful_response(Url ++ "/_all_docs").

test_buffer_response_changes(Url) ->
    assert_successful_response(Url ++ "/_changes").

assert_successful_response(Url) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            {ok, Code, _Headers, _Body} = test_request:get(Url, [?AUTH]),
            ?assertEqual(200, Code)
        end)}.
