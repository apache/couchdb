-module(global_changes_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup() ->
    Host = get_host(),
    add_admin("admin", <<"pass">>),
    DbName = "foo/" ++ ?b2l(?tempdb()),
    [fabric:create_db(Name, [?ADMIN_CTX])
        || Name <- ["_global_changes", DbName]],
    {Host, DbName}.

teardown({_, DbName}) ->
    delete_admin("admin"),
    [fabric:delete_db(Name, [?ADMIN_CTX])
        || Name <- ["_global_changes", DbName]],
    ok.

global_changes_test_() ->
    {
        "Checking global_changes endpoint",
        {
            setup,
            fun() -> test_util:start_couch([chttpd, global_changes]) end,
            fun test_util:stop/1,
            [
                check_response()
            ]
        }
    }.

check_response() ->
    {
        "Check response",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                fun should_return_correct_response_on_create/1,
                fun should_return_correct_response_on_update/1
            ]
        }
    }.

should_return_correct_response_on_create({Host, DbName}) ->
    ?_test(begin
        Headers = [{basic_auth, {"admin", "pass"}}],
        create_doc(Host, DbName, "bar/baz"),
        {Status, Events} = request_updates(Host, DbName, Headers),
        ?assertEqual(200, Status),
        ?assertEqual([<<"created">>, <<"updated">>], Events)
    end).

should_return_correct_response_on_update({Host, DbName}) ->
    ?_test(begin
        Headers = [{basic_auth, {"admin", "pass"}}],
        create_doc(Host, DbName, "bar/baz"),
        update_doc(Host, DbName, "bar/baz", "new_value"),
        {Status, Events} = request_updates(Host, DbName, Headers),
        ?assertEqual(200, Status),
        ?assertEqual([<<"created">>, <<"updated">>], Events)
    end).

create_doc(Host, DbName, Id) ->
    Headers = [{basic_auth, {"admin", "pass"}}],
    Url = Host ++ "/" ++ escape(DbName) ++ "/" ++ escape(Id),
    Body = jiffy:encode({[
        {key, "value"}
    ]}),
    {ok, 201, _Headers, _Body} = test_request:put(Url, Headers, Body),
    timer:sleep(1000),
    ok.

update_doc(Host, DbName, Id, Value) ->
    Headers = [{basic_auth, {"admin", "pass"}}],
    Url = Host ++ "/" ++ escape(DbName) ++ "/" ++ escape(Id),
    {ok, 200, _Headers0, BinBody} = test_request:get(Url, Headers),
    [Rev] = decode_response(BinBody, [<<"_rev">>]),
    Body = jiffy:encode({[
        {key, Value},
        {'_rev', Rev}
    ]}),
    {ok, 201, _Headers1, _Body} = test_request:put(Url, Headers, Body),
    timer:sleep(1000),
    ok.

request_updates(Host, DbName, Headers) ->
    Url = Host ++ "/_db_updates",
    {ok, Status, _Headers, BinBody} = test_request:get(Url, Headers),
    [Results] = decode_response(BinBody, [<<"results">>]),
    ToDecode = [<<"db_name">>, <<"type">>],
    Values = [decode_result(Result, ToDecode) || Result <- Results],
    Result = [Type || [DB, Type] <- Values, DB == ?l2b(DbName)],
    {Status, lists:sort(Result)}.

decode_result({Props}, ToDecode) ->
    [couch_util:get_value(Key, Props) || Key <- ToDecode].

decode_response(BinBody, ToDecode) ->
    {Body} = jiffy:decode(BinBody),
    [couch_util:get_value(Key, Body) || Key <- ToDecode].

add_admin(User, Pass) ->
    Hashed = couch_passwords:hash_admin_password(Pass),
    config:set("admins", User, ?b2l(Hashed), false).

delete_admin(User) ->
    config:delete("admins", User, false).

get_host() ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = config:get("chttpd", "port", "5984"),
    Host = "http://" ++ Addr ++ ":" ++ Port,
    Host.

escape(Path) ->
    re:replace(Path, "/", "%2f", [global, {return, list}]).
