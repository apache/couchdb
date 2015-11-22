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

-module(global_changes_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).

setup() ->
    Host = get_host(),
    ok = add_admin(?USER, ?PASS),
    DbName = "foo/" ++ ?b2l(?tempdb()),
    ok = http_create_db(DbName),
    {Host, DbName}.

teardown({_, DbName}) ->
    ok = http_delete_db(DbName),
    delete_admin(?USER),
    ok.

http_create_db(Name) ->
    Resp = {ok, Status, _, _} = test_request:put(db_url(Name), [?AUTH], ""),
    true = lists:member(Status, [201, 202]),
    ok.
    
http_delete_db(Name) ->
    {ok, Status, _, _} = test_request:delete(db_url(Name), [?AUTH]),
    true = lists:member(Status, [200, 202]),
    ok.

db_url(Name) ->
    get_host() ++ "/" ++ escape(Name).

start_couch() ->
    Ctx = test_util:start_couch([chttpd, global_changes]),
    ok = ensure_db_exists("_global_changes"),
    Ctx.

ensure_db_exists(Name) ->
    case fabric:create_db(Name) of
        ok ->
            ok;
        {error, file_exists} ->
            ok
    end.

global_changes_test_() ->
    {
        "Checking global_changes endpoint",
        {
            setup,
            fun start_couch/0,
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
        Headers = [?AUTH],
        create_doc(Host, DbName, "bar/baz"),
        {Status, Events} = request_updates(Host, DbName, Headers),
        ?assertEqual(200, Status),
        ?assertEqual([<<"created">>, <<"updated">>], Events)
    end).

should_return_correct_response_on_update({Host, DbName}) ->
    ?_test(begin
        Headers = [?AUTH],
        create_doc(Host, DbName, "bar/baz"),
        update_doc(Host, DbName, "bar/baz", "new_value"),
        {Status, Events} = request_updates(Host, DbName, Headers),
        ?assertEqual(200, Status),
        ?assertEqual([<<"created">>, <<"updated">>], Events)
    end).

create_doc(Host, DbName, Id) ->
    Headers = [?AUTH],
    Url = Host ++ "/" ++ escape(DbName) ++ "/" ++ escape(Id),
    Body = jiffy:encode({[
        {key, "value"}
    ]}),
    {ok, Status, _Headers, _Body} = test_request:put(Url, Headers, Body),
    ?assert(Status =:= 201 orelse Status =:= 202),
    timer:sleep(1000),
    ok.

update_doc(Host, DbName, Id, Value) ->
    Headers = [?AUTH],
    Url = Host ++ "/" ++ escape(DbName) ++ "/" ++ escape(Id),
    {ok, 200, _Headers0, BinBody} = test_request:get(Url, Headers),
    [Rev] = decode_response(BinBody, [<<"_rev">>]),
    Body = jiffy:encode({[
        {key, Value},
        {'_rev', Rev}
    ]}),
    {ok, Status, _Headers1, _Body} = test_request:put(Url, Headers, Body),
    ?assert(Status =:= 201 orelse Status =:= 202),
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
    config:set("admins", User, Pass, false).

delete_admin(User) ->
    config:delete("admins", User, false).

get_host() ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    "http://" ++ Addr ++ ":" ++ Port.

escape(Path) ->
    re:replace(Path, "/", "%2f", [global, {return, list}]).
