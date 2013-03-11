#!/usr/bin/env escript
%% -*- erlang -*-
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

main(_) ->
    test_util:init_code_path(),

    etap:plan(8),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.


test() ->
    couch_server_sup:start_link(test_util:config_files()),

    % commit sofort
    ok = couch_config:set("query_server_config", "commit_freq", "0"),

    test_upgrade(),

    couch_server_sup:stop(),
    ok.

fixture_path() ->
    test_util:source_file("test/etap/fixtures").

old_db() ->
    fixture_path() ++ "/" ++ old_db_name().

old_db_name() ->
    "test.couch".

old_view() ->
    fixture_path() ++ "/" ++ old_view_name().

old_view_name() ->
    "3b835456c235b1827e012e25666152f3.view".

new_view_name() ->
    "a1c5929f912aca32f13446122cc6ce50.view".

couch_url() ->
    "http://" ++ addr() ++ ":" ++ port().

addr() ->
    couch_config:get("httpd", "bind_address", "127.0.0.1").

port() ->
    integer_to_list(mochiweb_socket_server:get(couch_httpd, port)).


% <= 1.2.x
-record(index_header,
    {seq=0,
    purge_seq=0,
    id_btree_state=nil,
    view_states=nil
    }).

% >= 1.3.x
-record(mrheader, {
    seq=0,
    purge_seq=0,
    id_btree_state=nil,
    view_states=nil
}).

ensure_header(File, MatchFun, Msg) ->
    {ok, Fd} = couch_file:open(File),
    {ok, {_Sig, Header}} = couch_file:read_header(Fd),
    couch_file:close(Fd),
    etap:fun_is(MatchFun, Header, "ensure " ++ Msg ++ " header for file: " ++ File).

file_exists(File) ->
    % open without creating
    case file:open(File, [read, raw]) of
    {ok, Fd_Read} ->
        file:close(Fd_Read),
        true;
    _Error ->
        false
    end.

cleanup() ->
    DbDir = couch_config:get("couchdb", "database_dir"),
    Files = [
        DbDir ++ "/test.couch",
        DbDir ++ "/.test_design/" ++ old_view_name(),
        DbDir ++ "/.test_design/mrview/" ++ new_view_name()
    ],
    lists:foreach(fun(File) -> file:delete(File) end, Files),
    etap:ok(true, "cleanup").

test_upgrade() ->

    cleanup(),

    % copy old db file into db dir
    DbDir = couch_config:get("couchdb", "database_dir"),
    DbTarget = DbDir ++ "/" ++ old_db_name(),
    filelib:ensure_dir(DbDir),
    OldDbName = old_db(),
    {ok, _} = file:copy(OldDbName, DbTarget),

    % copy old view file into view dir
    ViewDir = couch_config:get("couchdb", "view_index_dir"),
    ViewTarget = ViewDir ++ "/.test_design/" ++ old_view_name(),
    filelib:ensure_dir(ViewTarget),
    OldViewName = old_view(),
    {ok, _} = file:copy(OldViewName, ViewTarget),

    % ensure old header
    ensure_header(ViewTarget, fun(#index_header{}) -> true; (_) -> false end, "old"),

    % query view
    ViewUrl = couch_url() ++ "/test/_design/test/_view/test",
    {ok, Code, _Headers, Body}  = test_util:request(ViewUrl, [], get),

    % expect results
    etap:is(Code, 200, "valid view result http status code"),
    ExpectBody = <<"{\"total_rows\":2,\"offset\":0,\"rows\":[\r\n{\"id\":\"193f2f9c596ddc7ad326f7da470009ec\",\"key\":1,\"value\":null},\r\n{\"id\":\"193f2f9c596ddc7ad326f7da470012b6\",\"key\":2,\"value\":null}\r\n]}\n">>,
    etap:is(Body, ExpectBody, "valid view result"),

    % ensure old file gone.
    etap:is(file_exists(ViewTarget), false, "ensure old file is gone"),

    % ensure new header
    NewViewFile = ViewDir ++ "/.test_design/mrview/" ++ new_view_name(),

    % add doc(s)
    test_util:request(
        couch_url() ++ "/test/boo",
        [{"Content-Type", "application/json"}],
        put,
        <<"{\"a\":3}">>),

    % query again
    {ok, Code2, _Headers2, Body2} = test_util:request(ViewUrl, [], get),

    % expect results
    etap:is(Code2, 200, "valid view result http status code"),
    ExpectBody2 = <<"{\"total_rows\":3,\"offset\":0,\"rows\":[\r\n{\"id\":\"193f2f9c596ddc7ad326f7da470009ec\",\"key\":1,\"value\":null},\r\n{\"id\":\"193f2f9c596ddc7ad326f7da470012b6\",\"key\":2,\"value\":null},\r\n{\"id\":\"boo\",\"key\":3,\"value\":null}\r\n]}\n">>,
    etap:is(Body2, ExpectBody2, "valid view result after doc add"),

    % ensure no rebuild
    % TBD no idea how to actually test this.

    % ensure new header.
    timer:sleep(2000),
    ensure_header(NewViewFile, fun(#mrheader{}) -> true; (_) -> false end, "new"),

    ok.
