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

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

test_db_name() ->
    <<"couch_test_compaction_daemon">>.

main(_) ->
    test_util:init_code_path(),

    etap:plan(10),
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
    timer:sleep(1000),
    put(addr, couch_config:get("httpd", "bind_address", "127.0.0.1")),
    put(port, integer_to_list(mochiweb_socket_server:get(couch_httpd, port))),

    disable_compact_daemon(),

    delete_db(),
    {ok, Db} = create_db(),

    add_design_doc(Db),
    couch_db:close(Db),
    populate(50, 20, 100 * 1024),

    {_, DbFileSize} = get_db_frag(),
    {_, ViewFileSize} = get_view_frag(),

    % enable automatic compaction
    ok = couch_config:set("compaction_daemon", "check_interval", "3", false),
    ok = couch_config:set("compaction_daemon", "min_file_size", "100000", false),
    ok = couch_config:set(
        "compactions",
        binary_to_list(test_db_name()),
        "[{db_fragmentation, \"50%\"}, {view_fragmentation, \"20%\"}]",
        false),

    ok = timer:sleep(4000), % something >= check_interval
    wait_compaction_finished(),

    {DbFrag2, DbFileSize2} = get_db_frag(),
    {ViewFrag2, ViewFileSize2} = get_view_frag(),

    etap:is(true, (DbFrag2 < 50), "Database fragmentation is < 50% after compaction"),
    etap:is(true, (ViewFrag2 < 20), "View fragmentation is < 20% after compaction"),
    etap:is(true, (DbFileSize2 < DbFileSize), "Database file size decreased"),
    etap:is(true, (ViewFileSize2 < ViewFileSize), "View file size decreased"),

    disable_compact_daemon(),
    ok = timer:sleep(6000), % 2 times check_interval
    etap:is(couch_db:is_idle(Db), true, "Database is idle"),
    populate(50, 20, 100 * 1024),
    {_DbFrag3, DbFileSize3} = get_db_frag(),
    {_ViewFrag3, ViewFileSize3} = get_view_frag(),

    % enable automatic compaction
    ok = couch_config:set(
        "compactions",
        "_default",
        "[{db_fragmentation, \"50%\"}, {view_fragmentation, \"20%\"}]",
        false),

    ok = timer:sleep(6000), % something >= check_interval
    wait_compaction_finished(),

    {DbFrag4, DbFileSize4} = get_db_frag(),
    {ViewFrag4, ViewFileSize4} = get_view_frag(),

    etap:is(true, (DbFrag4 < 50), "Database fragmentation is < 50% after compaction"),
    etap:is(true, (ViewFrag4 < 20), "View fragmentation is < 20% after compaction"),
    etap:is(true, (DbFileSize4 < DbFileSize3), "Database file size decreased again"),
    etap:is(true, (ViewFileSize4 < ViewFileSize3), "View file size decreased again"),

    ok = timer:sleep(6000), % 2 times check_interval
    etap:is(couch_db:is_idle(Db), true, "Database is idle"),

    delete_db(),
    couch_server_sup:stop(),
    ok.

disable_compact_daemon() ->
    Configs = couch_config:get("compactions"),
    lists:foreach(
        fun({DbName, _}) ->
            ok = couch_config:delete("compactions", DbName, false)
        end,
        Configs).

admin_user_ctx() ->
    {user_ctx, #user_ctx{roles = [<<"_admin">>]}}.

create_db() ->
    {ok, _} = couch_db:create(test_db_name(), [admin_user_ctx()]).

delete_db() ->
    couch_server:delete(test_db_name(), [admin_user_ctx()]).

add_design_doc(Db) ->
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/foo">>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {<<"foo">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}},
            {<<"foo2">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}},
            {<<"foo3">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}}
        ]}}
    ]}),
    {ok, _} = couch_db:update_docs(Db, [DDoc]),
    {ok, _} = couch_db:ensure_full_commit(Db),
    ok.

populate(DbFrag, ViewFrag, MinFileSize) ->
    {CurDbFrag, DbFileSize} = get_db_frag(),
    {CurViewFrag, ViewFileSize} = get_view_frag(),
    populate(
        DbFrag, ViewFrag, MinFileSize, CurDbFrag, CurViewFrag,
        lists:min([DbFileSize, ViewFileSize])).

populate(DbFrag, ViewFrag, MinFileSize, CurDbFrag, CurViewFrag, FileSize)
    when CurDbFrag >= DbFrag, CurViewFrag >= ViewFrag, FileSize >= MinFileSize ->
    ok;
populate(DbFrag, ViewFrag, MinFileSize, _, _, _) ->
    update(),
    {CurDbFrag, DbFileSize} = get_db_frag(),
    {CurViewFrag, ViewFileSize} = get_view_frag(),
    populate(
        DbFrag, ViewFrag, MinFileSize, CurDbFrag, CurViewFrag,
        lists:min([DbFileSize, ViewFileSize])).

update() ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    Docs = lists:map(
        fun(_) ->
            Doc = couch_doc:from_json_obj({[{<<"_id">>, couch_uuids:new()}]}),
            {ok, _} = couch_db:update_docs(Db, [Doc])
        end,
        lists:seq(1, 100)),
    couch_db:close(Db),
    query_view().

db_url() ->
    "http://" ++ get(addr) ++ ":" ++ get(port) ++ "/" ++
        binary_to_list(test_db_name()).

query_view() ->
    {ok, Code, _Headers, _Body} = test_util:request(
        db_url() ++ "/_design/foo/_view/foo", [], get),
    case Code of
    200 ->
        ok;
    _ ->
        etap:bail("error querying view")
    end.

get_db_frag() ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    {ok, Info} = couch_db:get_db_info(Db),
    couch_db:close(Db),
    FileSize = couch_util:get_value(disk_size, Info),
    DataSize = couch_util:get_value(data_size, Info),
    {round((FileSize - DataSize) / FileSize * 100), FileSize}.

get_view_frag() ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    {ok, Info} = couch_view:get_group_info(Db, <<"_design/foo">>),
    couch_db:close(Db),
    FileSize = couch_util:get_value(disk_size, Info),
    DataSize = couch_util:get_value(data_size, Info),
    {round((FileSize - DataSize) / FileSize * 100), FileSize}.


wait_compaction_finished() ->
    Parent = self(),
    Loop = spawn_link(fun() -> wait_loop(Parent) end),
    receive
    {done, Loop} ->
        etap:diag("Database and view compaction have finished")
    after 60000 ->
        etap:bail("Compaction not triggered")
    end.

wait_loop(Parent) ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    {ok, DbInfo} = couch_db:get_db_info(Db),
    {ok, ViewInfo} = couch_view:get_group_info(Db, <<"_design/foo">>),
    couch_db:close(Db),
    case (couch_util:get_value(compact_running, ViewInfo) =:= true) orelse
        (couch_util:get_value(compact_running, DbInfo) =:= true) of
    false ->
        Parent ! {done, self()};
    true ->
        ok = timer:sleep(500),
        wait_loop(Parent)
    end.
