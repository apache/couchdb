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

-module(couchdb_compaction_daemon).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).
-define(DELAY, 100).
-define(TIMEOUT, 30000).
-define(TIMEOUT_S, ?TIMEOUT div 1000).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    couch_config:set("compaction_daemon", "check_interval", "3", false),
    couch_config:set("compaction_daemon", "min_file_size", "100000", false),
    Pid.

stop(Pid) ->
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    create_design_doc(Db),
    ok = couch_db:close(Db),
    DbName.

teardown(DbName) ->
    Configs = couch_config:get("compactions"),
    lists:foreach(
        fun({Key, _}) ->
            ok = couch_config:delete("compactions", Key, false)
        end,
        Configs),
    couch_server:delete(DbName, [?ADMIN_USER]),
    ok.


compaction_daemon_test_() ->
    {
        "Compaction daemon tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_compact_by_default_rule/1,
                    fun should_compact_by_dbname_rule/1
                ]
            }
        }
    }.


should_compact_by_default_rule(DbName) ->
    {timeout, ?TIMEOUT_S, ?_test(begin
        {ok, Db} = couch_db:open_int(DbName, []),
        populate(DbName, 70, 70, 200 * 1024),

        {_, DbFileSize} = get_db_frag(DbName),
        {_, ViewFileSize} = get_view_frag(DbName),

        ok = couch_config:set("compactions", "_default",
            "[{db_fragmentation, \"70%\"}, {view_fragmentation, \"70%\"}]",
            false),

        ok = timer:sleep(4000), % something >= check_interval
        wait_compaction_finished(DbName),
        ok = couch_config:delete("compactions", "_default", false),

        {DbFrag2, DbFileSize2} = get_db_frag(DbName),
        {ViewFrag2, ViewFileSize2} = get_view_frag(DbName),

        ?assert(DbFrag2 < 70),
        ?assert(ViewFrag2 < 70),

        ?assert(DbFileSize > DbFileSize2),
        ?assert(ViewFileSize > ViewFileSize2),

        ?assert(couch_db:is_idle(Db)),
        ok = couch_db:close(Db)
    end)}.

should_compact_by_dbname_rule(DbName) ->
    {timeout, ?TIMEOUT_S, ?_test(begin
        {ok, Db} = couch_db:open_int(DbName, []),
        populate(DbName, 70, 70, 200 * 1024),

        {_, DbFileSize} = get_db_frag(DbName),
        {_, ViewFileSize} = get_view_frag(DbName),

        ok = couch_config:set("compactions", ?b2l(DbName),
            "[{db_fragmentation, \"70%\"}, {view_fragmentation, \"70%\"}]",
            false),

        ok = timer:sleep(4000), % something >= check_interval
        wait_compaction_finished(DbName),
        ok = couch_config:delete("compactions", ?b2l(DbName), false),

        {DbFrag2, DbFileSize2} = get_db_frag(DbName),
        {ViewFrag2, ViewFileSize2} = get_view_frag(DbName),

        ?assert(DbFrag2 < 70),
        ?assert(ViewFrag2 < 70),

        ?assert(DbFileSize > DbFileSize2),
        ?assert(ViewFileSize > ViewFileSize2),

        ?assert(couch_db:is_idle(Db)),
        ok = couch_db:close(Db)
    end)}.


create_design_doc(Db) ->
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

populate(DbName, DbFrag, ViewFrag, MinFileSize) ->
    {CurDbFrag, DbFileSize} = get_db_frag(DbName),
    {CurViewFrag, ViewFileSize} = get_view_frag(DbName),
    populate(DbName, DbFrag, ViewFrag, MinFileSize, CurDbFrag, CurViewFrag,
             lists:min([DbFileSize, ViewFileSize])).

populate(_Db, DbFrag, ViewFrag, MinFileSize, CurDbFrag, CurViewFrag, FileSize)
    when CurDbFrag >= DbFrag, CurViewFrag >= ViewFrag, FileSize >= MinFileSize ->
    ok;
populate(DbName, DbFrag, ViewFrag, MinFileSize, _, _, _) ->
    update(DbName),
    {CurDbFrag, DbFileSize} = get_db_frag(DbName),
    {CurViewFrag, ViewFileSize} = get_view_frag(DbName),
    populate(DbName, DbFrag, ViewFrag, MinFileSize, CurDbFrag, CurViewFrag,
             lists:min([DbFileSize, ViewFileSize])).

update(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    lists:foreach(fun(_) ->
        Doc = couch_doc:from_json_obj({[{<<"_id">>, couch_uuids:new()}]}),
        {ok, _} = couch_db:update_docs(Db, [Doc]),
        query_view(Db#db.name)
    end, lists:seq(1, 200)),
    couch_db:close(Db).

db_url(DbName) ->
    Addr = couch_config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(DbName).

query_view(DbName) ->
    {ok, Code, _Headers, _Body} = test_request:get(
        db_url(DbName) ++ "/_design/foo/_view/foo"),
    ?assertEqual(200, Code).

get_db_frag(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Info} = couch_db:get_db_info(Db),
    couch_db:close(Db),
    FileSize = couch_util:get_value(disk_size, Info),
    DataSize = couch_util:get_value(data_size, Info),
    {round((FileSize - DataSize) / FileSize * 100), FileSize}.

get_view_frag(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Info} = couch_mrview:get_info(Db, <<"_design/foo">>),
    couch_db:close(Db),
    FileSize = couch_util:get_value(disk_size, Info),
    DataSize = couch_util:get_value(data_size, Info),
    {round((FileSize - DataSize) / FileSize * 100), FileSize}.

wait_compaction_finished(DbName) ->
    Parent = self(),
    Loop = spawn_link(fun() -> wait_loop(DbName, Parent) end),
    receive
        {done, Loop} ->
            ok
    after ?TIMEOUT ->
        erlang:error(
            {assertion_failed,
             [{module, ?MODULE}, {line, ?LINE},
              {reason, "Compaction timeout"}]})
    end.

wait_loop(DbName, Parent) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, DbInfo} = couch_db:get_db_info(Db),
    {ok, ViewInfo} = couch_mrview:get_info(Db, <<"_design/foo">>),
    couch_db:close(Db),
    case (couch_util:get_value(compact_running, ViewInfo) =:= true) orelse
        (couch_util:get_value(compact_running, DbInfo) =:= true) of
        false ->
            Parent ! {done, self()};
        true ->
            ok = timer:sleep(?DELAY),
            wait_loop(DbName, Parent)
    end.
