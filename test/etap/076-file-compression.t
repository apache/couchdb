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

test_db_name() -> <<"couch_test_file_compression">>.
ddoc_id() -> <<"_design/test">>.
num_docs() -> 5000.


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
    couch_config:set("couchdb", "file_compression", "none", false),

    create_database(),
    compact_db(),
    compact_view(),
    DbDiskSize1 = db_disk_size(),
    ViewDiskSize1 = view_disk_size(),

    couch_config:set("couchdb", "file_compression", "snappy", false),
    compact_db(),
    compact_view(),
    DbDiskSize2 = db_disk_size(),
    ViewDiskSize2 = view_disk_size(),

    etap:is(DbDiskSize2 < DbDiskSize1, true, "Database disk size decreased"),
    etap:is(ViewDiskSize2 < ViewDiskSize1, true, "Index disk size decreased"),

    couch_config:set("couchdb", "file_compression", "deflate_9", false),
    compact_db(),
    compact_view(),
    DbDiskSize3 = db_disk_size(),
    ViewDiskSize3 = view_disk_size(),

    etap:is(DbDiskSize3 < DbDiskSize2, true, "Database disk size decreased again"),
    etap:is(ViewDiskSize3 < ViewDiskSize2, true, "Index disk size decreased again"),

    couch_config:set("couchdb", "file_compression", "deflate_1", false),
    compact_db(),
    compact_view(),
    DbDiskSize4 = db_disk_size(),
    ViewDiskSize4 = view_disk_size(),

    etap:is(DbDiskSize4 > DbDiskSize3, true, "Database disk size increased"),
    etap:is(ViewDiskSize4 > ViewDiskSize3, true, "Index disk size increased"),

    couch_config:set("couchdb", "file_compression", "snappy", false),
    compact_db(),
    compact_view(),
    DbDiskSize5 = db_disk_size(),
    ViewDiskSize5 = view_disk_size(),

    etap:is(DbDiskSize5 > DbDiskSize4, true, "Database disk size increased again"),
    etap:is(ViewDiskSize5 > ViewDiskSize4, true, "Index disk size increased again"),

    couch_config:set("couchdb", "file_compression", "none", false),
    compact_db(),
    compact_view(),
    DbDiskSize6 = db_disk_size(),
    ViewDiskSize6 = view_disk_size(),

    etap:is(DbDiskSize6 > DbDiskSize5, true, "Database disk size increased again"),
    etap:is(ViewDiskSize6 > ViewDiskSize5, true, "Index disk size increased again"),

    delete_db(),
    couch_server_sup:stop(),
    ok.


create_database() ->
    {ok, Db} = couch_db:create(
        test_db_name(),
        [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}, overwrite]),
    ok = populate_db(Db, num_docs()),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, ddoc_id()},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
                {<<"view1">>, {[
                    {<<"map">>, <<"function(doc) { emit(doc._id, doc.string); }">>}
                ]}}
            ]}
        }
    ]}),
    {ok, _} = couch_db:update_doc(Db, DDoc, []),
    refresh_index(),
    ok = couch_db:close(Db).


populate_db(_Db, NumDocs) when NumDocs =< 0 ->
    ok;
populate_db(Db, NumDocs) ->
    Docs = lists:map(
        fun(_) ->
            couch_doc:from_json_obj({[
                {<<"_id">>, couch_uuids:random()},
                {<<"string">>, list_to_binary(lists:duplicate(1000, $X))}
            ]})
        end,
        lists:seq(1, 500)),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    populate_db(Db, NumDocs - 500).


refresh_index() ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    {ok, _, _} = couch_view:get_map_view(Db, ddoc_id(), <<"view1">>, false),
    ok = couch_db:close(Db).


compact_db() ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    {ok, CompactPid} = couch_db:start_compact(Db),
    MonRef = erlang:monitor(process, CompactPid),
    receive
    {'DOWN', MonRef, process, CompactPid, normal} ->
        ok;
    {'DOWN', MonRef, process, CompactPid, Reason} ->
        etap:bail("Error compacting database: " ++ couch_util:to_list(Reason))
    after 120000 ->
        etap:bail("Timeout waiting for database compaction")
    end,
    ok = couch_db:close(Db).


compact_view() ->
    {ok, CompactPid} = couch_view_compactor:start_compact(test_db_name(), <<"test">>),
    MonRef = erlang:monitor(process, CompactPid),
    receive
    {'DOWN', MonRef, process, CompactPid, normal} ->
        ok;
    {'DOWN', MonRef, process, CompactPid, Reason} ->
        etap:bail("Error compacting view group: " ++ couch_util:to_list(Reason))
    after 120000 ->
        etap:bail("Timeout waiting for view group compaction")
    end.


db_disk_size() ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    {ok, Info} = couch_db:get_db_info(Db),
    ok = couch_db:close(Db),
    couch_util:get_value(disk_size, Info).


view_disk_size() ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    {ok, Info} = couch_view:get_group_info(Db, ddoc_id()),
    ok = couch_db:close(Db),
    couch_util:get_value(disk_size, Info).


delete_db() ->
    ok = couch_server:delete(
        test_db_name(), [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}]).
