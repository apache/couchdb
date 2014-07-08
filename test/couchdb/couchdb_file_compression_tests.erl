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

-module(couchdb_file_compression_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).
-define(DDOC_ID, <<"_design/test">>).
-define(DOCS_COUNT, 5000).
-define(TIMEOUT, 30000).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
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
    couch_config:set("couchdb", "file_compression", "none", false),
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    ok = populate_db(Db, ?DOCS_COUNT),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, ?DDOC_ID},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
                {<<"by_id">>, {[
                    {<<"map">>, <<"function(doc){emit(doc._id, doc.string);}">>}
                ]}}
            ]}
        }
    ]}),
    {ok, _} = couch_db:update_doc(Db, DDoc, []),
    refresh_index(DbName),
    ok = couch_db:close(Db),
    DbName.

teardown(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_USER]),
    ok.


couch_auth_cache_test_() ->
    {
        "CouchDB file compression tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_use_none/1,
                    fun should_use_deflate_1/1,
                    fun should_use_deflate_9/1,
                    fun should_use_snappy/1,
                    fun should_compare_compression_methods/1
                ]
            }
        }
    }.


should_use_none(DbName) ->
    couch_config:set("couchdb", "file_compression", "none", false),
    {
        "Use no compression",
        [
            {"compact database", ?_test(compact_db(DbName))},
            {"compact view", ?_test(compact_view(DbName))}
        ]
    }.

should_use_deflate_1(DbName) ->
    couch_config:set("couchdb", "file_compression", "deflate_1", false),
    {
        "Use deflate compression at level 1",
        [
            {"compact database", ?_test(compact_db(DbName))},
            {"compact view", ?_test(compact_view(DbName))}
        ]
    }.

should_use_deflate_9(DbName) ->
    couch_config:set("couchdb", "file_compression", "deflate_9", false),
    {
        "Use deflate compression at level 9",
        [
            {"compact database", ?_test(compact_db(DbName))},
            {"compact view", ?_test(compact_view(DbName))}
        ]
    }.

should_use_snappy(DbName) ->
    couch_config:set("couchdb", "file_compression", "snappy", false),
    {
        "Use snappy compression",
        [
            {"compact database", ?_test(compact_db(DbName))},
            {"compact view", ?_test(compact_view(DbName))}
        ]
    }.

should_compare_compression_methods(DbName) ->
    {"none > snappy > deflate_1 > deflate_9",
     {timeout, ?TIMEOUT div 1000, ?_test(compare_compression_methods(DbName))}}.

compare_compression_methods(DbName) ->
    couch_config:set("couchdb", "file_compression", "none", false),
    compact_db(DbName),
    compact_view(DbName),
    DbSizeNone = db_disk_size(DbName),
    ViewSizeNone = view_disk_size(DbName),

    couch_config:set("couchdb", "file_compression", "snappy", false),
    compact_db(DbName),
    compact_view(DbName),
    DbSizeSnappy = db_disk_size(DbName),
    ViewSizeSnappy = view_disk_size(DbName),

    ?assert(DbSizeNone > DbSizeSnappy),
    ?assert(ViewSizeNone > ViewSizeSnappy),

    couch_config:set("couchdb", "file_compression", "deflate_1", false),
    compact_db(DbName),
    compact_view(DbName),
    DbSizeDeflate1 = db_disk_size(DbName),
    ViewSizeDeflate1 = view_disk_size(DbName),

    ?assert(DbSizeSnappy > DbSizeDeflate1),
    ?assert(ViewSizeSnappy > ViewSizeDeflate1),

    couch_config:set("couchdb", "file_compression", "deflate_9", false),
    compact_db(DbName),
    compact_view(DbName),
    DbSizeDeflate9 = db_disk_size(DbName),
    ViewSizeDeflate9 = view_disk_size(DbName),

    ?assert(DbSizeDeflate1 > DbSizeDeflate9),
    ?assert(ViewSizeDeflate1 > ViewSizeDeflate9).


populate_db(_Db, NumDocs) when NumDocs =< 0 ->
    ok;
populate_db(Db, NumDocs) ->
    Docs = lists:map(
        fun(_) ->
            couch_doc:from_json_obj({[
                {<<"_id">>, couch_uuids:random()},
                {<<"string">>, ?l2b(lists:duplicate(1000, $X))}
            ]})
        end,
        lists:seq(1, 500)),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    populate_db(Db, NumDocs - 500).

refresh_index(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, DDoc} = couch_db:open_doc(Db, ?DDOC_ID, [ejson_body]),
    couch_mrview:query_view(Db, DDoc, <<"by_id">>, [{stale, false}]),
    ok = couch_db:close(Db).

compact_db(DbName) ->
    DiskSizeBefore = db_disk_size(DbName),
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, CompactPid} = couch_db:start_compact(Db),
    MonRef = erlang:monitor(process, CompactPid),
    receive
        {'DOWN', MonRef, process, CompactPid, normal} ->
            ok;
        {'DOWN', MonRef, process, CompactPid, Reason} ->
            erlang:error({assertion_failed,
                          [{module, ?MODULE},
                           {line, ?LINE},
                           {reason, "Error compacting database: "
                                    ++ couch_util:to_list(Reason)}]})
    after ?TIMEOUT ->
        erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {reason, "Timeout waiting for database compaction"}]})
    end,
    ok = couch_db:close(Db),
    DiskSizeAfter = db_disk_size(DbName),
    ?assert(DiskSizeBefore > DiskSizeAfter).

compact_view(DbName) ->
    DiskSizeBefore = view_disk_size(DbName),
    {ok, MonRef} = couch_mrview:compact(DbName, ?DDOC_ID, [monitor]),
    receive
        {'DOWN', MonRef, process, _CompactPid, normal} ->
            ok;
        {'DOWN', MonRef, process, _CompactPid, Reason} ->
            erlang:error({assertion_failed,
                          [{module, ?MODULE},
                           {line, ?LINE},
                           {reason, "Error compacting view group: "
                                    ++ couch_util:to_list(Reason)}]})
    after ?TIMEOUT ->
        erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {reason, "Timeout waiting for view group compaction"}]})
    end,
    DiskSizeAfter = view_disk_size(DbName),
    ?assert(DiskSizeBefore > DiskSizeAfter).

db_disk_size(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Info} = couch_db:get_db_info(Db),
    ok = couch_db:close(Db),
    couch_util:get_value(disk_size, Info).

view_disk_size(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, DDoc} = couch_db:open_doc(Db, ?DDOC_ID, [ejson_body]),
    {ok, Info} = couch_mrview:get_info(Db, DDoc),
    ok = couch_db:close(Db),
    couch_util:get_value(disk_size, Info).
