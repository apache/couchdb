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

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DDOC_ID, <<"_design/test">>).
-define(DOCS_COUNT, 5000).
-define(TIMEOUT, 30000).

setup() ->
    config:set("couchdb", "file_compression", "none", false),
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
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
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.


couch_file_compression_test_() ->
    {
        "CouchDB file compression tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
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
    config:set("couchdb", "file_compression", "none", false),
    {
        "Use no compression",
        [
            {"compact database",
             {timeout, 5 + ?TIMEOUT div 1000, ?_test(compact_db(DbName))}},
            {"compact view",
             {timeout, 5 + ?TIMEOUT div 1000, ?_test(compact_view(DbName))}}
        ]
    }.

should_use_deflate_1(DbName) ->
    config:set("couchdb", "file_compression", "deflate_1", false),
    {
        "Use deflate compression at level 1",
        [
            {"compact database",
             {timeout, 5 + ?TIMEOUT div 1000, ?_test(compact_db(DbName))}},
            {"compact view",
             {timeout, 5 + ?TIMEOUT div 1000, ?_test(compact_view(DbName))}}
        ]
    }.

should_use_deflate_9(DbName) ->
    config:set("couchdb", "file_compression", "deflate_9", false),
    {
        "Use deflate compression at level 9",
        [
            {"compact database",
             {timeout, 5 + ?TIMEOUT div 1000, ?_test(compact_db(DbName))}},
            {"compact view",
             {timeout, 5 + ?TIMEOUT div 1000, ?_test(compact_view(DbName))}}
        ]
    }.

should_use_snappy(DbName) ->
    config:set("couchdb", "file_compression", "snappy", false),
    {
        "Use snappy compression",
        [
            {"compact database",
             {timeout, 5 + ?TIMEOUT div 1000, ?_test(compact_db(DbName))}},
            {"compact view",
             {timeout, 5 + ?TIMEOUT div 1000, ?_test(compact_view(DbName))}}
        ]
    }.

should_compare_compression_methods(DbName) ->
    {"none > snappy > deflate_1 > deflate_9",
     {timeout, ?TIMEOUT div 1000, ?_test(compare_compression_methods(DbName))}}.

compare_compression_methods(DbName) ->
    config:set("couchdb", "file_compression", "none", false),
    ExternalSizePreCompact = db_external_size(DbName),
    compact_db(DbName),
    compact_view(DbName),
    DbSizeNone = db_disk_size(DbName),
    ViewSizeNone = view_disk_size(DbName),
    ExternalSizeNone = db_external_size(DbName),
    ViewExternalSizeNone = view_external_size(DbName),

    config:set("couchdb", "file_compression", "snappy", false),
    compact_db(DbName),
    compact_view(DbName),
    DbSizeSnappy = db_disk_size(DbName),
    ViewSizeSnappy = view_disk_size(DbName),
    ExternalSizeSnappy = db_external_size(DbName),
    ViewExternalSizeSnappy = view_external_size(DbName),

    ?assert(DbSizeNone > DbSizeSnappy),
    ?assert(ViewSizeNone > ViewSizeSnappy),

    config:set("couchdb", "file_compression", "deflate_1", false),
    compact_db(DbName),
    compact_view(DbName),
    DbSizeDeflate1 = db_disk_size(DbName),
    ViewSizeDeflate1 = view_disk_size(DbName),

    ?assert(DbSizeSnappy > DbSizeDeflate1),
    ?assert(ViewSizeSnappy > ViewSizeDeflate1),

    config:set("couchdb", "file_compression", "deflate_9", false),
    compact_db(DbName),
    compact_view(DbName),
    DbSizeDeflate9 = db_disk_size(DbName),
    ViewSizeDeflate9 = view_disk_size(DbName),
    ExternalSizeDeflate9 = db_external_size(DbName),
    ViewExternalSizeDeflate9 = view_external_size(DbName),

    ?assert(DbSizeDeflate1 > DbSizeDeflate9),
    ?assert(ViewSizeDeflate1 > ViewSizeDeflate9),
    ?assert(ExternalSizePreCompact =:= ExternalSizeNone),
    ?assert(ExternalSizeNone =:= ExternalSizeSnappy),
    ?assert(ExternalSizeNone =:= ExternalSizeDeflate9),
    ?assert(ViewExternalSizeNone =:= ViewExternalSizeSnappy),
    ?assert(ViewExternalSizeNone =:= ViewExternalSizeDeflate9).


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
    couch_mrview:query_view(Db, DDoc, <<"by_id">>, [{update, true}]),
    ok = couch_db:close(Db).

compact_db(DbName) ->
    DiskSizeBefore = db_disk_size(DbName),
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _CompactPid} = couch_db:start_compact(Db),
    wait_compaction(DbName, "database", ?LINE),
    ok = couch_db:close(Db),
    DiskSizeAfter = db_disk_size(DbName),
    ?assert(DiskSizeBefore > DiskSizeAfter).

compact_view(DbName) ->
    DiskSizeBefore = view_disk_size(DbName),
    {ok, _MonRef} = couch_mrview:compact(DbName, ?DDOC_ID, [monitor]),
    wait_compaction(DbName, "view group", ?LINE),
    DiskSizeAfter = view_disk_size(DbName),
    ?assert(DiskSizeBefore > DiskSizeAfter).

db_disk_size(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Info} = couch_db:get_db_info(Db),
    ok = couch_db:close(Db),
    active_size(Info).

db_external_size(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Info} = couch_db:get_db_info(Db),
    ok = couch_db:close(Db),
    external_size(Info).

view_disk_size(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, DDoc} = couch_db:open_doc(Db, ?DDOC_ID, [ejson_body]),
    {ok, Info} = couch_mrview:get_info(Db, DDoc),
    ok = couch_db:close(Db),
    active_size(Info).

view_external_size(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, DDoc} = couch_db:open_doc(Db, ?DDOC_ID, [ejson_body]),
    {ok, Info} = couch_mrview:get_info(Db, DDoc),
    ok = couch_db:close(Db),
    external_size(Info).

active_size(Info) ->
    couch_util:get_nested_json_value({Info}, [sizes, active]).

external_size(Info) ->
    couch_util:get_nested_json_value({Info}, [sizes, external]).

wait_compaction(DbName, Kind, Line) ->
    WaitFun = fun() ->
       case is_compaction_running(DbName) of
           true -> wait;
           false -> ok
       end
    end,
    case test_util:wait(WaitFun, ?TIMEOUT) of
        timeout ->
            erlang:error({assertion_failed,
                          [{module, ?MODULE},
                           {line, Line},
                           {reason, "Timeout waiting for "
                                    ++ Kind
                                    ++ " database compaction"}]});
        _ ->
            ok
    end.

is_compaction_running(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, DbInfo} = couch_db:get_db_info(Db),
    {ok, ViewInfo} = couch_mrview:get_info(Db, ?DDOC_ID),
    couch_db:close(Db),
    (couch_util:get_value(compact_running, ViewInfo) =:= true)
        orelse (couch_util:get_value(compact_running, DbInfo) =:= true).
