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

-module(couch_mrview_util_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(DDOC_ID, <<"_design/bar">>).

couch_mrview_util_test_() ->
    [
        ?_assertEqual(0, validate_group_level(undefined, undefined)),
        ?_assertEqual(exact, validate_group_level(true, undefined)),
        ?_assertEqual(0, validate_group_level(false, undefined)),
        ?_assertEqual(1, validate_group_level(undefined, 1)),
        ?_assertEqual(0, validate_group_level(true, 0)),
        ?_assertEqual(0, validate_group_level(undefined, 0)),
        ?_assertEqual(1, validate_group_level(true, 1)),
        ?_assertEqual(0, validate_group_level(false, 0)),
        ?_assertThrow(
            {query_parse_error, <<"Can't specify group=false and group_level>0 at the same time">>},
            validate_group_level(false, 1)
        )
    ].

validate_group_level(Group, GroupLevel) ->
    Args0 = #mrargs{group = Group, group_level = GroupLevel, view_type = red},
    Args1 = couch_mrview_util:validate_args(Args0),
    Args1#mrargs.group_level.

get_signature_from_filename_test() ->
    Sig = "da817c3d3f7413c1a610f25635a0c521",
    P1 = "/x.1667618375_design/mrview/da817c3d3f7413c1a610f25635a0c521.view",
    P2 = "/x.1667618375_design/mrview/da817c3d3f7413c1a610f25635a0c521.compact.view",
    P3 = "/x.1667618375_design/mrview/da817c3d3f7413c1a610f25635a0c521",
    ?assertEqual(Sig, couch_mrview_util:get_signature_from_filename(P1)),
    ?assertEqual(Sig, couch_mrview_util:get_signature_from_filename(P2)),
    ?assertEqual(Sig, couch_mrview_util:get_signature_from_filename(P3)).

verify_view_filename_test() ->
    P1 = "/x.1667618375_design/mrview/da817c3d3f7413c1a610f25635a0c521.view",
    P2 = "/x.1667618375_design/mrview/da817c3d3f7413c1a610f25635a0c521.compact.view",
    P3 = "/x.1667618375_design/mrview/da817c3d3f7413c1a610f25635a0c521",
    ?assert(couch_mrview_util:verify_view_filename(P1)),
    ?assert(couch_mrview_util:verify_view_filename(P2)),
    ?assertNot(couch_mrview_util:verify_view_filename(P3)),
    ?assertNot(couch_mrview_util:verify_view_filename("")),
    ?assertNot(couch_mrview_util:verify_view_filename("foo.view")).

setup() ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [?ADMIN_CTX, {q, 2}]),
    DDoc = couch_mrview_test_util:ddoc(map),
    {ok, _} = fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]),
    {ok, _} = fabric:query_view(DbName, <<"bar">>, <<"baz">>, #mrargs{}),
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), map),
    {ok, _} = couch_mrview:query_view(Db, ?DDOC_ID, <<"baz">>),
    {DbName, Db}.

teardown({DbName, Db}) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]).

get_signatures_test_() ->
    {
        setup,
        fun() -> test_util:start_couch([fabric]) end,
        fun test_util:stop_couch/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_get_signatures_local),
                ?TDEF_FE(t_get_signatures_clustered),
                ?TDEF_FE(t_get_purge_checkpoints_local),
                ?TDEF_FE(t_get_purge_checkpoints_clustered),
                ?TDEF_FE(t_get_index_files_local),
                ?TDEF_FE(t_get_index_files_clustered)
            ]
        }
    }.

t_get_signatures_local({_, Db}) ->
    DbName = couch_db:name(Db),
    Sigs = couch_mrview_util:get_signatures(DbName),
    ?assert(is_map(Sigs)),
    ?assertEqual(1, map_size(Sigs)),
    [{Sig, true}] = maps:to_list(Sigs),
    {ok, Info} = couch_mrview:get_info(Db, ?DDOC_ID),
    ?assertEqual(proplists:get_value(signature, Info), Sig),

    {ok, DDoc} = couch_db:open_doc(Db, ?DDOC_ID, [?ADMIN_CTX]),
    Deleted = DDoc#doc{deleted = true, body = {[]}},
    {ok, _} = couch_db:update_doc(Db, Deleted, []),
    ?assertEqual(#{}, couch_mrview_util:get_signatures(DbName)).

t_get_signatures_clustered({DbName, _Db}) ->
    [Shard1, Shard2] = mem3:local_shards(DbName),
    ShardName1 = mem3:name(Shard1),
    ShardName2 = mem3:name(Shard2),
    Sigs = couch_mrview_util:get_signatures(ShardName1),
    ?assertEqual(Sigs, couch_mrview_util:get_signatures(ShardName2)),
    ?assert(is_map(Sigs)),
    ?assertEqual(1, map_size(Sigs)),
    [{Sig, true}] = maps:to_list(Sigs),
    {ok, Info} = couch_mrview:get_info(ShardName1, ?DDOC_ID),
    ?assertEqual(proplists:get_value(signature, Info), Sig),

    {ok, DDoc} = fabric:open_doc(DbName, ?DDOC_ID, [?ADMIN_CTX]),
    Deleted = DDoc#doc{deleted = true, body = {[]}},
    {ok, _} = fabric:update_doc(DbName, Deleted, [?ADMIN_CTX]),
    ?assertEqual(#{}, couch_mrview_util:get_signatures(ShardName1)),
    ?assertEqual(#{}, couch_mrview_util:get_signatures(ShardName2)).

t_get_purge_checkpoints_local({_, Db}) ->
    DbName = couch_db:name(Db),
    Checkpoints = couch_mrview_util:get_purge_checkpoints(DbName),
    ?assert(is_map(Checkpoints)),
    ?assertEqual(1, map_size(Checkpoints)),
    [{Sig, <<"_local/", _/binary>>}] = maps:to_list(Checkpoints),
    {ok, Info} = couch_mrview:get_info(Db, ?DDOC_ID),
    ?assertEqual(proplists:get_value(signature, Info), Sig).

t_get_purge_checkpoints_clustered({DbName, _Db}) ->
    {ok, _} = fabric:query_view(DbName, <<"bar">>, <<"baz">>, #mrargs{}),
    [Shard1, Shard2] = mem3:local_shards(DbName),
    ShardName1 = mem3:name(Shard1),
    ShardName2 = mem3:name(Shard2),
    Sigs1 = couch_mrview_util:get_purge_checkpoints(ShardName1),
    Sigs2 = couch_mrview_util:get_purge_checkpoints(ShardName2),
    ?assertEqual(lists:sort(maps:keys(Sigs1)), lists:sort(maps:keys(Sigs2))),
    ?assert(is_map(Sigs1)),
    ?assertEqual(1, map_size(Sigs1)),
    [{Sig, <<"_local/", _/binary>>}] = maps:to_list(Sigs1),
    {ok, Info} = couch_mrview:get_info(ShardName1, ?DDOC_ID),
    ?assertEqual(proplists:get_value(signature, Info), Sig).

t_get_index_files_local({_, Db}) ->
    DbName = couch_db:name(Db),
    SigFilesMap = couch_mrview_util:get_index_files(DbName),
    ?assert(is_map(SigFilesMap)),
    ?assertEqual(1, map_size(SigFilesMap)),
    [{Sig, [File]}] = maps:to_list(SigFilesMap),
    ?assertMatch({ok, _}, file:read_file_info(File)),
    {ok, Info} = couch_mrview:get_info(Db, ?DDOC_ID),
    ?assertEqual(proplists:get_value(signature, Info), Sig).

t_get_index_files_clustered({DbName, _Db}) ->
    {ok, _} = fabric:query_view(DbName, <<"bar">>, <<"baz">>, #mrargs{}),
    [Shard1, Shard2] = mem3:local_shards(DbName),
    ShardName1 = mem3:name(Shard1),
    ShardName2 = mem3:name(Shard2),
    SigFilesMap1 = couch_mrview_util:get_index_files(ShardName1),
    SigFilesMap2 = couch_mrview_util:get_index_files(ShardName2),
    SigKeys1 = lists:sort(maps:keys(SigFilesMap1)),
    SigKeys2 = lists:sort(maps:keys(SigFilesMap2)),
    ?assertEqual(SigKeys1, SigKeys2),
    ?assert(is_map(SigFilesMap1)),
    ?assertEqual(1, map_size(SigFilesMap1)),
    [{Sig, [File]}] = maps:to_list(SigFilesMap1),
    ?assertMatch({ok, _}, file:read_file_info(File)),
    {ok, Info} = couch_mrview:get_info(ShardName1, ?DDOC_ID),
    ?assertEqual(proplists:get_value(signature, Info), Sig).
