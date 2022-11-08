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

-module(fabric_tests).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").

cleanup_index_files_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_cleanup_index_files),
            ?TDEF_FE(t_cleanup_index_files_with_existing_db),
            ?TDEF_FE(t_cleanup_index_files_with_view_data),
            ?TDEF_FE(t_cleanup_index_files_with_deleted_db),
            ?TDEF_FE(t_cleanup_index_file_after_ddoc_update),
            ?TDEF_FE(t_cleanup_index_file_after_ddoc_delete)
        ]
    }.

setup() ->
    Ctx = test_util:start_couch([fabric]),
    DbName = ?tempdb(),
    fabric:create_db(DbName, [{q, 1}]),
    create_ddoc(DbName, <<"_design/foo">>, <<"bar">>),
    {ok, _} = fabric:query_view(DbName, <<"foo">>, <<"bar">>),
    create_ddoc(DbName, <<"_design/boo">>, <<"baz">>),
    {ok, _} = fabric:query_view(DbName, <<"boo">>, <<"baz">>),
    {Ctx, DbName}.

teardown({Ctx, DbName}) ->
    fabric:delete_db(DbName),
    test_util:stop_couch(Ctx).

t_cleanup_index_files(_) ->
    CheckFun = fun(Res) -> Res =:= ok end,
    ?assert(lists:all(CheckFun, fabric:cleanup_index_files())).

t_cleanup_index_files_with_existing_db({_, DbName}) ->
    ?assertEqual(ok, fabric:cleanup_index_files(DbName)).

t_cleanup_index_files_with_view_data({_, DbName}) ->
    Sigs = sigs(DbName),
    Indices = indices(DbName),
    Purges = purges(DbName),
    ok = fabric:cleanup_index_files(DbName),
    % We haven't inadvertently removed any active index bits
    ?assertEqual(Sigs, sigs(DbName)),
    ?assertEqual(Indices, indices(DbName)),
    ?assertEqual(Purges, purges(DbName)).

t_cleanup_index_files_with_deleted_db(_) ->
    SomeDb = ?tempdb(),
    ?assertEqual(ok, fabric:cleanup_index_files(SomeDb)).

t_cleanup_index_file_after_ddoc_update({_, DbName}) ->
    ?assertEqual(
        [
            "4bcdf852098ff6b0578ddf472c320e9c.view",
            "da817c3d3f7413c1a610f25635a0c521.view"
        ],
        indices(DbName)
    ),
    ?assertEqual(
        [
            <<"_local/purge-mrview-4bcdf852098ff6b0578ddf472c320e9c">>,
            <<"_local/purge-mrview-da817c3d3f7413c1a610f25635a0c521">>
        ],
        purges(DbName)
    ),

    update_ddoc(DbName, <<"_design/foo">>, <<"bar1">>),
    ok = fabric:cleanup_index_files(DbName),
    {ok, _} = fabric:query_view(DbName, <<"foo">>, <<"bar1">>),

    % One 4bc stays, da8 should  gone and 9e3 is added
    ?assertEqual(
        [
            "4bcdf852098ff6b0578ddf472c320e9c.view",
            "9e355b0fee411b4257036b8fca56f263.view"
        ],
        indices(DbName)
    ),
    ?assertEqual(
        [
            <<"_local/purge-mrview-4bcdf852098ff6b0578ddf472c320e9c">>,
            <<"_local/purge-mrview-9e355b0fee411b4257036b8fca56f263">>
        ],
        purges(DbName)
    ).

t_cleanup_index_file_after_ddoc_delete({_, DbName}) ->
    ?assertEqual(
        [
            "4bcdf852098ff6b0578ddf472c320e9c.view",
            "da817c3d3f7413c1a610f25635a0c521.view"
        ],
        indices(DbName)
    ),
    ?assertEqual(
        [
            <<"_local/purge-mrview-4bcdf852098ff6b0578ddf472c320e9c">>,
            <<"_local/purge-mrview-da817c3d3f7413c1a610f25635a0c521">>
        ],
        purges(DbName)
    ),

    delete_ddoc(DbName, <<"_design/foo">>),
    ok = fabric:cleanup_index_files(DbName),

    % 4bc stays the same, da8 should be gone
    ?assertEqual(
        [
            "4bcdf852098ff6b0578ddf472c320e9c.view"
        ],
        indices(DbName)
    ),
    ?assertEqual(
        [
            <<"_local/purge-mrview-4bcdf852098ff6b0578ddf472c320e9c">>
        ],
        purges(DbName)
    ),

    delete_ddoc(DbName, <<"_design/boo">>),
    ok = fabric:cleanup_index_files(DbName),

    ?assertEqual([], indices(DbName)),
    ?assertEqual([], purges(DbName)),

    % cleaning a db with all deleted indices should still work
    ok = fabric:cleanup_index_files(DbName),

    ?assertEqual([], indices(DbName)),
    ?assertEqual([], purges(DbName)).

shard_names(DbName) ->
    [mem3:name(S) || S <- mem3:local_shards(DbName)].

% Sorted list of sigs
%
sigs(DbName) ->
    case shard_names(DbName) of
        [] ->
            [];
        [SomeDb | _] ->
            Sigs = couch_mrview_util:get_signatures(SomeDb),
            lists:sort(maps:keys(Sigs))
    end.

% Sorted list of index files
%
indices(DbName) ->
    case shard_names(DbName) of
        [] ->
            [];
        [_ | _] = Dbs ->
            AllIndices = lists:map(fun couch_mrview_util:get_index_files/1, Dbs),
            AsList = lists:sort(
                lists:foldl(
                    fun(Indices, Acc) ->
                        maps:values(Indices) ++ Acc
                    end,
                    [],
                    AllIndices
                )
            ),
            % Keep only file names and extensions. Since we use q=1, we shouldn't
            % have any duplicates
            [filename:basename(F) || F <- AsList]
    end.

% Sorted list of purge checkpoint doc ids
%
purges(DbName) ->
    case shard_names(DbName) of
        [] ->
            [];
        [_ | _] = Dbs ->
            AllPurges = lists:map(fun couch_mrview_util:get_purge_checkpoints/1, Dbs),
            lists:sort(
                lists:foldl(
                    fun(Purges, Acc) ->
                        maps:values(Purges) ++ Acc
                    end,
                    [],
                    AllPurges
                )
            )
    end.

create_ddoc(DbName, DDocId, ViewName) ->
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDocId},
            {<<"language">>, <<"javascript">>},
            {<<"views">>,
                {[
                    {ViewName,
                        {[
                            {<<"map">>, <<"function(doc) { emit(doc.value, null); }">>}
                        ]}}
                ]}}
        ]}
    ),
    fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]).

update_ddoc(DbName, DDocId, ViewName) ->
    {ok, DDoc0} = fabric:open_doc(DbName, DDocId, [?ADMIN_CTX]),
    DDoc = DDoc0#doc{
        body =
            {[
                {<<"language">>, <<"javascript">>},
                {<<"views">>,
                    {[
                        {ViewName,
                            {[
                                {<<"map">>, <<"function(doc) { emit(doc.value, 1); }">>}
                            ]}}
                    ]}}
            ]}
    },
    fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]).

delete_ddoc(DbName, DDocId) ->
    {ok, DDoc0} = fabric:open_doc(DbName, DDocId, [?ADMIN_CTX]),
    DDoc = DDoc0#doc{deleted = true, body = {[]}},
    fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]).
