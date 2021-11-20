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

-module(cpse_test_purge_docs).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(REV_DEPTH, 100).

setup_each() ->
    {ok, Db} = cpse_util:create_db(),
    couch_db:name(Db).

teardown_each(DbName) ->
    ok = couch_server:delete(DbName, []).

cpse_purge_simple(DbName) ->
    {ok, Rev} = cpse_util:save_doc(DbName, {[{'_id', foo1}, {vsn, 1.1}]}),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 1},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo1">>, [Rev]}
    ],

    {ok, [{ok, PRevs}]} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([Rev], PRevs),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 0},
        {del_doc_count, 0},
        {update_seq, 2},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_simple_info_check(DbName) ->
    {ok, Rev} = cpse_util:save_doc(DbName, {[{'_id', foo1}, {vsn, 1.1}]}),
    PurgeInfos = [
        {cpse_util:uuid(), <<"foo1">>, [Rev]}
    ],
    {ok, [{ok, PRevs}]} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([Rev], PRevs),

    {ok, AllInfos} = couch_util:with_db(DbName, fun(Db) ->
        couch_db_engine:fold_purge_infos(Db, 0, fun fold_all_infos/2, [], [])
    end),

    ?assertMatch([{1, <<_/binary>>, <<"foo1">>, [Rev]}], AllInfos).

cpse_purge_empty_db(DbName) ->
    PurgeInfos = [
        {cpse_util:uuid(), <<"foo">>, [{0, <<0>>}]}
    ],

    {ok, [{ok, PRevs}]} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([], PRevs),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 0},
        {del_doc_count, 0},
        {update_seq, 1},
        {changes, 0},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_single_docid(DbName) ->
    {ok, [Rev1, _Rev2]} = cpse_util:save_docs(DbName, [
        {[{'_id', foo1}, {vsn, 1}]},
        {[{'_id', foo2}, {vsn, 2}]}
    ]),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 2},
        {del_doc_count, 0},
        {update_seq, 2},
        {changes, 2},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo1">>, [Rev1]}
    ],
    {ok, [{ok, PRevs}]} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([Rev1], PRevs),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 3},
        {changes, 1},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_multiple_docids(DbName) ->
    {ok, [Rev1, Rev2]} = cpse_util:save_docs(DbName, [
        {[{'_id', foo1}, {vsn, 1.1}]},
        {[{'_id', foo2}, {vsn, 1.2}]}
    ]),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 2},
        {del_doc_count, 0},
        {update_seq, 2},
        {changes, 2},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo1">>, [Rev1]},
        {cpse_util:uuid(), <<"foo2">>, [Rev2]}
    ],

    {ok, [{ok, PRevs1}, {ok, PRevs2}]} = cpse_util:purge(DbName, PurgeInfos),

    ?assertEqual([Rev1], PRevs1),
    ?assertEqual([Rev2], PRevs2),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 0},
        {del_doc_count, 0},
        {update_seq, 3},
        {changes, 0},
        {purge_seq, 2},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_no_docids(DbName) ->
    {ok, [_Rev1, _Rev2]} = cpse_util:save_docs(DbName, [
        {[{'_id', foo1}, {vsn, 1}]},
        {[{'_id', foo2}, {vsn, 2}]}
    ]),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 2},
        {del_doc_count, 0},
        {update_seq, 2},
        {changes, 2},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    {ok, []} = cpse_util:purge(DbName, []),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 2},
        {del_doc_count, 0},
        {update_seq, 2},
        {changes, 2},
        {purge_seq, 0},
        {purge_infos, []}
    ]).

cpse_purge_rev_path(DbName) ->
    {ok, Rev1} = cpse_util:save_doc(DbName, {[{'_id', foo}, {vsn, 1}]}),
    Update =
        {[
            {<<"_id">>, <<"foo">>},
            {<<"_rev">>, couch_doc:rev_to_str(Rev1)},
            {<<"_deleted">>, true},
            {<<"vsn">>, 2}
        ]},
    {ok, Rev2} = cpse_util:save_doc(DbName, Update),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 0},
        {del_doc_count, 1},
        {update_seq, 2},
        {changes, 1},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo">>, [Rev2]}
    ],

    {ok, [{ok, PRevs}]} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([Rev2], PRevs),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 0},
        {del_doc_count, 0},
        {update_seq, 3},
        {changes, 0},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_deep_revision_path(DbName) ->
    {ok, InitRev} = cpse_util:save_doc(DbName, {[{'_id', bar}, {vsn, 0}]}),
    LastRev = lists:foldl(
        fun(Count, PrevRev) ->
            Update =
                {[
                    {'_id', bar},
                    {'_rev', couch_doc:rev_to_str(PrevRev)},
                    {vsn, Count}
                ]},
            {ok, NewRev} = cpse_util:save_doc(DbName, Update),
            NewRev
        end,
        InitRev,
        lists:seq(1, ?REV_DEPTH)
    ),

    PurgeInfos = [
        {cpse_util:uuid(), <<"bar">>, [LastRev]}
    ],

    {ok, [{ok, PRevs}]} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([LastRev], PRevs),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 0},
        {del_doc_count, 0},
        {update_seq, ?REV_DEPTH + 2},
        {changes, 0},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_partial_revs(DbName) ->
    {ok, Rev1} = cpse_util:save_doc(DbName, {[{'_id', foo}, {vsn, <<"1.1">>}]}),
    Update =
        {[
            {'_id', foo},
            {'_rev', couch_doc:rev_to_str({1, [couch_hash:md5_hash(<<"1.2">>)]})},
            {vsn, <<"1.2">>}
        ]},
    {ok, [_Rev2]} = cpse_util:save_docs(DbName, [Update], [replicated_changes]),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo">>, [Rev1]}
    ],

    {ok, [{ok, PRevs}]} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([Rev1], PRevs),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 3},
        {changes, 1},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_missing_docid(DbName) ->
    {ok, [Rev1, _Rev2]} = cpse_util:save_docs(DbName, [
        {[{'_id', foo1}, {vsn, 1}]},
        {[{'_id', foo2}, {vsn, 2}]}
    ]),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 2},
        {del_doc_count, 0},
        {update_seq, 2},
        {changes, 2},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos = [
        {cpse_util:uuid(), <<"baz">>, [Rev1]}
    ],

    {ok, [{ok, []}]} = cpse_util:purge(DbName, PurgeInfos),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 2},
        {del_doc_count, 0},
        {update_seq, 3},
        {changes, 2},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_duplicate_docids(DbName) ->
    {ok, [Rev1, _Rev2]} = cpse_util:save_docs(DbName, [
        {[{'_id', foo1}, {vsn, 1}]},
        {[{'_id', foo2}, {vsn, 2}]}
    ]),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 2},
        {del_doc_count, 0},
        {update_seq, 2},
        {purge_seq, 0},
        {changes, 2},
        {purge_infos, []}
    ]),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo1">>, [Rev1]},
        {cpse_util:uuid(), <<"foo1">>, [Rev1]}
    ],

    {ok, Resp} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([{ok, [Rev1]}, {ok, []}], Resp),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 3},
        {purge_seq, 2},
        {changes, 1},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_internal_revision(DbName) ->
    {ok, Rev1} = cpse_util:save_doc(DbName, {[{'_id', foo}, {vsn, 1}]}),
    Update =
        {[
            {'_id', foo},
            {'_rev', couch_doc:rev_to_str(Rev1)},
            {vsn, 2}
        ]},
    {ok, _Rev2} = cpse_util:save_doc(DbName, Update),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo">>, [Rev1]}
    ],

    {ok, [{ok, PRevs}]} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([], PRevs),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 3},
        {changes, 1},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_missing_revision(DbName) ->
    {ok, [_Rev1, Rev2]} = cpse_util:save_docs(DbName, [
        {[{'_id', foo1}, {vsn, 1}]},
        {[{'_id', foo2}, {vsn, 2}]}
    ]),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo1">>, [Rev2]}
    ],

    {ok, [{ok, PRevs}]} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([], PRevs),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 2},
        {del_doc_count, 0},
        {update_seq, 3},
        {changes, 2},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).

cpse_purge_repeated_revisions(DbName) ->
    {ok, Rev1} = cpse_util:save_doc(DbName, {[{'_id', foo}, {vsn, <<"1.1">>}]}),
    Update =
        {[
            {'_id', foo},
            {'_rev', couch_doc:rev_to_str({1, [couch_hash:md5_hash(<<"1.2">>)]})},
            {vsn, <<"1.2">>}
        ]},
    {ok, [Rev2]} = cpse_util:save_docs(DbName, [Update], [replicated_changes]),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 2},
        {changes, 1},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos1 = [
        {cpse_util:uuid(), <<"foo">>, [Rev1]},
        {cpse_util:uuid(), <<"foo">>, [Rev1, Rev2]}
    ],

    {ok, [{ok, PRevs1}, {ok, PRevs2}]} = cpse_util:purge(DbName, PurgeInfos1),
    ?assertEqual([Rev1], PRevs1),
    ?assertEqual([Rev2], PRevs2),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 0},
        {del_doc_count, 0},
        {update_seq, 3},
        {changes, 0},
        {purge_seq, 2},
        {purge_infos, PurgeInfos1}
    ]).

cpse_purge_repeated_uuid(DbName) ->
    {ok, Rev} = cpse_util:save_doc(DbName, {[{'_id', foo1}, {vsn, 1.1}]}),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 1},
        {changes, 1},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo1">>, [Rev]}
    ],

    {ok, [{ok, PRevs1}]} = cpse_util:purge(DbName, PurgeInfos),
    ?assertEqual([Rev], PRevs1),

    % Attempting to purge a repeated UUID is an error
    ?assertThrow({badreq, _}, cpse_util:purge(DbName, PurgeInfos)),

    % Although we can replicate it in
    {ok, []} = cpse_util:purge(DbName, PurgeInfos, [replicated_changes]),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 0},
        {del_doc_count, 0},
        {update_seq, 2},
        {changes, 0},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).

fold_all_infos(Info, Acc) ->
    {ok, [Info | Acc]}.
