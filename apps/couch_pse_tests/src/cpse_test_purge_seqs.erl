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

-module(cpse_test_purge_seqs).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup_each() ->
    {ok, Db} = cpse_util:create_db(),
    couch_db:name(Db).

teardown_each(DbName) ->
    ok = couch_server:delete(DbName, []).

cpse_increment_purge_seq_on_complete_purge(DbName) ->
    {ok, Rev1} = cpse_util:save_doc(DbName, {[{'_id', foo1}, {vsn, 1.1}]}),
    {ok, Rev2} = cpse_util:save_doc(DbName, {[{'_id', foo2}, {vsn, 1.2}]}),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 2},
        {del_doc_count, 0},
        {update_seq, 2},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos1 = [
        {cpse_util:uuid(), <<"foo1">>, [Rev1]}
    ],
    {ok, [{ok, PRevs1}]} = cpse_util:purge(DbName, PurgeInfos1),
    ?assertEqual([Rev1], PRevs1),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 3},
        {purge_seq, 1},
        {purge_infos, PurgeInfos1}
    ]),

    PurgeInfos2 = [
        {cpse_util:uuid(), <<"foo2">>, [Rev2]}
    ],
    {ok, [{ok, PRevs2}]} = cpse_util:purge(DbName, PurgeInfos2),
    ?assertEqual([Rev2], PRevs2),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 0},
        {del_doc_count, 0},
        {update_seq, 4},
        {purge_seq, 2},
        {purge_infos, PurgeInfos1 ++ PurgeInfos2}
    ]).

cpse_increment_purge_multiple_times(DbName) ->
    {ok, Rev1} = cpse_util:save_doc(DbName, {[{'_id', foo1}, {vsn, 1.1}]}),
    {ok, Rev2} = cpse_util:save_doc(DbName, {[{'_id', foo2}, {vsn, 1.2}]}),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 2},
        {del_doc_count, 0},
        {update_seq, 2},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos1 = [
        {cpse_util:uuid(), <<"foo1">>, [Rev1]},
        {cpse_util:uuid(), <<"foo2">>, [Rev2]}
    ],
    {ok, [{ok, PRevs1}, {ok, PRevs2}]} = cpse_util:purge(DbName, PurgeInfos1),
    ?assertEqual([Rev1], PRevs1),
    ?assertEqual([Rev2], PRevs2),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 0},
        {del_doc_count, 0},
        {update_seq, 3},
        {purge_seq, 2},
        {purge_infos, PurgeInfos1}
    ]).

cpse_increment_purge_seq_on_partial_purge(DbName) ->
    {ok, Rev1} = cpse_util:save_doc(DbName, {[{'_id', foo1}, {vsn, <<"1.1">>}]}),
    Update =
        {[
            {'_id', foo1},
            {'_rev', couch_doc:rev_to_str({1, [couch_hash:md5_hash(<<"1.2">>)]})},
            {vsn, <<"1.2">>}
        ]},
    {ok, [_Rev2]} = cpse_util:save_docs(DbName, [Update], [replicated_changes]),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 2},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos1 = [
        {cpse_util:uuid(), <<"foo1">>, [Rev1]}
    ],
    {ok, [{ok, PRevs1}]} = cpse_util:purge(DbName, PurgeInfos1),
    ?assertEqual([Rev1], PRevs1),

    cpse_util:assert_db_props(?MODULE, ?LINE, DbName, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 3},
        {purge_seq, 1},
        {purge_infos, PurgeInfos1}
    ]).
