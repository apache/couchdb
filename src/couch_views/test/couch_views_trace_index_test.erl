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


-module(couch_views_trace_index_test).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


% Steps for this to work
% Run export FDB_NETWORK_OPTION_TRACE_ENABLE="" &&
% make eunit apps=couch_views suites=couch_views_trace_index_test
% look in src/couch_views/.eunit for trace file
% Might need to add extra </Trace> to finish up file
% Analyze!


-define(EUNIT_FTW(Tests), [{with, [T]} || T <- Tests]).


indexer_test_() ->
    {
        "Trace view indexing",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {
                foreach,
                fun foreach_setup/0,
                fun foreach_teardown/1,
                ?EUNIT_FTW([
                    fun trace_single_doc/1
                ])
            }
        }
    }.


setup() ->
    test_util:start_couch([fabric]).


cleanup(Ctx) ->
    test_util:stop_couch(Ctx).


foreach_setup() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    Db.


foreach_teardown(Db) ->
    meck:unload(),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


trace_single_doc(Db) ->
    DbName = fabric2_db:name(Db),
    DDoc = create_ddoc(),
    Doc = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_doc(Db, Doc, []),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),

    JobData = #{
        <<"db_name">> => DbName,
        <<"ddoc_id">> => <<"_design/bar">>,
        <<"sig">> => fabric2_util:to_hex(Mrst#mrst.sig)
    },
    meck:expect(couch_jobs, accept, 2, {ok, job, JobData}),
    meck:expect(couch_jobs, update, 3, {ok, job}),
    meck:expect(couch_jobs, finish, 3, ok),
    put(erlfdb_trace, <<"views_write_one_doc">>),
    couch_views_indexer:init(),

    put(erlfdb_trace, <<"views_read_one_doc">>),
    {ok, Out} = couch_views:query(
        Db,
        DDoc,
        <<"map_fun1">>,
        fun fold_fun/2,
        [],
        #mrargs{}
    ),

    ?assertEqual([{row, [
        {id, <<"0">>},
        {key, 0},
        {value, 0}
    ]}], Out).


create_ddoc() ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"map_fun1">>, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>}
            ]}},
            {<<"map_fun2">>, {[
                {<<"map">>, <<"function(doc) {}">>}
            ]}}
        ]}}
    ]}).


doc(Id) ->
    doc(Id, Id).


doc(Id, Val) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Id))},
        {<<"val">>, Val}
    ]}).


fold_fun({meta, _Meta}, Acc) ->
    {ok, Acc};

fold_fun({row, _} = Row, Acc) ->
    {ok, [Row | Acc]};

fold_fun(complete, Acc) ->
    {ok, lists:reverse(Acc)}.
