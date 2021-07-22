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

-module(couch_views_custom_red_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/test/fabric2_test.hrl").
-include("couch_views.hrl").

-define(NUM_DOCS, 100).

custom_reduce_disabled_test_() ->
    {
        "Custom Reduce Disabled",
        {
            setup,
            fun setup_disabled/0,
            fun teardown/1,
            with([
                ?TDEF(builtin_reductions_work),
                ?TDEF(custom_reduces_disabled)
            ])
        }
    }.

custom_reduce_enabled_test_() ->
    {
        "Custom Reduce Disabled",
        {
            setup,
            fun setup_enabled/0,
            fun teardown/1,
            with([
                ?TDEF(builtin_reductions_work),
                ?TDEF(custom_reduces_enabled)
            ])
        }
    }.

sigs_change_test_() ->
    {
        "Sigs Change Test",
        {
            setup,
            fun setup_sigs_change/0,
            fun teardown_sigs_change/1,
            with([
                ?TDEF(sigs_change)
            ])
        }
    }.

setup_disabled() ->
    setup_common(false).

setup_enabled() ->
    setup_common(true).

setup_common(Enabled) ->
    Ctx = test_util:start_couch([
        fabric,
        couch_jobs,
        js_engine,
        couch_views
    ]),
    config:set_boolean("couch_views", "custom_reduce_enabled", Enabled, false),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    fabric2_db:update_docs(Db, [create_ddoc()]),
    make_docs(Db, ?NUM_DOCS),
    run_query(Db, <<"builtin">>, #{limit => 0}),
    {Db, Ctx}.

teardown({Db, Ctx}) ->
    fabric2_db:delete(fabric2_db:name(Db), [{user_ctx, ?ADMIN_USER}]),
    test_util:stop_couch(Ctx).

setup_sigs_change() ->
    meck:new(config, [passthrough]),
    meck:expect(config, get, fun(_, _, Default) -> Default end).

teardown_sigs_change(_) ->
    meck:unload().

builtin_reductions_work({Db, _}) ->
    Result = run_query(Db, <<"builtin">>, #{}),
    Expect = {ok, [row(null, ?NUM_DOCS)]},
    ?assertEqual(Expect, Result).

custom_reduces_disabled({Db, _}) ->
    ?assertThrow({disabled, _}, run_query(Db, <<"custom">>, #{})).

custom_reduces_enabled({Db, _}) ->
    Result = run_query(Db, <<"custom">>, #{}),
    Expect = {ok, [row(null, <<"silly_reduce">>)]},
    ?assertEqual(Expect, Result).

sigs_change(_) ->
    meck:expect(config, get_boolean, fun("couch_views", _, _) -> false end),
    {ok, Mrst1} = couch_views_util:ddoc_to_mrst(<<"foo">>, create_ddoc()),
    meck:expect(config, get_boolean, fun("couch_views", _, _) -> true end),
    {ok, Mrst2} = couch_views_util:ddoc_to_mrst(<<"foo">>, create_ddoc()),
    ?assertNotEqual(Mrst1#mrst.sig, Mrst2#mrst.sig).

run_query(Db, Idx, Args) ->
    DDoc = create_ddoc(),
    run_query(Db, DDoc, Idx, Args).

run_query(Db, DDoc, Idx, Args) ->
    couch_views:query(Db, DDoc, Idx, fun default_cb/2, [], Args).

default_cb(complete, Acc) ->
    {ok, lists:reverse(Acc)};
default_cb({final, Info}, []) ->
    {ok, [Info]};
default_cb({final, _}, Acc) ->
    {ok, Acc};
default_cb({meta, _}, Acc) ->
    {ok, Acc};
default_cb(ok, ddoc_updated) ->
    {ok, ddoc_updated};
default_cb(Row, Acc) ->
    {ok, [Row | Acc]}.

row(Key, Value) ->
    {row, [{key, Key}, {value, Value}]}.

create_ddoc() ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"_design/bar">>},
            {<<"views">>,
                {[
                    {<<"custom">>,
                        {[
                            {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>},
                            {<<"reduce">>, <<
                                "function(keys, values, rereduce) {\n"
                                "  return \"silly_reduce\";\n"
                                "}\n"
                            >>}
                        ]}},
                    {<<"builtin">>,
                        {[
                            {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>},
                            {<<"reduce">>, <<"_count">>}
                        ]}}
                ]}}
        ]}
    ).

make_docs(Db, TotalDocs) when TotalDocs > 0 ->
    make_docs(Db, TotalDocs, 0).

make_docs(Db, TotalDocs, DocsMade) when TotalDocs > DocsMade ->
    DocCount = min(TotalDocs - DocsMade, 500),
    Docs = [doc(I + DocsMade) || I <- lists:seq(1, DocCount)],
    fabric2_db:update_docs(Db, Docs),
    make_docs(Db, TotalDocs, DocsMade + DocCount);
make_docs(_Db, TotalDocs, DocsMade) when TotalDocs =< DocsMade ->
    ok.

doc(Id) ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, list_to_binary(integer_to_list(Id))},
            {<<"val">>, Id}
        ]}
    ).
