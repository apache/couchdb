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

-module(couch_mrview_design_docs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).

setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), design),
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok.

design_docs_test_() ->
    {
        "_design_docs view tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun should_query/1,
                    fun should_query_with_range/1,
                    fun should_query_with_range_rev/1,
                    fun should_query_with_limit_and_skip/1,
                    fun should_query_with_include_docs/1
                ]
            }
        }
    }.

should_query(Db) ->
    Result = run_query(Db, []),
    Expect =
        {ok, [
            {meta, [{total, 10}, {offset, 10}]},
            mk_row(<<"_design/bar01">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar02">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar03">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar04">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar05">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar06">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar07">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar08">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar09">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar10">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>)
        ]},
    ?_assertEqual(Expect, Result).

should_query_with_range(Db) ->
    Result = run_query(Db, [
        {start_key, <<"_design/bar03">>},
        {end_key, <<"_design/bar05">>}
    ]),
    Expect =
        {ok, [
            {meta, [{total, 10}, {offset, 12}]},
            mk_row(<<"_design/bar03">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar04">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar05">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>)
        ]},
    ?_assertEqual(Expect, Result).

should_query_with_range_rev(Db) ->
    Result = run_query(Db, [
        {direction, rev},
        {start_key, <<"_design/bar05">>},
        {end_key, <<"_design/bar03">>},
        {inclusive_end, true}
    ]),
    Expect =
        {ok, [
            {meta, [{total, 10}, {offset, 5}]},
            mk_row(<<"_design/bar05">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar04">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar03">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>)
        ]},
    ?_assertEqual(Expect, Result).

should_query_with_limit_and_skip(Db) ->
    Result = run_query(Db, [
        {start_key, <<"_design/bar02">>},
        {limit, 3},
        {skip, 3}
    ]),
    Expect =
        {ok, [
            {meta, [{total, 10}, {offset, 14}]},
            mk_row(<<"_design/bar05">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar06">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>),
            mk_row(<<"_design/bar07">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>)
        ]},
    ?_assertEqual(Expect, Result).

should_query_with_include_docs(Db) ->
    Result = run_query(Db, [
        {start_key, <<"_design/bar08">>},
        {end_key, <<"_design/bar08">>},
        {include_docs, true}
    ]),
    Doc =
        {[
            {<<"_id">>, <<"_design/bar08">>},
            {<<"_rev">>, <<"1-0b24e44a44af45e51e562fd124ce3007">>},
            {<<"views">>, {[]}}
        ]},
    Val = {[{rev, <<"1-0b24e44a44af45e51e562fd124ce3007">>}]},
    Expect =
        {ok, [
            {meta, [{total, 10}, {offset, 17}]},
            {row, [
                {id, <<"_design/bar08">>},
                {key, <<"_design/bar08">>},
                {value, Val},
                {doc, Doc}
            ]}
        ]},
    ?_assertEqual(Expect, Result).

mk_row(Id, Rev) ->
    {row, [{id, Id}, {key, Id}, {value, {[{rev, Rev}]}}]}.

run_query(Db, Opts0) ->
    Opts = [{extra, [{namespace, <<"_design">>}]} | Opts0],
    couch_mrview:query_all_docs(Db, Opts).
