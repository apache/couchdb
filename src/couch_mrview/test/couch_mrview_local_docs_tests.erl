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

-module(couch_mrview_local_docs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).



setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), local),
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok.


all_docs_test_() ->
    {
        "_local_docs view tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
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
    Expect = {ok, [
        {meta, [{total, 10}, {offset, 0}]},
        mk_row(1),
        mk_row(10),
        mk_row(2),
        mk_row(3),
        mk_row(4),
        mk_row(5),
        mk_row(6),
        mk_row(7),
        mk_row(8),
        mk_row(9)
    ]},
    ?_assertEqual(Expect, Result).

should_query_with_range(Db) ->
    Result = run_query(Db, [
        {start_key, <<"_local/3">>},
        {end_key, <<"_local/5">>}
    ]),
    Expect = {ok, [
        {meta, [{total, 10}, {offset, 3}]},
        mk_row(3),
        mk_row(4),
        mk_row(5)
    ]},
    ?_assertEqual(Expect, Result).

should_query_with_range_rev(Db) ->
    Result = run_query(Db, [
        {direction, rev},
        {start_key, <<"_local/5">>}, {end_key, <<"_local/3">>},
        {inclusive_end, true}
    ]),
    Expect = {ok, [
        {meta, [{total, 10}, {offset, 4}]},
        mk_row(5),
        mk_row(4),
        mk_row(3)
    ]},
    ?_assertEqual(Expect, Result).

should_query_with_limit_and_skip(Db) ->
    Result = run_query(Db, [
        {start_key, <<"_local/2">>},
        {limit, 3},
        {skip, 3}
    ]),
    Expect = {ok, [
        {meta, [{total, 10}, {offset, 5}]},
        mk_row(5),
        mk_row(6),
        mk_row(7)
    ]},
    ?_assertEqual(Expect, Result).

should_query_with_include_docs(Db) ->
    Result = run_query(Db, [
        {start_key, <<"_local/8">>},
        {end_key, <<"_local/8">>},
        {include_docs, true}
    ]),
    {row, Doc0} = mk_row(8),
    Doc = Doc0 ++ [{doc, {[
        {<<"_id">>, <<"_local/8">>},
        {<<"_rev">>, <<"0-1">>},
        {<<"val">>, 8}
    ]}}],
    Expect = {ok, [
        {meta, [{total, 10}, {offset, 8}]},
        {row, Doc}
    ]},
    ?_assertEqual(Expect, Result).


mk_row(IntId) ->
    Id = list_to_binary(io_lib:format("_local/~b", [IntId])),
    {row, [{id, Id}, {key, Id}, {value, {[{rev, <<"0-1">>}]}}]}.

run_query(Db, Opts0) ->
    Opts = [{extra, [{namespace, <<"_local">>}]} | Opts0],
    couch_mrview:query_all_docs(Db, Opts).
