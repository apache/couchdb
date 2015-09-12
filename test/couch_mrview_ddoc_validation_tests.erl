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

-module(couch_mrview_ddoc_validation_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), map),
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(Db#db.name, [?ADMIN_CTX]),
    ok.

ddoc_validation_test_() ->
    {
        "ddoc validation tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_reject_invalid_js_map/1,
                    fun should_reject_invalid_js_reduce/1,
                    fun should_reject_invalid_builtin_reduce/1
                ]
            }
        }
    }.

should_reject_invalid_js_map(Db) ->
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/should_reject_invalid_js_map">>},
        {<<"views">>, {[
            {<<"foo">>, {[
                {<<"map">>, <<"function(doc) }{">>}
            ]}}
        ]}}
    ]}),
    ?_assertThrow(
        {bad_request, compilation_error, _},
        couch_db:update_doc(Db, Doc, [])).

should_reject_invalid_js_reduce(Db) ->
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/should_reject_invalid_js_reduce">>},
        {<<"views">>, {[
            {<<"foo">>, {[
                {<<"map">>, <<"function(doc) { emit(null); }">>},
                {<<"reduce">>, <<"function(k, v, r) }{}">>}
            ]}}
        ]}}
    ]}),
    ?_assertThrow(
        {bad_request, compilation_error, _},
        couch_db:update_doc(Db, Doc, [])).

should_reject_invalid_builtin_reduce(Db) ->
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/should_reject_invalid_builtin_reduce">>},
        {<<"views">>, {[
            {<<"foo">>, {[
                {<<"map">>, <<"function(doc) { emit(null); }">>},
                {<<"reduce">>, <<"_foobar">>}
            ]}}
        ]}}
    ]}),
    ?_assertThrow(
        {bad_request, invalid_design_doc, _},
        couch_db:update_doc(Db, Doc, [])).
