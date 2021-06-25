% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_eval_error_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(LANG_BINARY, <<"foo_lang">>).
-define(LANG_STRING, binary_to_list(?LANG_BINARY)).

setup() ->
    meck:new(mock_language_server, [non_strict]),
    Ctx = test_util:start_couch(),
    config:set(
        "couch_eval.languages",
        ?LANG_STRING,
        atom_to_list(mock_language_server)
    ),
    Ctx.

teardown(Ctx) ->
    test_util:stop_couch(Ctx),
    meck:unload().

error_test_() ->
    {
        "Error tests",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                fun acquire_map_context_error_handled/0
            ]
        }
    }.

acquire_map_context_error_handled() ->
    meck:expect(mock_language_server, acquire_map_context, fun(_) ->
        {error, foo_error}
    end),
    Result = couch_eval:acquire_map_context(
        <<"foo">>,
        <<"bar">>,
        ?LANG_BINARY,
        <<"baz">>,
        <<"quux">>,
        [<<"quuz">>]
    ),
    ?assertEqual({error, foo_error}, Result).
