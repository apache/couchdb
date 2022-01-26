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

-module(fabric_db_create_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-define(TDEF(A), {atom_to_list(A), fun A/0}).

main_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF(t_handle_shard_doc_conflict)
        ]
    }.

setup() ->
    test_util:start_couch([fabric]).

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

t_handle_shard_doc_conflict() ->
    DbName = ?tempdb(),
    meck:new(mem3, [passthrough]),
    meck:new(fabric_util, [passthrough]),
    ok = meck:sequence(mem3, shards, 1, [
        fun(_) -> meck:raise(error, database_does_not_exist) end,
        [#shard{dbname = DbName}]
    ]),
    meck:expect(fabric_util, recv, 4, {error, conflict}),
    ?assertEqual({error, file_exists}, fabric_db_create:go(DbName, [])),

    meck:unload(),
    ok = fabric:delete_db(DbName).
