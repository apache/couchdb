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

-module(fabric_db_uuids_tests).

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
            ?TDEF(t_can_get_shard_uuids)
        ]
    }.

setup() ->
    test_util:start_couch([fabric]).

teardown(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).

t_can_get_shard_uuids() ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, []),
    Shards = mem3:shards(DbName),
    {ok, Uuids} = fabric:db_uuids(DbName),
    ?assertEqual(length(Shards), map_size(Uuids)),
    UuidsFromShards = lists:foldl(
        fun(#shard{} = Shard, Acc) ->
            Uuid = couch_util:with_db(Shard#shard.name, fun(Db) ->
                couch_db:get_uuid(Db)
            end),
            Acc#{Uuid => Shard}
        end,
        #{},
        Shards
    ),
    ?assertEqual(UuidsFromShards, Uuids),
    ok = fabric:delete_db(DbName, []).
