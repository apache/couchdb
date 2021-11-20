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

-module(fabric_db_info_tests).

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
            ?TDEF(t_update_seq_has_uuids)
        ]
    }.

setup() ->
    test_util:start_couch([fabric]).

teardown(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).

t_update_seq_has_uuids() ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 1}, {n, 1}]),

    {ok, Info} = fabric:get_db_info(DbName),
    UpdateSeq = couch_util:get_value(update_seq, Info),
    UnpackedSeq = fabric_view_changes:decode_seq(UpdateSeq),

    ?assertMatch([{_, _, _}], UnpackedSeq),
    [{Node, Range, Seq}] = UnpackedSeq,
    ?assert(is_atom(Node)),
    ?assertMatch([_, _], Range),
    ?assertMatch({_, _, _}, Seq),
    {SeqNum, SeqUuid, EpochNode} = Seq,
    ?assert(is_integer(SeqNum)),
    ?assert(is_binary(SeqUuid)),
    ?assert(is_atom(EpochNode)),

    {ok, UuidMap} = fabric:db_uuids(DbName),
    PrefixLen = fabric_util:get_uuid_prefix_len(),
    Uuids = [binary:part(Uuid, {0, PrefixLen}) || Uuid <- maps:keys(UuidMap)],
    [UuidFromShard] = Uuids,
    ?assertEqual(UuidFromShard, SeqUuid),

    ok = fabric:delete_db(DbName, []).
