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

-module(cpse_test_purge_bad_checkpoints).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup_each() ->
    {ok, Db1} = cpse_util:create_db(),
    {ok, Revs} = cpse_util:save_docs(couch_db:name(Db1), [
        {[{'_id', foo0}, {vsn, 0}]},
        {[{'_id', foo1}, {vsn, 1}]},
        {[{'_id', foo2}, {vsn, 2}]},
        {[{'_id', foo3}, {vsn, 3}]},
        {[{'_id', foo4}, {vsn, 4}]},
        {[{'_id', foo5}, {vsn, 5}]},
        {[{'_id', foo6}, {vsn, 6}]},
        {[{'_id', foo7}, {vsn, 7}]},
        {[{'_id', foo8}, {vsn, 8}]},
        {[{'_id', foo9}, {vsn, 9}]}
    ]),
    PInfos = lists:map(
        fun(Idx) ->
            DocId = iolist_to_binary(["foo", $0 + Idx]),
            Rev = lists:nth(Idx + 1, Revs),
            {cpse_util:uuid(), DocId, [Rev]}
        end,
        lists:seq(0, 9)
    ),
    {ok, _} = cpse_util:purge(couch_db:name(Db1), PInfos),
    {ok, Db2} = couch_db:reopen(Db1),
    Db2.

teardown_each(Db) ->
    ok = couch_server:delete(couch_db:name(Db), []).

cpse_bad_purge_seq(Db1) ->
    Db2 = save_local_doc(Db1, <<"foo">>),
    ?assertEqual(0, couch_db:get_minimum_purge_seq(Db2)),

    ok = couch_db:set_purge_infos_limit(Db2, 5),
    {ok, Db3} = couch_db:reopen(Db2),
    ?assertEqual(1, couch_db:get_minimum_purge_seq(Db3)).

cpse_verify_non_boolean(Db1) ->
    Db2 = save_local_doc(Db1, 2),
    ?assertEqual(0, couch_db:get_minimum_purge_seq(Db2)),

    ok = couch_db:set_purge_infos_limit(Db2, 5),
    {ok, Db3} = couch_db:reopen(Db2),
    ?assertEqual(5, couch_db:get_minimum_purge_seq(Db3)).

save_local_doc(Db1, PurgeSeq) ->
    {Mega, Secs, _} = os:timestamp(),
    NowSecs = Mega * 1000000 + Secs,
    Doc = couch_doc:from_json_obj(
        ?JSON_DECODE(
            ?JSON_ENCODE(
                {[
                    {<<"_id">>, <<"_local/purge-test-stuff">>},
                    {<<"purge_seq">>, PurgeSeq},
                    {<<"timestamp_utc">>, NowSecs},
                    {<<"verify_options">>, {[{<<"signature">>, <<"stuff">>}]}},
                    {<<"type">>, <<"test">>}
                ]}
            )
        )
    ),
    {ok, _} = couch_db:update_doc(Db1, Doc, []),
    {ok, Db2} = couch_db:reopen(Db1),
    Db2.
