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

-module(couch_index_compaction_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    couch_db:close(Db),
    {ok, IndexerPid} = fake_index(Db),
    ?assertNot(is_opened(Db)),
    {Db, IndexerPid}.

fake_index(Db) ->
    DbName = couch_db:name(Db),
    ok = meck:new([test_index], [non_strict]),
    ok = meck:expect(test_index, init, ['_', '_'], {ok, 10}),
    ok = meck:expect(test_index, open, fun(_Db, State) ->
        {ok, State}
    end),
    ok = meck:expect(test_index, compact, ['_', '_', '_'],
        meck:seq([{ok, 9}, {ok, 10}])), %% to trigger recompaction
    ok = meck:expect(test_index, commit, ['_'], ok),
    ok = meck:expect(test_index, get, fun
        (db_name, _) ->
            DbName;
        (idx_name, _) ->
            <<"idx_name">>;
        (signature, _) ->
            <<61,237,157,230,136,93,96,201,204,17,137,186,50,249,44,135>>;
        (update_seq, Seq) ->
            Seq
    end),

    couch_index_server:get_index(test_index, Db, undefined).

teardown(_) ->
    (catch meck:unload(test_index)),
    (catch meck:unload(couch_util)),
    ok.

compaction_test_() ->
    {
        "Check compaction",
        {
            setup,
            fun() -> test_util:start_couch([]) end, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun hold_db_for_recompaction/1
                ]
            }
        }
    }.


hold_db_for_recompaction({Db, Idx}) ->
    ?_test(begin
        ?assertNot(is_opened(Db)),
        ok = meck:reset(test_index),
        {ok, Monitor} = couch_index:compact(Idx, [monitor]),

        %% we expect Mod:commit/1 to be called twice
        %% once for compact and once for recompact
        meck:wait(2, test_index, commit, ['_'], 5000),
        ?assertEqual(1, meck:num_calls(test_index, compact, ['_', '_', []])),
        ?assertEqual(1, meck:num_calls(test_index, compact, ['_', '_', [recompact]])),

        %% wait compaction finish
        receive
            {'DOWN', Monitor, _, _, _} -> ok
        after 5000 ->
            throw(timeout)
        end,

        ?assertNot(is_opened(Db)),
        ok
    end).

is_opened(Db) ->
    Monitors = [M || M <- couch_db:monitored_by(Db), M =/= self()],
    Monitors /= [].
