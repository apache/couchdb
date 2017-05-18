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

-module(couch_index_lru_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(MAX_INDICES_OPEN, 10).

-record(test_idx, {
    db_name,
    idx_name,
    signature
}).


setup() ->
    test_util:start_couch([]),
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    config:set("couchdb", "max_indices_open", integer_to_list(?MAX_INDICES_OPEN)),
    Db.


teardown(Db) ->
    ok = couch_server:delete(Db#db.name, [?ADMIN_CTX]),
    config:delete("couchdb", "max_indices_open"),
    (catch couch_db:close(Db)),
    ok.


lru_test_() ->
    {
        "Test the view index LRU",
        {
            setup,
            fun() -> test_util:start_couch([]) end, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun test_close_while_compacting/1,
                    fun test_soft_max/1
                ]
            }
        }
    }.


test_close_while_compacting(Db) ->
    ?_test(begin
        ok = meck:new([couch_index_server], [passthrough]),
        Self = self(),
        ok = meck:expect(couch_index_server, set_compacting, fun(Idx, IsCompacting) ->
            meck:passthrough([Idx, IsCompacting]),
            Self ! {compact, IsCompacting, self()},
            receive finish ->
                ok
            end,
            ok
        end),

        ok = meck:expect(couch_index_server, set_committing, fun(Idx, IsCommitting) ->
            meck:passthrough([Idx, IsCommitting]),
            Self ! {commit, IsCommitting, self()},
            ok
        end),

        % create ddocs
        DDocIds = lists:map(fun(I) ->
            BI = integer_to_binary(I),
            <<"_design/ddoc_", BI/binary>>
        end, lists:seq(1,?MAX_INDICES_OPEN+10)),
        ok = create_ddocs(Db, DDocIds),

        % open and compact indexes
        Openers = lists:map(fun(DDocId) ->
            spawn_link(fun() ->
                {ok, Pid, Mon} = couch_index_server:get_index(couch_mrview_index, Db#db.name, DDocId),
                couch_index:compact(Pid),
                receive close ->
                    ok
                end,
                couch_index_server:close(Mon)
            end)
        end, DDocIds),

        % check that all indexes are open
        ToClose = wait_all_compacting(true, [], ?MAX_INDICES_OPEN+10),
        ?assertEqual(?MAX_INDICES_OPEN+10, gen_server:call(couch_index_server, get_open_count)),
        % close compactor pids, but still block flag from being set in BY_SIG table
        lists:foreach(fun(Opener) -> Opener ! close end, Openers),
        % check that compaction flag block pids from closing
        gen_server:cast(couch_index_server, close_indexes),
        ?assertEqual(?MAX_INDICES_OPEN+10, gen_server:call(couch_index_server, get_open_count)),
        % allow compaction flag to be unset
        lists:foreach(fun(CPid) -> CPid ! finish end, ToClose),
        % wait until all compaction flags are unset
        Finished = wait_all_compacting(false, [], ?MAX_INDICES_OPEN+10),
        lists:foreach(fun(CPid) -> CPid ! finish end, Finished),
        gen_server:cast(couch_index_server, close_indexes),
        ?assertEqual(?MAX_INDICES_OPEN+10, gen_server:call(couch_index_server, get_open_count)),
        % wait for all commits to start
        Indexers = wait_all_committing(dict:new(), true, ?MAX_INDICES_OPEN+10),
        ?assertEqual(?MAX_INDICES_OPEN+10, gen_server:call(couch_index_server, get_open_count)),
        % force premature commit
        [Indexer ! commit || Indexer <- Indexers],
        % wait until commits happen
        wait_all_committing(dict:new(), false, ?MAX_INDICES_OPEN+10),
        gen_server:cast(couch_index_server, close_indexes),
        % since all commits and all compacts are done, make sure indexes are closed
        ?assertEqual(?MAX_INDICES_OPEN, gen_server:call(couch_index_server, get_open_count)),
        % clean up
        (catch meck:unload(couch_index_server)),
        ok
    end).


test_soft_max(Db) ->
    ?_test(begin
        ok = meck:new([test_index], [non_strict]),
        ok = meck:expect(test_index, init, fun(Db0, DDoc) ->
            Sig = couch_crypto:hash(md5, term_to_binary({Db0#db.name, DDoc})),
            {ok, #test_idx{db_name=Db0#db.name, idx_name=DDoc, signature=Sig}}
        end),
        ok = meck:expect(test_index, close, ['_'], {true, true}),
        ok = meck:expect(test_index, open, fun(_Db, State) ->
            {ok, State}
        end),
        ok = meck:expect(test_index, compact, ['_', '_', '_'],
            meck:seq([{ok, 9}, {ok, 10}])), %% to trigger recompaction
        ok = meck:expect(test_index, commit, ['_'], ok),
        ok = meck:expect(test_index, get, fun
            (db_name, State) ->
                State#test_idx.db_name;
            (idx_name, State) ->
                State#test_idx.idx_name;
            (signature, State) ->
                State#test_idx.signature;
            (update_seq, Seq) ->
                Seq
        end),

        ok = meck:reset(test_index),

        IdxOpens = lists:map(fun(I) ->
            BI = integer_to_binary(I),
            % hack: use tuple as index name so couch_index_server won't try to open
            % it as a design document.
            IndexName = {<<"_design/i", BI/binary>>},
            ?assertEqual(I-1, gen_server:call(couch_index_server, get_open_count)),
            couch_index_server:get_index(test_index, Db, IndexName)
        end, lists:seq(1, 500)),

        lists:foldl(fun(IdxOpen, Acc) ->
            ?assertMatch({ok, _, _}, IdxOpen),
            {ok, Pid, Mon} = IdxOpen,
            ?assert(is_pid(Pid)),
            ?assert(is_reference(Mon)),
            ?assertNotEqual(undefined, process_info(Pid)),
            gen_server:cast(couch_index_server, close_indexes),
            OpenCount = gen_server:call(couch_index_server, get_open_count),
            ?assertEqual(max(?MAX_INDICES_OPEN, Acc), OpenCount),
            couch_index_server:close(Mon),
            Acc-1
        end, 500, IdxOpens),

        config:delete("couchdb", "max_indices_open"),
        (catch meck:unload(test_index)),
        (catch meck:unload(couch_util)),
        ok
    end).


wait_all_compacting(_IsCompacting, Acc, 0) ->
    Acc;
wait_all_compacting(IsCompacting, Acc, Remaining) ->
    receive {compact, IsCompacting, From} ->
        wait_all_compacting(IsCompacting, [From | Acc], Remaining-1)
    end.


wait_all_committing(Pids, ShouldBe, Count) ->
    receive {commit, IsCommitting, From} ->
        Pids0 = dict:store(From, IsCommitting, Pids),
        CommitCount = dict:fold(fun(_K, V, Acc) ->
            case V of
                ShouldBe -> Acc+1;
                _ -> Acc
            end
        end, 0, Pids0),
        case Count =:= CommitCount of
            true ->
                [Pid || {Pid, _} <- dict:to_list(Pids0)];
            false ->
                wait_all_committing(Pids0, ShouldBe, Count)
        end
    end.


create_ddocs(Db, DDocIds) ->
    Docs = lists:map(fun(DDocId) ->
        MapFun = <<"function(doc) {emit(\"", DDocId/binary, "\", 1);}">>,
        Json = {[
            {<<"_id">>, DDocId},
            {<<"language">>, <<"javascript">>},
            {<<"views">>, {[
                {<<"v">>, {[
                    {<<"map">>, MapFun}
                ]}}
            ]}}
        ]},
        couch_doc:from_json_obj(Json)
    end, DDocIds),
    {ok, _} = couch_db:update_docs(Db, Docs, [?ADMIN_CTX]),
    ok.
