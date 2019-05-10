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

-module(couch_mrview_ddoc_updated_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).


setup() ->
    Name = ?tempdb(),
    couch_server:delete(Name, [?ADMIN_CTX]),
    {ok, Db} = couch_db:create(Name, [?ADMIN_CTX]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"baz">>, {[
                {<<"map">>, <<
                    "function(doc) {\n"
                    "    emit(doc.val, doc.val);\n"
                    "}"
                >>}
            ]}}
        ]}}
    ]}),
    [Doc1 | Docs999] = couch_mrview_test_util:make_docs(map, 100),
    {ok, _} = couch_db:update_docs(Db, [DDoc, Doc1], []),
    {ok, Db2} = couch_db:reopen(Db),

    % run a query with 1 doc to initialize couch_index process
    CB = fun
        ({row, _}, Count) -> {ok, Count+1};
        (_, Count) -> {ok, Count}
    end,
    {ok, _} =
        couch_mrview:query_view(Db2, <<"_design/bar">>, <<"baz">>, [], CB, 0),

    meck:new(couch_index_updater, [passthrough]),
    meck:expect(couch_index_updater, update, fun(Idx, Mod, IdxSt) ->
        timer:sleep(5000),
        meck:passthrough([Idx, Mod, IdxSt])
    end),

    % add more docs
    {ok, _} = couch_db:update_docs(Db2, Docs999, []),
    {ok, Db3} = couch_db:reopen(Db2),
    Db3.

teardown(Db) ->
    meck:unload(couch_index_updater),
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok.


ddoc_update_test_() ->
    {
        "Check ddoc update actions",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun check_indexing_stops_on_ddoc_change/1
                ]
            }
        }
    }.


check_indexing_stops_on_ddoc_change(Db) ->
    ?_test(begin
        DDocID = <<"_design/bar">>,

        IndexesBefore = get_indexes_by_ddoc(DDocID, 1),
        ?assertEqual(1, length(IndexesBefore)),
        AliveBefore = lists:filter(fun erlang:is_process_alive/1, IndexesBefore),
        ?assertEqual(1, length(AliveBefore)),

        {ok, DDoc} = couch_db:open_doc(Db, DDocID, [ejson_body, ?ADMIN_CTX]),
        DDocJson2 = couch_doc:from_json_obj({[
            {<<"_id">>, DDocID},
            {<<"_deleted">>, true},
            {<<"_rev">>, couch_doc:rev_to_str(DDoc#doc.revs)}
        ]}),

        % spawn a process for query
        Self = self(),
        QPid = spawn(fun() ->
            {ok, Result} = couch_mrview:query_view(
                Db, <<"_design/bar">>, <<"baz">>, []),
            Self ! {self(), Result}
        end),

        % while indexing for the query is in progress, delete DDoc
        {ok, _} = couch_db:update_doc(Db, DDocJson2, []),
        receive
            {QPid, Msg} ->
                ?assertEqual(Msg, ddoc_updated)
        after ?TIMEOUT ->
            erlang:error(
                {assertion_failed, [{module, ?MODULE}, {line, ?LINE},
                {reason, "test failed"}]})
        end,

        %% assert that previously running indexes are gone
        IndexesAfter = get_indexes_by_ddoc(DDocID, 0),
        ?assertEqual(0, length(IndexesAfter)),
        AliveAfter = lists:filter(fun erlang:is_process_alive/1, IndexesBefore),
        ?assertEqual(0, length(AliveAfter))
    end).


get_indexes_by_ddoc(DDocID, N) ->
    Indexes = test_util:wait(fun() ->
        Indxs = ets:match_object(
            couchdb_indexes_by_db, {'$1', {DDocID, '$2'}}),
        case length(Indxs) == N of
            true ->
                Indxs;
            false ->
                wait
        end
    end),
    lists:foldl(fun({DbName, {_DDocID, Sig}}, Acc) ->
        case ets:lookup(couchdb_indexes_by_sig, {DbName, Sig}) of
            [{_, Pid}] -> [Pid|Acc];
            _ -> Acc
        end
    end, [], Indexes).


