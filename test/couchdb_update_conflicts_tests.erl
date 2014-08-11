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

-module(couchdb_update_conflicts_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(i2l(I), integer_to_list(I)).
-define(ADMIN_USER, {userctx, #user_ctx{roles=[<<"_admin">>]}}).
-define(DOC_ID, <<"foobar">>).
-define(NUM_CLIENTS, [100, 500, 1000, 2000, 5000, 10000]).
-define(TIMEOUT, 10000).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    couch_config:set("couchdb", "delayed_commits", "true", false),
    Pid.

stop(Pid) ->
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER, overwrite]),
    Doc = couch_doc:from_json_obj({[{<<"_id">>, ?DOC_ID},
                                    {<<"value">>, 0}]}),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    ok = couch_db:close(Db),
    RevStr = couch_doc:rev_to_str(Rev),
    {DbName, RevStr}.
setup(_) ->
    setup().

teardown({DbName, _}) ->
    ok = couch_server:delete(DbName, []),
    ok.
teardown(_, {DbName, _RevStr}) ->
    teardown({DbName, _RevStr}).


view_indexes_cleanup_test_() ->
    {
        "Update conflicts",
        {
            setup,
            fun start/0, fun stop/1,
            [
                concurrent_updates(),
                couchdb_188()
            ]
        }
    }.

concurrent_updates()->
    {
        "Concurrent updates",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{NumClients, fun should_concurrently_update_doc/2}
             || NumClients <- ?NUM_CLIENTS]
        }
    }.

couchdb_188()->
    {
        "COUCHDB-188",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [fun should_bulk_create_delete_doc/1]
        }
    }.


should_concurrently_update_doc(NumClients, {DbName, InitRev})->
     {?i2l(NumClients) ++ " clients",
      {inorder,
       [{"update doc",
         {timeout, ?TIMEOUT div 1000,
          ?_test(concurrent_doc_update(NumClients, DbName, InitRev))}},
        {"ensure in single leaf",
         ?_test(ensure_in_single_revision_leaf(DbName))}]}}.

should_bulk_create_delete_doc({DbName, InitRev})->
    ?_test(bulk_delete_create(DbName, InitRev)).


concurrent_doc_update(NumClients, DbName, InitRev) ->
    Clients = lists:map(
        fun(Value) ->
            ClientDoc = couch_doc:from_json_obj({[
                {<<"_id">>, ?DOC_ID},
                {<<"_rev">>, InitRev},
                {<<"value">>, Value}
            ]}),
            Pid = spawn_client(DbName, ClientDoc),
            {Value, Pid, erlang:monitor(process, Pid)}
        end,
        lists:seq(1, NumClients)),

    lists:foreach(fun({_, Pid, _}) -> Pid ! go end, Clients),

    {NumConflicts, SavedValue} = lists:foldl(
        fun({Value, Pid, MonRef}, {AccConflicts, AccValue}) ->
            receive
                {'DOWN', MonRef, process, Pid, {ok, _NewRev}} ->
                    {AccConflicts, Value};
                {'DOWN', MonRef, process, Pid, conflict} ->
                    {AccConflicts + 1, AccValue};
                {'DOWN', MonRef, process, Pid, Error} ->
                    erlang:error({assertion_failed,
                         [{module, ?MODULE},
                          {line, ?LINE},
                          {reason, "Client " ++ ?i2l(Value)
                                             ++ " got update error: "
                                             ++ couch_util:to_list(Error)}]})
            after ?TIMEOUT div 2 ->
                 erlang:error({assertion_failed,
                         [{module, ?MODULE},
                          {line, ?LINE},
                          {reason, "Timeout waiting for client "
                                   ++ ?i2l(Value) ++ " to die"}]})
            end
        end, {0, nil}, Clients),
    ?assertEqual(NumClients - 1, NumConflicts),

    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Leaves} = couch_db:open_doc_revs(Db, ?DOC_ID, all, []),
    ok = couch_db:close(Db),
    ?assertEqual(1, length(Leaves)),

    [{ok, Doc2}] = Leaves,
    {JsonDoc} = couch_doc:to_json_obj(Doc2, []),
    ?assertEqual(SavedValue, couch_util:get_value(<<"value">>, JsonDoc)).

ensure_in_single_revision_leaf(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Leaves} = couch_db:open_doc_revs(Db, ?DOC_ID, all, []),
    ok = couch_db:close(Db),
    [{ok, Doc}] = Leaves,

    %% FIXME: server restart won't work from test side
    %% stop(ok),
    %% start(),

    {ok, Db2} = couch_db:open_int(DbName, []),
    {ok, Leaves2} = couch_db:open_doc_revs(Db2, ?DOC_ID, all, []),
    ok = couch_db:close(Db2),
    ?assertEqual(1, length(Leaves2)),

    [{ok, Doc2}] = Leaves,
    ?assertEqual(Doc, Doc2).
    
bulk_delete_create(DbName, InitRev) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    
    DeletedDoc = couch_doc:from_json_obj({[
        {<<"_id">>, ?DOC_ID},
        {<<"_rev">>, InitRev},
        {<<"_deleted">>, true}
    ]}),
    NewDoc = couch_doc:from_json_obj({[
        {<<"_id">>, ?DOC_ID},
        {<<"value">>, 666}
    ]}),

    {ok, Results} = couch_db:update_docs(Db, [DeletedDoc, NewDoc], []),
    ok = couch_db:close(Db),

    ?assertEqual(2, length([ok || {ok, _} <- Results])),
    [{ok, Rev1}, {ok, Rev2}] = Results,
    
    {ok, Db2} = couch_db:open_int(DbName, []),
    {ok, [{ok, Doc1}]} = couch_db:open_doc_revs(
        Db2, ?DOC_ID, [Rev1], [conflicts, deleted_conflicts]),
    {ok, [{ok, Doc2}]} = couch_db:open_doc_revs(
        Db2, ?DOC_ID, [Rev2], [conflicts, deleted_conflicts]),
    ok = couch_db:close(Db2),

    {Doc1Props} = couch_doc:to_json_obj(Doc1, []),
    {Doc2Props} = couch_doc:to_json_obj(Doc2, []),

    %% Document was deleted
    ?assert(couch_util:get_value(<<"_deleted">>, Doc1Props)),
    %% New document not flagged as deleted
    ?assertEqual(undefined, couch_util:get_value(<<"_deleted">>,
                                                 Doc2Props)),
    %% New leaf revision has the right value
    ?assertEqual(666, couch_util:get_value(<<"value">>,
                                           Doc2Props)),
    %% Deleted document has no conflicts
    ?assertEqual(undefined, couch_util:get_value(<<"_conflicts">>,
                                                 Doc1Props)),
    %% Deleted document has no deleted conflicts
    ?assertEqual(undefined, couch_util:get_value(<<"_deleted_conflicts">>,
                                                 Doc1Props)),
    %% New leaf revision doesn't have conflicts
    ?assertEqual(undefined, couch_util:get_value(<<"_conflicts">>,
                                                 Doc1Props)),
    %% New leaf revision doesn't have deleted conflicts
    ?assertEqual(undefined, couch_util:get_value(<<"_deleted_conflicts">>,
                                                 Doc1Props)),

    %% Deleted revision has position 2
    ?assertEqual(2, element(1, Rev1)),
    %% New leaf revision has position 1
    ?assertEqual(1, element(1, Rev2)).


spawn_client(DbName, Doc) ->
    spawn(fun() ->
        {ok, Db} = couch_db:open_int(DbName, []),
        receive
            go -> ok
        end,
        erlang:yield(),
        Result = try
            couch_db:update_doc(Db, Doc, [])
        catch _:Error ->
            Error
        end,
        ok = couch_db:close(Db),
        exit(Result)
    end).
