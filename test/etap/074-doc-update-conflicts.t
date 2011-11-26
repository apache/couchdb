#!/usr/bin/env escript
%% -*- erlang -*-
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

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

-define(i2l(I), integer_to_list(I)).

test_db_name() -> <<"couch_test_update_conflicts">>.


main(_) ->
    test_util:init_code_path(),

    etap:plan(25),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.


test() ->
    couch_server_sup:start_link(test_util:config_files()),
    couch_config:set("couchdb", "delayed_commits", "true", false),

    lists:foreach(
        fun(NumClients) -> test_concurrent_doc_update(NumClients) end,
        [100, 500, 1000, 2000, 5000]),

    couch_server_sup:stop(),
    ok.


% Verify that if multiple clients try to update the same document
% simultaneously, only one of them will get success response and all
% the other ones will get a conflict error. Also validate that the
% client which got the success response got its document version
% persisted into the database.
test_concurrent_doc_update(NumClients) ->
    {ok, Db} = create_db(test_db_name()),
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"foobar">>},
        {<<"value">>, 0}
    ]}),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    ok = couch_db:close(Db),
    RevStr = couch_doc:rev_to_str(Rev),
    etap:diag("Created first revision of test document"),

    etap:diag("Spawning " ++ ?i2l(NumClients) ++
        " clients to update the document"),
    Clients = lists:map(
        fun(Value) ->
            ClientDoc = couch_doc:from_json_obj({[
                {<<"_id">>, <<"foobar">>},
                {<<"_rev">>, RevStr},
                {<<"value">>, Value}
            ]}),
            Pid = spawn_client(ClientDoc),
            {Value, Pid, erlang:monitor(process, Pid)}
        end,
        lists:seq(1, NumClients)),

    lists:foreach(fun({_, Pid, _}) -> Pid ! go end, Clients),
    etap:diag("Waiting for clients to finish"),

    {NumConflicts, SavedValue} = lists:foldl(
        fun({Value, Pid, MonRef}, {AccConflicts, AccValue}) ->
            receive
            {'DOWN', MonRef, process, Pid, {ok, _NewRev}} ->
                {AccConflicts, Value};
            {'DOWN', MonRef, process, Pid, conflict} ->
                {AccConflicts + 1, AccValue};
            {'DOWN', MonRef, process, Pid, Error} ->
                etap:bail("Client " ++ ?i2l(Value) ++
                    " got update error: " ++ couch_util:to_list(Error))
            after 60000 ->
                etap:bail("Timeout waiting for client " ++ ?i2l(Value) ++ " to die")
            end
        end,
        {0, nil},
        Clients),

    etap:diag("Verifying client results"),
    etap:is(
        NumConflicts,
        NumClients - 1,
        "Got " ++ ?i2l(NumClients - 1) ++ " client conflicts"),

    {ok, Db2} = couch_db:open_int(test_db_name(), []),
    {ok, Leaves} = couch_db:open_doc_revs(Db2, <<"foobar">>, all, []),
    ok = couch_db:close(Db2),
    etap:is(length(Leaves), 1, "Only one document revision was persisted"),
    [{ok, Doc2}] = Leaves,
    {JsonDoc} = couch_doc:to_json_obj(Doc2, []),
    etap:is(
        couch_util:get_value(<<"value">>, JsonDoc),
        SavedValue,
        "Persisted doc has the right value"),

    ok = timer:sleep(1000),
    etap:diag("Restarting the server"),
    couch_server_sup:stop(),
    ok = timer:sleep(1000),
    couch_server_sup:start_link(test_util:config_files()),

    {ok, Db3} = couch_db:open_int(test_db_name(), []),
    {ok, Leaves2} = couch_db:open_doc_revs(Db3, <<"foobar">>, all, []),
    ok = couch_db:close(Db3),
    etap:is(length(Leaves2), 1, "Only one document revision was persisted"),
    [{ok, Doc3}] = Leaves,
    etap:is(Doc3, Doc2, "Got same document after server restart"),

    delete_db(Db3).


spawn_client(Doc) ->
    spawn(fun() ->
        {ok, Db} = couch_db:open_int(test_db_name(), []),
        receive go -> ok end,
        erlang:yield(),
        Result = try
            couch_db:update_doc(Db, Doc, [])
        catch _:Error ->
            Error
        end,
        ok = couch_db:close(Db),
        exit(Result)
    end).


create_db(DbName) ->
    couch_db:create(
        DbName,
        [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}, overwrite]).


delete_db(Db) ->
    ok = couch_server:delete(
        couch_db:name(Db), [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}]).
