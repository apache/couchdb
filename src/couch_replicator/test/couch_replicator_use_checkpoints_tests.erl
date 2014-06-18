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

-module(couch_replicator_use_checkpoints_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_ROLE, #user_ctx{roles=[<<"_admin">>]}).
-define(ADMIN_USER, {user_ctx, ?ADMIN_ROLE}).
-define(DOCS_COUNT, 100).
-define(TIMEOUT_STOP, 1000).
-define(TIMEOUT_EUNIT, 30).
-define(i2l(I), integer_to_list(I)).
-define(io2b(Io), iolist_to_binary(Io)).


start(false) ->
    fun
        ({finished, _, {CheckpointHistory}}) ->
            ?assertEqual([{<<"use_checkpoints">>,false}], CheckpointHistory);
        (_) ->
            ok
    end;
start(true) ->
    fun
        ({finished, _, {CheckpointHistory}}) ->
            ?assertNotEqual(false, lists:keyfind(<<"session_id">>,
                                                 1, CheckpointHistory));
        (_) ->
            ok
    end.

stop(_, _) ->
    ok.

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    ok = couch_db:close(Db),
    DbName.

setup(local) ->
    setup();
setup(remote) ->
    {remote, setup()};
setup({_, Fun, {A, B}}) ->
    {ok, _} = couch_server_sup:start_link(?CONFIG_CHAIN),
    {ok, Listener} = couch_replicator_notifier:start_link(Fun),
    Source = setup(A),
    Target = setup(B),
    {Source, Target, Listener}.

teardown({remote, DbName}) ->
    teardown(DbName);
teardown(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_USER]),
    ok.

teardown(_, {Source, Target, Listener}) ->
    teardown(Source),
    teardown(Target),

    couch_replicator_notifier:stop(Listener),
    Pid = whereis(couch_server_sup),
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT_STOP ->
        throw({timeout, server_stop})
    end.


use_checkpoints_test_() ->
    {
        "Replication use_checkpoints feature tests",
        {
            foreachx,
            fun start/1, fun stop/2,
            [{UseCheckpoints, fun use_checkpoints_tests/2}
             || UseCheckpoints <- [false, true]]
        }
    }.

use_checkpoints_tests(UseCheckpoints, Fun) ->
    Pairs = [{local, local}, {local, remote},
             {remote, local}, {remote, remote}],
    {
        "use_checkpoints: " ++ atom_to_list(UseCheckpoints),
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{{UseCheckpoints, Fun, Pair}, fun should_test_checkpoints/2}
             || Pair <- Pairs]
        }
    }.

should_test_checkpoints({UseCheckpoints, _, {From, To}}, {Source, Target, _}) ->
    should_test_checkpoints(UseCheckpoints, {From, To}, {Source, Target}).
should_test_checkpoints(UseCheckpoints, {From, To}, {Source, Target}) ->
    {lists:flatten(io_lib:format("~p -> ~p", [From, To])),
     {inorder, [
        should_populate_source(Source, ?DOCS_COUNT),
        should_replicate(Source, Target, UseCheckpoints),
        should_compare_databases(Source, Target)
     ]}}.

should_populate_source({remote, Source}, DocCount) ->
    should_populate_source(Source, DocCount);
should_populate_source(Source, DocCount) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(populate_db(Source, DocCount))}.

should_replicate({remote, Source}, Target, UseCheckpoints) ->
    should_replicate(db_url(Source), Target, UseCheckpoints);
should_replicate(Source, {remote, Target}, UseCheckpoints) ->
    should_replicate(Source, db_url(Target), UseCheckpoints);
should_replicate(Source, Target, UseCheckpoints) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(replicate(Source, Target, UseCheckpoints))}.

should_compare_databases({remote, Source}, Target) ->
    should_compare_databases(Source, Target);
should_compare_databases(Source, {remote, Target}) ->
    should_compare_databases(Source, Target);
should_compare_databases(Source, Target) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(compare_dbs(Source, Target))}.


populate_db(DbName, DocCount) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Id = ?io2b(["doc", ?i2l(DocIdCounter)]),
            Value = ?io2b(["val", ?i2l(DocIdCounter)]),
            Doc = #doc{
                id = Id,
                body = {[ {<<"value">>, Value} ]}
            },
            [Doc | Acc]
        end,
        [], lists:seq(1, DocCount)),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    ok = couch_db:close(Db).

compare_dbs(Source, Target) ->
    {ok, SourceDb} = couch_db:open_int(Source, []),
    {ok, TargetDb} = couch_db:open_int(Target, []),
    Fun = fun(FullDocInfo, _, Acc) ->
        {ok, Doc} = couch_db:open_doc(SourceDb, FullDocInfo),
        {Props} = DocJson = couch_doc:to_json_obj(Doc, [attachments]),
        DocId = couch_util:get_value(<<"_id">>, Props),
        DocTarget = case couch_db:open_doc(TargetDb, DocId) of
            {ok, DocT} ->
                DocT;
            Error ->
                erlang:error(
                    {assertion_failed,
                     [{module, ?MODULE}, {line, ?LINE},
                      {reason, lists:concat(["Error opening document '",
                                             ?b2l(DocId), "' from target: ",
                                             couch_util:to_list(Error)])}]})
            end,
        DocTargetJson = couch_doc:to_json_obj(DocTarget, [attachments]),
        ?assertEqual(DocJson, DocTargetJson),
        {ok, Acc}
    end,
    {ok, _, _} = couch_db:enum_docs(SourceDb, Fun, [], []),
    ok = couch_db:close(SourceDb),
    ok = couch_db:close(TargetDb).

db_url(DbName) ->
    iolist_to_binary([
        "http://", couch_config:get("httpd", "bind_address", "127.0.0.1"),
        ":", integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
        "/", DbName
    ]).

replicate(Source, Target, UseCheckpoints) ->
    RepObject = {[
        {<<"source">>, Source},
        {<<"target">>, Target},
        {<<"use_checkpoints">>, UseCheckpoints}
    ]},
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(RepObject, ?ADMIN_ROLE),
    {ok, Pid} = couch_replicator:async_replicate(Rep),
    MonRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, Pid, _} ->
            ok
    end.
