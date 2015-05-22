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

-module(couch_replicator_filtered_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").

-define(DDOC, {[
    {<<"_id">>, <<"_design/filter_ddoc">>},
    {<<"filters">>, {[
        {<<"testfilter">>, <<"
            function(doc, req){if (doc.class == 'mammal') return true;}
        ">>}
    ]}}
]}).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    upload_ddoc(DbName),
    create_docs(DbName),
    DbName.

setup(local) ->
    setup();
setup(remote) ->
    {remote, setup()};
setup({A, B}) ->
    Ctx = test_util:start_couch([couch_replicator]),
    Source = setup(A),
    Target = setup(B),
    {Ctx, {Source, Target}}.

teardown({remote, DbName}) ->
    teardown(DbName);
teardown(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.

teardown(_, {Ctx, {Source, Target}}) ->
    teardown(Source),
    teardown(Target),
    ok = application:stop(couch_replicator),
    ok = test_util:stop_couch(Ctx).

filtered_replication_test_() ->
    Pairs = [{local, local}, {local, remote},
             {remote, local}, {remote, remote}],
    {
        "Filtered replication tests",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Pair, fun should_succeed/2}
             || Pair <- Pairs]
        }
    }.


should_succeed({From, To}, {_Ctx, {Source, Target}}) ->
    {ok, RepPid, RepId} = replicate(Source, Target),
    %% FilteredFun is an Erlang version of following JS function
    %% function(doc, req){if (doc.class == 'mammal') return true;}
    FilterFun = fun(_DocId, {Props}) ->
        couch_util:get_value(<<"class">>, Props) == <<"mammal">>
    end,
    {lists:flatten(io_lib:format("~p -> ~p", [From, To])),
        {inorder, [
            should_ensure_replication_runs(RepPid),
            should_compare_databases(Source, Target, FilterFun),
            should_cancel_replication(RepId, RepPid)
    ]}}.

should_ensure_replication_runs(RepPid) ->
    ?_assert(begin
        ?assertMatch(ok, wait_for_replicator(RepPid)),
        is_process_alive(RepPid)
    end).

should_compare_databases({remote, Source}, Target, FilterFun) ->
    should_compare_databases(Source, Target, FilterFun);
should_compare_databases(Source, {remote, Target}, FilterFun) ->
    should_compare_databases(Source, Target, FilterFun);
should_compare_databases(Source, Target, FilterFun) ->
    ?_assertEqual(1, begin
        compare_dbs(Source, Target, FilterFun)
    end).

should_cancel_replication(RepId, RepPid) ->
    ?_assertNot(begin
        ?assertMatch({ok, _}, couch_replicator:cancel_replication(RepId)),
        is_process_alive(RepPid)
    end).

compare_dbs(Source, Target, FilterFun) ->
    {ok, SourceDb} = couch_db:open_int(Source, []),
    {ok, TargetDb} = couch_db:open_int(Target, []),
    Fun = fun(FullDocInfo, _, Acc) ->
        {ok, DocId, SourceDoc} = read_doc(SourceDb, FullDocInfo, ?LINE),
        case FilterFun(DocId, SourceDoc) of
            true ->
                {ok, DocId, TargetDoc} = read_doc(TargetDb, DocId, ?LINE),
                ?assertEqual(SourceDoc, TargetDoc),
                {ok, Acc + 1};
            false ->
                {ok, Acc}
        end
    end,
    {ok, _, Acc} = couch_db:enum_docs(SourceDb, Fun, 0, []),
    ok = couch_db:close(SourceDb),
    ok = couch_db:close(TargetDb),
    Acc.

read_doc(Db, DocIdOrInfo, Line) ->
    case couch_db:open_doc(Db, DocIdOrInfo) of
        {ok, Doc} ->
            {Props} = couch_doc:to_json_obj(Doc, [attachments]),
            DocId = couch_util:get_value(<<"_id">>, Props),
            {ok, DocId, {Props}};
        Error ->
            Reason = lists:concat([
                "Error opening document '", ?b2l(doc_id(DocIdOrInfo)),
                "' from target: ", couch_util:to_list(Error)]),
            erlang:error(
                {assertion_failed,
                 [{module, ?MODULE}, {line, Line},
                  {reason, Reason}]})
    end.

doc_id(#full_doc_info{id = Id}) -> Id;
doc_id(Id) -> Id.


wait_for_replicator(Pid) ->
    %% since replicator started asynchronously
    %% we need to wait when it would be in couch_task_status
    %% we query replicator:details to ensure that do_init happen
    ?assertMatch({ok, _}, couch_replicator:details(Pid)),
    ok.

create_docs(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    Doc1 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc1">>},
        {<<"class">>, <<"mammal">>},
        {<<"value">>, 1}

    ]}),
    Doc2 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc2">>},
        {<<"value">>, 2}

    ]}),
    Doc3 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc3">>},
        {<<"class">>, <<"reptiles">>},
        {<<"value">>, 3}

    ]}),
    {ok, _} = couch_db:update_docs(Db, [Doc1, Doc2, Doc3]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).


upload_ddoc(DbName) ->
    Url = db_url(DbName) ++ "/_design/filter_ddoc",
    Body = couch_util:json_encode(?DDOC),
    {ok, 201, _Resp, _Body} = test_request:put(Url, Body),
    ok.

replicate({remote, Db}, Target) ->
    replicate(?l2b(db_url(Db)), Target);

replicate(Source, {remote, Db}) ->
    replicate(Source, ?l2b(db_url(Db)));

replicate(Source, Target) ->
    RepObject = {[
        {<<"source">>, Source},
        {<<"target">>, Target},
        {<<"continuous">>, true}
    ]},
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(RepObject, ?ADMIN_USER),
    {ok, Pid} = couch_replicator:async_replicate(Rep),
    {ok, Pid, Rep#rep.id}.

db_url(DbName) ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(DbName).
