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

-module(fabric_drop_seq_prop_tests).

-behaviour(proper_statem).

-include_lib("couch/include/couch_eunit_proper.hrl").

-export([
    initial_state/0,
    command/1,
    precondition/2,
    postcondition/3,
    next_state/3
]).

-export([
    get_document/2,
    changes/1,
    update_document/2,
    delete_document/2,
    update_peer_checkpoint/1,
    update_drop_seq/1,
    compact/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").

-type docid() :: binary().
-type seq() :: non_neg_integer().

-record(state, {
    docs :: [{docid()}],
    deleted_docs :: [{docid(), seq()}],
    current_seq :: seq(),
    peer_checkpoint_seq :: seq(),
    drop_seq :: seq(),
    drop_count :: non_neg_integer()
}).

property_test_() ->
    ?EUNIT_QUICKCHECK(10000, 2000).

prop_drop_seq() ->
    ?FORALL(
        Cmds,
        commands(?MODULE),
        ?TRAPEXIT(
            begin
                {DbName, Ctx} = setup(),
                {History, State, Result} = run_commands(?MODULE, Cmds, [{dbname, DbName}]),
                teardown(DbName, Ctx),
                ?WHENFAIL(
                    io:format(
                        "~nHistory: ~p~n~nState: ~p~n~nResult: ~p~n~n",
                        [History, State, Result]
                    ),
                    aggregate(command_names(Cmds), Result =:= ok)
                )
            end
        )
    ).

setup() ->
    Ctx = test_util:start_couch([fabric]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 4}, {n, 1}]),
    {DbName, Ctx}.

teardown(DbName, Ctx) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    test_util:stop_couch(Ctx).

initial_state() ->
    #state{
        docs = [],
        deleted_docs = [],
        current_seq = 0,
        peer_checkpoint_seq = undefined,
        drop_seq = undefined,
        drop_count = 0
    }.

command(S) ->
    HasDocs = (S#state.docs =/= []),
    frequency(
        [
            {4, {call, ?MODULE, update_document, [{var, dbname}, docid()]}},
            {1, {call, ?MODULE, get_document, [{var, dbname}, docid()]}},
            {1, {call, ?MODULE, changes, [{var, dbname}]}},
            {1, {call, ?MODULE, update_peer_checkpoint, [{var, dbname}]}},
            {1, {call, ?MODULE, update_drop_seq, [{var, dbname}]}},
            {1, {call, ?MODULE, compact, [{var, dbname}]}}
        ] ++
            [{4, {call, ?MODULE, delete_document, [{var, dbname}, docid()]}} || HasDocs]
    ).

get_document(DbName, DocId) ->
    fabric:open_doc(DbName, DocId, [?ADMIN_CTX]).

changes(DbName) ->
    Acc0 = {[], []},
    Callback = fun
        ({change, {Change}}, {DocIds, DelDocIds}) ->
            Id = couch_util:get_value(id, Change),
            Deleted = couch_util:get_value(deleted, Change),
            if
                Deleted ->
                    {ok, {DocIds, lists:merge([Id], DelDocIds)}};
                true ->
                    {ok, {lists:merge([Id], DocIds), DelDocIds}}
            end;
        (_Else, Acc) ->
            {ok, Acc}
    end,
    fabric:changes(DbName, Callback, Acc0, []).

update_document(DbName, DocId) ->
    update_document(DbName, DocId, false).

delete_document(DbName, DocId) ->
    update_document(DbName, DocId, true).

update_document(DbName, DocId, Deleted) ->
    case fabric:open_doc(DbName, DocId, [?ADMIN_CTX]) of
        {ok, Doc} ->
            {ok, _} = fabric:update_doc(DbName, Doc#doc{deleted = Deleted}, [?ADMIN_CTX]);
        {not_found, _} when not Deleted ->
            {ok, _} = fabric:update_doc(DbName, #doc{id = DocId}, [?ADMIN_CTX]);
        {not_found, _} ->
            ok
    end.

update_peer_checkpoint(DbName) ->
    {ok, DbInfo} = fabric:get_db_info(DbName),
    UpdateSeq = couch_util:get_value(update_seq, DbInfo),
    Doc = #doc{id = <<"_local/peer-checkpoint-foo">>, body = {[{<<"update_seq">>, UpdateSeq}]}},
    {ok, _} = fabric:update_doc(
        DbName,
        Doc,
        [?ADMIN_CTX]
    ).

update_drop_seq(DbName) ->
    {ok, _} = fabric_drop_seq:go(DbName).

compact(DbName) ->
    ok = fabric:compact(DbName),
    wait_for_compaction_to_finish(DbName).

wait_for_compaction_to_finish(DbName) ->
    {ok, DbInfo} = fabric:get_db_info(DbName),
    CompactRunning = couch_util:get_value(compact_running, DbInfo),
    if
        CompactRunning ->
            timer:sleep(500),
            wait_for_compaction_to_finish(DbName);
        true ->
            timer:sleep(500),
            ok
    end.

precondition(S, {call, _, update_document, [_DbName, DocId]}) ->
    not doc_exists(S, DocId);
precondition(S, {call, _, delete_document, [_DbName, DocId]}) ->
    doc_exists(S, DocId);
precondition(_, _) ->
    true.

next_state(S, _V, {call, _, update_document, [_DbName, DocId]}) ->
    S#state{
        current_seq = S#state.current_seq + 1,
        docs = lists:merge([DocId], S#state.docs),
        deleted_docs = lists:keydelete(DocId, 1, S#state.deleted_docs)
    };
next_state(S, _V, {call, _, delete_document, [_DbName, DocId]}) ->
    S#state{
        current_seq = S#state.current_seq + 1,
        docs = lists:delete(DocId, S#state.docs),
        deleted_docs = lists:merge([{DocId, S#state.current_seq + 1}], S#state.deleted_docs)
    };
next_state(S, _V, {call, _, update_peer_checkpoint, [_DbName]}) ->
    S#state{
        peer_checkpoint_seq = S#state.current_seq
    };
next_state(S, _V, {call, _, update_drop_seq, [_DbName]}) ->
    S#state{
        drop_seq = S#state.peer_checkpoint_seq
    };
next_state(S, _V, {call, _, compact, [_DbName]}) ->
    {KeepDocs, DropDocs} = lists:partition(
        fun({_, Seq}) ->
            S#state.drop_seq == undefined orelse Seq > S#state.drop_seq
        end,
        S#state.deleted_docs
    ),
    S#state{
        deleted_docs = KeepDocs,
        drop_count = S#state.drop_count + length(DropDocs)
    };
next_state(S, _Res, _Call) ->
    S.

postcondition(S, {call, _, get_document, [_DbName, DocId]}, {ok, _Doc}) ->
    doc_exists(S, DocId) andalso not deleted_doc_exists(S, DocId);
postcondition(S, {call, _, get_document, [_DbName, DocId]}, {not_found, deleted}) ->
    not doc_exists(S, DocId) andalso deleted_doc_exists(S, DocId);
postcondition(S, {call, _, get_document, [_DbName, DocId]}, {not_found, missing}) ->
    not doc_exists(S, DocId) andalso not deleted_doc_exists(S, DocId);
postcondition(S, {call, _, changes, [_DbName]}, {ok, {DocIds, DelDocIds}}) ->
    same_list(DocIds, doc_ids(S)) andalso same_list(DelDocIds, deleted_doc_ids(S));
postcondition(_, _, _) ->
    true.

docid() ->
    elements([<<"doc", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 10)]).

docid(Bin) when is_binary(Bin) ->
    Bin;
docid(Tuple) when is_tuple(Tuple) ->
    element(1, Tuple).

doc_exists(S, DocId) ->
    lists:member(DocId, S#state.docs).

deleted_doc_exists(S, DocId) ->
    lists:keymember(DocId, 1, S#state.deleted_docs).

doc_ids(S) ->
    S#state.docs.
deleted_doc_ids(S) ->
    [docid(T) || T <- S#state.deleted_docs].

same_list(ListA, ListB) ->
    SortedA = lists:sort(ListA),
    SortedB = lists:sort(ListB),
    SortedA == SortedB.
