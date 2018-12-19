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

-module(cpse_test_compaction).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup_each() ->
    {ok, Db} = cpse_util:create_db(),
    Db.


teardown_each(Db) ->
    ok = couch_server:delete(couch_db:name(Db), []).


cpse_compact_empty(Db1) ->
    Term1 = cpse_util:db_as_term(Db1),

    cpse_util:compact(Db1),

    {ok, Db2} = couch_db:reopen(Db1),
    Term2 = cpse_util:db_as_term(Db2),

    Diff = cpse_util:term_diff(Term1, Term2),
    ?assertEqual(nodiff, Diff).


cpse_compact_doc(Db1) ->
    Actions = [{create, {<<"foo">>, {[]}}}],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    Term1 = cpse_util:db_as_term(Db2),

    cpse_util:compact(Db2),

    {ok, Db3} = couch_db:reopen(Db2),
    Term2 = cpse_util:db_as_term(Db3),

    Diff = cpse_util:term_diff(Term1, Term2),
    ?assertEqual(nodiff, Diff).


cpse_compact_local_doc(Db1) ->
    Actions = [{create, {<<"_local/foo">>, {[]}}}],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    Term1 = cpse_util:db_as_term(Db2),

    cpse_util:compact(Db2),

    {ok, Db3} = couch_db:reopen(Db2),
    Term2 = cpse_util:db_as_term(Db3),

    Diff = cpse_util:term_diff(Term1, Term2),
    ?assertEqual(nodiff, Diff).


cpse_compact_with_everything(Db1) ->
    % Add a whole bunch of docs
    DocActions = lists:map(fun(Seq) ->
        {create, {docid(Seq), {[{<<"int">>, Seq}]}}}
    end, lists:seq(1, 1000)),

    LocalActions = lists:map(fun(I) ->
        {create, {local_docid(I), {[{<<"int">>, I}]}}}
    end, lists:seq(1, 25)),

    Actions1 = DocActions ++ LocalActions,

    {ok, Db2} = cpse_util:apply_batch(Db1, Actions1),
    ok = couch_db:set_security(Db1, {[{<<"foo">>, <<"bar">>}]}),
    ok = couch_db:set_revs_limit(Db1, 500),

    Actions2 = [
        {create, {<<"foo">>, {[]}}},
        {create, {<<"bar">>, {[{<<"hooray">>, <<"purple">>}]}}},
        {conflict, {<<"bar">>, {[{<<"booo">>, false}]}}}
    ],

    {ok, Db3} = cpse_util:apply_actions(Db2, Actions2),

    [FooFDI, BarFDI] = couch_db_engine:open_docs(Db3, [<<"foo">>, <<"bar">>]),

    FooRev = cpse_util:prev_rev(FooFDI),
    BarRev = cpse_util:prev_rev(BarFDI),

    Actions3 = [
        {purge, {<<"foo">>, FooRev#rev_info.rev}},
        {purge, {<<"bar">>, BarRev#rev_info.rev}}
    ],

    {ok, Db4} = cpse_util:apply_actions(Db3, Actions3),

    PurgedIdRevs = [
        {<<"bar">>, [BarRev#rev_info.rev]},
        {<<"foo">>, [FooRev#rev_info.rev]}
    ],

    {ok, PIdRevs4} = couch_db_engine:fold_purge_infos(
            Db4, 0, fun fold_fun/2, [], []),
    ?assertEqual(PurgedIdRevs, PIdRevs4),

    {ok, Db5} = try
        [Att0, Att1, Att2, Att3, Att4] = cpse_util:prep_atts(Db4, [
                {<<"ohai.txt">>, crypto:strong_rand_bytes(2048)},
                {<<"stuff.py">>, crypto:strong_rand_bytes(32768)},
                {<<"a.erl">>, crypto:strong_rand_bytes(29)},
                {<<"a.hrl">>, crypto:strong_rand_bytes(5000)},
                {<<"a.app">>, crypto:strong_rand_bytes(400)}
            ]),

        Actions4 = [
            {create, {<<"small_att">>, {[]}, [Att0]}},
            {create, {<<"large_att">>, {[]}, [Att1]}},
            {create, {<<"multi_att">>, {[]}, [Att2, Att3, Att4]}}
        ],
        cpse_util:apply_actions(Db4, Actions4)
    catch throw:not_supported ->
        {ok, Db4}
    end,
    {ok, _} = couch_db:ensure_full_commit(Db5),
    {ok, Db6} = couch_db:reopen(Db5),

    Term1 = cpse_util:db_as_term(Db6),

    Config = [
        {"database_compaction", "doc_buffer_size", "1024"},
        {"database_compaction", "checkpoint_after", "2048"}
    ],

    cpse_util:with_config(Config, fun() ->
        cpse_util:compact(Db6)
    end),

    {ok, Db7} = couch_db:reopen(Db6),
    Term2 = cpse_util:db_as_term(Db7),

    Diff = cpse_util:term_diff(Term1, Term2),
    ?assertEqual(nodiff, Diff).


cpse_recompact_updates(Db1) ->
    Actions1 = lists:map(fun(Seq) ->
        {create, {docid(Seq), {[{<<"int">>, Seq}]}}}
    end, lists:seq(1, 1000)),
    {ok, Db2} = cpse_util:apply_batch(Db1, Actions1),

    {ok, Compactor} = couch_db:start_compact(Db2),
    catch erlang:suspend_process(Compactor),

    Actions2 = [
        {update, {<<"0001">>, {[{<<"updated">>, true}]}}},
        {create, {<<"boop">>, {[]}}}
    ],

    {ok, Db3} = cpse_util:apply_actions(Db2, Actions2),
    Term1 = cpse_util:db_as_term(Db3),

    catch erlang:resume_process(Compactor),
    cpse_util:compact(Db3),

    {ok, Db4} = couch_db:reopen(Db3),
    Term2 = cpse_util:db_as_term(Db4),

    Diff = cpse_util:term_diff(Term1, Term2),
    ?assertEqual(nodiff, Diff).


cpse_purge_during_compact(Db1) ->
    Actions1 = lists:map(fun(Seq) ->
        {create, {docid(Seq), {[{<<"int">>, Seq}]}}}
    end, lists:seq(1, 1000)),
    Actions2 = [
        {create, {<<"foo">>, {[]}}},
        {create, {<<"bar">>, {[]}}},
        {create, {<<"baz">>, {[]}}}
    ],
    {ok, Db2} = cpse_util:apply_batch(Db1, Actions1 ++ Actions2),
    Actions3 = [
        {conflict, {<<"bar">>, {[{<<"vsn">>, 2}]}}}
    ],
    {ok, Db3} = cpse_util:apply_actions(Db2, Actions3),

    {ok, Pid} = couch_db:start_compact(Db3),
    catch erlang:suspend_process(Pid),

    [BarFDI, BazFDI] = couch_db_engine:open_docs(Db3, [<<"bar">>, <<"baz">>]),
    BarRev = cpse_util:prev_rev(BarFDI),
    BazRev = cpse_util:prev_rev(BazFDI),
    Actions4 = [
        {purge, {<<"bar">>, BarRev#rev_info.rev}},
        {purge, {<<"baz">>, BazRev#rev_info.rev}}
    ],

    {ok, Db4} = cpse_util:apply_actions(Db3, Actions4),
    Term1 = cpse_util:db_as_term(Db4),

    catch erlang:resume_process(Pid),
    cpse_util:compact(Db4),

    {ok, Db5} = couch_db:reopen(Db4),
    Term2 = cpse_util:db_as_term(Db5),

    Diff = cpse_util:term_diff(Term1, Term2),
    ?assertEqual(nodiff, Diff).


cpse_multiple_purge_during_compact(Db1) ->
    Actions1 = lists:map(fun(Seq) ->
        {create, {docid(Seq), {[{<<"int">>, Seq}]}}}
    end, lists:seq(1, 1000)),
    Actions2 = [
        {create, {<<"foo">>, {[]}}},
        {create, {<<"bar">>, {[]}}},
        {create, {<<"baz">>, {[]}}}
    ],
    {ok, Db2} = cpse_util:apply_batch(Db1, Actions1 ++ Actions2),

    Actions3 = [
        {conflict, {<<"bar">>, {[{<<"vsn">>, 2}]}}}
    ],
    {ok, Db3} = cpse_util:apply_actions(Db2, Actions3),


    {ok, Pid} = couch_db:start_compact(Db3),
    catch erlang:suspend_process(Pid),

    [BarFDI, BazFDI] = couch_db_engine:open_docs(Db3, [<<"bar">>, <<"baz">>]),
    BarRev = cpse_util:prev_rev(BarFDI),
    Actions4 = [
        {purge, {<<"bar">>, BarRev#rev_info.rev}}
    ],
    {ok, Db4} = cpse_util:apply_actions(Db3, Actions4),

    BazRev = cpse_util:prev_rev(BazFDI),
    Actions5 = [
        {purge, {<<"baz">>, BazRev#rev_info.rev}}
    ],

    {ok, Db5} = cpse_util:apply_actions(Db4, Actions5),
    Term1 = cpse_util:db_as_term(Db5),

    catch erlang:resume_process(Pid),
    cpse_util:compact(Db5),

    {ok, Db6} = couch_db:reopen(Db5),
    Term2 = cpse_util:db_as_term(Db6),

    Diff = cpse_util:term_diff(Term1, Term2),
    ?assertEqual(nodiff, Diff).


cpse_compact_purged_docs_limit(Db1) ->
    NumDocs = 1200,
    {RActions, RIds} = lists:foldl(fun(Id, {CActions, CIds}) ->
        Id1 = docid(Id),
        Action = {create, {Id1, {[{<<"int">>, Id}]}}},
        {[Action| CActions], [Id1| CIds]}
    end, {[], []}, lists:seq(1, NumDocs)),
    Ids = lists:reverse(RIds),
    {ok, Db2} = cpse_util:apply_batch(Db1, lists:reverse(RActions)),

    FDIs = couch_db_engine:open_docs(Db2, Ids),
    RActions2 = lists:foldl(fun(FDI, CActions) ->
        Id = FDI#full_doc_info.id,
        PrevRev = cpse_util:prev_rev(FDI),
        Rev = PrevRev#rev_info.rev,
        [{purge, {Id, Rev}}| CActions]
    end, [], FDIs),
    {ok, Db3} = cpse_util:apply_batch(Db2, lists:reverse(RActions2)),

    % check that before compaction all NumDocs of purge_requests
    % are in purge_tree,
    % even if NumDocs=1200 is greater than purged_docs_limit=1000
    {ok, PurgedIdRevs} = couch_db_engine:fold_purge_infos(
            Db3, 0, fun fold_fun/2, [], []),
    ?assertEqual(1, couch_db_engine:get_oldest_purge_seq(Db3)),
    ?assertEqual(NumDocs, length(PurgedIdRevs)),

    % compact db
    cpse_util:compact(Db3),
    {ok, Db4} = couch_db:reopen(Db3),

    % check that after compaction only purged_docs_limit purge_requests
    % are in purge_tree
    PurgedDocsLimit = couch_db_engine:get_purge_infos_limit(Db4),
    OldestPSeq = couch_db_engine:get_oldest_purge_seq(Db4),
    {ok, PurgedIdRevs2} = couch_db_engine:fold_purge_infos(
        Db4, OldestPSeq - 1, fun fold_fun/2, [], []),
    ExpectedOldestPSeq = NumDocs - PurgedDocsLimit + 1,
    ?assertEqual(ExpectedOldestPSeq, OldestPSeq),
    ?assertEqual(PurgedDocsLimit, length(PurgedIdRevs2)).


docid(I) ->
    Str = io_lib:format("~4..0b", [I]),
    iolist_to_binary(Str).


local_docid(I) ->
    Str = io_lib:format("_local/~4..0b", [I]),
    iolist_to_binary(Str).


fold_fun({_PSeq, _UUID, Id, Revs}, Acc) ->
    {ok, [{Id, Revs} | Acc]}.
