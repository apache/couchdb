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


cet_compact_empty() ->
    {ok, Db1} = cpse_util:create_db(),
    Term1 = cpse_util:db_as_term(Db1),

    cpse_util:compact(Db1),

    {ok, Db2} = couch_db:reopen(Db1),
    Term2 = cpse_util:db_as_term(Db2),

    Diff = cpse_util:term_diff(Term1, Term2),
    ?assertEqual(nodiff, Diff).


cet_compact_doc() ->
    {ok, Db1} = cpse_util:create_db(),
    Actions = [{create, {<<"foo">>, {[]}}}],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    Term1 = cpse_util:db_as_term(Db2),

    cpse_util:compact(Db2),

    {ok, Db3} = couch_db:reopen(Db2),
    Term2 = cpse_util:db_as_term(Db3),

    Diff = cpse_util:term_diff(Term1, Term2),
    ?assertEqual(nodiff, Diff).


cet_compact_local_doc() ->
    {ok, Db1} = cpse_util:create_db(),
    Actions = [{create, {<<"_local/foo">>, {[]}}}],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    Term1 = cpse_util:db_as_term(Db2),

    cpse_util:compact(Db2),

    {ok, Db3} = couch_db:reopen(Db2),
    Term2 = cpse_util:db_as_term(Db3),

    Diff = cpse_util:term_diff(Term1, Term2),
    ?assertEqual(nodiff, Diff).


cet_compact_with_everything() ->
    {ok, Db1} = cpse_util:create_db(),

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
        {batch, [
            {purge, {<<"foo">>, FooRev#rev_info.rev}},
            {purge, {<<"bar">>, BarRev#rev_info.rev}}
        ]}
    ],

    {ok, Db4} = cpse_util:apply_actions(Db3, Actions3),

    PurgedIdRevs = [
        {<<"bar">>, [BarRev#rev_info.rev]},
        {<<"foo">>, [FooRev#rev_info.rev]}
    ],

    ?assertEqual(
            PurgedIdRevs,
            lists:sort(couch_db_engine:get_last_purged(Db4))
        ),

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


cet_recompact_updates() ->
    {ok, Db1} = cpse_util:create_db(),

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


docid(I) ->
    Str = io_lib:format("~4..0b", [I]),
    iolist_to_binary(Str).


local_docid(I) ->
    Str = io_lib:format("_local/~4..0b", [I]),
    iolist_to_binary(Str).
