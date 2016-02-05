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

-module(test_engine_compaction).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


cet_compact_empty() ->
    {ok, Engine, Path, St1} = test_engine_util:init_engine(dbpath),
    Db1 = test_engine_util:db_as_term(Engine, St1),
    {ok, St2, DbName, _, Term} = test_engine_util:compact(Engine, St1, Path),
    {ok, St3, undefined} = Engine:finish_compaction(St2, DbName, [], Term),
    Db2 = test_engine_util:db_as_term(Engine, St3),
    Diff = test_engine_util:term_diff(Db1, Db2),
    ?assertEqual(nodiff, Diff).


cet_compact_doc() ->
    {ok, Engine, Path, St1} = test_engine_util:init_engine(dbpath),
    Actions = [{create, {<<"foo">>, []}}],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    Db1 = test_engine_util:db_as_term(Engine, St2),
    {ok, St3, DbName, _, Term} = test_engine_util:compact(Engine, St2, Path),
    {ok, St4, undefined} = Engine:finish_compaction(St3, DbName, [], Term),
    Db2 = test_engine_util:db_as_term(Engine, St4),
    Diff = test_engine_util:term_diff(Db1, Db2),
    ?assertEqual(nodiff, Diff).


cet_compact_local_doc() ->
    {ok, Engine, Path, St1} = test_engine_util:init_engine(dbpath),
    Actions = [{create, {<<"_local/foo">>, []}}],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    Db1 = test_engine_util:db_as_term(Engine, St2),
    {ok, St3, DbName, _, Term} = test_engine_util:compact(Engine, St2, Path),
    {ok, St4, undefined} = Engine:finish_compaction(St3, DbName, [], Term),
    Db2 = test_engine_util:db_as_term(Engine, St4),
    Diff = test_engine_util:term_diff(Db1, Db2),
    ?assertEqual(nodiff, Diff).


cet_compact_with_everything() ->
    {ok, Engine, Path, St1} = test_engine_util:init_engine(dbpath),

    % Add a whole bunch of docs
    DocActions = lists:map(fun(Seq) ->
        {create, {docid(Seq), [{<<"int">>, Seq}]}}
    end, lists:seq(1, 1000)),

    LocalActions = lists:map(fun(I) ->
        {create, {local_docid(I), [{<<"int">>, I}]}}
    end, lists:seq(1, 25)),

    Actions1 = DocActions ++ LocalActions,

    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions1),
    {ok, St3} = Engine:set_security(St2, [{<<"readers">>, <<"ohai">>}]),
    {ok, St4} = Engine:set_revs_limit(St3, 500),

    Actions2 = [
        {create, {<<"foo">>, []}},
        {create, {<<"bar">>, [{<<"hooray">>, <<"purple">>}]}},
        {conflict, {<<"bar">>, [{<<"booo">>, false}]}}
    ],

    {ok, St5} = test_engine_util:apply_actions(Engine, St4, Actions2),

    [FooFDI, BarFDI] = Engine:open_docs(St5, [<<"foo">>, <<"bar">>]),

    FooRev = test_engine_util:prev_rev(FooFDI),
    BarRev = test_engine_util:prev_rev(BarFDI),

    Actions3 = [
        {batch, [
            {purge, {<<"foo">>, FooRev#rev_info.rev}},
            {purge, {<<"bar">>, BarRev#rev_info.rev}}
        ]}
    ],

    {ok, St6} = test_engine_util:apply_actions(Engine, St5, Actions3),

    PurgedIdRevs = [
        {<<"bar">>, [BarRev#rev_info.rev]},
        {<<"foo">>, [FooRev#rev_info.rev]}
    ],

    ?assertEqual(PurgedIdRevs, lists:sort(Engine:get_last_purged(St6))),

    {ok, St7} = try
        [Att0, Att1, Att2, Att3, Att4] = test_engine_util:prep_atts(Engine, St6, [
                {<<"ohai.txt">>, crypto:rand_bytes(2048)},
                {<<"stuff.py">>, crypto:rand_bytes(32768)},
                {<<"a.erl">>, crypto:rand_bytes(29)},
                {<<"a.hrl">>, crypto:rand_bytes(5000)},
                {<<"a.app">>, crypto:rand_bytes(400)}
            ]),

        Actions4 = [
            {create, {<<"small_att">>, [], [Att0]}},
            {create, {<<"large_att">>, [], [Att1]}},
            {create, {<<"multi_att">>, [], [Att2, Att3, Att4]}}
        ],
        test_engine_util:apply_actions(Engine, St6, Actions4)
    catch throw:not_supported ->
        {ok, St6}
    end,
    {ok, St8} = Engine:commit_data(St7),

    Db1 = test_engine_util:db_as_term(Engine, St8),

    Config = [
        {"database_compaction", "doc_buffer_size", "1024"},
        {"database_compaction", "checkpoint_after", "2048"}
    ],

    {ok, St9, DbName, _, Term} = test_engine_util:with_config(Config, fun() ->
        test_engine_util:compact(Engine, St8, Path)
    end),

    {ok, St10, undefined} = Engine:finish_compaction(St9, DbName, [], Term),
    Db2 = test_engine_util:db_as_term(Engine, St10),
    Diff = test_engine_util:term_diff(Db1, Db2),
    ?assertEqual(nodiff, Diff).


cet_recompact_updates() ->
    {ok, Engine, Path, St1} = test_engine_util:init_engine(dbpath),

    Actions1 = [
        {create, {<<"foo">>, []}},
        {create, {<<"bar">>, []}}
    ],

    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions1),
    {ok, St3, DbName, _, Term} = test_engine_util:compact(Engine, St2, Path),

    Actions2 = [
        {update, {<<"foo">>, [{<<"updated">>, true}]}},
        {create, {<<"baz">>, []}}
    ],

    {ok, St4} = test_engine_util:apply_actions(Engine, St3, Actions2),
    Db1 = test_engine_util:db_as_term(Engine, St4),

    {ok, St5, NewPid} = Engine:finish_compaction(St4, DbName, [], Term),

    ?assertEqual(true, is_pid(NewPid)),
    Ref = erlang:monitor(process, NewPid),

    NewTerm = receive
        {'$gen_cast', {compact_done, Engine, Term0}} ->
            Term0;
        {'DOWN', Ref, _, _, Reason} ->
            erlang:error({compactor_died, Reason})
        after 10000 ->
            erlang:error(compactor_timed_out)
    end,

    {ok, St6, undefined} = Engine:finish_compaction(St5, DbName, [], NewTerm),
    Db2 = test_engine_util:db_as_term(Engine, St6),
    Diff = test_engine_util:term_diff(Db1, Db2),
    ?assertEqual(nodiff, Diff).


docid(I) ->
    Str = io_lib:format("~4..0b", [I]),
    iolist_to_binary(Str).


local_docid(I) ->
    Str = io_lib:format("_local/~4..0b", [I]),
    iolist_to_binary(Str).
