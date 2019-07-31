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

-module(cpse_test_purge_replication).
-compile(export_all).
-compile(nowarn_export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").


setup_all() ->
    cpse_util:setup_all([mem3, fabric, chttpd, couch_replicator]).


setup_each() ->
    {ok, Src} = cpse_util:create_db(),
    {ok, Tgt} = cpse_util:create_db(),
    {couch_db:name(Src), couch_db:name(Tgt)}.


teardown_each({SrcDb, TgtDb}) ->
    ok = couch_server:delete(SrcDb, []),
    ok = couch_server:delete(TgtDb, []).


cpse_purge_http_replication({Source, Target}) ->
    {ok, Rev1} = cpse_util:save_doc(Source, {[{'_id', foo}, {vsn, 1}]}),

    cpse_util:assert_db_props(?MODULE, ?LINE, Source, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 1},
        {changes, 1},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    RepObject = {[
        {<<"source">>, db_url(Source)},
        {<<"target">>, db_url(Target)}
    ]},

    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    {ok, Doc1} = cpse_util:open_doc(Target, foo),

    cpse_util:assert_db_props(?MODULE, ?LINE, Target, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 1},
        {changes, 1},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo">>, [Rev1]}
    ],

    {ok, [{ok, PRevs}]} = cpse_util:purge(Source, PurgeInfos),
    ?assertEqual([Rev1], PRevs),

    cpse_util:assert_db_props(?MODULE, ?LINE, Source, [
        {doc_count, 0},
        {del_doc_count, 0},
        {update_seq, 2},
        {changes, 0},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]),

    % Show that a purge on the source is
    % not replicated to the target
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    {ok, Doc2} = cpse_util:open_doc(Target, foo),
    [Rev2] = Doc2#doc_info.revs,
    ?assertEqual(Rev1, Rev2#rev_info.rev),
    ?assertEqual(Doc1, Doc2),

    cpse_util:assert_db_props(?MODULE, ?LINE, Target, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 1},
        {changes, 1},
        {purge_seq, 0},
        {purge_infos, []}
    ]),

    % Show that replicating from the target
    % back to the source reintroduces the doc
    RepObject2 = {[
        {<<"source">>, db_url(Target)},
        {<<"target">>, db_url(Source)}
    ]},

    {ok, _} = couch_replicator:replicate(RepObject2, ?ADMIN_USER),
    {ok, Doc3} = cpse_util:open_doc(Source, foo),
    [Revs3] = Doc3#doc_info.revs,
    ?assertEqual(Rev1, Revs3#rev_info.rev),

    cpse_util:assert_db_props(?MODULE, ?LINE, Source, [
        {doc_count, 1},
        {del_doc_count, 0},
        {update_seq, 3},
        {changes, 1},
        {purge_seq, 1},
        {purge_infos, PurgeInfos}
    ]).


cpse_purge_internal_repl_disabled({Source, Target}) ->
    cpse_util:with_config([{"mem3", "replicate_purges", "false"}], fun() ->
        repl(Source, Target),

        {ok, [Rev1, Rev2]} = cpse_util:save_docs(Source, [
            {[{'_id', foo1}, {vsn, 1}]},
            {[{'_id', foo2}, {vsn, 2}]}
        ]),

        repl(Source, Target),

        PurgeInfos1 = [
            {cpse_util:uuid(), <<"foo1">>, [Rev1]}
        ],
        {ok, [{ok, PRevs1}]} = cpse_util:purge(Source, PurgeInfos1),
        ?assertEqual([Rev1], PRevs1),

        PurgeInfos2 = [
            {cpse_util:uuid(), <<"foo2">>, [Rev2]}
        ],
        {ok, [{ok, PRevs2}]} = cpse_util:purge(Target, PurgeInfos2),
        ?assertEqual([Rev2], PRevs2),

        SrcShard = make_shard(Source),
        TgtShard = make_shard(Target),
        ?assertEqual({ok, 0}, mem3_rep:go(SrcShard, TgtShard)),
        ?assertEqual({ok, 0}, mem3_rep:go(TgtShard, SrcShard)),

        ?assertMatch({ok, #doc_info{}}, cpse_util:open_doc(Source, <<"foo2">>)),
        ?assertMatch({ok, #doc_info{}}, cpse_util:open_doc(Target, <<"foo1">>))
    end).


cpse_purge_repl_simple_pull({Source, Target}) ->
    repl(Source, Target),

    {ok, Rev} = cpse_util:save_doc(Source, {[{'_id', foo}, {vsn, 1}]}),
    repl(Source, Target),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo">>, [Rev]}
    ],
    {ok, [{ok, PRevs}]} = cpse_util:purge(Target, PurgeInfos),
    ?assertEqual([Rev], PRevs),
    repl(Source, Target).


cpse_purge_repl_simple_push({Source, Target}) ->
    repl(Source, Target),

    {ok, Rev} = cpse_util:save_doc(Source, {[{'_id', foo}, {vsn, 1}]}),
    repl(Source, Target),

    PurgeInfos = [
        {cpse_util:uuid(), <<"foo">>, [Rev]}
    ],
    {ok, [{ok, PRevs}]} = cpse_util:purge(Source, PurgeInfos),
    ?assertEqual([Rev], PRevs),
    repl(Source, Target).


repl(Source, Target) ->
    SrcShard = make_shard(Source),
    TgtShard = make_shard(Target),

    ?assertEqual({ok, 0}, mem3_rep:go(SrcShard, TgtShard)),

    SrcTerm = cpse_util:db_as_term(Source, replication),
    TgtTerm = cpse_util:db_as_term(Target, replication),

    Diff = cpse_util:term_diff(SrcTerm, TgtTerm),
    ?assertEqual(nodiff, Diff).


make_shard(DbName) ->
    #shard{
        name = DbName,
        node = node(),
        dbname = DbName,
        range = [0, 16#FFFFFFFF]
    }.


db_url(DbName) ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(couch_httpd, port),
    ?l2b(io_lib:format("http://~s:~b/~s", [Addr, Port, DbName])).
