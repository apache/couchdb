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

-module(chttpd_changes_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").

-define(USER, "chttpd_changes_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(JSON, {"Content-Type", "application/json"}).

-define(DOC1, <<"doc1">>).
-define(DDOC2, <<"_design/doc2">>).
-define(DOC3, <<"doc3">>).
-define(REVA, <<"a">>).
-define(REVB, <<"b">>).
-define(REVC, <<"c">>).
-define(DELETED, true).
-define(LEAFREV, false).

% doc1 starts as rev-a, then gets 2 conflicting revisions b and c
% ddoc2 starts as deleted at rev-a, then gets re-created as rev-c
% doc3 starts as rev-a, then gets deleted as rev-c
%
test_docs() ->
    [
        {?DOC1, [?REVA], ?LEAFREV},
        {?DDOC2, [?REVA], ?DELETED},
        {?DOC3, [?REVA], ?LEAFREV},
        {?DOC1, [?REVB, ?REVA], ?LEAFREV},
        {?DOC1, [?REVC, ?REVA], ?LEAFREV},
        {?DOC3, [?REVB, ?REVA], ?DELETED},
        {?DDOC2, [?REVC, ?REVA], ?LEAFREV}
    ].

% These are run against a Q=1, N=1 db, so we can make
% some stronger assumptions about the exact Seq prefixes
% returned sequences will have
%
changes_test_() ->
    {
        setup,
        fun setup_basic/0,
        fun teardown_basic/1,
        with([
            ?TDEF(t_basic),
            ?TDEF(t_basic_post),
            ?TDEF(t_continuous),
            ?TDEF(t_continuous_zero_timeout),
            ?TDEF(t_longpoll),
            ?TDEF(t_limit_zero),
            ?TDEF(t_continuous_limit_zero),
            ?TDEF(t_limit_one),
            ?TDEF(t_since_now),
            ?TDEF(t_continuous_since_now),
            ?TDEF(t_longpoll_since_now),
            ?TDEF(t_style_all_docs),
            ?TDEF(t_reverse),
            ?TDEF(t_continuous_reverse),
            ?TDEF(t_reverse_limit_zero),
            ?TDEF(t_reverse_limit_one),
            ?TDEF(t_seq_interval),
            ?TDEF(t_selector_filter),
            ?TDEF(t_design_filter),
            ?TDEF(t_docs_id_filter),
            ?TDEF(t_docs_id_filter_over_limit)
        ])
    }.

% For Q=8 sharded dbs, unlike Q=1, we cannot make strong
% assumptions about the exact sequence IDs for each row
% so we'll test all the changes return and that the sequences
% are increasing.
%
changes_q8_test_() ->
    {
        setup,
        fun setup_q8/0,
        fun teardown_basic/1,
        with([
            ?TDEF(t_basic_q8),
            ?TDEF(t_continuous_q8),
            ?TDEF(t_limit_zero),
            ?TDEF(t_limit_one_q8),
            ?TDEF(t_since_now),
            ?TDEF(t_longpoll_since_now),
            ?TDEF(t_reverse_q8),
            ?TDEF(t_reverse_limit_zero),
            ?TDEF(t_reverse_limit_one_q8),
            ?TDEF(t_selector_filter),
            ?TDEF(t_design_filter),
            ?TDEF(t_docs_id_filter_q8)
        ])
    }.

% These tests are separate as they create aditional design docs during
% the test. That ends up bumping the update sequence in the db, so
% last_seq and other sequences returned become dependent on the test
% order.  To avoid that dependence, run them in a separate suite with
% a foreach construct insted of a setup one. This way setup/teardown
% happens for each individual test case.
%
changes_js_filters_test_() ->
    {
        foreach,
        fun setup_basic/0,
        fun teardown_basic/1,
        [
            ?TDEF_FE(t_js_filter),
            ?TDEF_FE(t_js_filter_no_match),
            ?TDEF_FE(t_js_filter_with_query_param),
            ?TDEF_FE(t_view_filter),
            ?TDEF_FE(t_view_filter_no_match)
        ]
    }.

t_basic({_, DbUrl}) ->
    Res = {Seq, Pending, Rows} = changes(DbUrl),
    ?assertEqual(7, Seq),
    ?assertEqual(0, Pending),
    ?assertEqual(
        [
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED},
            {7, {?DDOC2, <<"2-c">>}, ?LEAFREV}
        ],
        Rows
    ),
    % since=0 is the default, so it should look exactly the same
    ?assertEqual(Res, changes(DbUrl, "?since=0")).

t_basic_q8({_, DbUrl}) ->
    {Seq, Pending, Rows} = changes(DbUrl),
    ?assertEqual(7, Seq),
    ?assertEqual(0, Pending),
    {Seqs, Revs, _Deleted} = lists:unzip3(Rows),
    ?assertEqual(
        [
            {?DDOC2, <<"2-c">>},
            {?DOC1, <<"2-c">>},
            {?DOC3, <<"2-b">>}
        ],
        lists:sort(Revs)
    ),
    ?assertEqual(Seqs, lists:sort(Seqs)).

t_basic_post({_, DbUrl}) ->
    {Seq, Pending, Rows} = changes_post(DbUrl, #{}),
    ?assertEqual(7, Seq),
    ?assertEqual(0, Pending),
    ?assertEqual(
        [
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED},
            {7, {?DDOC2, <<"2-c">>}, ?LEAFREV}
        ],
        Rows
    ).

t_continuous({_, DbUrl}) ->
    Params = "?feed=continuous&timeout=10",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(7, Seq),
    ?assertEqual(0, Pending),
    ?assertEqual(
        [
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED},
            {7, {?DDOC2, <<"2-c">>}, ?LEAFREV}
        ],
        Rows
    ).

t_continuous_q8({_, DbUrl}) ->
    Params = "?feed=continuous&timeout=10",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(7, Seq),
    ?assertEqual(0, Pending),
    {Seqs, Revs, _Deleted} = lists:unzip3(Rows),
    ?assertEqual(
        [
            {?DDOC2, <<"2-c">>},
            {?DOC1, <<"2-c">>},
            {?DOC3, <<"2-b">>}
        ],
        lists:sort(Revs)
    ),
    ?assertEqual(Seqs, lists:sort(Seqs)).

t_continuous_zero_timeout({_, DbUrl}) ->
    Params = "?feed=continuous&timeout=0",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(7, Seq),
    ?assertEqual(0, Pending),
    ?assertEqual(
        [
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED},
            {7, {?DDOC2, <<"2-c">>}, ?LEAFREV}
        ],
        Rows
    ).

t_longpoll({_, DbUrl}) ->
    Params = "?feed=longpoll",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(7, Seq),
    ?assertEqual(0, Pending),
    ?assertEqual(
        [
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED},
            {7, {?DDOC2, <<"2-c">>}, ?LEAFREV}
        ],
        Rows
    ).

t_limit_zero({_, DbUrl}) ->
    Params = "?limit=0",
    ?assertEqual({0, 3, []}, changes(DbUrl, Params)).

t_continuous_limit_zero({_, DbUrl}) ->
    Params = "?feed=continuous&timeout=10&limit=0",
    ?assertEqual({0, 3, []}, changes(DbUrl, Params)).

t_limit_one({_, DbUrl}) ->
    Params = "?limit=1",
    ?assertEqual(
        {5, 2, [
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV}
        ]},
        changes(DbUrl, Params)
    ).

t_limit_one_q8({_, DbUrl}) ->
    Params = "?limit=1",
    ?assertMatch(
        {_, _, [
            {_, {<<_/binary>>, <<_/binary>>}, _}
        ]},
        changes(DbUrl, Params)
    ).

t_style_all_docs({_, DbUrl}) ->
    Params = "?style=all_docs",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(7, Seq),
    ?assertEqual(0, Pending),
    ?assertEqual(
        [
            {5, {?DOC1, [<<"2-c">>, <<"2-b">>]}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED},
            {7, {?DDOC2, <<"2-c">>}, ?LEAFREV}
        ],
        Rows
    ).

t_since_now({_, DbUrl}) ->
    Params = "?since=now",
    ?assertEqual({7, 0, []}, changes(DbUrl, Params)).

t_continuous_since_now({_, DbUrl}) ->
    Params = "?feed=continuous&timeout=10&since=now",
    ?assertEqual({7, 0, []}, changes(DbUrl, Params)).

t_longpoll_since_now({_, DbUrl}) ->
    Params = "?feed=longpoll&timeout=10&since=now",
    ?assertEqual({7, 0, []}, changes(DbUrl, Params)).

t_reverse({_, DbUrl}) ->
    Params = "?descending=true",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(5, Seq),
    ?assertEqual(-3, Pending),
    ?assertEqual(
        [
            {7, {?DDOC2, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED},
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV}
        ],
        Rows
    ).

t_continuous_reverse({_, DbUrl}) ->
    Params = "?feed=continuous&timeout=10&descending=true",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(5, Seq),
    ?assertEqual(-3, Pending),
    ?assertEqual(
        [
            {7, {?DDOC2, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED},
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV}
        ],
        Rows
    ).

t_reverse_q8({_, DbUrl}) ->
    Params = "?descending=true",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(7, Seq),
    ?assertEqual(-3, Pending),
    {Seqs, Revs, _Deleted} = lists:unzip3(Rows),
    ?assertEqual(
        [
            {?DDOC2, <<"2-c">>},
            {?DOC1, <<"2-c">>},
            {?DOC3, <<"2-b">>}
        ],
        lists:sort(Revs)
    ),
    ?assertEqual(Seqs, lists:sort(Seqs)).

t_reverse_limit_zero({_, DbUrl}) ->
    Params = "?descending=true&limit=0",
    ?assertEqual({7, 0, []}, changes(DbUrl, Params)).

t_reverse_limit_one({_, DbUrl}) ->
    Params = "?descending=true&limit=1",
    ?assertEqual(
        {7, -1, [
            {7, {?DDOC2, <<"2-c">>}, ?LEAFREV}
        ]},
        changes(DbUrl, Params)
    ).

t_reverse_limit_one_q8({_, DbUrl}) ->
    Params = "?descending=true&limit=1",
    ?assertMatch(
        {7, -1, [
            {_, {<<_/binary>>, <<_/binary>>}, _}
        ]},
        changes(DbUrl, Params)
    ).

t_seq_interval({_, DbUrl}) ->
    Params = "?seq_interval=3",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(7, Seq),
    ?assertEqual(0, Pending),
    ?assertEqual(
        [
            {null, {?DOC1, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED},
            {null, {?DDOC2, <<"2-c">>}, ?LEAFREV}
        ],
        Rows
    ).

t_selector_filter({_, DbUrl}) ->
    Params = "?filter=_selector",
    Body = #{<<"selector">> => #{<<"_id">> => ?DOC1}},
    {Seq, Pending, Rows} = changes_post(DbUrl, Body, Params),
    ?assertEqual(7, Seq),
    ?assertEqual(0, Pending),
    ?assertMatch([{_, {?DOC1, <<"2-c">>}, ?LEAFREV}], Rows).

t_design_filter({_, DbUrl}) ->
    Params = "?filter=_design",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(7, Seq),
    ?assertEqual(2, Pending),
    ?assertMatch([{_, {?DDOC2, <<"2-c">>}, ?LEAFREV}], Rows).

t_docs_id_filter({_, DbUrl}) ->
    Params = "?filter=_doc_ids",
    Body = #{<<"doc_ids">> => [?DOC3, ?DOC1]},
    meck:reset(couch_changes),
    {_, _, Rows} = changes_post(DbUrl, Body, Params),
    ?assertEqual(1, meck:num_calls(couch_changes, send_changes_doc_ids, 6)),
    ?assertEqual(
        [
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED}
        ],
        Rows
    ).

t_docs_id_filter_q8({_, DbUrl}) ->
    Params = "?filter=_doc_ids",
    Body = #{<<"doc_ids">> => [?DOC3, ?DOC1]},
    {_, _, Rows} = changes_post(DbUrl, Body, Params),
    {Seqs, Revs, _Deleted} = lists:unzip3(Rows),
    ?assertEqual(
        [
            {?DOC1, <<"2-c">>},
            {?DOC3, <<"2-b">>}
        ],
        lists:sort(Revs)
    ),
    ?assertEqual(Seqs, lists:sort(Seqs)).

t_docs_id_filter_over_limit({_, DbUrl}) ->
    Params = "?filter=_doc_ids",
    Body = #{<<"doc_ids">> => [<<"missingdoc">>, ?DOC3, <<"notthere">>, ?DOC1]},
    meck:reset(couch_changes),
    {_, _, Rows} = changes_post(DbUrl, Body, Params),
    ?assertEqual(0, meck:num_calls(couch_changes, send_changes_doc_ids, 6)),
    ?assertEqual(
        [
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED}
        ],
        Rows
    ).

t_js_filter({_, DbUrl}) ->
    DDocId = "_design/filters",
    FilterFun = <<"function(doc, req) {return (doc._id == 'doc3')}">>,
    DDoc = #{<<"filters">> => #{<<"f">> => FilterFun}},
    DDocUrl = DbUrl ++ "/" ++ DDocId,
    {_, #{<<"rev">> := Rev, <<"ok">> := true}} = req(put, DDocUrl, DDoc),
    Params = "?filter=filters/f",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(8, Seq),
    ?assertEqual(0, Pending),
    ?assertEqual(
        [
            {6, {?DOC3, <<"2-b">>}, ?DELETED}
        ],
        Rows
    ),
    {200, #{}} = req(delete, DDocUrl ++ "?rev=" ++ binary_to_list(Rev)).

t_js_filter_no_match({_, DbUrl}) ->
    DDocId = "_design/filters",
    FilterFun = <<"function(doc, req) {return false}">>,
    DDoc = #{<<"filters">> => #{<<"f">> => FilterFun}},
    DDocUrl = DbUrl ++ "/" ++ DDocId,
    {_, #{<<"rev">> := Rev, <<"ok">> := true}} = req(put, DDocUrl, DDoc),
    Params = "?filter=filters/f",
    ?assertEqual({8, 0, []}, changes(DbUrl, Params)),
    {200, #{}} = req(delete, DDocUrl ++ "?rev=" ++ binary_to_list(Rev)).

t_js_filter_with_query_param({_, DbUrl}) ->
    DDocId = "_design/filters",
    FilterFun = <<"function(doc, req) {return (req.query.yup == 1)}">>,
    DDoc = #{<<"filters">> => #{<<"f">> => FilterFun}},
    DDocUrl = DbUrl ++ "/" ++ DDocId,
    {_, #{<<"rev">> := Rev, <<"ok">> := true}} = req(put, DDocUrl, DDoc),
    Params = "?filter=filters/f&yup=1",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(8, Seq),
    ?assertEqual(0, Pending),
    ?assertMatch(
        [
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV},
            {6, {?DOC3, <<"2-b">>}, ?DELETED},
            {7, {?DDOC2, <<"2-c">>}, ?LEAFREV},
            {8, {<<"_design/filters">>, <<"1-", _/binary>>}, ?LEAFREV}
        ],
        Rows
    ),
    {200, #{}} = req(delete, DDocUrl ++ "?rev=" ++ binary_to_list(Rev)).

t_view_filter({_, DbUrl}) ->
    DDocId = "_design/views",
    ViewFun = <<"function(doc) {if (doc._id == 'doc1') {emit(1, 1);}}">>,
    DDoc = #{<<"views">> => #{<<"v">> => #{<<"map">> => ViewFun}}},
    DDocUrl = DbUrl ++ "/" ++ DDocId,
    {_, #{<<"rev">> := Rev, <<"ok">> := true}} = req(put, DDocUrl, DDoc),
    Params = "?filter=_view&view=views/v",
    {Seq, Pending, Rows} = changes(DbUrl, Params),
    ?assertEqual(8, Seq),
    ?assertEqual(0, Pending),
    ?assertEqual(
        [
            {5, {?DOC1, <<"2-c">>}, ?LEAFREV}
        ],
        Rows
    ),
    {200, #{}} = req(delete, DDocUrl ++ "?rev=" ++ binary_to_list(Rev)).

t_view_filter_no_match({_, DbUrl}) ->
    DDocId = "_design/views",
    ViewFun = <<"function(doc) {if (doc._id == 'docX') {emit(1, 1);}}">>,
    DDoc = #{<<"views">> => #{<<"v">> => #{<<"map">> => ViewFun}}},
    DDocUrl = DbUrl ++ "/" ++ DDocId,
    {_, #{<<"rev">> := Rev, <<"ok">> := true}} = req(put, DDocUrl, DDoc),
    Params = "?filter=_view&view=views/v",
    ?assertEqual({8, 0, []}, changes(DbUrl, Params)),
    {200, #{}} = req(delete, DDocUrl ++ "?rev=" ++ binary_to_list(Rev)).

% Utility functions

setup_ctx(DbCreateParams) ->
    Ctx = test_util:start_couch([chttpd]),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Db = ?b2l(?tempdb()),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    ok = create_db(Url, Db, DbCreateParams),
    {Ctx, Url ++ Db}.

teardown_ctx({Ctx, DbUrl}) ->
    meck:unload(),
    delete_db(DbUrl),
    ok = config:delete("admins", ?USER, _Persist = false),
    test_util:stop_couch(Ctx).

setup_q8() ->
    {Ctx, DbUrl} = setup_ctx("?q=8"),
    ok = create_docs(DbUrl, test_docs()),
    {Ctx, DbUrl}.

setup_basic() ->
    {Ctx, DbUrl} = setup_ctx("?q=1"),
    ok = create_docs(DbUrl, test_docs()),
    CfgKey = "changes_doc_ids_optimization_threshold",
    ok = config:set("couchdb", CfgKey, "2", _Persist = false),
    meck:new(couch_changes, [passthrough]),
    {Ctx, DbUrl}.

teardown_basic({Ctx, DbUrl}) ->
    CfgKey = "changes_doc_ids_optimization_threshold",
    ok = config:delete("couchdb", CfgKey, _Persist = false),
    teardown_ctx({Ctx, DbUrl}).

create_db(Top, Db, Params) ->
    case req(put, Top ++ Db ++ Params) of
        {201, #{}} ->
            ok;
        Error ->
            error({failed_to_create_test_db, Db, Error})
    end.

delete_db(DbUrl) ->
    case req(delete, DbUrl) of
        {200, #{}} ->
            ok;
        Error ->
            error({failed_to_delete_test_db, DbUrl, Error})
    end.

doc_fun({Id, Revs, Deleted}) ->
    Doc = #{
        <<"_id">> => Id,
        <<"_revisions">> => #{
            <<"ids">> => Revs,
            <<"start">> => length(Revs)
        }
    },
    case Deleted of
        true -> Doc#{<<"_deleted">> => true};
        false -> Doc
    end.

create_docs(DbUrl, DocRevs) ->
    lists:foreach(
        fun(#{<<"_id">> := Id} = Doc) ->
            Url = DbUrl ++ "/" ++ binary_to_list(Id),
            {_, #{<<"ok">> := true}} = req(put, Url ++ "?new_edits=false", Doc)
        end,
        lists:map(fun doc_fun/1, DocRevs)
    ).

changes(DbUrl) ->
    changes(DbUrl, "").

changes_post(DbUrl, #{} = Body) ->
    changes_post(DbUrl, Body, "").

changes(DbUrl, Params) when is_list(Params) ->
    {Code, Res} = reqraw(get, DbUrl ++ "/_changes" ++ Params),
    ?assertEqual(200, Code),
    res(Res, Params).

changes_post(DbUrl, #{} = Body, Params) ->
    {Code, Res} = reqraw(post, DbUrl ++ "/_changes" ++ Params, Body),
    ?assertEqual(200, Code),
    res(Res, Params).

req(Method, Url) ->
    {Code, Res} = reqraw(Method, Url),
    {Code, json(Res)}.

req(Method, Url, #{} = Body) ->
    {Code, Res} = reqraw(Method, Url, Body),
    {Code, json(Res)}.

reqraw(Method, Url) ->
    Headers = [?JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, Res}.

reqraw(Method, Url, #{} = Body) ->
    reqraw(Method, Url, jiffy:encode(Body));
reqraw(Method, Url, Body) ->
    Headers = [?JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, Res}.

json(Bin) when is_binary(Bin) ->
    jiffy:decode(Bin, [return_maps]).

jsonl(Bin) when is_binary(Bin) ->
    Lines = [string:trim(L) || L <- binary:split(Bin, <<"\n">>, [global])],
    [json(L) || L <- Lines, L =/= <<>>].

res(<<_/binary>> = Bin, Params) ->
    Continuous = string:find(Params, "feed=continuous") =/= nomatch,
    case Continuous of
        true -> parse_response(jsonl(Bin));
        false -> parse_response(json(Bin))
    end.

parse_response(#{} = Resp) ->
    #{
        <<"last_seq">> := LastSeq,
        <<"pending">> := Pending,
        <<"results">> := Results
    } = Resp,
    Results1 = lists:map(fun parse_row/1, Results),
    {seq(LastSeq), Pending, Results1};
parse_response([#{} | _] = Lines) ->
    #{<<"pending">> := Pending, <<"last_seq">> := LastSeq} = lists:last(Lines),
    Results1 = lists:map(fun parse_row/1, lists:droplast(Lines)),
    {seq(LastSeq), Pending, Results1}.

parse_row(#{} = Row) ->
    #{
        <<"changes">> := Revs,
        <<"id">> := Id,
        <<"seq">> := Seq
    } = Row,
    Revs1 = lists:map(fun(#{<<"rev">> := Rev}) -> Rev end, Revs),
    Revs2 =
        case Revs1 of
            [Rev] -> Rev;
            [_ | _] -> Revs1
        end,
    case maps:get(<<"deleted">>, Row, false) of
        true -> {seq(Seq), {Id, Revs2}, ?DELETED};
        false -> {seq(Seq), {Id, Revs2}, ?LEAFREV}
    end.

% This will be reliable for q=1 dbs only.
%
seq(<<_/binary>> = Seq) ->
    [NumStr, _] = binary:split(Seq, <<"-">>),
    binary_to_integer(NumStr);
seq(null) ->
    null.
