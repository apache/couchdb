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

-module(chttpd_purge_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Db = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    DbUrl = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(Db)]),
    create_db(DbUrl),
    DbUrl.

teardown(DbUrl) ->
    delete_db(DbUrl),
    ok = config:delete("admins", ?USER, _Persist = false).

purge_test_() ->
    {
        "chttpd purge tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_purge_only_post_allowed),
                    ?TDEF_FE(t_empty_purge_request),
                    ?TDEF_FE(t_ok_purge_request),
                    ?TDEF_FE(t_ok_purge_with_max_document_id_number),
                    ?TDEF_FE(t_accepted_purge_request),
                    ?TDEF_FE(t_partial_purge_request),
                    ?TDEF_FE(t_mixed_purge_request),
                    ?TDEF_FE(t_over_many_ids_or_revs_purge_request),
                    ?TDEF_FE(t_purged_infos_limit_only_get_put_allowed),
                    ?TDEF_FE(t_exceed_limits_on_purge_infos),
                    ?TDEF_FE(t_should_error_set_purged_docs_limit_to_0),
                    ?TDEF_FE(t_timeout_set_purged_infos_limit),
                    ?TDEF_FE(t_purged_infos_only_get_allowed),
                    ?TDEF_FE(t_empty_purged_infos),
                    ?TDEF_FE(t_purged_infos_after_purge_request),
                    ?TDEF_FE(t_purged_infos_after_multiple_purge_requests)
                ]
            }
        }
    }.

t_purge_only_post_allowed(DbUrl) ->
    {Status, Response} = req(put, url(DbUrl, "_purge")),
    ?assertMatch(#{<<"reason">> := <<"Only POST allowed">>}, Response),
    ?assert(Status =:= 405).

t_empty_purge_request(DbUrl) ->
    {Status, Response} = req(post, url(DbUrl, "_purge"), #{}),
    ?assertMatch(#{<<"purge_seq">> := null, <<"purged">> := #{}}, Response),
    ?assert(Status =:= 201 orelse Status =:= 202).

t_ok_purge_request(DbUrl) ->
    {201, Response1} = create_docs(DbUrl, docs(3)),
    IdsRevs = ids_revs(Response1),
    {Status, Response2} = req(post, url(DbUrl, "_purge"), IdsRevs),
    ?assertMatch(#{<<"purge_seq">> := null, <<"purged">> := IdsRevs}, Response2),
    ?assert(Status =:= 201 orelse Status =:= 202).

t_ok_purge_with_max_document_id_number(DbUrl) ->
    PurgedDocsNum = 101,
    {201, Response1} = create_docs(DbUrl, docs(PurgedDocsNum)),
    IdsRevs = ids_revs(Response1),

    {400, #{<<"reason">> := Error}} = req(post, url(DbUrl, "_purge"), IdsRevs),
    ?assertEqual(<<"Exceeded maximum number of documents.">>, Error),

    ok = config:set("purge", "max_document_id_number", "101", _Persist = false),
    try
        {Status, Response2} = req(post, url(DbUrl, "_purge"), IdsRevs),
        ?assertMatch(#{<<"purge_seq">> := null, <<"purged">> := IdsRevs}, Response2),
        ?assert(Status =:= 201 orelse Status =:= 202)
    after
        ok = config:delete("purge", "max_document_id_number", _Persist)
    end.

t_accepted_purge_request(DbUrl) ->
    try
        meck:new(fabric, [passthrough]),
        meck:expect(fabric, purge_docs, fun(_, _, _) ->
            {accepted, [
                {accepted, [
                    {1, <<187, 82, 160, 135, 14, 97, 52, 47, 28, 172, 13, 249, 96, 182, 127, 97>>}
                ]}
            ]}
        end),
        {_, IdsRevs} = get_id_rev_map(DbUrl, "doc1"),
        {Status, Response} = req(post, url(DbUrl, "_purge"), IdsRevs),
        ?assertMatch(#{<<"purge_seq">> := null, <<"purged">> := IdsRevs}, Response),
        ?assert(Status =:= 202)
    after
        meck:unload(fabric)
    end.

t_partial_purge_request(DbUrl) ->
    IdsRevs = create_and_update_doc(DbUrl, "doc1"),
    {Status1, Response1} = req(post, url(DbUrl, "_purge"), IdsRevs),
    ?assertMatch(#{<<"purge_seq">> := null, <<"purged">> := IdsRevs}, Response1),
    ?assert(Status1 =:= 201 orelse Status1 =:= 202),
    {Status2, #{<<"content">> := Content}} = req(get, url(DbUrl, "doc1")),
    ?assertEqual(<<"updated">>, Content),
    ?assert(Status2 =:= 200).

t_mixed_purge_request(DbUrl) ->
    Doc1IdRevs = create_and_update_doc(DbUrl, "doc1"),
    [Rev1] = maps:get(?l2b("doc1"), Doc1IdRevs),
    get_id_rev_map(DbUrl, "doc2"),
    {Rev3, _} = get_id_rev_map(DbUrl, "doc3"),

    IdsRevs = #{
        % partial purge
        <<"doc1">> => [Rev1],
        % correct format, but invalid rev
        <<"doc2">> => [Rev1, Rev3],
        % correct format and rev
        <<"doc3">> => [Rev3]
    },

    {Status1, Response} = req(post, url(DbUrl, "_purge"), IdsRevs),
    ?assertMatch(
        #{
            <<"purge_seq">> := null,
            <<"purged">> := #{<<"doc1">> := [Rev1], <<"doc2">> := [], <<"doc3">> := [Rev3]}
        },
        Response
    ),
    ?assert(Status1 =:= 201 orelse Status1 =:= 202),

    {Status2, #{<<"content">> := Content}} = req(get, url(DbUrl, "doc1")),
    ?assertEqual(<<"updated">>, Content),
    ?assert(Status2 =:= 200).

t_over_many_ids_or_revs_purge_request(DbUrl) ->
    Doc1IdRevs = create_and_update_doc(DbUrl, "doc1"),
    [Rev1] = maps:get(?l2b("doc1"), Doc1IdRevs),
    get_id_rev_map(DbUrl, "doc2"),
    {Rev3, _} = get_id_rev_map(DbUrl, "doc3"),

    IdsRevs = #{
        % partial purge
        <<"doc1">> => [Rev1],
        % correct format, but invalid rev
        <<"doc2">> => [Rev1, Rev3],
        % correct format and rev
        <<"doc3">> => [Rev3]
    },

    % Ids larger than expected
    config:set("purge", "max_document_id_number", "1", _Persist = false),
    try
        {Status1, #{<<"reason">> := Error1}} = req(post, url(DbUrl, "_purge"), IdsRevs),
        ?assertEqual(<<"Exceeded maximum number of documents.">>, Error1),
        ?assertEqual(400, Status1)
    after
        config:delete("purge", "max_document_id_number", _Persist)
    end,

    % Revs larger than expected
    config:set("purge", "max_revisions_number", "1", _Persist),
    try
        {Status2, #{<<"reason">> := Error2}} = req(post, url(DbUrl, "_purge"), IdsRevs),
        ?assertEqual(<<"Exceeded maximum number of revisions.">>, Error2),
        ?assertEqual(400, Status2)
    after
        config:delete("purge", "max_revisions_number", _Persist)
    end.

t_purged_infos_limit_only_get_put_allowed(DbUrl) ->
    {Status, Response} = req(post, url(DbUrl, "_purged_infos_limit"), "2"),
    ?assertMatch(#{<<"reason">> := <<"Only GET,PUT allowed">>}, Response),
    ?assert(Status =:= 405).

t_exceed_limits_on_purge_infos(DbUrl) ->
    {200, _} = req(put, url(DbUrl, "_purged_infos_limit"), "2"),
    {201, Response1} = create_docs(DbUrl, docs(3)),
    IdsRevs = ids_revs(Response1),

    {Status2, Response2} = req(post, url(DbUrl, "_purge"), IdsRevs),
    ?assertMatch(#{<<"purge_seq">> := null, <<"purged">> := IdsRevs}, Response2),
    ?assert(Status2 =:= 201 orelse Status2 =:= 202).

t_should_error_set_purged_docs_limit_to_0(DbUrl) ->
    {Status, Res} = req(put, url(DbUrl, "_purged_infos_limit"), "0"),
    ?assertMatch(
        #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"`purged_infos_limit` must be positive integer">>
        },
        Res
    ),
    ?assert(Status =:= 400).

t_timeout_set_purged_infos_limit(DbUrl) ->
    try
        meck:new(fabric, [passthrough]),
        meck:expect(fabric, set_purge_infos_limit, fun(_, _, _) -> {error, timeout} end),
        {Status, #{<<"reason">> := Error}} = req(put, url(DbUrl, "_purged_infos_limit"), "2"),
        ?assertEqual(<<"timeout">>, Error),
        ?assert(Status =:= 500)
    after
        meck:unload(fabric)
    end.

t_purged_infos_only_get_allowed(DbUrl) ->
    {Status, Response} = req(post, url(DbUrl, "_purged_infos")),
    ?assertMatch(#{<<"reason">> := <<"Only GET allowed">>}, Response),
    ?assert(Status =:= 405).

t_empty_purged_infos(DbUrl) ->
    {Status, Response} = req(get, url(DbUrl, "_purged_infos")),
    ?assertMatch(#{<<"purged_infos">> := []}, Response),
    ?assert(Status =:= 200).

t_purged_infos_after_purge_request(DbUrl) ->
    PurgedDocsNum = 3,
    {Status1, Response1} = create_docs(DbUrl, docs(PurgedDocsNum)),
    ?assert(Status1 =:= 201 orelse Status1 =:= 202),
    IdsRevs = ids_revs(Response1),

    {Status2, Response2} = req(post, url(DbUrl, "_purge"), IdsRevs),
    ?assert(Status2 =:= 201 orelse Status2 =:= 202),
    ?assertMatch(#{<<"purge_seq">> := null, <<"purged">> := IdsRevs}, Response2),

    {Status3, Response3} = req(get, url(DbUrl, "_purged_infos")),
    ?assert(Status3 =:= 200),
    ?assertMatch(#{<<"purged_infos">> := [_ | _]}, Response3),
    Info = maps:get(<<"purged_infos">>, Response3),
    ?assertEqual(3, length(Info)).

t_purged_infos_after_multiple_purge_requests(DbUrl) ->
    {Rev1, Doc1IdRevs} = get_id_rev_map(DbUrl, "doc1"),
    {Rev2, Doc2IdRevs} = get_id_rev_map(DbUrl, "doc2"),
    {201, #{<<"rev">> := Rev1New}} =
        req(put, url(DbUrl, "doc1?rev=" ++ ?b2l(Rev1)), #{<<"val">> => <<"updated">>}),
    req(post, url(DbUrl, "_purge"), Doc1IdRevs),
    req(post, url(DbUrl, "_purge"), Doc2IdRevs),
    req(post, url(DbUrl, "_purge"), #{<<"doc1">> => [Rev1New]}),
    {Status, #{<<"purged_infos">> := IdRevsList}} = req(get, url(DbUrl, "_purged_infos")),
    ?assert(lists:member(#{<<"id">> => <<"doc1">>, <<"revs">> => [Rev1]}, IdRevsList)),
    ?assert(lists:member(#{<<"id">> => <<"doc1">>, <<"revs">> => [Rev1New]}, IdRevsList)),
    ?assert(lists:member(#{<<"id">> => <<"doc2">>, <<"revs">> => [Rev2]}, IdRevsList)),
    ?assert(Status =:= 200).

%%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%%%%
url(Url, Path) ->
    Url ++ "/" ++ Path.

create_db(Url) ->
    case req(put, Url) of
        {201, #{}} -> ok;
        Error -> error({failed_to_create_test_db, Error})
    end.

delete_db(Url) ->
    case req(delete, Url) of
        {200, #{}} -> ok;
        Error -> error({failed_to_delete_test_db, Error})
    end.

create_doc(Url, Id) ->
    req(put, url(Url, Id), #{<<"val">> => ?l2b(Id)}).

create_docs(Url, Docs) ->
    req(post, url(Url, "_bulk_docs"), #{<<"docs">> => Docs}).

docs(Counter) ->
    lists:foldl(
        fun(I, Acc) ->
            Id = ?l2b(integer_to_list(I)),
            Doc = #{<<"_id">> => Id, <<"val">> => I},
            [Doc | Acc]
        end,
        [],
        lists:seq(1, Counter)
    ).

ids_revs(Response) ->
    IdsRevs = lists:map(
        fun(DocResp) ->
            Id = maps:get(<<"id">>, DocResp),
            Rev = maps:get(<<"rev">>, DocResp),
            {Id, [Rev]}
        end,
        Response
    ),
    maps:from_list(IdsRevs).

get_id_rev_map(Url, Id) ->
    {_, #{<<"rev">> := Rev}} = create_doc(Url, Id),
    {Rev, #{?l2b(Id) => [Rev]}}.

create_and_update_doc(Url, Id) ->
    {Rev, IdRevs} = get_id_rev_map(Url, Id),
    NewDoc = new_doc(Rev),
    {201, _} = req(post, url(Url, "_bulk_docs"), NewDoc),
    IdRevs.

new_doc(Rev) ->
    #{
        <<"new_edits">> => false,
        <<"docs">> => [
            #{
                <<"_id">> => <<"doc1">>,
                <<"_revisions">> => #{
                    <<"start">> => 1,
                    <<"ids">> => [<<"12345">>, <<"67890">>]
                },
                <<"content">> => <<"updated">>,
                <<"_rev">> => Rev
            }
        ]
    }.

req(Method, Url) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, jiffy:decode(Res, [return_maps])}.

req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.
