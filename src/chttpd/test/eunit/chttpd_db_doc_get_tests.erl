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

-module(chttpd_db_doc_get_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(USER, "chttpd_db_doc_get_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(JSON, "application/json").
-define(JSON_CT, {"Content-Type", ?JSON}).
-define(ACCEPT_JSON, {"Accept", ?JSON}).
-define(ACCEPT_MP, {"Accept", "multipart/mixed"}).

-define(DOC, <<"doc">>).

-define(REVA, <<"reva">>).
-define(REVB, <<"revb">>).
-define(REVC, <<"revc">>).

-define(ATTA, <<"atta">>).
-define(ATTB, <<"attb">>).
-define(ATTC, <<"attc">>).

-define(CTYPE, <<"application/octet-stream">>).
-define(DATA, <<"dGhlZGF0YQ==">>).

-define(DISPOSITION, <<"content-disposition">>).
-define(CONTENT_TYPE, <<"content-type">>).

%
%  reva (atta)
%    |\
%    | +-revb (attb)
%    |
%     \
%      +-recvc (attc) [deleted]
%
-define(TEST_DOCS, [
    #{
        <<"_id">> => ?DOC,
        <<"_revisions">> => #{<<"start">> => 1, <<"ids">> => [?REVA]},
        <<"_attachments">> => #{
            ?ATTA => #{
                <<"revpos">> => 1,
                <<"content_type">> => ?CTYPE,
                <<"data">> => ?DATA
            }
        }
    },
    #{
        <<"_id">> => ?DOC,
        <<"_revisions">> => #{<<"start">> => 2, <<"ids">> => [?REVB, ?REVA]},
        <<"_attachments">> => #{
            ?ATTA => #{
                <<"revpos">> => 1,
                <<"content_type">> => ?CTYPE,
                <<"data">> => ?DATA
            },
            ?ATTB => #{
                <<"revpos">> => 2,
                <<"content_type">> => ?CTYPE,
                <<"data">> => ?DATA
            }
        }
    },
    #{
        <<"_id">> => ?DOC,
        <<"_revisions">> => #{<<"start">> => 2, <<"ids">> => [?REVC, ?REVA]},
        <<"_attachments">> => #{
            ?ATTA => #{
                <<"revpos">> => 1,
                <<"content_type">> => ?CTYPE,
                <<"data">> => ?DATA
            },
            ?ATTC => #{
                <<"revpos">> => 2,
                <<"content_type">> => ?CTYPE,
                <<"data">> => ?DATA
            }
        },
        <<"_deleted">> => true
    }
]).

doc_get_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        with([
            ?TDEF(t_basic),
            ?TDEF(t_missing_doc),
            ?TDEF(t_invalid_rev),
            ?TDEF(t_missing_rev),
            ?TDEF(t_doc_all_revs),
            ?TDEF(t_mp_doc_all_revs),
            ?TDEF(t_specific_rev),
            ?TDEF(t_specific_rev_latest),
            ?TDEF(t_ancestor_rev_latest),
            ?TDEF(t_ancestor_open_revs_latest),
            ?TDEF(t_open_revs_latest),
            ?TDEF(t_mp_open_revs_latest),
            ?TDEF(t_revs_true),
            ?TDEF(t_attachments_true),
            ?TDEF(t_atts_since_ancestor),
            ?TDEF(t_atts_since_latest),
            ?TDEF(t_atts_since_unknown),
            ?TDEF(t_invalid_atts_since),
            ?TDEF(t_mp_atts_since)
        ])
    }.

t_basic({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC),
    ?assertEqual(200, Code),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{?ATTA := #{<<"revpos">> := 1}, ?ATTB := #{<<"revpos">> := 2}}
        },
        Res
    ).

t_missing_doc({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, "missing"),
    ?assertEqual(404, Code),
    ?assertMatch(
        #{
            <<"error">> := <<"not_found">>,
            <<"reason">> := <<"missing">>
        },
        Res
    ).

t_invalid_rev({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?rev=a"),
    ?assertEqual(400, Code),
    ?assertMatch(
        #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Invalid rev format">>
        },
        Res
    ).

t_missing_rev({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?rev=1-z"),
    ?assertEqual(404, Code),
    ?assertMatch(
        #{
            <<"error">> := <<"not_found">>,
            <<"reason">> := <<"missing">>
        },
        Res
    ).

t_doc_all_revs({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?open_revs=all"),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"ok">> := #{
                    <<"_id">> := ?DOC,
                    <<"_rev">> := <<"2-revb">>,
                    <<"_attachments">> := #{
                        ?ATTA := #{<<"stub">> := true, <<"revpos">> := 1},
                        ?ATTB := #{<<"stub">> := true, <<"revpos">> := 2}
                    }
                }
            },
            #{
                <<"ok">> := #{
                    <<"_id">> := ?DOC,
                    <<"_rev">> := <<"2-revc">>,
                    <<"_attachments">> := #{
                        ?ATTA := #{<<"stub">> := true, <<"revpos">> := 1},
                        ?ATTC := #{<<"stub">> := true, <<"revpos">> := 2}
                    }
                }
            }
        ],
        Res
    ).

t_mp_doc_all_revs({_, DbUrl}) ->
    {Code, Res} = get_doc_mp(DbUrl, ?DOC, "?open_revs=all"),
    ?assertEqual(200, Code),
    % Two main doc revisions, each with a map chunk header
    % and then main doc body followed by two attachments
    ?assertMatch([{#{}, [_, _, _]}, {#{}, [_, _, _]}], Res),
    [{Headers1, [Doc1, Att1B, Att1A]}, {Headers2, [Doc2, Att2C, Att2A]}] = Res,

    % Chunk headers
    ?assertMatch(#{?CONTENT_TYPE := <<"multipart/related", _/binary>>}, Headers1),
    ?assertMatch(#{?CONTENT_TYPE := <<"multipart/related", _/binary>>}, Headers2),

    % Doc bodies - doc1
    {DocHeaders1, DocBody1} = Doc1,
    ?assertMatch(#{?CONTENT_TYPE := <<?JSON>>}, DocHeaders1),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATTA := #{<<"follows">> := true}
            }
        },
        DocBody1
    ),

    % Doc bodies - doc2
    {DocHeaders2, DocBody2} = Doc2,
    ?assertMatch(#{?CONTENT_TYPE := <<?JSON>>}, DocHeaders2),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revc">>,
            <<"_attachments">> := #{
                ?ATTA := #{<<"follows">> := true}
            }
        },
        DocBody2
    ),

    % Doc1 reva, revb attachments
    {Att1AHeaders, Att1AData} = Att1A,
    ?assertMatch(#{?DISPOSITION := <<"attachment; filename=\"atta\"">>}, Att1AHeaders),
    ?assertEqual(<<"thedata">>, Att1AData),
    {Att1BHeaders, AttB1Data} = Att1B,
    ?assertMatch(#{?DISPOSITION := <<"attachment; filename=\"attb\"">>}, Att1BHeaders),
    ?assertEqual(<<"thedata">>, AttB1Data),

    % Doc2 reva, recvc attachments
    {Att2AHeaders, Att2AData} = Att2A,
    ?assertMatch(#{?DISPOSITION := <<"attachment; filename=\"atta\"">>}, Att2AHeaders),
    ?assertEqual(<<"thedata">>, Att2AData),
    {Att2CHeaders, Att2CData} = Att2C,
    ?assertMatch(#{?DISPOSITION := <<"attachment; filename=\"attc\"">>}, Att2CHeaders),
    ?assertEqual(<<"thedata">>, Att2CData).

t_specific_rev({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?rev=2-revb"),
    ?assertEqual(200, Code),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{?ATTA := #{<<"revpos">> := 1}, ?ATTB := #{<<"revpos">> := 2}}
        },
        Res
    ).

t_specific_rev_latest({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?rev=2-revb&latest=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{?ATTA := #{<<"revpos">> := 1}, ?ATTB := #{<<"revpos">> := 2}}
        },
        Res
    ).

t_ancestor_rev_latest({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?rev=1-reva&latest=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{?ATTA := #{<<"revpos">> := 1}, ?ATTB := #{<<"revpos">> := 2}}
        },
        Res
    ).

t_ancestor_open_revs_latest({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?open_revs=[\"1-reva\"]&latest=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"ok">> := #{
                    <<"_id">> := ?DOC,
                    <<"_rev">> := <<"2-revb">>,
                    <<"_attachments">> := #{
                        ?ATTA := #{<<"stub">> := true, <<"revpos">> := 1},
                        ?ATTB := #{<<"stub">> := true, <<"revpos">> := 2}
                    }
                }
            },
            #{
                <<"ok">> := #{
                    <<"_id">> := ?DOC,
                    <<"_rev">> := <<"2-revc">>,
                    <<"_attachments">> := #{
                        ?ATTA := #{<<"stub">> := true, <<"revpos">> := 1},
                        ?ATTC := #{<<"stub">> := true, <<"revpos">> := 2}
                    }
                }
            }
        ],
        Res
    ).

t_open_revs_latest({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?open_revs=[\"2-revc\"]&latest=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"ok">> := #{
                    <<"_id">> := ?DOC,
                    <<"_rev">> := <<"2-revc">>,
                    <<"_attachments">> := #{
                        ?ATTA := #{<<"stub">> := true, <<"revpos">> := 1},
                        ?ATTC := #{<<"stub">> := true, <<"revpos">> := 2}
                    }
                }
            }
        ],
        Res
    ).

t_mp_open_revs_latest({_, DbUrl}) ->
    {Code, Res} = get_doc_mp(DbUrl, ?DOC, "?open_revs=[\"2-revc\"]&latest=true"),
    ?assertEqual(200, Code),
    % One doc only, with a map chunk header, then a doc with two attachments
    ?assertMatch([{#{}, [_, _, _]}], Res),
    [{Headers, [Doc, AttC, AttA]}] = Res,
    ?assertMatch(#{?CONTENT_TYPE := <<"multipart/related", _/binary>>}, Headers),
    {DocHeaders, DocBody} = Doc,
    ?assertMatch(#{?CONTENT_TYPE := <<?JSON>>}, DocHeaders),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revc">>,
            <<"_attachments">> := #{
                ?ATTA := #{<<"follows">> := true},
                ?ATTC := #{<<"follows">> := true}
            }
        },
        DocBody
    ),
    {AttAHeaders, AttAData} = AttA,
    ?assertMatch(#{?DISPOSITION := <<"attachment; filename=\"atta\"">>}, AttAHeaders),
    ?assertEqual(<<"thedata">>, AttAData),
    {AttCHeaders, AttCData} = AttC,
    ?assertMatch(#{?DISPOSITION := <<"attachment; filename=\"attc\"">>}, AttCHeaders),
    ?assertEqual(<<"thedata">>, AttCData).

t_revs_true({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?revs=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_revisions">> := #{<<"ids">> := [?REVB, ?REVA], <<"start">> := 2},
            <<"_attachments">> := #{?ATTA := #{<<"revpos">> := 1}, ?ATTB := #{<<"revpos">> := 2}}
        },
        Res
    ).

t_attachments_true({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?attachments=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATTA := #{<<"revpos">> := 1, <<"data">> := ?DATA},
                ?ATTB := #{<<"revpos">> := 2, <<"data">> := ?DATA}
            }
        },
        Res
    ).

t_atts_since_ancestor({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?atts_since=[\"1-reva\"]"),
    ?assertEqual(200, Code),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATTA := #{<<"revpos">> := 1, <<"stub">> := true},
                ?ATTB := #{<<"revpos">> := 2, <<"data">> := ?DATA}
            }
        },
        Res
    ).

t_atts_since_latest({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?atts_since=[\"2-revb\"]"),
    ?assertEqual(200, Code),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATTA := #{<<"revpos">> := 1, <<"stub">> := true},
                ?ATTB := #{<<"revpos">> := 2, <<"stub">> := true}
            }
        },
        Res
    ).

t_atts_since_unknown({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?atts_since=[\"0-x\"]"),
    ?assertEqual(200, Code),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATTA := #{<<"revpos">> := 1, <<"data">> := ?DATA},
                ?ATTB := #{<<"revpos">> := 2, <<"data">> := ?DATA}
            }
        },
        Res
    ).

t_invalid_atts_since({_, DbUrl}) ->
    {Code, Res} = get_doc(DbUrl, ?DOC, "?atts_since=badsince"),
    ?assertEqual(400, Code),
    ?assertMatch(#{<<"error">> := <<"bad_request">>}, Res).

t_mp_atts_since({_, DbUrl}) ->
    {Code, Res} = get_doc_mp(DbUrl, ?DOC, "?open_revs=[\"2-revb\"]&atts_since=[\"1-reva\"]"),
    ?assertEqual(200, Code),
    % One doc only, with a map chunk header, then a doc with two attachments
    ?assertMatch([{#{}, [_, _]}], Res),
    [{Headers, [Doc, Att]}] = Res,
    ?assertMatch(#{?CONTENT_TYPE := <<"multipart/related", _/binary>>}, Headers),
    {DocHeaders, DocBody} = Doc,
    ?assertMatch(#{?CONTENT_TYPE := <<?JSON>>}, DocHeaders),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATTA := #{<<"stub">> := true, <<"revpos">> := 1},
                ?ATTB := #{<<"follows">> := true, <<"revpos">> := 2}
            }
        },
        DocBody
    ),
    {AttHeaders, AttData} = Att,
    ?assertMatch(#{?DISPOSITION := <<"attachment; filename=\"attb\"">>}, AttHeaders),
    ?assertEqual(<<"thedata">>, AttData).

% Utility functions

setup_ctx() ->
    Ctx = test_util:start_couch([chttpd]),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    HashedList = binary_to_list(Hashed),
    ok = config:set("admins", ?USER, HashedList, false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Db = binary_to_list(?tempdb()),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    ok = create_db(Url, Db),
    {Ctx, Url, Db}.

teardown({Ctx, DbUrl}) ->
    delete_db(DbUrl),
    Persist = false,
    ok = config:delete("admins", ?USER, Persist),
    test_util:stop_couch(Ctx).

setup() ->
    {Ctx, Url, Db} = setup_ctx(),
    DbUrl = Url ++ Db,
    create_docs(DbUrl),
    {Ctx, DbUrl}.

create_db(Top, Db) ->
    case req(put, Top ++ Db) of
        {201, #{}} -> ok;
        Error -> error({failed_to_create_test_db, Db, Error})
    end.

delete_db(DbUrl) ->
    case req(delete, DbUrl) of
        {200, #{}} -> ok;
        Error -> error({failed_to_delete_test_db, DbUrl, Error})
    end.

create_docs(DbUrl) ->
    Body = #{<<"docs">> => ?TEST_DOCS, <<"new_edits">> => false},
    {Code, Res} = req(post, DbUrl ++ "/_bulk_docs", Body),
    ?assertEqual(201, Code),
    ?assertEqual([], Res).

get_doc(DbUrl, DocId) ->
    get_doc(DbUrl, DocId, "").

get_doc(DbUrl, <<DocId/binary>>, Params) ->
    get_doc(DbUrl, binary_to_list(DocId), Params);
get_doc(DbUrl, DocId, Params) ->
    Url = DbUrl ++ "/" ++ DocId ++ Params,
    req(get, Url).

req(Method, Url) ->
    Headers = [?JSON_CT, ?AUTH, ?ACCEPT_JSON],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, json_decode(Res)}.

req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?JSON_CT, ?AUTH, ?ACCEPT_JSON],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, json_decode(Res)}.

% Handle multipart

get_doc_mp(DbUrl, <<DocId/binary>>, Params) ->
    get_doc_mp(DbUrl, binary_to_list(DocId), Params);
get_doc_mp(DbUrl, DocId, Params) ->
    Url = DbUrl ++ "/" ++ DocId ++ Params,
    {Code, Res} = get_mp(Url, ?ACCEPT_MP),
    {Code, Res}.

get_mp(Url, MpType) ->
    Headers = [?JSON_CT, ?AUTH, MpType],
    {ok, Code, ResHeaders, Res} = test_request:request(get, Url, Headers),
    CType = header_value("Content-Type", ResHeaders),
    case CType of
        ?JSON ->
            {Code, json_decode(Res)};
        "multipart/" ++ _ ->
            Chunks = split(Res, CType),
            {Code, lists:map(fun chunk_parse_fun/1, Chunks)}
    end.

% In a multipart response, each chunk would have its own headers, content type,
% and potentially nested parts with their own multipart encoding
%
chunk_parse_fun(Chunk) when is_binary(Chunk) ->
    {Headers, Body} = parse_headers_and_body(Chunk),
    #{?CONTENT_TYPE := CType} = Headers,
    case CType of
        <<?JSON, _/binary>> ->
            {Headers, json_decode(Body)};
        <<"multipart/", _/binary>> ->
            {Headers, lists:map(fun chunk_parse_fun/1, split(Body, CType))};
        _ ->
            {Headers, Body}
    end.

% Split the binary into parts based on the provided boundary. The splitting is
% naive, after a basic binary:split/3 we have to do some cleanups and remove a
% few trailing bits off the start and end.
%
split(Chunk, CType) ->
    Boundary = get_boundary(CType),
    Parts = binary:split(Chunk, <<"--", Boundary/binary>>, [global]),
    Parts1 = [string:trim(P) || P <- Parts],
    [P || P <- Parts1, P =/= <<>> andalso P =/= <<"--">>].

% Parse the headers and body from a binary chunk. This does just enough to
% parse things out for the test and is not a full featured multipart parser
%
parse_headers_and_body(Bin) ->
    [HeadersBin, BodyBin] = binary:split(Bin, <<"\r\n\r\n">>),
    HeaderLines = binary:split(HeadersBin, <<"\r\n">>, [global, trim_all]),
    MapFun = fun(Header) ->
        [Name, Val] = binary:split(Header, <<":">>),
        {string:lowercase(string:trim(Name)), string:trim(Val)}
    end,
    {maps:from_list(lists:map(MapFun, HeaderLines)), BodyBin}.

header_value(Key, Headers) ->
    header_value(Key, Headers, undefined).

header_value(Key, Headers, Default) ->
    Headers1 = [{string:to_lower(K), V} || {K, V} <- Headers],
    case lists:keyfind(string:to_lower(Key), 1, Headers1) of
        {_, Value} -> Value;
        _ -> Default
    end.

get_boundary(CType) when is_binary(CType) ->
    get_boundary(binary_to_list(CType));
get_boundary(CType) when is_list(CType) ->
    case mochiweb_util:parse_header(CType) of
        {"multipart/" ++ _, HeaderOpts} ->
            case couch_util:get_value("boundary", HeaderOpts) of
                undefined -> undefined;
                B when is_list(B) -> iolist_to_binary(B)
            end;
        _ ->
            undefined
    end.

json_decode(Bin) when is_binary(Bin) ->
    jiffy:decode(Bin, [return_maps]).
