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

-module(chttpd_bulk_get_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").

-define(USER, "chttpd_bulk_get_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(JSON, {"Content-Type", "application/json"}).
-define(MP_MIXED, {"Accept", "multipart/mixed"}).
-define(MP_RELATED, {"Accept", "multipart/related"}).

-define(DOC, <<"doc">>).
-define(REVA, <<"reva">>).
-define(REVB, <<"revb">>).
-define(REVC, <<"revc">>).
-define(ATT, <<"att">>).
-define(ATT_DATA, <<"dGhlZGF0YQ==">>).

-define(DOC_COUNT, 2000).

-define(BULK_GET_TESTS, [
    ?TDEF(t_invalid_method),
    ?TDEF(t_empty_request),
    ?TDEF(t_no_docs),
    ?TDEF(t_invalid_query_params),
    ?TDEF(t_invalid_doc),
    ?TDEF(t_doc_no_id),
    ?TDEF(t_invalid_doc_id),
    ?TDEF(t_missing_doc),
    ?TDEF(t_invalid_rev),
    ?TDEF(t_missing_rev),
    ?TDEF(t_doc_all_revs),
    ?TDEF(t_specific_rev),
    ?TDEF(t_specific_rev_latest),
    ?TDEF(t_ancestor_rev_latest),
    ?TDEF(t_revs_true),
    ?TDEF(t_attachments_true),
    ?TDEF(t_atts_since),
    ?TDEF(t_invalid_atts_since),
    ?TDEF(t_invalid_atts_since_invalid_rev),
    ?TDEF(t_atts_since_returns_attachment),
    ?TDEF(t_atts_since_overrides_attachments_true),
    ?TDEF(t_atts_since_multiple),
    ?TDEF(t_atts_since_multiple_attachments_true),
    ?TDEF(t_missing_rev_latest),
    ?TDEF(t_fabric_worker_error)
]).

-define(BULK_GET_MULTIPART_TESTS, [
    ?TDEF(t_mp_empty_request),
    ?TDEF(t_mp_no_docs),
    ?TDEF(t_mp_invalid_doc),
    ?TDEF(t_mp_doc_no_id),
    ?TDEF(t_mp_invalid_doc_id),
    ?TDEF(t_mp_missing_doc),
    ?TDEF(t_mp_invalid_rev),
    ?TDEF(t_mp_missing_rev),
    ?TDEF(t_mp_doc_all_revs),
    ?TDEF(t_mp_specific_rev),
    ?TDEF(t_mp_specific_rev_multipart_related),
    ?TDEF(t_mp_revs_true),
    ?TDEF(t_mp_atts_since),
    ?TDEF(t_mp_atts_since_returns_attachment),
    ?TDEF(t_mp_atts_since_overrides_attachments_true)
]).

test_docs_revs() ->
    [
        {?DOC, [?REVA]},
        {?DOC, [?REVB, ?REVA]},
        {?DOC, [?REVC, ?REVA]}
    ].

bulk_get_test_() ->
    {
        setup,
        fun setup_basic/0,
        fun teardown/1,
        with(?BULK_GET_TESTS)
    }.

bulk_get_multipart_test_() ->
    {
        setup,
        fun setup_basic/0,
        fun teardown/1,
        with(?BULK_GET_MULTIPART_TESTS)
    }.

bulk_get_test_no_batches_test_() ->
    {
        setup,
        fun setup_no_batches/0,
        fun teardown_no_batches/1,
        with(?BULK_GET_TESTS)
    }.

bulk_get_multipart_no_batches_test_() ->
    {
        setup,
        fun setup_no_batches/0,
        fun teardown_no_batches/1,
        with(?BULK_GET_MULTIPART_TESTS)
    }.

bulk_get_multiple_docs_test_() ->
    {
        foreach,
        fun setup_multiple/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_multiple_docs, 10),
            ?TDEF_FE(t_mp_multiple_docs, 10)
        ]
    }.

bulk_get_multiple_docs_no_batches_test_() ->
    {
        foreach,
        fun setup_multiple_no_batches/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_multiple_docs, 10),
            ?TDEF_FE(t_mp_multiple_docs, 10)
        ]
    }.

t_invalid_method({_, DbUrl}) ->
    ?assertMatch({405, _}, req(put, DbUrl ++ "/_bulk_get", #{})).

t_empty_request({_, DbUrl}) ->
    {Code, Res} = bulk_get(DbUrl, []),
    ?assertEqual(200, Code),
    ?assertEqual([], Res).

t_mp_empty_request({_, DbUrl}) ->
    {Code, Res} = bulk_get_mp(DbUrl, []),
    ?assertEqual(200, Code),
    ?assertEqual([], Res).

t_no_docs({_, DbUrl}) ->
    {Code, Res} = req(post, DbUrl ++ "/_bulk_get", #{}),
    ?assertEqual(400, Code),
    ?assertMatch(#{<<"error">> := <<"bad_request">>}, Res).

t_mp_no_docs({_, DbUrl}) ->
    {Code, Res} = req_mp(post, DbUrl ++ "/_bulk_get", #{}, ?MP_MIXED),
    ?assertEqual(400, Code),
    ?assertMatch(#{<<"error">> := <<"bad_request">>}, Res).

t_invalid_query_params({_, DbUrl}) ->
    Docs = #{<<"docs">> => [#{<<"id">> => ?DOC, <<"rev">> => <<"2-revb">>}]},
    Params = ["rev", "open_revs", "atts_since", "w", "new_edits"],
    lists:foreach(
        fun(Param) ->
            {Code, Res} = req(post, DbUrl ++ "/_bulk_get?" ++ Param, Docs),
            ?assertEqual(400, Code),
            ?assertMatch(#{<<"error">> := <<"bad_request">>}, Res)
        end,
        Params
    ).

t_invalid_doc({_, DbUrl}) ->
    {Code, Res} = bulk_get(DbUrl, [<<"foo">>]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"error">> := #{
                            <<"id">> := null,
                            <<"rev">> := null,
                            <<"error">> := <<"bad_request">>
                        }
                    }
                ],
                <<"id">> := null
            }
        ],
        Res
    ).

t_mp_invalid_doc({_, DbUrl}) ->
    {Code, Res} = bulk_get_mp(DbUrl, [<<"foo">>]),
    ?assertEqual(200, Code),
    ?assertMatch([{#{}, #{}}], Res),
    [{ChunkHeaders, Error}] = Res,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json; error=\"true\"">>
        },
        ChunkHeaders
    ),
    ?assertMatch(
        #{
            <<"error">> := <<"bad_request">>,
            <<"id">> := null,
            <<"rev">> := null
        },
        Error
    ).

t_doc_no_id({_, DbUrl}) ->
    {Code, Res} = bulk_get(DbUrl, [#{<<"rev">> => <<"1-foo">>}]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"error">> := #{
                            <<"id">> := null,
                            <<"rev">> := null,
                            <<"error">> := <<"bad_request">>
                        }
                    }
                ],
                <<"id">> := null
            }
        ],
        Res
    ).

t_mp_doc_no_id({_, DbUrl}) ->
    {Code, Res} = bulk_get_mp(DbUrl, [#{<<"rev">> => <<"1-foo">>}]),
    ?assertEqual(200, Code),
    ?assertMatch([{#{}, #{}}], Res),
    [{ChunkHeaders, Error}] = Res,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json; error=\"true\"">>
        },
        ChunkHeaders
    ),
    ?assertMatch(
        #{
            <<"error">> := <<"bad_request">>,
            <<"id">> := null,
            <<"rev">> := null
        },
        Error
    ).

t_invalid_doc_id({_, DbUrl}) ->
    {Code, Res} = bulk_get(DbUrl, [#{<<"id">> => <<>>, <<"rev">> => <<"1-foo">>}]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"error">> := #{
                            <<"id">> := <<>>,
                            <<"rev">> := null,
                            <<"error">> := <<"illegal_docid">>
                        }
                    }
                ],
                <<"id">> := <<>>
            }
        ],
        Res
    ).

t_mp_invalid_doc_id({_, DbUrl}) ->
    {Code, Res} = bulk_get_mp(DbUrl, [#{<<"id">> => <<>>, <<"rev">> => <<"1-foo">>}]),
    ?assertEqual(200, Code),
    ?assertMatch([{#{}, #{}}], Res),
    [{ChunkHeaders, Error}] = Res,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json; error=\"true\"">>
        },
        ChunkHeaders
    ),
    ?assertMatch(
        #{
            <<"id">> := <<>>,
            <<"rev">> := null,
            <<"error">> := <<"illegal_docid">>
        },
        Error
    ).

t_missing_doc({_, DbUrl}) ->
    {Code, Res} = bulk_get(DbUrl, [#{<<"id">> => <<"missing">>}]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"error">> := #{
                            <<"error">> := <<"not_found">>,
                            <<"id">> := <<"missing">>,
                            <<"rev">> := <<"undefined">>
                        }
                    }
                ],
                <<"id">> := <<"missing">>
            }
        ],
        Res
    ).

t_mp_missing_doc({_, DbUrl}) ->
    {Code, Res} = bulk_get_mp(DbUrl, [#{<<"id">> => <<"missing">>}]),
    ?assertEqual(200, Code),
    ?assertMatch([{#{}, #{}}], Res),
    [{ChunkHeaders, Error}] = Res,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json; error=\"true\"">>
        },
        ChunkHeaders
    ),
    ?assertMatch(
        #{
            <<"error">> := <<"not_found">>,
            <<"id">> := <<"missing">>,
            <<"rev">> := <<"undefined">>
        },
        Error
    ).

t_invalid_rev({_, DbUrl}) ->
    Doc = #{<<"id">> => ?DOC, <<"rev">> => 42},
    {Code, Res} = bulk_get(DbUrl, [Doc]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"error">> := #{
                            <<"error">> := <<"bad_request">>,
                            <<"id">> := ?DOC,
                            <<"rev">> := 42
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_mp_invalid_rev({_, DbUrl}) ->
    Doc = #{<<"id">> => ?DOC, <<"rev">> => 42},
    {Code, Res} = bulk_get_mp(DbUrl, [Doc]),
    ?assertEqual(200, Code),
    ?assertMatch([{#{}, #{}}], Res),
    [{ChunkHeaders, Error}] = Res,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json; error=\"true\"">>
        },
        ChunkHeaders
    ),
    ?assertMatch(
        #{
            <<"error">> := <<"bad_request">>,
            <<"id">> := ?DOC,
            <<"rev">> := 42
        },
        Error
    ).

t_missing_rev({_, DbUrl}) ->
    Doc = #{<<"id">> => ?DOC, <<"rev">> => <<"1-x">>},
    {Code, Res} = bulk_get(DbUrl, [Doc]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"error">> := #{
                            <<"error">> := <<"not_found">>,
                            <<"id">> := ?DOC,
                            <<"rev">> := <<"1-x">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_mp_missing_rev({_, DbUrl}) ->
    Doc = #{<<"id">> => ?DOC, <<"rev">> => <<"1-x">>},
    {Code, Res} = bulk_get_mp(DbUrl, [Doc]),
    ?assertEqual(200, Code),
    ?assertMatch([{#{}, #{}}], Res),
    [{ChunkHeaders, Error}] = Res,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json; error=\"true\"">>
        },
        ChunkHeaders
    ),
    ?assertMatch(
        #{
            <<"error">> := <<"not_found">>,
            <<"reason">> := <<"missing">>,
            <<"rev">> := <<"1-x">>
        },
        Error
    ).

t_doc_all_revs({_, DbUrl}) ->
    {Code, Res} = bulk_get(DbUrl, [#{<<"id">> => ?DOC}]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>,
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            }
                        }
                    },
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revc">>,
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            }
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_mp_doc_all_revs({_, DbUrl}) ->
    {Code, Res0} = bulk_get_mp(DbUrl, [#{<<"id">> => ?DOC}]),
    ?assertEqual(200, Code),
    ?assertMatch([{#{}, [_, _]}, {#{}, [_, _]}], Res0),

    % Sort to ensure we get a determinstic order for results
    CmpFun = fun({#{<<"x-rev-id">> := A}, _}, {#{<<"x-rev-id">> := B}, _}) ->
        A =< B
    end,
    Res = lists:sort(CmpFun, Res0),

    [
        {ChunkHeaders1, [Doc1, AttA]},
        {ChunkHeaders2, [Doc2, AttB]}
    ] = Res,

    % Chunk headers
    ?assertMatch(
        #{
            <<"x-doc-id">> := ?DOC,
            <<"x-rev-id">> := <<"2-revb">>
        },
        ChunkHeaders1
    ),

    ?assertMatch(
        #{
            <<"x-doc-id">> := ?DOC,
            <<"x-rev-id">> := <<"2-revc">>
        },
        ChunkHeaders2
    ),

    % Doc bodies
    ?assertMatch({#{}, #{}}, Doc1),
    {DocHeaders1, DocBody1} = Doc1,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json">>
        },
        DocHeaders1
    ),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATT := #{<<"follows">> := true}
            }
        },
        DocBody1
    ),

    ?assertMatch({#{}, #{}}, Doc2),
    {DocHeaders2, DocBody2} = Doc2,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json">>
        },
        DocHeaders2
    ),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revc">>,
            <<"_attachments">> := #{
                ?ATT := #{<<"follows">> := true}
            }
        },
        DocBody2
    ),

    % 2-revb attachments
    ?assertMatch({#{}, <<_/binary>>}, AttA),
    {AttAHeaders, AttAData} = AttA,
    ?assertMatch(
        #{
            <<"content-disposition">> := <<"attachment; filename=\"att\"">>
        },
        AttAHeaders
    ),
    ?assertEqual(<<"thedata">>, AttAData),

    % 2-revc attachments
    ?assertMatch({#{}, <<_/binary>>}, AttB),
    {AttBHeaders, AttBData} = AttB,
    ?assertMatch(
        #{
            <<"content-disposition">> := <<"attachment; filename=\"att\"">>
        },
        AttBHeaders
    ),
    ?assertEqual(<<"thedata">>, AttBData).

t_specific_rev({_, DbUrl}) ->
    Doc = #{<<"id">> => ?DOC, <<"rev">> => <<"2-revb">>},
    {Code, Res} = bulk_get(DbUrl, [Doc]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>,
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            }
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_mp_specific_rev({_, DbUrl}) ->
    Doc = #{<<"id">> => ?DOC, <<"rev">> => <<"2-revb">>},
    {Code, Res} = bulk_get_mp(DbUrl, [Doc]),
    ?assertEqual(200, Code),
    ?assertMatch([{#{}, [_, _]}], Res),

    [{ChunkHeaders, [Doc1, Att]}] = Res,

    % Whole doc + att chunk headers
    ?assertMatch(
        #{
            <<"x-doc-id">> := ?DOC,
            <<"x-rev-id">> := <<"2-revb">>
        },
        ChunkHeaders
    ),

    % Doc body
    ?assertMatch({#{}, #{}}, Doc1),
    {DocHeaders, DocBody} = Doc1,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json">>
        },
        DocHeaders
    ),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATT := #{<<"follows">> := true}
            }
        },
        DocBody
    ),

    % Att
    ?assertMatch({#{}, <<_/binary>>}, Att),
    {AttHeaders, AttData} = Att,
    ?assertMatch(
        #{
            <<"content-disposition">> := <<"attachment; filename=\"att\"">>
        },
        AttHeaders
    ),
    ?assertEqual(<<"thedata">>, AttData).

t_mp_specific_rev_multipart_related({_, DbUrl}) ->
    Doc = #{<<"id">> => ?DOC, <<"rev">> => <<"2-revb">>},
    {Code, Res} = bulk_get_mp(DbUrl, [Doc], "", ?MP_RELATED),
    ?assertEqual(200, Code),
    ?assertMatch([{#{}, [_, _]}], Res),

    [{ChunkHeaders, [Doc1, Att]}] = Res,

    % Whole doc + att chunk headers
    ?assertMatch(
        #{
            <<"x-doc-id">> := ?DOC,
            <<"x-rev-id">> := <<"2-revb">>
        },
        ChunkHeaders
    ),

    % Doc body
    ?assertMatch({#{}, #{}}, Doc1),
    {DocHeaders, DocBody} = Doc1,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json">>
        },
        DocHeaders
    ),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATT := #{<<"follows">> := true}
            }
        },
        DocBody
    ),

    % Att
    ?assertMatch({#{}, <<_/binary>>}, Att),
    {AttHeaders, AttData} = Att,
    ?assertMatch(
        #{
            <<"content-disposition">> := <<"attachment; filename=\"att\"">>
        },
        AttHeaders
    ),
    ?assertEqual(<<"thedata">>, AttData).

t_specific_rev_latest({_, DbUrl}) ->
    Doc = #{<<"id">> => ?DOC, <<"rev">> => <<"2-revb">>},
    {Code, Res} = bulk_get(DbUrl, [Doc], "?latest=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>,
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            }
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_ancestor_rev_latest({_, DbUrl}) ->
    Doc = #{<<"id">> => ?DOC, <<"rev">> => <<"1-reva">>},
    {Code, Res} = bulk_get(DbUrl, [Doc], "?latest=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>,
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            }
                        }
                    },
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revc">>,
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            }
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_revs_true({_, DbUrl}) ->
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"1-reva">>
    },
    {Code, Res} = bulk_get(DbUrl, [Doc], "?revs=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"1-reva">>,
                            <<"_revisions">> :=
                                #{<<"ids">> := [<<"reva">>], <<"start">> := 1},
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            }
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_mp_revs_true({_, DbUrl}) ->
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"1-reva">>
    },
    {Code, Res} = bulk_get_mp(DbUrl, [Doc], "?revs=true"),
    ?assertEqual(200, Code),
    ?assertMatch([{#{}, [_, _]}], Res),

    [{ChunkHeaders, [Doc1, Att]}] = Res,

    % Whole doc + att chunk headers
    ?assertMatch(
        #{
            <<"x-doc-id">> := ?DOC,
            <<"x-rev-id">> := <<"1-reva">>
        },
        ChunkHeaders
    ),

    % Doc body
    ?assertMatch({#{}, #{}}, Doc1),
    {DocHeaders, DocBody} = Doc1,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json">>
        },
        DocHeaders
    ),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"1-reva">>,
            <<"_revisions">> :=
                #{<<"ids">> := [<<"reva">>], <<"start">> := 1},
            <<"_attachments">> := #{
                ?ATT := #{<<"follows">> := true}
            }
        },
        DocBody
    ),

    % Att
    ?assertMatch({#{}, <<_/binary>>}, Att),
    {AttHeaders, AttData} = Att,
    ?assertMatch(
        #{
            <<"content-disposition">> := <<"attachment; filename=\"att\"">>
        },
        AttHeaders
    ),
    ?assertEqual(<<"thedata">>, AttData).

t_attachments_true({_, DbUrl}) ->
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"1-reva">>
    },
    {Code, Res} = bulk_get(DbUrl, [Doc], "?attachments=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"1-reva">>,
                            <<"_attachments">> := #{
                                ?ATT := #{<<"data">> := ?ATT_DATA}
                            }
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_atts_since({_, DbUrl}) ->
    % Attachment should not be returned as 2 from 2-revb is not stricly greater
    % than 1 from our attachment's revpos
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"2-revb">>,
        <<"atts_since">> => [<<"2-revb">>]
    },
    {Code, Res} = bulk_get(DbUrl, [Doc]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>,
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            }
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_invalid_atts_since({_, DbUrl}) ->
    % atts_since is not a list even
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"2-revb">>,
        <<"atts_since">> => <<"badsince">>
    },
    {Code, Res} = bulk_get(DbUrl, [Doc]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"error">> := #{
                            <<"id">> := ?DOC,
                            <<"error">> := <<"bad_request">>,
                            <<"rev">> := <<"badsince">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_invalid_atts_since_invalid_rev({_, DbUrl}) ->
    % atts_since is list but the revision is bad
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"2-revb">>,
        <<"atts_since">> => [<<"badsince">>]
    },
    {Code, Res} = bulk_get(DbUrl, [Doc]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"error">> := #{
                            <<"id">> := ?DOC,
                            <<"error">> := <<"bad_request">>,
                            <<"rev">> := <<"badsince">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_mp_atts_since({_, DbUrl}) ->
    % Attachments should not be returned as 2 from 2-revb is not stricly
    % greater than 1 from our attachment's revpos. As far as multpart encoding
    % goes, this is an odd corner case: when all attachments are stubs it seems
    % the doc body is encoded directly as the multipart/* top level part
    % instead of having another nested application/json doc.
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"2-revb">>,
        <<"atts_since">> => [<<"2-revb">>]
    },
    {Code, Res} = bulk_get_mp(DbUrl, [Doc]),
    ?assertEqual(200, Code),

    ?assertMatch([{#{}, #{}}], Res),
    [{ChunkHeaders, DocBody}] = Res,

    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json">>
        },
        ChunkHeaders
    ),

    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATT := #{<<"stub">> := true}
            }
        },
        DocBody
    ).

t_atts_since_returns_attachment({_, DbUrl}) ->
    % 0-baz revpos 0 is less than revpos 1 of our attachment
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"2-revb">>,
        <<"atts_since">> => [<<"0-baz">>]
    },
    {Code, Res} = bulk_get(DbUrl, [Doc]),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>,
                            <<"_attachments">> := #{
                                ?ATT := #{<<"data">> := ?ATT_DATA}
                            }
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_mp_atts_since_returns_attachment({_, DbUrl}) ->
    % 0-baz revpos 0 is less than revpos 1 of our attachment
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"2-revb">>,
        <<"atts_since">> => [<<"0-baz">>]
    },
    {Code, Res} = bulk_get_mp(DbUrl, [Doc]),
    ?assertEqual(200, Code),

    ?assertMatch([{#{}, [_, _]}], Res),

    [{ChunkHeaders, [Doc1, Att]}] = Res,

    % Whole doc + att chunk headers
    ?assertMatch(
        #{
            <<"x-doc-id">> := ?DOC,
            <<"x-rev-id">> := <<"2-revb">>
        },
        ChunkHeaders
    ),

    % Doc body
    ?assertMatch({#{}, #{}}, Doc1),
    {DocHeaders, DocBody} = Doc1,
    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json">>
        },
        DocHeaders
    ),
    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATT := #{<<"follows">> := true}
            }
        },
        DocBody
    ),

    % Att
    ?assertMatch({#{}, <<_/binary>>}, Att),
    {AttHeaders, AttData} = Att,
    ?assertMatch(
        #{
            <<"content-disposition">> := <<"attachment; filename=\"att\"">>
        },
        AttHeaders
    ),
    ?assertEqual(<<"thedata">>, AttData).

t_atts_since_overrides_attachments_true({_, DbUrl}) ->
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"2-revb">>,
        <<"atts_since">> => [<<"2-revb">>]
    },
    {Code, Res} = bulk_get(DbUrl, [Doc], "?attachments=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>,
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            }
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_mp_atts_since_overrides_attachments_true({_, DbUrl}) ->
    Doc = #{
        <<"id">> => ?DOC,
        <<"rev">> => <<"2-revb">>,
        <<"atts_since">> => [<<"2-revb">>]
    },
    {Code, Res} = bulk_get_mp(DbUrl, [Doc], "?attachments=true"),
    ?assertEqual(200, Code),

    ?assertMatch([{#{}, #{}}], Res),
    [{ChunkHeaders, DocBody}] = Res,

    ?assertMatch(
        #{
            <<"content-type">> := <<"application/json">>
        },
        ChunkHeaders
    ),

    ?assertMatch(
        #{
            <<"_id">> := ?DOC,
            <<"_rev">> := <<"2-revb">>,
            <<"_attachments">> := #{
                ?ATT := #{<<"stub">> := true}
            }
        },
        DocBody
    ).

t_atts_since_multiple({_, DbUrl}) ->
    % Attachment revpos is 1 so we do not expect this attachment body
    Docs = [
        #{
            <<"id">> => ?DOC,
            <<"rev">> => <<"2-revb">>,
            <<"atts_since">> => [<<"2-revb">>]
        },
        % Should get the attachment as revpos=1 is greater than 0-foo
        #{
            <<"id">> => ?DOC,
            <<"rev">> => <<"2-revb">>,
            <<"atts_since">> => [<<"0-foo">>]
        },
        % Empty atts_since. Do not expect to get the attachment
        #{
            <<"id">> => ?DOC,
            <<"rev">> => <<"2-revb">>,
            <<"atts_since">> => []
        },
        % Include a document without atts_since to ensure atts_since applies only to
        % individual requests
        #{
            <<"id">> => ?DOC,
            <<"rev">> => <<"2-revb">>
        }
    ],
    {Code, Res} = bulk_get(DbUrl, Docs),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            },
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            },
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_attachments">> := #{
                                ?ATT := #{<<"data">> := ?ATT_DATA}
                            },
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            },
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            },
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            },
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            },
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_atts_since_multiple_attachments_true({_, DbUrl}) ->
    % Since attachments=true, expect to always get the attachments, unless
    % there is an atts_since present and atts_since would prevent the
    % attachment from being returned.
    Docs = [
        % Attachment revpos is 1 so we do not expect this attachment body
        #{
            <<"id">> => ?DOC,
            <<"rev">> => <<"2-revb">>,
            <<"atts_since">> => [<<"2-revb">>]
        },
        % Should get the attachment as revpos=1 is greater than 0-foo
        #{
            <<"id">> => ?DOC,
            <<"rev">> => <<"2-revb">>,
            <<"atts_since">> => [<<"0-foo">>]
        },
        % Should get the attachment as it is set as a default option
        #{
            <<"id">> => ?DOC,
            <<"rev">> => <<"2-revb">>,
            <<"atts_since">> => []
        },
        % Check a doc without atts_since to ensure atts_since applies only to
        % individual requests, otherwise default options apply
        #{
            <<"id">> => ?DOC,
            <<"rev">> => <<"2-revb">>
        }
    ],
    {Code, Res} = bulk_get(DbUrl, Docs, "?attachments=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_attachments">> := #{
                                ?ATT := #{<<"stub">> := true}
                            },
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            },
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_attachments">> := #{
                                ?ATT := #{<<"data">> := ?ATT_DATA}
                            },
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            },
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_attachments">> := #{
                                ?ATT := #{<<"data">> := ?ATT_DATA}
                            },
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            },
            #{
                <<"docs">> := [
                    #{
                        <<"ok">> := #{
                            <<"_attachments">> := #{
                                ?ATT := #{<<"data">> := ?ATT_DATA}
                            },
                            <<"_id">> := ?DOC,
                            <<"_rev">> := <<"2-revb">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_missing_rev_latest({_, DbUrl}) ->
    % Check the case of latest and a missing rev
    Doc = #{<<"id">> => ?DOC, <<"rev">> => <<"1-x">>},
    {Code, Res} = bulk_get(DbUrl, [Doc], "?latest=true"),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"error">> := #{
                            <<"error">> := <<"not_found">>,
                            <<"id">> := ?DOC,
                            <<"rev">> := <<"1-x">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_fabric_worker_error({_, DbUrl}) ->
    % Check the case handling errors returned by fabric:open_revs/3,4
    Doc = #{<<"id">> => ?DOC, <<"rev">> => <<"1-reva">>},
    meck:expect(fabric, open_revs, 3, meck:val({error, fabric_error_foo})),
    meck:expect(fabric, open_revs, 4, meck:val({error, fabric_error_foo})),
    {Code, Res} = bulk_get(DbUrl, [Doc], "?latest=true"),
    meck:expect(fabric, open_revs, 3, meck:passthrough()),
    meck:expect(fabric, open_revs, 4, meck:passthrough()),
    ?assertEqual(200, Code),
    ?assertMatch(
        [
            #{
                <<"docs">> := [
                    #{
                        <<"error">> := #{
                            <<"error">> := <<"internal_fabric_error">>,
                            <<"id">> := ?DOC,
                            <<"rev">> := <<"1-reva">>,
                            <<"reason">> := <<"fabric_error_foo">>
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
    ).

t_multiple_docs({_, DbUrl}) ->
    Reqs = [#{<<"id">> => integer_to_binary(I)} || I <- lists:seq(1, ?DOC_COUNT)],
    {Code, Res} = bulk_get(DbUrl, Reqs),
    ?assertEqual(200, Code),
    ?assertMatch([#{<<"docs">> := _} | _], Res),
    ?assertEqual(?DOC_COUNT, length(Res)),
    lists:foreach(
        fun({I, Docs}) ->
            Id = integer_to_binary(I),
            ?assertMatch(
                #{
                    <<"docs">> := [
                        #{
                            <<"ok">> := #{
                                <<"_id">> := Id,
                                <<"_rev">> := <<"1-reva">>
                            }
                        }
                    ],
                    <<"id">> := Id
                },
                Docs
            )
        end,
        lists:zip(lists:seq(1, ?DOC_COUNT), Res)
    ).

t_mp_multiple_docs({_, DbUrl}) ->
    Reqs = [#{<<"id">> => integer_to_binary(I)} || I <- lists:seq(1, ?DOC_COUNT)],
    {Code, Res} = bulk_get_mp(DbUrl, Reqs),
    ?assertEqual(200, Code),
    ?assertEqual(?DOC_COUNT, length(Res)),
    lists:foreach(
        fun({I, Docs}) ->
            Id = integer_to_binary(I),
            ?assertMatch(
                {
                    #{<<"content-type">> := <<"application/json">>},
                    #{<<"_id">> := Id, <<"_rev">> := <<"1-reva">>}
                },
                Docs
            )
        end,
        lists:zip(lists:seq(1, ?DOC_COUNT), Res)
    ).

% Utility functions

setup_ctx() ->
    Ctx = test_util:start_couch([chttpd]),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Db = ?b2l(?tempdb()),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    ok = create_db(Url, Db),
    {Ctx, Url, Db}.

teardown({Ctx, DbUrl}) ->
    meck:unload(),
    delete_db(DbUrl),
    Persist = false,
    ok = config:delete("admins", ?USER, Persist),
    ok = config:delete("chttpd", "use_batches", Persist),
    test_util:stop_couch(Ctx).

teardown_no_batches({Ctx, DbUrl}) ->
    % Verify that the non-batched open_revs/4 was called
    ?assert(meck:num_calls(fabric, open_revs, 4) >= 1),
    % Verify that no calls to the batched open_revs/3 were made
    ?assertEqual(0, meck:num_calls(fabric, open_revs, 3)),
    teardown({Ctx, DbUrl}).

setup_basic() ->
    {Ctx, Url, Db} = setup_ctx(),
    DbUrl = Url ++ Db,
    ok = create_docs(DbUrl, test_docs_revs()),
    meck:new(fabric, [passthrough]),
    {Ctx, DbUrl}.

setup_no_batches() ->
    {Ctx, DbUrl} = setup_basic(),
    config:set("chttpd", "bulk_get_use_batches", "false", _Persist = false),
    {Ctx, DbUrl}.

setup_multiple() ->
    {Ctx, Url, Db} = setup_ctx(),
    DbUrl = Url ++ Db,
    Docs = [{integer_to_binary(I), [?REVA]} || I <- lists:seq(1, ?DOC_COUNT)],
    ok = create_docs(DbUrl, Docs, _WithAtts = false),
    {Ctx, DbUrl}.

setup_multiple_no_batches() ->
    {Ctx, DbUrl} = setup_multiple(),
    config:set("chttpd", "bulk_get_use_batches", "false", _Persist = false),
    {Ctx, DbUrl}.

create_db(Top, Db) ->
    case req(put, Top ++ Db) of
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

create_docs(DbUrl, DocRevs) ->
    create_docs(DbUrl, DocRevs, true).

create_docs(DbUrl, DocRevs, WithAtts) ->
    Docs = lists:map(
        fun({Id, Revs}) ->
            Doc = #{
                <<"_id">> => Id,
                <<"_revisions">> => #{
                    <<"ids">> => Revs,
                    <<"start">> => length(Revs)
                }
            },
            case WithAtts of
                true -> add_att(Doc);
                false -> Doc
            end
        end,
        DocRevs
    ),
    Body = #{
        <<"docs">> => Docs,
        <<"new_edits">> => false
    },
    {Code, Res} = req(post, DbUrl ++ "/_bulk_docs", Body),
    ?assertEqual(201, Code),
    ?assertEqual([], Res),
    ok.

add_att(#{} = Doc) ->
    Doc#{
        <<"_attachments">> => #{
            ?ATT => #{
                <<"revpos">> => 1,
                <<"content_type">> => <<"application/octet-stream">>,
                <<"data">> => ?ATT_DATA
            }
        }
    }.

bulk_get(DbUrl, Docs) ->
    bulk_get(DbUrl, Docs, "").

bulk_get(DbUrl, Docs, Params) ->
    Url = DbUrl ++ "/_bulk_get" ++ Params,
    {Code, Res} = req(post, Url, #{<<"docs">> => Docs}),
    #{<<"results">> := DocResults} = Res,
    {Code, DocResults}.

req(Method, Url) ->
    Headers = [?JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, json_decode(Res)}.

req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, json_decode(Res)}.

% Handle multipart _bulk_get requests

bulk_get_mp(DbUrl, Docs) ->
    bulk_get_mp(DbUrl, Docs, "").

bulk_get_mp(DbUrl, Doc, Params) ->
    bulk_get_mp(DbUrl, Doc, Params, ?MP_MIXED).

bulk_get_mp(DbUrl, Docs, Params, MpType) ->
    Url = DbUrl ++ "/_bulk_get" ++ Params,
    {Code, Res} = req_mp(post, Url, #{<<"docs">> => Docs}, MpType),
    {Code, Res}.

req_mp(Method, Url, #{} = Body, MpType) ->
    req_mp(Method, Url, jiffy:encode(Body), MpType);
req_mp(Method, Url, Body, MpType) ->
    Headers = [?JSON, ?AUTH, MpType],
    {ok, Code, ResHeaders, Res} = test_request:request(Method, Url, Headers, Body),
    CType = header_value("Content-Type", ResHeaders),
    case CType of
        "application/json" ->
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
    #{<<"content-type">> := CType} = Headers,
    case CType of
        <<"application/json", _/binary>> ->
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
