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

-define(DOC, <<"doc">>).
-define(REVA, <<"reva">>).
-define(REVB, <<"revb">>).
-define(REVC, <<"revc">>).
-define(ATT, <<"att">>).
-define(ATT_DATA, <<"dGhlZGF0YQ==">>).

-define(DOC_COUNT, 2000).

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
        with([
            ?TDEF(t_empty_request),
            ?TDEF(t_no_docs),
            ?TDEF(t_invalid_doc),
            ?TDEF(t_doc_no_id),
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
            ?TDEF(t_atts_since_returns_attachment),
            ?TDEF(t_atts_since_overrides_attachments_true),
            ?TDEF(t_atts_since_multiple),
            ?TDEF(t_atts_since_multiple_attachments_true),
            ?TDEF(t_missing_rev_latest)
        ])
    }.

bulk_get_multiple_docs_test_() ->
    {
        foreach,
        fun setup_multiple/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_multiple_docs, 10)
        ]
    }.

t_empty_request({_, DbUrl}) ->
    {Code, Res} = bulk_get(DbUrl, []),
    ?assertEqual(200, Code),
    ?assertEqual([], Res).

t_no_docs({_, DbUrl}) ->
    {Code, #{}} = req(post, DbUrl ++ "/_bulk_get", #{}),
    ?assertEqual(400, Code).

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
                                ?ATT := #{
                                    <<"stub">> := true
                                }
                            }
                        }
                    }
                ],
                <<"id">> := ?DOC
            }
        ],
        Res
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
                            <<"_attachments">> :=
                                #{?ATT := #{<<"stub">> := true}},
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
                            <<"_attachments">> :=
                                #{?ATT := #{<<"data">> := ?ATT_DATA}},
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
    delete_db(DbUrl),
    ok = config:delete("admins", ?USER, _Persist = false),
    test_util:stop_couch(Ctx).

setup_basic() ->
    {Ctx, Url, Db} = setup_ctx(),
    DbUrl = Url ++ Db,
    ok = create_docs(DbUrl, test_docs_revs()),
    {Ctx, DbUrl}.

setup_multiple() ->
    {Ctx, Url, Db} = setup_ctx(),
    DbUrl = Url ++ Db,
    Docs = [{integer_to_binary(I), [?REVA]} || I <- lists:seq(1, ?DOC_COUNT)],
    ok = create_docs(DbUrl, Docs),
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
    Docs = lists:map(
        fun({Id, Revs}) ->
            Doc = #{
                <<"_id">> => Id,
                <<"_revisions">> => #{
                    <<"ids">> => Revs,
                    <<"start">> => length(Revs)
                }
            },
            add_att(Doc)
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
                <<"content_type">> => <<"text/plain">>,
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
    {Code, jiffy:decode(Res, [return_maps])}.

req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.
