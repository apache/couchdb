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

-module(couch_replicator_parse).

-export([
    parse_rep_doc/1,
    parse_rep_doc/2,
    parse_rep_db/3,
    parse_rep_doc_without_id/1,
    parse_rep_doc_without_id/2
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("ibrowse/include/ibrowse.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").
-include("couch_replicator.hrl").

-define(DEFAULT_SOCK_OPTS, [{keepalive, true}, {nodelay, false}]).
-define(VALID_SOCK_OPTS, [
    buffer,
    keepalive,
    nodelay,
    priority,
    recbuf,
    sndbuf
]).
-define(DEFAULT_IBROWSE_OPTS, []).
-define(VALID_IBROWSE_OPTS, [
    prefer_ipv6
]).
-define(VALID_PROTOCOLS, #{
    endpoint => [http, https],
    proxy => [http, https, socks5]
}).

%% erlfmt-ignore
default_options() ->
    [
        {connection_timeout,  cfg_int("connection_timeout", 30000)},
        {retries,             cfg_int("retries_per_request", 5)},
        {http_connections,    cfg_int("http_connections", 20)},
        {worker_batch_size,   cfg_int("worker_batch_size", 500)},
        {worker_processes,    cfg_int("worker_processes", 4)},
        {checkpoint_interval, cfg_int("checkpoint_interval", 30000)},
        {use_checkpoints,     cfg_boolean("use_checkpoints", true)},
        {use_bulk_get,        cfg_boolean("use_bulk_get", true)},
        {ibrowse_options,     cfg_ibrowse_opts()},
        {socket_options,      cfg_sock_opts()}
    ].

% Note: parse_rep_doc can handle filtered replications. During parsing of the
% replication doc it will make possibly remote http requests to the source
% database. If failure or parsing of filter docs fails, parse_doc throws a
% {filter_fetch_error, Error} exception. This exception should be considered
% transient in respect to the contents of the document itself, since it depends
% on netowrk availability of the source db and other factors.
-spec parse_rep_doc({[_]}) -> #rep{}.
parse_rep_doc(RepDoc) ->
    {ok, Rep} =
        try
            parse_rep_doc(RepDoc, rep_user_ctx(RepDoc))
        catch
            throw:{error, Reason} ->
                throw({bad_rep_doc, Reason});
            throw:{filter_fetch_error, Reason} ->
                throw({filter_fetch_error, Reason});
            Tag:Err ->
                throw({bad_rep_doc, to_binary({Tag, Err})})
        end,
    Rep.

-spec parse_rep_doc_without_id({[_]}) -> #rep{}.
parse_rep_doc_without_id(RepDoc) ->
    {ok, Rep} =
        try
            parse_rep_doc_without_id(RepDoc, rep_user_ctx(RepDoc))
        catch
            throw:{error, Reason} ->
                throw({bad_rep_doc, Reason});
            Tag:Err ->
                throw({bad_rep_doc, to_binary({Tag, Err})})
        end,
    Rep.

-spec parse_rep_doc({[_]}, #user_ctx{}) -> {ok, #rep{}}.
parse_rep_doc(Doc, UserCtx) ->
    {ok, Rep} = parse_rep_doc_without_id(Doc, UserCtx),
    Cancel = get_value(cancel, Rep#rep.options, false),
    Id = get_value(id, Rep#rep.options, nil),
    case {Cancel, Id} of
        {true, nil} ->
            % Cancel request with no id, must parse id out of body contents
            {ok, update_rep_id(Rep)};
        {true, Id} ->
            % Cancel request with an id specified, so do not parse id from body
            {ok, Rep};
        {false, _Id} ->
            % Not a cancel request, regular replication doc
            {ok, update_rep_id(Rep)}
    end.

-spec parse_rep_doc_without_id({[_]}, #user_ctx{}) -> {ok, #rep{}}.
parse_rep_doc_without_id({Props}, UserCtx) ->
    {SrcProxy, TgtProxy} = parse_proxy_settings(Props),
    Opts = make_options(Props),
    case
        get_value(cancel, Opts, false) andalso
            (get_value(id, Opts, nil) =/= nil)
    of
        true ->
            {ok, #rep{options = Opts, user_ctx = UserCtx}};
        false ->
            Source = parse_rep_db(get_value(<<"source">>, Props), SrcProxy, Opts),
            Target = parse_rep_db(get_value(<<"target">>, Props), TgtProxy, Opts),
            {Type, View} =
                case couch_replicator_filters:view_type(Props, Opts) of
                    {error, Error} ->
                        throw({bad_request, Error});
                    Result ->
                        Result
                end,
            Rep = #rep{
                source = Source,
                target = Target,
                options = Opts,
                user_ctx = UserCtx,
                type = Type,
                view = View,
                doc_id = get_value(<<"_id">>, Props, null)
            },
            % Check if can parse filter code, if not throw exception
            case couch_replicator_filters:parse(Opts) of
                {error, FilterError} ->
                    throw({error, FilterError});
                {ok, _Filter} ->
                    ok
            end,
            {ok, Rep}
    end.

parse_proxy_settings(Props) when is_list(Props) ->
    Proxy = get_value(<<"proxy">>, Props, <<>>),
    SrcProxy = get_value(<<"source_proxy">>, Props, <<>>),
    TgtProxy = get_value(<<"target_proxy">>, Props, <<>>),

    case Proxy =/= <<>> of
        true when SrcProxy =/= <<>> ->
            Error = "`proxy` is mutually exclusive with `source_proxy`",
            throw({bad_request, Error});
        true when TgtProxy =/= <<>> ->
            Error = "`proxy` is mutually exclusive with `target_proxy`",
            throw({bad_request, Error});
        true ->
            {Proxy, Proxy};
        false ->
            {SrcProxy, TgtProxy}
    end.

% Update a #rep{} record with a replication_id. Calculating the id might involve
% fetching a filter from the source db, and so it could fail intermetently.
% In case of a failure to fetch the filter this function will throw a
%  `{filter_fetch_error, Reason} exception.
update_rep_id(Rep) ->
    RepId = couch_replicator_ids:replication_id(Rep),
    Rep#rep{id = RepId}.

-spec rep_user_ctx({[_]}) -> #user_ctx{}.
rep_user_ctx({RepDoc}) ->
    case get_json_value(<<"user_ctx">>, RepDoc) of
        undefined ->
            #user_ctx{};
        {UserCtx} ->
            #user_ctx{
                name = get_json_value(<<"name">>, UserCtx, null),
                roles = get_json_value(<<"roles">>, UserCtx, [])
            }
    end.

-spec parse_rep_db({[_]} | binary(), [_] | binary(), [_]) -> #httpdb{} | no_return().
parse_rep_db({Props}, Proxy, Options) ->
    Url0 = get_value(<<"url">>, Props),
    ok = check_url(Url0, endpoint),
    Url = maybe_add_trailing_slash(Url0),
    ProxyParams = parse_proxy_params(Proxy),
    ProxyURL =
        case ProxyParams of
            [] -> undefined;
            _ -> binary_to_list(Proxy)
        end,
    {AuthProps} = get_value(<<"auth">>, Props, {[]}),
    {BinHeaders} = get_value(<<"headers">>, Props, {[]}),
    Headers = lists:ukeysort(1, [{?b2l(K), ?b2l(V)} || {K, V} <- BinHeaders]),
    DefaultHeaders = (#httpdb{})#httpdb.headers,
    IbrowseOptions = get_value(ibrowse_options, Options, []),
    HttpDb = #httpdb{
        url = Url,
        auth_props = AuthProps,
        headers = lists:ukeymerge(1, Headers, DefaultHeaders),
        ibrowse_options = lists:keysort(
            1,
            [
                {socket_options, get_value(socket_options, Options)}
                | ProxyParams ++ ssl_params(Url) ++ IbrowseOptions
            ]
        ),
        timeout = get_value(connection_timeout, Options),
        http_connections = get_value(http_connections, Options),
        retries = get_value(retries, Options),
        proxy_url = ProxyURL
    },
    couch_replicator_utils:normalize_basic_auth(HttpDb);
parse_rep_db(<<"http://", _/binary>> = Url, Proxy, Options) ->
    parse_rep_db({[{<<"url">>, Url}]}, Proxy, Options);
parse_rep_db(<<"https://", _/binary>> = Url, Proxy, Options) ->
    parse_rep_db({[{<<"url">>, Url}]}, Proxy, Options);
parse_rep_db(<<_/binary>>, _Proxy, _Options) ->
    throw({error, local_endpoints_not_supported});
parse_rep_db(undefined, _Proxy, _Options) ->
    throw({error, <<"Missing replicator database">>}).

check_url(<<_/binary>> = Url, Type) when is_atom(Type) ->
    case ibrowse_lib:parse_url(?b2l(Url)) of
        #url{protocol = Protocol} ->
            check_protocol(Protocol, Type);
        {error, _} ->
            BinType = atom_to_binary(Type),
            throw({error, <<BinType/binary, " has an invalid url">>})
    end;
check_url(_, Type) when is_atom(Type) ->
    BinType = atom_to_binary(Type),
    throw({error, <<BinType/binary, " has an invalid url">>}).

check_protocol(Protocol, Type) ->
    CfgName = "valid_" ++ atom_to_list(Type) ++ "_protocols",
    Allowed = cfg_atoms(CfgName, maps:get(Type, ?VALID_PROTOCOLS)),
    check_protocol(Protocol, Type, Allowed).

check_protocol(Protocol, Type, Allowed) ->
    case lists:member(Protocol, Allowed) of
        true ->
            ok;
        false ->
            BinType = atom_to_binary(Type),
            throw({error, <<BinType/binary, " has an invalid url">>})
    end.

-spec maybe_add_trailing_slash(binary() | list()) -> list().
maybe_add_trailing_slash(Url) when is_binary(Url) ->
    maybe_add_trailing_slash(?b2l(Url));
maybe_add_trailing_slash(Url) ->
    case lists:member($?, Url) of
        true ->
            % skip if there are query params
            Url;
        false ->
            case lists:last(Url) of
                $/ ->
                    Url;
                _ ->
                    Url ++ "/"
            end
    end.

-spec make_options([_]) -> [_].
make_options(Props) ->
    Options0 = lists:ukeysort(1, convert_options(Props)),
    Options = check_options(Options0),
    Defaults = lists:keysort(1, default_options()),
    lists:ukeymerge(1, Options, Defaults).

cfg_int(Var, Default) ->
    config:get_integer("replicator", Var, Default).

cfg_boolean(Var, Default) ->
    config:get_boolean("replicator", Var, Default).

cfg_atoms(Cfg, Default) ->
    case cfg(Cfg) of
        undefined ->
            Default;
        V when is_list(V) ->
            [list_to_atom(string:strip(S)) || S <- string:split(V, ",", all)]
    end.

cfg_sock_opts() ->
    CfgTerm = cfg("socket_options"),
    parse_opts(CfgTerm, ?DEFAULT_SOCK_OPTS, ?VALID_SOCK_OPTS).

cfg_ibrowse_opts() ->
    CfgTerm = cfg("ibrowse_options"),
    parse_opts(CfgTerm, ?DEFAULT_IBROWSE_OPTS, ?VALID_IBROWSE_OPTS).

cfg(Var) ->
    config:get("replicator", Var).

parse_opts(undefined, Defaults, _) ->
    Defaults;
parse_opts(Term, _Defaults, ValidOpts) ->
    SocketOptions =
        case couch_util:parse_term(Term) of
            {ok, Opts} -> Opts;
            {error, _Error} -> []
        end,
    Fun = fun({K, _}) -> lists:member(K, ValidOpts) end,
    lists:filtermap(Fun, SocketOptions).

sock_opts(CfgTerm) ->
    ValidOpts = cfg_atoms("valid_socket_options", ?VALID_SOCK_OPTS),
    parse_opts(CfgTerm, ?DEFAULT_SOCK_OPTS, ValidOpts).

ibrowse_opts(CfgTerm) ->
    ValidOpts = cfg_atoms("valid_ibrowse_options", ?VALID_IBROWSE_OPTS),
    parse_opts(CfgTerm, ?DEFAULT_IBROWSE_OPTS, ValidOpts).

-spec convert_options([_]) -> [_].
convert_options([]) ->
    [];
convert_options([{<<"cancel">>, V} | _R]) when not is_boolean(V) ->
    throw({bad_request, <<"parameter `cancel` must be a boolean">>});
convert_options([{<<"cancel">>, V} | R]) ->
    [{cancel, V} | convert_options(R)];
convert_options([{IdOpt, V} | R]) when
    IdOpt =:= <<"_local_id">>;
    IdOpt =:= <<"replication_id">>;
    IdOpt =:= <<"id">>
->
    [{id, couch_replicator_ids:convert(V)} | convert_options(R)];
convert_options([{<<"create_target">>, V} | _R]) when not is_boolean(V) ->
    throw({bad_request, <<"parameter `create_target` must be a boolean">>});
convert_options([{<<"create_target">>, V} | R]) ->
    [{create_target, V} | convert_options(R)];
convert_options([{<<"create_target_params">>, V} | _R]) when not is_tuple(V) ->
    throw({bad_request, <<"parameter `create_target_params` must be a JSON object">>});
convert_options([{<<"create_target_params">>, V} | R]) ->
    [{create_target_params, V} | convert_options(R)];
convert_options([{<<"winning_revs_only">>, V} | _R]) when not is_boolean(V) ->
    throw({bad_request, <<"parameter `winning_revs_only` must be a boolean">>});
convert_options([{<<"winning_revs_only">>, V} | R]) ->
    [{winning_revs_only, V} | convert_options(R)];
convert_options([{<<"continuous">>, V} | _R]) when not is_boolean(V) ->
    throw({bad_request, <<"parameter `continuous` must be a boolean">>});
convert_options([{<<"continuous">>, V} | R]) ->
    [{continuous, V} | convert_options(R)];
convert_options([{<<"filter">>, V} | R]) ->
    [{filter, V} | convert_options(R)];
convert_options([{<<"query_params">>, V} | _R]) when not is_tuple(V) ->
    throw({bad_request, <<"parameter `query_params` must be an object">>});
convert_options([{<<"query_params">>, V} | R]) ->
    [{query_params, V} | convert_options(R)];
convert_options([{<<"doc_ids">>, null} | R]) ->
    convert_options(R);
convert_options([{<<"doc_ids">>, V} | _R]) when not is_list(V) ->
    throw({bad_request, <<"parameter `doc_ids` must be an array">>});
convert_options([{<<"doc_ids">>, V} | R]) ->
    % Ensure same behaviour as old replicator: accept a list of percent
    % encoded doc IDs.
    DocIds = lists:usort([?l2b(couch_httpd:unquote(Id)) || Id <- V]),
    [{doc_ids, DocIds} | convert_options(R)];
convert_options([{<<"selector">>, V} | _R]) when not is_tuple(V) ->
    throw({bad_request, <<"parameter `selector` must be a JSON object">>});
convert_options([{<<"selector">>, V} | R]) ->
    [{selector, V} | convert_options(R)];
convert_options([{<<"worker_processes">>, V} | R]) ->
    [{worker_processes, couch_util:to_integer(V)} | convert_options(R)];
convert_options([{<<"worker_batch_size">>, V} | R]) ->
    [{worker_batch_size, couch_util:to_integer(V)} | convert_options(R)];
convert_options([{<<"http_connections">>, V} | R]) ->
    [{http_connections, couch_util:to_integer(V)} | convert_options(R)];
convert_options([{<<"connection_timeout">>, V} | R]) ->
    [{connection_timeout, couch_util:to_integer(V)} | convert_options(R)];
convert_options([{<<"retries_per_request">>, V} | R]) ->
    [{retries, couch_util:to_integer(V)} | convert_options(R)];
convert_options([{<<"socket_options">>, V} | R]) ->
    [{socket_options, sock_opts(V)} | convert_options(R)];
convert_options([{<<"ibrowse_options">>, V} | R]) ->
    [{ibrowse_options, ibrowse_opts(V)} | convert_options(R)];
convert_options([{<<"since_seq">>, V} | R]) ->
    [{since_seq, V} | convert_options(R)];
convert_options([{<<"use_checkpoints">>, V} | R]) ->
    [{use_checkpoints, V} | convert_options(R)];
convert_options([{<<"use_bulk_get">>, V} | _R]) when not is_boolean(V) ->
    throw({bad_request, <<"parameter `use_bulk_get` must be a boolean">>});
convert_options([{<<"use_bulk_get">>, V} | R]) ->
    [{use_bulk_get, V} | convert_options(R)];
convert_options([{<<"checkpoint_interval">>, V} | R]) ->
    [{checkpoint_interval, couch_util:to_integer(V)} | convert_options(R)];
% skip unknown option
convert_options([_ | R]) ->
    convert_options(R).

-spec check_options([_]) -> [_].
check_options(Options) ->
    DocIds = lists:keyfind(doc_ids, 1, Options),
    Filter = lists:keyfind(filter, 1, Options),
    Selector = lists:keyfind(selector, 1, Options),
    case {DocIds, Filter, Selector} of
        {false, false, false} -> Options;
        {false, false, _} -> Options;
        {false, _, false} -> Options;
        {_, false, false} -> Options;
        _ -> throw({bad_request, "`doc_ids`,`filter`,`selector` are mutually exclusive"})
    end.

-spec parse_proxy_params(binary() | [_]) -> [_].
parse_proxy_params(ProxyUrl) when is_binary(ProxyUrl) ->
    parse_proxy_params(?b2l(ProxyUrl));
parse_proxy_params([]) ->
    [];
parse_proxy_params(ProxyUrl) when is_list(ProxyUrl) ->
    #url{
        host = Host,
        port = Port,
        username = User,
        password = Passwd,
        protocol = Protocol
    } = ibrowse_lib:parse_url(ProxyUrl),
    Params =
        [
            {proxy_host, Host},
            {proxy_port, Port}
        ] ++
            case is_list(User) andalso is_list(Passwd) of
                false ->
                    [];
                true ->
                    [{proxy_user, User}, {proxy_password, Passwd}]
            end,
    ok = check_protocol(Protocol, proxy),
    case Protocol of
        socks5 ->
            [proxy_to_socks5(Param) || Param <- Params];
        _ ->
            Params
    end;
parse_proxy_params(_) ->
    throw({error, <<"Invalid proxy url">>}).

-spec proxy_to_socks5({atom(), string()}) -> {atom(), string()}.
proxy_to_socks5({proxy_host, Val}) ->
    {socks5_host, Val};
proxy_to_socks5({proxy_port, Val}) ->
    {socks5_port, Val};
proxy_to_socks5({proxy_user, Val}) ->
    {socks5_user, Val};
proxy_to_socks5({proxy_password, Val}) ->
    {socks5_password, Val}.

-spec ssl_params([_]) -> [_].
ssl_params(Url) ->
    case ibrowse_lib:parse_url(Url) of
        #url{protocol = https} ->
            Depth = cfg_int("ssl_certificate_max_depth", 3),
            VerifyCerts = cfg_boolean("verify_ssl_certificates", false),
            CertFile = cfg("cert_file"),
            KeyFile = cfg("key_file"),
            Password = cfg("password"),
            SslOpts = [{depth, Depth} | ssl_verify_options(VerifyCerts)],
            SslOpts1 =
                case CertFile /= undefined andalso KeyFile /= undefined of
                    true ->
                        case Password of
                            undefined ->
                                [{certfile, CertFile}, {keyfile, KeyFile}] ++ SslOpts;
                            _ ->
                                [
                                    {certfile, CertFile},
                                    {keyfile, KeyFile},
                                    {password, Password}
                                ] ++ SslOpts
                        end;
                    false ->
                        SslOpts
                end,
            [{is_ssl, true}, {ssl_options, SslOpts1}];
        #url{protocol = http} ->
            []
    end.

-spec ssl_verify_options(true | false) -> [_].
ssl_verify_options(true) ->
    CAFile = cfg("ssl_trusted_certificates_file"),
    [
        {verify, verify_peer},
        {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]},
        {cacertfile, CAFile}
    ];
ssl_verify_options(false) ->
    [{verify, verify_none}].

get_value(Key, Props) ->
    couch_util:get_value(Key, Props).

get_value(Key, Props, Default) ->
    couch_util:get_value(Key, Props, Default).

to_binary(Val) ->
    couch_util:to_binary(Val).

get_json_value(Key, Obj) ->
    couch_replicator_utils:get_json_value(Key, Obj).

get_json_value(Key, Obj, Default) ->
    couch_replicator_utils:get_json_value(Key, Obj, Default).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

check_options_pass_values_test() ->
    ?assertEqual(check_options([]), []),
    ?assertEqual(check_options([baz, {other, fiz}]), [baz, {other, fiz}]),
    ?assertEqual(check_options([{doc_ids, x}]), [{doc_ids, x}]),
    ?assertEqual(check_options([{filter, x}]), [{filter, x}]),
    ?assertEqual(check_options([{selector, x}]), [{selector, x}]).

check_options_fail_values_test() ->
    ?assertThrow(
        {bad_request, _},
        check_options([{doc_ids, x}, {filter, y}])
    ),
    ?assertThrow(
        {bad_request, _},
        check_options([{doc_ids, x}, {selector, y}])
    ),
    ?assertThrow(
        {bad_request, _},
        check_options([{filter, x}, {selector, y}])
    ),
    ?assertThrow(
        {bad_request, _},
        check_options([{doc_ids, x}, {selector, y}, {filter, z}])
    ).

check_convert_options_pass_test() ->
    ?assertEqual([], convert_options([])),
    ?assertEqual([], convert_options([{<<"random">>, 42}])),
    ?assertEqual(
        [{cancel, true}],
        convert_options([{<<"cancel">>, true}])
    ),
    ?assertEqual(
        [{create_target, true}],
        convert_options([{<<"create_target">>, true}])
    ),
    ?assertEqual(
        [{winning_revs_only, true}],
        convert_options([{<<"winning_revs_only">>, true}])
    ),
    ?assertEqual(
        [{continuous, true}],
        convert_options([{<<"continuous">>, true}])
    ),
    ?assertEqual(
        [{doc_ids, [<<"id">>]}],
        convert_options([{<<"doc_ids">>, [<<"id">>]}])
    ),
    ?assertEqual(
        [{selector, {key, value}}],
        convert_options([{<<"selector">>, {key, value}}])
    ),
    ?assertEqual(
        [{query_params, {[{<<"x">>, 1}]}}],
        convert_options([{<<"query_params">>, {[{<<"x">>, 1}]}}])
    ).

check_convert_options_fail_test() ->
    ?assertThrow(
        {bad_request, _},
        convert_options([{<<"cancel">>, <<"true">>}])
    ),
    ?assertThrow(
        {bad_request, _},
        convert_options([{<<"create_target">>, <<"true">>}])
    ),
    ?assertThrow(
        {bad_request, _},
        convert_options([{<<"winning_revs_only">>, <<"foo">>}])
    ),
    ?assertThrow(
        {bad_request, _},
        convert_options([{<<"continuous">>, <<"true">>}])
    ),
    ?assertThrow(
        {bad_request, _},
        convert_options([{<<"doc_ids">>, not_a_list}])
    ),
    ?assertThrow(
        {bad_request, _},
        convert_options([{<<"selector">>, [{key, value}]}])
    ),
    ?assertThrow(
        {bad_request, _},
        convert_options([{<<"query_params">>, 42}])
    ).

rep_user_ctx_test() ->
    RepDoc = {[{<<"user_ctx">>, {[]}}]},
    ?assertEqual(
        #user_ctx{name = null, roles = [], handler = undefined},
        rep_user_ctx(RepDoc)
    ).

local_replication_endpoint_error_test_() ->
    {
        foreach,
        fun meck_config/0,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_error_on_local_endpoint),
            ?TDEF_FE(t_proxy_params_default),
            ?TDEF_FE(t_parse_db_string),
            ?TDEF_FE(t_parse_db_url),
            ?TDEF_FE(t_parse_db_invalid_protocol),
            ?TDEF_FE(t_parse_proxy_invalid_protocol),
            ?TDEF_FE(t_parse_sock_opts),
            ?TDEF_FE(t_parse_ibrowse_opts),
            ?TDEF_FE(t_parse_opts_invalid)
        ]
    }.

meck_config() ->
    meck:expect(config, get, fun(_, _, Default) -> Default end).

t_error_on_local_endpoint(_) ->
    RepDoc =
        {[
            {<<"_id">>, <<"someid">>},
            {<<"source">>, <<"localdb">>},
            {<<"target">>, <<"http://somehost.local/tgt">>}
        ]},
    Expect = local_endpoints_not_supported,
    ?assertThrow({bad_rep_doc, Expect}, parse_rep_doc_without_id(RepDoc)).

t_proxy_params_default(_) ->
    ?assertEqual(
        [
            {proxy_host, "foo.com"},
            {proxy_port, 443},
            {proxy_user, "u"},
            {proxy_password, "p"}
        ],
        parse_proxy_params("https://u:p@foo.com")
    ),
    ?assertEqual(
        [
            {socks5_host, "foo.com"},
            {socks5_port, 1080},
            {socks5_user, "u"},
            {socks5_password, "p"}
        ],
        parse_proxy_params("socks5://u:p@foo.com")
    ).

t_parse_db_string(_) ->
    ?assertMatch(
        #httpdb{
            url = "http://a/",
            proxy_url = "http://x"
        },
        parse_rep_db(<<"http://a">>, <<"http://x">>, [])
    ),
    ?assertMatch(
        #httpdb{
            url = "https://a/",
            proxy_url = "https://x"
        },
        parse_rep_db(<<"https://a">>, <<"https://x">>, [])
    ),
    ?assertThrow({error, _}, parse_rep_db(<<"abc">>, <<"foo">>, [])),
    ?assertThrow({error, _}, parse_rep_db(undefined, <<"foo">>, [])).

t_parse_db_url(_) ->
    ?assertMatch(
        #httpdb{
            url = "http://a?foo",
            proxy_url = "http://x"
        },
        parse_rep_db({[{<<"url">>, <<"http://a?foo">>}]}, <<"http://x">>, [])
    ),
    ?assertMatch(
        #httpdb{
            url = "https://a/",
            proxy_url = "https://x"
        },
        parse_rep_db({[{<<"url">>, <<"https://a">>}]}, <<"https://x">>, [])
    ),
    PUrl = <<"http://x">>,
    ?assertThrow({error, _}, parse_rep_db({[{<<"url">>, <<"abc">>}]}, PUrl, [])),
    ?assertThrow({error, _}, parse_rep_db({[{<<"url">>, <<"httpx://a">>}]}, PUrl, [])),
    ?assertThrow({error, _}, parse_rep_db({[]}, PUrl, [])).

t_parse_db_invalid_protocol(_) ->
    MeckFun = fun
        ("replicator", "valid_endpoint_protocols", _) -> "https";
        (_, _, Default) -> Default
    end,
    meck:expect(config, get, MeckFun),
    PUrl = <<"http://x">>,
    ?assertMatch(
        #httpdb{
            url = "https://a/",
            proxy_url = "http://x"
        },
        parse_rep_db({[{<<"url">>, <<"https://a">>}]}, PUrl, [])
    ),
    ?assertThrow({error, _}, parse_rep_db({[{<<"url">>, <<"http://a">>}]}, PUrl, [])).

t_parse_proxy_invalid_protocol(_) ->
    MeckFun = fun
        ("replicator", "valid_proxy_protocols", _) -> "socks5";
        (_, _, Default) -> Default
    end,
    meck:expect(config, get, MeckFun),
    Url = <<"http://a">>,
    ?assertMatch(
        #httpdb{
            url = "https://a/",
            proxy_url = "socks5://x"
        },
        parse_rep_db({[{<<"url">>, <<"https://a">>}]}, <<"socks5://x">>, [])
    ),
    ?assertThrow({error, _}, parse_rep_db({[{<<"url">>, Url}]}, <<"http://x">>, [])).

t_parse_sock_opts(_) ->
    %% more than two to ensure we string:split correctly
    Allowed = "priority, sndbuf, recbuf",
    MeckFun = fun
        ("replicator", "valid_socket_options", _) -> Allowed;
        (_, _, Default) -> Default
    end,
    meck:expect(config, get, MeckFun),
    RepDoc =
        {[
            {<<"source">>, <<"http://a">>},
            {<<"target">>, <<"http://b/">>},
            {<<"socket_options">>,
                <<"[{priority, 3}, {potato, true}, {sndbuf, 10000}, {recbuf, 10000}]">>}
        ]},
    Rep = parse_rep_doc_without_id(RepDoc),
    ?assertMatch(
        #rep{
            source = #httpdb{url = "http://a/"},
            target = #httpdb{url = "http://b/"},
            options = [{_, _} | _]
        },
        Rep
    ),
    Options = Rep#rep.options,
    ?assertEqual(
        [
            {checkpoint_interval, 30000},
            {connection_timeout, 30000},
            {http_connections, 20},
            {ibrowse_options, []},
            {retries, 5},
            {socket_options, [
                {priority, 3},
                {sndbuf, 10000},
                {recbuf, 10000}
            ]},
            {use_bulk_get, true},
            {use_checkpoints, true},
            {worker_batch_size, 500},
            {worker_processes, 4}
        ],
        Options
    ).

t_parse_ibrowse_opts(_) ->
    Allowed = "prefer_ipv6",
    MeckFun = fun
        ("replicator", "valid_ibrowse_options", _) -> Allowed;
        (_, _, Default) -> Default
    end,
    meck:expect(config, get, MeckFun),
    RepDoc =
        {[
            {<<"source">>, <<"http://a">>},
            {<<"target">>, <<"http://b/">>},
            {<<"ibrowse_options">>, <<"[{prefer_ipv6, true}]">>}
        ]},
    Rep = parse_rep_doc_without_id(RepDoc),
    ?assertMatch(
        #rep{
            source = #httpdb{url = "http://a/"},
            target = #httpdb{url = "http://b/"},
            options = [{_, _} | _]
        },
        Rep
    ),
    Options = Rep#rep.options,
    ?assertEqual(
        [
            {checkpoint_interval, 30000},
            {connection_timeout, 30000},
            {http_connections, 20},
            {ibrowse_options, [
                {prefer_ipv6, true}
            ]},
            {retries, 5},
            {socket_options, [
                {keepalive, true},
                {nodelay, false}
            ]},
            {use_bulk_get, true},
            {use_checkpoints, true},
            {worker_batch_size, 500},
            {worker_processes, 4}
        ],
        Options
    ).

t_parse_opts_invalid(_) ->
    ?assertEqual([], parse_opts(<<"<}garbage]][">>, [], [])).

-endif.
