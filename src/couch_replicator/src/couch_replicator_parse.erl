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
    parse_transient_rep/2,
    parse_rep/2,
    parse_rep_db/3
]).

-include_lib("ibrowse/include/ibrowse.hrl").
-include("couch_replicator.hrl").
-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_SOCK_OPTS, "[{keepalive, true}, {nodelay, false}]").
-define(VALID_SOCK_OPTS, [
    buffer,
    delay_send,
    exit_on_close,
    ipv6_v6only,
    keepalive,
    nodelay,
    recbuf,
    send_timeout,
    send_timout_close,
    sndbuf,
    priority,
    tos,
    tclass
]).
-define(VALID_PROXY_PROTOCOLS, [http, https, socks5]).
-define(CONFIG_DEFAULTS, [
    {"worker_processes", "4", fun list_to_integer/1},
    {"worker_batch_size", "500", fun list_to_integer/1},
    {"http_connections", "20", fun list_to_integer/1},
    {"connection_timeout", "30000", fun list_to_integer/1},
    {"retries_per_request", "5", fun list_to_integer/1},
    {"use_checkpoints", "true", fun list_to_existing_atom/1},
    {"checkpoint_interval", "30000", fun list_to_integer/1},
    {"socket_options", ?DEFAULT_SOCK_OPTS, fun parse_sock_opts/1}
]).

-spec parse_rep_doc({[_]}) -> #{}.
parse_rep_doc(RepDoc) ->
    {ok, Rep} =
        try
            parse_rep(RepDoc, null)
        catch
            throw:{error, Reason}:Stack ->
                ?LOG_ERROR(#{
                    what => replication_doc_parse_error,
                    in => replicator,
                    details => Reason,
                    stacktrace => Stack
                }),
                LogErr1 = "~p parse_rep_doc fail ~p ~p",
                couch_log:error(LogErr1, [?MODULE, Reason, Stack]),
                throw({bad_rep_doc, Reason});
            Tag:Err:Stack ->
                ?LOG_ERROR(#{
                    what => replication_doc_parse_error,
                    in => replicator,
                    tag => Tag,
                    details => Err,
                    stacktrace => Stack
                }),
                LogErr2 = "~p parse_rep_doc fail ~p:~p ~p",
                couch_log:error(LogErr2, [?MODULE, Tag, Err, Stack]),
                throw({bad_rep_doc, couch_util:to_binary({Tag, Err})})
        end,
    Rep.

-spec parse_transient_rep({[_]} | #{}, user_name()) -> {ok, #{}}.
parse_transient_rep({Props} = EJson, UserName) when is_list(Props) ->
    Str = couch_util:json_encode(EJson),
    Map = couch_util:json_decode(Str, [return_maps]),
    parse_transient_rep(Map, UserName);
parse_transient_rep(#{} = Body, UserName) ->
    {ok, Rep} =
        try
            parse_rep(Body, UserName)
        catch
            throw:{error, Reason}:Stack ->
                ?LOG_ERROR(#{
                    what => transient_replication_parse_error,
                    in => replicator,
                    details => Reason,
                    stacktrace => Stack
                }),
                LogErr1 = "~p parse_transient_rep fail ~p ~p",
                couch_log:error(LogErr1, [?MODULE, Reason, Stack]),
                throw({bad_request, Reason});
            Tag:Err:Stack ->
                ?LOG_ERROR(#{
                    what => transient_replication_parse_error,
                    in => replicator,
                    tag => Tag,
                    details => Err,
                    stacktrace => Stack
                }),
                LogErr2 = "~p parse_transient_rep fail ~p ~p",
                couch_log:error(LogErr2, [?MODULE, Tag, Err, Stack]),
                throw({bad_request, couch_util:to_binary({Tag, Err})})
        end,
    #{?OPTIONS := Options} = Rep,
    Cancel = maps:get(<<"cancel">>, Options, false),
    Id = maps:get(<<"id">>, Options, nil),
    case {Cancel, Id} of
        {true, nil} ->
            % Cancel request with no id, must parse id out of body contents
            JobId = couch_replicator_ids:job_id(Rep, null, null),
            {ok, JobId, Rep};
        {true, Id} ->
            % Cancel request with an id specified, so do not parse id from body
            {ok, Id, Rep};
        {false, _Id} ->
            JobId = couch_replicator_ids:job_id(Rep, null, null),
            % Not a cancel request, regular replication doc
            {ok, JobId, Rep}
    end.

-spec parse_rep({[_]} | #{}, user_name()) -> {ok, #{}}.
parse_rep({Props} = EJson, UserName) when is_list(Props) ->
    Str = couch_util:json_encode(EJson),
    Map = couch_util:json_decode(Str, [return_maps]),
    parse_rep(Map, UserName);
parse_rep(#{} = Doc, UserName) ->
    {SrcProxy, TgtProxy} = parse_proxy_settings(Doc),
    Opts = make_options(Doc),
    Cancel = maps:get(<<"cancel">>, Opts, false),
    Id = maps:get(<<"id">>, Opts, nil),
    case Cancel andalso Id =/= nil of
        true ->
            {ok, #{?OPTIONS => Opts, ?REP_USER => UserName}};
        false ->
            case {maps:is_key(?SOURCE, Doc), maps:is_key(?TARGET, Doc)} of
                {false, _} -> throw({error, <<"Missing `source` field">>});
                {_, false} -> throw({error, <<"Missing `target` field">>});
                {true, true} -> ok
            end,
            #{?SOURCE := Source0, ?TARGET := Target0} = Doc,
            Source = parse_rep_db(Source0, SrcProxy, Opts),
            Target = parse_rep_db(Target0, TgtProxy, Opts),
            case couch_replicator_filters:view_type(Doc, Opts) of
                {error, Error} -> throw({error, Error});
                _ -> ok
            end,
            case couch_replicator_filters:parse(Opts) of
                {ok, _} -> ok;
                {error, FilterError} -> throw({error, FilterError})
            end,
            Rep = #{
                ?SOURCE => Source,
                ?TARGET => Target,
                ?OPTIONS => Opts,
                ?REP_USER => UserName,
                ?START_TIME => erlang:system_time(second)
            },
            {ok, Rep}
    end.

-spec parse_rep_db(#{}, #{}, #{}) -> #{}.
parse_rep_db(#{} = Endpoint, #{} = ProxyParams, #{} = Options) ->
    ProxyUrl =
        case ProxyParams of
            #{<<"proxy_url">> := PUrl} -> PUrl;
            _ -> null
        end,

    Url0 = maps:get(<<"url">>, Endpoint),
    Url = maybe_add_trailing_slash(Url0),

    AuthProps = maps:get(<<"auth">>, Endpoint, #{}),
    if
        is_map(AuthProps) -> ok;
        true -> throw({error, "if defined, `auth` must be an object"})
    end,

    Headers0 = maps:get(<<"headers">>, Endpoint, #{}),
    if
        is_map(Headers0) -> ok;
        true -> throw({error, "if defined `headers` must be an object"})
    end,
    DefaultHeaders = couch_replicator_utils:default_headers_map(),
    Headers = maps:merge(DefaultHeaders, Headers0),

    SockOpts = maps:get(<<"socket_options">>, Options, #{}),
    SockAndProxy = maps:merge(
        #{
            <<"socket_options">> => SockOpts
        },
        ProxyParams
    ),
    SslParams = ssl_params(Url),

    HttpDb = #{
        <<"url">> => Url,
        <<"auth_props">> => AuthProps,
        <<"headers">> => Headers,
        <<"ibrowse_options">> => maps:merge(SslParams, SockAndProxy),
        <<"timeout">> => maps:get(<<"connection_timeout">>, Options),
        <<"http_connections">> => maps:get(<<"http_connections">>, Options),
        <<"retries">> => maps:get(<<"retries_per_request">>, Options),
        <<"proxy_url">> => ProxyUrl
    },
    normalize_basic_auth(HttpDb);
parse_rep_db(<<"http://", _/binary>> = Url, Proxy, Options) ->
    parse_rep_db(#{<<"url">> => Url}, Proxy, Options);
parse_rep_db(<<"https://", _/binary>> = Url, Proxy, Options) ->
    parse_rep_db(#{<<"url">> => Url}, Proxy, Options);
parse_rep_db(<<_/binary>>, _Proxy, _Options) ->
    throw({error, local_endpoints_not_supported});
parse_rep_db(undefined, _Proxy, _Options) ->
    throw({error, <<"Missing replication endpoint">>}).

parse_proxy_settings(#{} = Doc) ->
    Proxy = maps:get(?PROXY, Doc, <<>>),
    SrcProxy = maps:get(?SOURCE_PROXY, Doc, <<>>),
    TgtProxy = maps:get(?TARGET_PROXY, Doc, <<>>),

    case Proxy =/= <<>> of
        true when SrcProxy =/= <<>> ->
            Error = "`proxy` is mutually exclusive with `source_proxy`",
            throw({error, Error});
        true when TgtProxy =/= <<>> ->
            Error = "`proxy` is mutually exclusive with `target_proxy`",
            throw({error, Error});
        true ->
            {parse_proxy_params(Proxy), parse_proxy_params(Proxy)};
        false ->
            {parse_proxy_params(SrcProxy), parse_proxy_params(TgtProxy)}
    end.

-spec maybe_add_trailing_slash(binary()) -> binary().
maybe_add_trailing_slash(<<>>) ->
    <<>>;
maybe_add_trailing_slash(Url) when is_binary(Url) ->
    case binary:match(Url, <<"?">>) of
        nomatch ->
            case binary:last(Url) of
                $/ -> Url;
                _ -> <<Url/binary, "/">>
            end;
        _ ->
            % skip if there are query params
            Url
    end.

-spec make_options(#{}) -> #{}.
make_options(#{} = RepDoc) ->
    Options0 = convert_options(RepDoc),
    Options = check_options(Options0),
    ConfigOptions = lists:foldl(
        fun({K, Default, ConversionFun}, Acc) ->
            V = ConversionFun(config:get("replicator", K, Default)),
            Acc#{list_to_binary(K) => V}
        end,
        #{},
        ?CONFIG_DEFAULTS
    ),
    maps:merge(ConfigOptions, Options).

-spec convert_options(#{}) -> #{} | no_return().
convert_options(#{} = Doc) ->
    maps:fold(fun convert_fold/3, #{}, Doc).

-spec convert_fold(binary(), any(), #{}) -> #{}.
convert_fold(<<"cancel">>, V, Acc) when is_boolean(V) ->
    Acc#{<<"cancel">> => V};
convert_fold(<<"cancel">>, _, _) ->
    throw({error, <<"`cancel` must be a boolean">>});
convert_fold(IdOpt, V, Acc) when
    IdOpt =:= <<"_local_id">>;
    IdOpt =:= <<"replication_id">>;
    IdOpt =:= <<"id">>
->
    Acc#{<<"id">> => couch_replicator_ids:convert(V)};
convert_fold(<<"create_target">>, V, Acc) when is_boolean(V) ->
    Acc#{<<"create_target">> => V};
convert_fold(<<"create_target">>, _, _) ->
    throw({error, <<"`create_target` must be a boolean">>});
convert_fold(<<"create_target_params">>, #{} = V, Acc) ->
    Acc#{<<"create_target_params">> => V};
convert_fold(<<"create_target_params">>, _, _) ->
    throw({error, <<"`create_target_params` must be an object">>});
convert_fold(<<"continuous">>, V, Acc) when is_boolean(V) ->
    Acc#{<<"continuous">> => V};
convert_fold(<<"continuous">>, _, _) ->
    throw({error, <<"`continuous` must be a boolean">>});
convert_fold(<<"filter">>, V, Acc) when is_binary(V), byte_size(V) > 1 ->
    Acc#{<<"filter">> => V};
convert_fold(<<"filter">>, _, _) ->
    throw({error, <<"`filter` must be a string">>});
convert_fold(<<"query_params">>, V, Acc) when is_map(V) orelse V =:= null ->
    Acc#{<<"query_params">> => V};
convert_fold(<<"query_params">>, _, _Acc) ->
    throw({error, <<"`query_params` is not `null` or object">>});
convert_fold(<<"doc_ids">>, null, Acc) ->
    Acc;
convert_fold(<<"doc_ids">>, V, Acc) when is_list(V) ->
    % Compatibility behaviour as: accept a list of percent encoded doc IDs
    Ids = lists:map(
        fun(Id) ->
            case is_binary(Id) andalso byte_size(Id) > 0 of
                true -> list_to_binary(couch_httpd:unquote(Id));
                false -> throw({error, <<"`doc_ids` array must contain strings">>})
            end
        end,
        V
    ),
    Acc#{<<"doc_ids">> => lists:usort(Ids)};
convert_fold(<<"doc_ids">>, _, _) ->
    throw({error, <<"`doc_ids` must be an array">>});
convert_fold(<<"selector">>, #{} = V, Acc) ->
    Acc#{<<"selector">> => V};
convert_fold(<<"selector">>, _, _Acc) ->
    throw({error, <<"`selector` must be a JSON object">>});
convert_fold(<<"worker_processes">>, V, Acc) ->
    Acc#{<<"worker_processes">> => bin2int(V, <<"worker_processes">>)};
convert_fold(<<"worker_batch_size">>, V, Acc) ->
    Acc#{<<"worker_batch_size">> => bin2int(V, <<"worker_batch_size">>)};
convert_fold(<<"http_connections">>, V, Acc) ->
    Acc#{<<"http_connections">> => bin2int(V, <<"http_connections">>)};
convert_fold(<<"connection_timeout">>, V, Acc) ->
    Acc#{<<"connection_timeout">> => bin2int(V, <<"connection_timeout">>)};
convert_fold(<<"retries_per_request">>, V, Acc) ->
    Acc#{<<"retries_per_request">> => bin2int(V, <<"retries_per_request">>)};
convert_fold(<<"socket_options">>, V, Acc) ->
    Acc#{<<"socket_options">> => parse_sock_opts(V)};
convert_fold(<<"since_seq">>, V, Acc) ->
    Acc#{<<"since_seq">> => V};
convert_fold(<<"use_checkpoints">>, V, Acc) when is_boolean(V) ->
    Acc#{<<"use_checkpoints">> => V};
convert_fold(<<"use_checkpoints">>, _, _) ->
    throw({error, <<"`use_checkpoints` must be a boolean">>});
convert_fold(<<"checkpoint_interval">>, V, Acc) ->
    Acc#{<<"checkpoint_interval">> => bin2int(V, <<"checkpoint_interval">>)};
% skip unknown option
convert_fold(_K, _V, Acc) ->
    Acc.

bin2int(V, _Field) when is_integer(V) ->
    V;
bin2int(V, Field) when is_binary(V) ->
    try
        erlang:binary_to_integer(V)
    catch
        error:badarg ->
            throw({error, <<"`", Field/binary, "` must be an integer">>})
    end;
bin2int(_V, Field) ->
    throw({error, <<"`", Field/binary, "` must be an integer">>}).

-spec check_options(#{}) -> #{}.
check_options(Options) ->
    DocIds = maps:is_key(<<"doc_ids">>, Options),
    Filter = maps:is_key(<<"filter">>, Options),
    Selector = maps:is_key(<<"selector">>, Options),
    case {DocIds, Filter, Selector} of
        {false, false, false} ->
            Options;
        {false, false, _} ->
            Options;
        {false, _, false} ->
            Options;
        {_, false, false} ->
            Options;
        _ ->
            throw(
                {error, <<
                    "`doc_ids`,`filter`,`selector` are mutually "
                    " exclusive"
                >>}
            )
    end.

parse_sock_opts(Term) ->
    {ok, SocketOptions} = couch_util:parse_term(Term),
    lists:foldl(
        fun
            ({K, V}, Acc) when is_atom(K) ->
                case lists:member(K, ?VALID_SOCK_OPTS) of
                    true -> Acc#{atom_to_binary(K, utf8) => V};
                    false -> Acc
                end;
            (_, Acc) ->
                Acc
        end,
        #{},
        SocketOptions
    ).

-spec parse_proxy_params(binary() | #{}) -> #{}.
parse_proxy_params(<<>>) ->
    #{};
parse_proxy_params(ProxyUrl) when is_binary(ProxyUrl) ->
    #url{
        host = Host,
        port = Port,
        username = User,
        password = Passwd,
        protocol = Prot0
    } = ibrowse_lib:parse_url(binary_to_list(ProxyUrl)),
    Prot =
        case lists:member(Prot0, ?VALID_PROXY_PROTOCOLS) of
            true -> atom_to_binary(Prot0, utf8);
            false -> throw({error, <<"Unsupported proxy protocol">>})
        end,
    ProxyParams = #{
        <<"proxy_url">> => ProxyUrl,
        <<"proxy_protocol">> => Prot,
        <<"proxy_host">> => list_to_binary(Host),
        <<"proxy_port">> => Port
    },
    case is_list(User) andalso is_list(Passwd) of
        true ->
            ProxyParams#{
                <<"proxy_user">> => list_to_binary(User),
                <<"proxy_password">> => list_to_binary(Passwd)
            };
        false ->
            ProxyParams
    end.

-spec ssl_params(binary()) -> #{}.
ssl_params(Url) ->
    case ibrowse_lib:parse_url(binary_to_list(Url)) of
        #url{protocol = https} ->
            Depth = list_to_integer(
                config:get("replicator", "ssl_certificate_max_depth", "3")
            ),
            VerifyCerts = config:get("replicator", "verify_ssl_certificates"),
            CertFile = config:get("replicator", "cert_file", null),
            KeyFile = config:get("replicator", "key_file", null),
            Password = config:get("replicator", "password", null),
            VerifySslOptions = ssl_verify_options(VerifyCerts =:= "true"),
            SslOpts = maps:merge(VerifySslOptions, #{<<"depth">> => Depth}),
            HaveCertAndKey = CertFile /= null andalso KeyFile /= null,
            SslOpts1 =
                case HaveCertAndKey of
                    false ->
                        SslOpts;
                    true ->
                        CertOpts0 = #{
                            <<"certfile">> => list_to_binary(CertFile),
                            <<"keyfile">> => list_to_binary(KeyFile)
                        },
                        CertOpts =
                            case Password of
                                null -> CertOpts0;
                                _ -> CertOpts0#{<<"password">> => list_to_binary(Password)}
                            end,
                        maps:merge(SslOpts, CertOpts)
                end,
            #{<<"is_ssl">> => true, <<"ssl_options">> => SslOpts1};
        #url{protocol = http} ->
            #{}
    end.

-spec ssl_verify_options(true | false) -> [_].
ssl_verify_options(true) ->
    case config:get("replicator", "ssl_trusted_certificates_file") of
        undefined ->
            #{
                <<"verify">> => <<"verify_peer">>,
                <<"cacertfile">> => null
            };
        CAFile when is_list(CAFile) ->
            #{
                <<"verify">> => <<"verify_peer">>,
                <<"cacertfile">> => list_to_binary(CAFile)
            }
    end;
ssl_verify_options(false) ->
    #{
        <<"verify">> => <<"verify_none">>
    }.

-spec set_basic_auth_creds(string(), string(), map()) -> map().
set_basic_auth_creds(undefined, undefined, #{} = HttpDb) ->
    HttpDb;
set_basic_auth_creds(User, Pass, #{} = HttpDb) when
    is_list(User), is_list(Pass)
->
    #{<<"auth_props">> := AuthProps} = HttpDb,
    UserPass = #{
        <<"username">> => list_to_binary(User),
        <<"password">> => list_to_binary(Pass)
    },
    AuthProps1 = AuthProps#{<<"basic">> => UserPass},
    HttpDb#{<<"auth_props">> := AuthProps1}.

-spec extract_creds_from_url(binary()) ->
    {ok, {string() | undefined, string() | undefined}, string()}
    | {error, term()}.
extract_creds_from_url(Url0) ->
    Url = binary_to_list(Url0),
    case ibrowse_lib:parse_url(Url) of
        {error, Error} ->
            {error, Error};
        #url{username = undefined, password = undefined} ->
            {ok, {undefined, undefined}, Url0};
        #url{protocol = Proto, username = User, password = Pass} ->
            % Excise user and pass parts from the url. Try to keep the host,
            % port and path as they were in the original.
            Prefix = lists:concat([Proto, "://", User, ":", Pass, "@"]),
            Suffix = lists:sublist(Url, length(Prefix) + 1, length(Url) + 1),
            NoCreds = lists:concat([Proto, "://", Suffix]),
            {ok, {User, Pass}, list_to_binary(NoCreds)}
    end.

% Normalize basic auth credentials so they are set only in the auth props
% object. If multiple basic auth credentials are provided, the resulting
% credentials are picked in the following order.
%  1) {"auth": "basic": {"username":.., "password": ...} ...}
%  2) URL userinfo part
%  3) "Authentication" : "basic $base64" headers
%
-spec normalize_basic_auth(map()) -> map().
normalize_basic_auth(#{} = HttpDb) ->
    #{
        <<"url">> := Url,
        <<"headers">> := Headers
    } = HttpDb,
    {HeaderCreds, HeadersNoCreds} = remove_basic_auth_from_headers(Headers),
    {UrlCreds, UrlWithoutCreds} =
        case extract_creds_from_url(Url) of
            {ok, Creds = {_, _}, UrlNoCreds} ->
                {Creds, UrlNoCreds};
            {error, _Error} ->
                % Don't crash replicator if user provided an invalid
                % userinfo part
                {undefined, undefined}
        end,
    AuthCreds = {_, _} = couch_replicator_utils:get_basic_auth_creds(HttpDb),
    HttpDb1 = HttpDb#{
        <<"url">> := UrlWithoutCreds,
        <<"headers">> := HeadersNoCreds
    },
    {User, Pass} =
        case {AuthCreds, UrlCreds, HeaderCreds} of
            {{U, P}, {_, _}, {_, _}} when is_list(U), is_list(P) -> {U, P};
            {{_, _}, {U, P}, {_, _}} when is_list(U), is_list(P) -> {U, P};
            {{_, _}, {_, _}, {U, P}} -> {U, P}
        end,
    set_basic_auth_creds(User, Pass, HttpDb1).

remove_basic_auth_from_headers(#{} = HeadersMap) ->
    % Headers are passed in a map however mochiweb_headers expects them to be
    % lists so we transform them to lists first, then back to maps
    Headers = maps:fold(
        fun(K, V, Acc) ->
            [{binary_to_list(K), binary_to_list(V)} | Acc]
        end,
        [],
        HeadersMap
    ),
    Headers1 = mochiweb_headers:make(Headers),
    case mochiweb_headers:get_value("Authorization", Headers1) of
        undefined ->
            {{undefined, undefined}, HeadersMap};
        Auth ->
            {Basic, B64} = lists:splitwith(fun(X) -> X =/= $\s end, Auth),
            BasicLower = string:to_lower(Basic),
            Result = maybe_remove_basic_auth(BasicLower, B64, Headers1),
            {{User, Pass}, Headers2} = Result,
            HeadersMapResult = lists:foldl(
                fun({K, V}, Acc) ->
                    Acc#{list_to_binary(K) => list_to_binary(V)}
                end,
                #{},
                Headers2
            ),
            {{User, Pass}, HeadersMapResult}
    end.

maybe_remove_basic_auth("basic", " " ++ Base64, Headers) ->
    Headers1 = mochiweb_headers:delete_any("Authorization", Headers),
    {decode_basic_creds(Base64), mochiweb_headers:to_list(Headers1)};
maybe_remove_basic_auth(_, _, Headers) ->
    {{undefined, undefined}, mochiweb_headers:to_list(Headers)}.

decode_basic_creds(Base64) ->
    try re:split(base64:decode(Base64), ":", [{return, list}, {parts, 2}]) of
        [User, Pass] ->
            {User, Pass};
        _ ->
            {undefined, undefined}
    catch
        % Tolerate invalid B64 values here to avoid crashing replicator
        error:function_clause ->
            {undefined, undefined}
    end.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("fabric/test/fabric2_test.hrl").

check_options_pass_values_test() ->
    ?assertEqual(check_options(#{}), #{}),
    ?assertEqual(
        check_options(#{<<"baz">> => <<"foo">>}),
        #{<<"baz">> => <<"foo">>}
    ),
    ?assertEqual(
        check_options(#{<<"doc_ids">> => [<<"x">>]}),
        #{<<"doc_ids">> => [<<"x">>]}
    ),
    ?assertEqual(
        check_options(#{<<"filter">> => <<"f">>}),
        #{<<"filter">> => <<"f">>}
    ),
    ?assertEqual(
        check_options(#{<<"selector">> => <<"s">>}),
        #{<<"selector">> => <<"s">>}
    ).

check_options_fail_values_test() ->
    ?assertThrow(
        {error, _},
        check_options(#{<<"doc_ids">> => [], <<"filter">> => <<"f">>})
    ),
    ?assertThrow(
        {error, _},
        check_options(#{<<"doc_ids">> => [], <<"selector">> => <<"s">>})
    ),
    ?assertThrow(
        {error, _},
        check_options(#{<<"filter">> => <<"f">>, <<"selector">> => <<"s">>})
    ),
    ?assertThrow(
        {error, _},
        check_options(#{
            <<"doc_ids">> => [],
            <<"filter">> => <<"f">>,
            <<"selector">> => <<"s">>
        })
    ).

check_convert_options_pass_test() ->
    ?assertEqual(#{}, convert_options(#{})),
    ?assertEqual(#{}, convert_options(#{<<"random">> => 42})),
    ?assertEqual(
        #{<<"cancel">> => true},
        convert_options(#{<<"cancel">> => true})
    ),
    ?assertEqual(
        #{<<"create_target">> => true},
        convert_options(#{<<"create_target">> => true})
    ),
    ?assertEqual(
        #{<<"continuous">> => true},
        convert_options(#{<<"continuous">> => true})
    ),
    ?assertEqual(
        #{<<"doc_ids">> => [<<"id">>]},
        convert_options(#{<<"doc_ids">> => [<<"id">>]})
    ),
    ?assertEqual(
        #{<<"selector">> => #{<<"key">> => <<"value">>}},
        convert_options(#{<<"selector">> => #{<<"key">> => <<"value">>}})
    ).

check_convert_options_fail_test() ->
    ?assertThrow(
        {error, _},
        convert_options(#{<<"cancel">> => <<"true">>})
    ),
    ?assertThrow(
        {error, _},
        convert_options(#{<<"create_target">> => <<"true">>})
    ),
    ?assertThrow(
        {error, _},
        convert_options(#{<<"continuous">> => <<"true">>})
    ),
    ?assertThrow(
        {error, _},
        convert_options(#{<<"doc_ids">> => <<"not_a_list">>})
    ),
    ?assertThrow(
        {error, _},
        convert_options(#{<<"selector">> => <<"bad">>})
    ).

local_replication_endpoint_error_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_error_on_local_endpoint)
        ]
    }.

setup() ->
    meck:expect(config, get, fun(_, _, Default) -> Default end).

teardown(_) ->
    meck:unload().

t_error_on_local_endpoint(_) ->
    RepDoc =
        {[
            {<<"_id">>, <<"someid">>},
            {<<"source">>, <<"localdb">>},
            {<<"target">>, <<"http://somehost.local/tgt">>}
        ]},
    Expect = local_endpoints_not_supported,
    ?assertThrow({bad_rep_doc, Expect}, parse_rep_doc(RepDoc)).

remove_basic_auth_from_headers_test_() ->
    B64 = list_to_binary(b64creds("user", "pass")),
    [
        ?_assertEqual(
            {{User, Pass}, NoAuthHeaders},
            remove_basic_auth_from_headers(Headers)
        )
     || {{User, Pass, NoAuthHeaders}, Headers} <- [
            {
                {undefined, undefined, #{}},
                #{}
            },
            {
                {undefined, undefined, #{<<"h">> => <<"v">>}},
                #{<<"h">> => <<"v">>}
            },
            {
                {undefined, undefined, #{<<"Authorization">> => <<"junk">>}},
                #{<<"Authorization">> => <<"junk">>}
            },
            {
                {undefined, undefined, #{}},
                #{<<"Authorization">> => <<"basic X">>}
            },
            {
                {"user", "pass", #{}},
                #{<<"Authorization">> => <<"Basic ", B64/binary>>}
            },
            {
                {"user", "pass", #{}},
                #{<<"AuThorization">> => <<"Basic ", B64/binary>>}
            },
            {
                {"user", "pass", #{}},
                #{<<"Authorization">> => <<"bAsIc ", B64/binary>>}
            },
            {
                {"user", "pass", #{<<"h">> => <<"v">>}},
                #{
                    <<"Authorization">> => <<"Basic ", B64/binary>>,
                    <<"h">> => <<"v">>
                }
            }
        ]
    ].

b64creds(User, Pass) ->
    base64:encode_to_string(User ++ ":" ++ Pass).

set_basic_auth_creds_test() ->
    Check = fun(User, Pass, Props) ->
        HttpDb = #{<<"auth_props">> => Props},
        HttpDb1 = set_basic_auth_creds(User, Pass, HttpDb),
        maps:get(<<"auth_props">>, HttpDb1)
    end,

    ?assertEqual(#{}, Check(undefined, undefined, #{})),

    ?assertEqual(
        #{<<"other">> => #{}},
        Check(
            undefined,
            undefined,
            #{<<"other">> => #{}}
        )
    ),

    ?assertEqual(
        #{
            <<"basic">> => #{
                <<"username">> => <<"u">>,
                <<"password">> => <<"p">>
            }
        },
        Check("u", "p", #{})
    ),

    ?assertEqual(
        #{
            <<"other">> => #{},
            <<"basic">> => #{
                <<"username">> => <<"u">>,
                <<"password">> => <<"p">>
            }
        },
        Check("u", "p", #{<<"other">> => #{}})
    ).

normalize_basic_creds_test_() ->
    DefaultHeaders = couch_replicator_utils:default_headers_map(),
    [
        ?_assertEqual(Expect, normalize_basic_auth(Input))
     || {Input, Expect} <- [
            {
                #{
                    <<"url">> => <<"http://u:p@x.y/db">>,
                    <<"auth_props">> => #{},
                    <<"headers">> => DefaultHeaders
                },
                #{
                    <<"url">> => <<"http://x.y/db">>,
                    <<"auth_props">> => auth_props("u", "p"),
                    <<"headers">> => DefaultHeaders
                }
            },
            {
                #{
                    <<"url">> => <<"http://u:p@h:80/db">>,
                    <<"auth_props">> => #{},
                    <<"headers">> => DefaultHeaders
                },
                #{
                    <<"url">> => <<"http://h:80/db">>,
                    <<"auth_props">> => auth_props("u", "p"),
                    <<"headers">> => DefaultHeaders
                }
            },
            {
                #{
                    <<"url">> => <<"https://u:p@h/db">>,
                    <<"auth_props">> => #{},
                    <<"headers">> => DefaultHeaders
                },
                #{
                    <<"url">> => <<"https://h/db">>,
                    <<"auth_props">> => auth_props("u", "p"),
                    <<"headers">> => DefaultHeaders
                }
            },
            {
                #{
                    <<"url">> => <<"http://u:p@[2001:db8:a1b:12f9::1]/db">>,
                    <<"auth_props">> => #{},
                    <<"headers">> => DefaultHeaders
                },
                #{
                    <<"url">> => <<"http://[2001:db8:a1b:12f9::1]/db">>,
                    <<"auth_props">> => auth_props("u", "p"),
                    <<"headers">> => DefaultHeaders
                }
            },
            {
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => #{},
                    <<"headers">> => maps:merge(DefaultHeaders, #{
                        <<"authorization">> => basic_b64("u", "p")
                    })
                },
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => auth_props("u", "p"),
                    <<"headers">> => DefaultHeaders
                }
            },
            {
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => #{},
                    <<"headers">> => maps:merge(DefaultHeaders, #{
                        <<"authorization">> => basic_b64("u", "p@")
                    })
                },
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => auth_props("u", "p@"),
                    <<"headers">> => DefaultHeaders
                }
            },
            {
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => #{},
                    <<"headers">> => maps:merge(DefaultHeaders, #{
                        <<"authorization">> => basic_b64("u", "p@%40")
                    })
                },
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => auth_props("u", "p@%40"),
                    <<"headers">> => DefaultHeaders
                }
            },
            {
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => #{},
                    <<"headers">> => maps:merge(DefaultHeaders, #{
                        <<"aUthoriZation">> => basic_b64("U", "p")
                    })
                },
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => auth_props("U", "p"),
                    <<"headers">> => DefaultHeaders
                }
            },
            {
                #{
                    <<"url">> => <<"http://u1:p1@h/db">>,
                    <<"auth_props">> => #{},
                    <<"headers">> => maps:merge(DefaultHeaders, #{
                        <<"Authorization">> => basic_b64("u2", "p2")
                    })
                },
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => auth_props("u1", "p1"),
                    <<"headers">> => DefaultHeaders
                }
            },
            {
                #{
                    <<"url">> => <<"http://u1:p1@h/db">>,
                    <<"auth_props">> => auth_props("u2", "p2"),
                    <<"headers">> => DefaultHeaders
                },
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => auth_props("u2", "p2"),
                    <<"headers">> => DefaultHeaders
                }
            },
            {
                #{
                    <<"url">> => <<"http://u1:p1@h/db">>,
                    <<"auth_props">> => auth_props("u2", "p2"),
                    <<"headers">> => maps:merge(DefaultHeaders, #{
                        <<"Authorization">> => basic_b64("u3", "p3")
                    })
                },
                #{
                    <<"url">> => <<"http://h/db">>,
                    <<"auth_props">> => auth_props("u2", "p2"),
                    <<"headers">> => DefaultHeaders
                }
            }
        ]
    ].

basic_b64(User, Pass) when is_list(User), is_list(Pass) ->
    B64Creds = list_to_binary(b64creds(User, Pass)),
    <<"basic ", B64Creds/binary>>.

auth_props(User, Pass) when is_list(User), is_list(Pass) ->
    #{
        <<"basic">> => #{
            <<"username">> => list_to_binary(User),
            <<"password">> => list_to_binary(Pass)
        }
    }.

-endif.
