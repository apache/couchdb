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

-module(couch_replicator_utils).

-export([parse_rep_doc/2]).
-export([open_db/1, close_db/1]).
-export([start_db_compaction_notifier/2, stop_db_compaction_notifier/1]).
-export([replication_id/2]).
-export([sum_stats/2]).

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").
-include("couch_replicator.hrl").
-include("../ibrowse/ibrowse.hrl").

-import(couch_util, [
    get_value/2,
    get_value/3
]).


parse_rep_doc({Props}, UserCtx) ->
    ProxyParams = parse_proxy_params(get_value(<<"proxy">>, Props, <<>>)),
    Options = make_options(Props),
    case get_value(cancel, Options, false) andalso
        (get_value(id, Options, nil) =/= nil) of
    true ->
        {ok, #rep{options = Options, user_ctx = UserCtx}};
    false ->
        Source = parse_rep_db(get_value(<<"source">>, Props), ProxyParams, Options),
        Target = parse_rep_db(get_value(<<"target">>, Props), ProxyParams, Options),
        Rep = #rep{
            source = Source,
            target = Target,
            options = Options,
            user_ctx = UserCtx,
            doc_id = get_value(<<"_id">>, Props, null)
        },
        {ok, Rep#rep{id = replication_id(Rep)}}
    end.


replication_id(#rep{options = Options} = Rep) ->
    BaseId = replication_id(Rep, ?REP_ID_VERSION),
    {BaseId, maybe_append_options([continuous, create_target], Options)}.


% Versioned clauses for generating replication IDs.
% If a change is made to how replications are identified,
% please add a new clause and increase ?REP_ID_VERSION.

replication_id(#rep{user_ctx = UserCtx} = Rep, 2) ->
    {ok, HostName} = inet:gethostname(),
    Port = case (catch mochiweb_socket_server:get(couch_httpd, port)) of
    P when is_number(P) ->
        P;
    _ ->
        % On restart we might be called before the couch_httpd process is
        % started.
        % TODO: we might be under an SSL socket server only, or both under
        % SSL and a non-SSL socket.
        % ... mochiweb_socket_server:get(https, port)
        list_to_integer(couch_config:get("httpd", "port", "5984"))
    end,
    Src = get_rep_endpoint(UserCtx, Rep#rep.source),
    Tgt = get_rep_endpoint(UserCtx, Rep#rep.target),
    maybe_append_filters([HostName, Port, Src, Tgt], Rep);

replication_id(#rep{user_ctx = UserCtx} = Rep, 1) ->
    {ok, HostName} = inet:gethostname(),
    Src = get_rep_endpoint(UserCtx, Rep#rep.source),
    Tgt = get_rep_endpoint(UserCtx, Rep#rep.target),
    maybe_append_filters([HostName, Src, Tgt], Rep).


maybe_append_filters(Base,
        #rep{source = Source, user_ctx = UserCtx, options = Options}) ->
    Base2 = Base ++
        case get_value(filter, Options) of
        undefined ->
            case get_value(doc_ids, Options) of
            undefined ->
                [];
            DocIds ->
                [DocIds]
            end;
        Filter ->
            [filter_code(Filter, Source, UserCtx),
                get_value(query_params, Options, {[]})]
        end,
    couch_util:to_hex(couch_util:md5(term_to_binary(Base2))).


filter_code(Filter, Source, UserCtx) ->
    {DDocName, FilterName} =
    case re:run(Filter, "(.*?)/(.*)", [{capture, [1, 2], binary}]) of
    {match, [DDocName0, FilterName0]} ->
        {DDocName0, FilterName0};
    _ ->
        throw({error, <<"Invalid filter. Must match `ddocname/filtername`.">>})
    end,
    Db = case (catch couch_api_wrap:db_open(Source, [{user_ctx, UserCtx}])) of
    {ok, Db0} ->
        Db0;
    DbError ->
        DbErrorMsg = io_lib:format("Could not open source database `~s`: ~s",
           [couch_api_wrap:db_uri(Source), couch_util:to_binary(DbError)]),
        throw({error, iolist_to_binary(DbErrorMsg)})
    end,
    try
        Body = case (catch couch_api_wrap:open_doc(
            Db, <<"_design/", DDocName/binary>>, [ejson_body])) of
        {ok, #doc{body = Body0}} ->
            Body0;
        DocError ->
            DocErrorMsg = io_lib:format(
                "Couldn't open document `_design/~s` from source "
                "database `~s`: ~s", [DDocName, couch_api_wrap:db_uri(Source),
                    couch_util:to_binary(DocError)]),
            throw({error, iolist_to_binary(DocErrorMsg)})
        end,
        Code = couch_util:get_nested_json_value(
            Body, [<<"filters">>, FilterName]),
        re:replace(Code, [$^, "\s*(.*?)\s*", $$], "\\1", [{return, binary}])
    after
        couch_api_wrap:db_close(Db)
    end.


maybe_append_options(Options, RepOptions) ->
    lists:foldl(fun(Option, Acc) ->
        Acc ++
        case get_value(Option, RepOptions, false) of
        true ->
            "+" ++ atom_to_list(Option);
        false ->
            ""
        end
    end, [], Options).


get_rep_endpoint(_UserCtx, #httpdb{url=Url, headers=Headers, oauth=OAuth}) ->
    DefaultHeaders = (#httpdb{})#httpdb.headers,
    case OAuth of
    nil ->
        {remote, Url, Headers -- DefaultHeaders};
    #oauth{} ->
        {remote, Url, Headers -- DefaultHeaders, OAuth}
    end;
get_rep_endpoint(UserCtx, <<DbName/binary>>) ->
    {local, DbName, UserCtx}.


parse_rep_db({Props}, ProxyParams, Options) ->
    Url = maybe_add_trailing_slash(get_value(<<"url">>, Props)),
    {AuthProps} = get_value(<<"auth">>, Props, {[]}),
    {BinHeaders} = get_value(<<"headers">>, Props, {[]}),
    Headers = lists:ukeysort(1, [{?b2l(K), ?b2l(V)} || {K, V} <- BinHeaders]),
    DefaultHeaders = (#httpdb{})#httpdb.headers,
    OAuth = case get_value(<<"oauth">>, AuthProps) of
    undefined ->
        nil;
    {OauthProps} ->
        #oauth{
            consumer_key = ?b2l(get_value(<<"consumer_key">>, OauthProps)),
            token = ?b2l(get_value(<<"token">>, OauthProps)),
            token_secret = ?b2l(get_value(<<"token_secret">>, OauthProps)),
            consumer_secret = ?b2l(get_value(<<"consumer_secret">>, OauthProps)),
            signature_method =
                case get_value(<<"signature_method">>, OauthProps) of
                undefined ->        hmac_sha1;
                <<"PLAINTEXT">> ->  plaintext;
                <<"HMAC-SHA1">> ->  hmac_sha1;
                <<"RSA-SHA1">> ->   rsa_sha1
                end
        }
    end,
    #httpdb{
        url = Url,
        oauth = OAuth,
        headers = lists:ukeymerge(1, Headers, DefaultHeaders),
        ibrowse_options = lists:keysort(1,
            [{socket_options, get_value(socket_options, Options)} |
                ProxyParams ++ ssl_params(Url)]),
        timeout = get_value(connection_timeout, Options),
        http_connections = get_value(http_connections, Options),
        retries = get_value(retries, Options)
    };
parse_rep_db(<<"http://", _/binary>> = Url, ProxyParams, Options) ->
    parse_rep_db({[{<<"url">>, Url}]}, ProxyParams, Options);
parse_rep_db(<<"https://", _/binary>> = Url, ProxyParams, Options) ->
    parse_rep_db({[{<<"url">>, Url}]}, ProxyParams, Options);
parse_rep_db(<<DbName/binary>>, _ProxyParams, _Options) ->
    DbName.


maybe_add_trailing_slash(Url) when is_binary(Url) ->
    maybe_add_trailing_slash(?b2l(Url));
maybe_add_trailing_slash(Url) ->
    case lists:last(Url) of
    $/ ->
        Url;
    _ ->
        Url ++ "/"
    end.


make_options(Props) ->
    Options = lists:ukeysort(1, convert_options(Props)),
    DefWorkers = couch_config:get("replicator", "worker_processes", "4"),
    DefBatchSize = couch_config:get("replicator", "worker_batch_size", "500"),
    DefConns = couch_config:get("replicator", "http_connections", "20"),
    DefTimeout = couch_config:get("replicator", "connection_timeout", "30000"),
    DefRetries = couch_config:get("replicator", "retries_per_request", "10"),
    {ok, DefSocketOptions} = couch_util:parse_term(
        couch_config:get("replicator", "socket_options",
            "[{keepalive, true}, {nodelay, false}]")),
    lists:ukeymerge(1, Options, lists:keysort(1, [
        {connection_timeout, list_to_integer(DefTimeout)},
        {retries, list_to_integer(DefRetries)},
        {http_connections, list_to_integer(DefConns)},
        {socket_options, DefSocketOptions},
        {worker_batch_size, list_to_integer(DefBatchSize)},
        {worker_processes, list_to_integer(DefWorkers)}
    ])).


convert_options([])->
    [];
convert_options([{<<"cancel">>, V} | R]) ->
    [{cancel, V} | convert_options(R)];
convert_options([{IdOpt, V} | R]) when IdOpt =:= <<"_local_id">>;
        IdOpt =:= <<"replication_id">>; IdOpt =:= <<"id">> ->
    Id = lists:splitwith(fun(X) -> X =/= $+ end, ?b2l(V)),
    [{id, Id} | convert_options(R)];
convert_options([{<<"create_target">>, V} | R]) ->
    [{create_target, V} | convert_options(R)];
convert_options([{<<"continuous">>, V} | R]) ->
    [{continuous, V} | convert_options(R)];
convert_options([{<<"filter">>, V} | R]) ->
    [{filter, V} | convert_options(R)];
convert_options([{<<"query_params">>, V} | R]) ->
    [{query_params, V} | convert_options(R)];
convert_options([{<<"doc_ids">>, null} | R]) ->
    convert_options(R);
convert_options([{<<"doc_ids">>, V} | R]) ->
    % Ensure same behaviour as old replicator: accept a list of percent
    % encoded doc IDs.
    DocIds = [?l2b(couch_httpd:unquote(Id)) || Id <- V],
    [{doc_ids, DocIds} | convert_options(R)];
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
    {ok, SocketOptions} = couch_util:parse_term(V),
    [{socket_options, SocketOptions} | convert_options(R)];
convert_options([{<<"since_seq">>, V} | R]) ->
    [{since_seq, V} | convert_options(R)];
convert_options([_ | R]) -> % skip unknown option
    convert_options(R).


parse_proxy_params(ProxyUrl) when is_binary(ProxyUrl) ->
    parse_proxy_params(?b2l(ProxyUrl));
parse_proxy_params([]) ->
    [];
parse_proxy_params(ProxyUrl) ->
    #url{
        host = Host,
        port = Port,
        username = User,
        password = Passwd
    } = ibrowse_lib:parse_url(ProxyUrl),
    [{proxy_host, Host}, {proxy_port, Port}] ++
        case is_list(User) andalso is_list(Passwd) of
        false ->
            [];
        true ->
            [{proxy_user, User}, {proxy_password, Passwd}]
        end.


ssl_params(Url) ->
    case ibrowse_lib:parse_url(Url) of
    #url{protocol = https} ->
        Depth = list_to_integer(
            couch_config:get("replicator", "ssl_certificate_max_depth", "3")
        ),
        VerifyCerts = couch_config:get("replicator", "verify_ssl_certificates"),
        CertFile = couch_config:get("replicator", "cert_file", nil),
        KeyFile = couch_config:get("replicator", "key_file", nil),
        Password = couch_config:get("replicator", "password", nil),
        SslOpts = [{depth, Depth} | ssl_verify_options(VerifyCerts =:= "true")],
        SslOpts1 = case CertFile /= nil andalso KeyFile /= nil of
            true ->
                case Password of
                    nil -> 
                        [{certfile, CertFile}, {keyfile, KeyFile}] ++ SslOpts;
                    _ -> 
                        [{certfile, CertFile}, {keyfile, KeyFile},
                            {password, Password}] ++ SslOpts
                end;
            false -> SslOpts
        end,
        [{is_ssl, true}, {ssl_options, SslOpts1}];
    #url{protocol = http} ->
        []
    end.

ssl_verify_options(Value) ->
    ssl_verify_options(Value, erlang:system_info(otp_release)).

ssl_verify_options(true, OTPVersion) when OTPVersion >= "R14" ->
    CAFile = couch_config:get("replicator", "ssl_trusted_certificates_file"),
    [{verify, verify_peer}, {cacertfile, CAFile}];
ssl_verify_options(false, OTPVersion) when OTPVersion >= "R14" ->
    [{verify, verify_none}];
ssl_verify_options(true, _OTPVersion) ->
    CAFile = couch_config:get("replicator", "ssl_trusted_certificates_file"),
    [{verify, 2}, {cacertfile, CAFile}];
ssl_verify_options(false, _OTPVersion) ->
    [{verify, 0}].


open_db(#db{name = Name, user_ctx = UserCtx, options = Options}) ->
    {ok, Db} = couch_db:open(Name, [{user_ctx, UserCtx} | Options]),
    Db;
open_db(HttpDb) ->
    HttpDb.


close_db(#db{} = Db) ->
    couch_db:close(Db);
close_db(_HttpDb) ->
    ok.


start_db_compaction_notifier(#db{name = DbName}, Server) ->
    {ok, Notifier} = couch_db_update_notifier:start_link(
        fun({compacted, DbName1}) when DbName1 =:= DbName ->
                ok = gen_server:cast(Server, {db_compacted, DbName});
            (_) ->
                ok
        end),
    Notifier;
start_db_compaction_notifier(_, _) ->
    nil.


stop_db_compaction_notifier(nil) ->
    ok;
stop_db_compaction_notifier(Notifier) ->
    couch_db_update_notifier:stop(Notifier).


sum_stats(#rep_stats{} = S1, #rep_stats{} = S2) ->
    #rep_stats{
        missing_checked =
            S1#rep_stats.missing_checked + S2#rep_stats.missing_checked,
        missing_found = S1#rep_stats.missing_found + S2#rep_stats.missing_found,
        docs_read = S1#rep_stats.docs_read + S2#rep_stats.docs_read,
        docs_written = S1#rep_stats.docs_written + S2#rep_stats.docs_written,
        doc_write_failures =
            S1#rep_stats.doc_write_failures + S2#rep_stats.doc_write_failures
    }.
