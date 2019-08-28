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

-module(couch_replicator_docs).

-export([
    parse_rep_doc/1,
    parse_rep_doc/2,
    parse_rep_db/3,
    parse_rep_doc_without_id/1,
    parse_rep_doc_without_id/2,
    before_doc_update/3,
    after_doc_read/2,
    ensure_rep_db_exists/0,
    ensure_rep_ddoc_exists/1,
    ensure_cluster_rep_ddoc_exists/1,
    remove_state_fields/2,
    update_doc_completed/3,
    update_failed/3,
    update_rep_id/1,
    update_triggered/2,
    update_error/2
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("ibrowse/include/ibrowse.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").
-include("couch_replicator.hrl").
-include("couch_replicator_js_functions.hrl").

-import(couch_util, [
    get_value/2,
    get_value/3,
    to_binary/1
]).

-import(couch_replicator_utils, [
    get_json_value/2,
    get_json_value/3
]).


-define(REP_DB_NAME, <<"_replicator">>).
-define(REP_DESIGN_DOC, <<"_design/_replicator">>).
-define(OWNER, <<"owner">>).
-define(CTX, {user_ctx, #user_ctx{roles=[<<"_admin">>, <<"_replicator">>]}}).
-define(replace(L, K, V), lists:keystore(K, 1, L, {K, V})).


remove_state_fields(DbName, DocId) ->
    update_rep_doc(DbName, DocId, [
        {<<"_replication_state">>, undefined},
        {<<"_replication_state_time">>, undefined},
        {<<"_replication_state_reason">>, undefined},
        {<<"_replication_id">>, undefined},
        {<<"_replication_stats">>, undefined}]).


-spec update_doc_completed(binary(), binary(), [_]) -> any().
update_doc_completed(DbName, DocId, Stats) ->
    update_rep_doc(DbName, DocId, [
        {<<"_replication_state">>, <<"completed">>},
        {<<"_replication_state_reason">>, undefined},
        {<<"_replication_stats">>, {Stats}}]),
    couch_stats:increment_counter([couch_replicator, docs,
        completed_state_updates]).


-spec update_failed(binary(), binary(), any()) -> any().
update_failed(DbName, DocId, Error) ->
    Reason = error_reason(Error),
    couch_log:error("Error processing replication doc `~s` from `~s`: ~s",
        [DocId, DbName, Reason]),
    update_rep_doc(DbName, DocId, [
        {<<"_replication_state">>, <<"failed">>},
        {<<"_replication_stats">>, undefined},
        {<<"_replication_state_reason">>, Reason}]),
    couch_stats:increment_counter([couch_replicator, docs,
        failed_state_updates]).


-spec update_triggered(#rep{}, rep_id()) -> ok.
update_triggered(Rep, {Base, Ext}) ->
    #rep{
        db_name = DbName,
        doc_id = DocId
    } = Rep,
    update_rep_doc(DbName, DocId, [
        {<<"_replication_state">>, <<"triggered">>},
        {<<"_replication_state_reason">>, undefined},
        {<<"_replication_id">>, iolist_to_binary([Base, Ext])},
        {<<"_replication_stats">>, undefined}]),
    ok.


-spec update_error(#rep{}, any()) -> ok.
update_error(#rep{db_name = DbName, doc_id = DocId, id = RepId}, Error) ->
    Reason = error_reason(Error),
    BinRepId = case RepId of
        {Base, Ext} ->
            iolist_to_binary([Base, Ext]);
        _Other ->
            null
    end,
    update_rep_doc(DbName, DocId, [
        {<<"_replication_state">>, <<"error">>},
        {<<"_replication_state_reason">>, Reason},
        {<<"_replication_stats">>, undefined},
        {<<"_replication_id">>, BinRepId}]),
    ok.


-spec ensure_rep_db_exists() -> {ok, Db::any()}.
ensure_rep_db_exists() ->
    Db = case couch_db:open_int(?REP_DB_NAME, [?CTX, sys_db,
            nologifmissing]) of
        {ok, Db0} ->
            Db0;
        _Error ->
            {ok, Db0} = couch_db:create(?REP_DB_NAME, [?CTX, sys_db]),
            Db0
    end,
    ok = ensure_rep_ddoc_exists(?REP_DB_NAME),
    {ok, Db}.


-spec ensure_rep_ddoc_exists(binary()) -> ok.
ensure_rep_ddoc_exists(RepDb) ->
    case mem3:belongs(RepDb, ?REP_DESIGN_DOC) of
        true ->
            ensure_rep_ddoc_exists(RepDb, ?REP_DESIGN_DOC);
        false ->
            ok
    end.


-spec ensure_rep_ddoc_exists(binary(), binary()) -> ok.
ensure_rep_ddoc_exists(RepDb, DDocId) ->
    case open_rep_doc(RepDb, DDocId) of
        {not_found, no_db_file} ->
            %% database was deleted.
            ok;
        {not_found, _Reason} ->
            DocProps = replication_design_doc_props(DDocId),
            DDoc = couch_doc:from_json_obj({DocProps}),
            couch_log:notice("creating replicator ddoc ~p", [RepDb]),
            {ok, _Rev} = save_rep_doc(RepDb, DDoc);
        {ok, Doc} ->
            Latest = replication_design_doc_props(DDocId),
            {Props0} = couch_doc:to_json_obj(Doc, []),
            {value, {_, Rev}, Props} = lists:keytake(<<"_rev">>, 1, Props0),
            case compare_ejson({Props}, {Latest}) of
                true ->
                    ok;
                false ->
                    LatestWithRev = [{<<"_rev">>, Rev} | Latest],
                    DDoc = couch_doc:from_json_obj({LatestWithRev}),
                    couch_log:notice("updating replicator ddoc ~p", [RepDb]),
                    try
                        {ok, _} = save_rep_doc(RepDb, DDoc)
                    catch
                        throw:conflict ->
                            %% ignore, we'll retry next time
                            ok
                    end
            end
    end,
    ok.


-spec ensure_cluster_rep_ddoc_exists(binary()) -> ok.
ensure_cluster_rep_ddoc_exists(RepDb) ->
    DDocId = ?REP_DESIGN_DOC,
    [#shard{name = DbShard} | _] = mem3:shards(RepDb, DDocId),
    ensure_rep_ddoc_exists(DbShard, DDocId).


-spec compare_ejson({[_]}, {[_]}) -> boolean().
compare_ejson(EJson1, EJson2) ->
    EjsonSorted1 = couch_replicator_filters:ejsort(EJson1),
    EjsonSorted2 = couch_replicator_filters:ejsort(EJson2),
    EjsonSorted1 == EjsonSorted2.


-spec replication_design_doc_props(binary()) -> [_].
replication_design_doc_props(DDocId) ->
    [
        {<<"_id">>, DDocId},
        {<<"language">>, <<"javascript">>},
        {<<"validate_doc_update">>, ?REP_DB_DOC_VALIDATE_FUN}
    ].


% Note: parse_rep_doc can handle filtered replications. During parsing of the
% replication doc it will make possibly remote http requests to the source
% database. If failure or parsing of filter docs fails, parse_doc throws a
% {filter_fetch_error, Error} excation. This exception should be considered
% transient in respect to the contents of the document itself, since it depends
% on netowrk availability of the source db and other factors.
-spec parse_rep_doc({[_]}) -> #rep{}.
parse_rep_doc(RepDoc) ->
    {ok, Rep} = try
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
    {ok, Rep} = try
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
    Proxy = get_value(<<"proxy">>, Props, <<>>),
    Opts = make_options(Props),
    case get_value(cancel, Opts, false) andalso
        (get_value(id, Opts, nil) =/= nil) of
    true ->
        {ok, #rep{options = Opts, user_ctx = UserCtx}};
    false ->
        Source = parse_rep_db(get_value(<<"source">>, Props), Proxy, Opts),
        Target = parse_rep_db(get_value(<<"target">>, Props), Proxy, Opts),
        {Type, View} = case couch_replicator_filters:view_type(Props, Opts) of
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


% Update a #rep{} record with a replication_id. Calculating the id might involve
% fetching a filter from the source db, and so it could fail intermetently.
% In case of a failure to fetch the filter this function will throw a
%  `{filter_fetch_error, Reason} exception.
update_rep_id(Rep) ->
    RepId = couch_replicator_ids:replication_id(Rep),
    Rep#rep{id = RepId}.


update_rep_doc(RepDbName, RepDocId, KVs) ->
    update_rep_doc(RepDbName, RepDocId, KVs, 1).


update_rep_doc(RepDbName, RepDocId, KVs, Wait) when is_binary(RepDocId) ->
    try
        case open_rep_doc(RepDbName, RepDocId) of
            {ok, LastRepDoc} ->
                update_rep_doc(RepDbName, LastRepDoc, KVs, Wait * 2);
            _ ->
                ok
        end
    catch
        throw:conflict ->
            Msg = "Conflict when updating replication doc `~s`. Retrying.",
            couch_log:error(Msg, [RepDocId]),
            ok = timer:sleep(couch_rand:uniform(erlang:min(128, Wait)) * 100),
            update_rep_doc(RepDbName, RepDocId, KVs, Wait * 2)
    end;

update_rep_doc(RepDbName, #doc{body = {RepDocBody}} = RepDoc, KVs, _Try) ->
    NewRepDocBody = lists:foldl(
        fun({K, undefined}, Body) ->
                lists:keydelete(K, 1, Body);
           ({<<"_replication_state">> = K, State} = KV, Body) ->
                case get_json_value(K, Body) of
                State ->
                    Body;
                _ ->
                    Body1 = lists:keystore(K, 1, Body, KV),
                    Timestamp = couch_replicator_utils:iso8601(os:timestamp()),
                    lists:keystore(
                        <<"_replication_state_time">>, 1, Body1,
                        {<<"_replication_state_time">>, Timestamp})
                end;
            ({K, _V} = KV, Body) ->
                lists:keystore(K, 1, Body, KV)
        end,
        RepDocBody, KVs),
    case NewRepDocBody of
    RepDocBody ->
        ok;
    _ ->
        % Might not succeed - when the replication doc is deleted right
        % before this update (not an error, ignore).
        save_rep_doc(RepDbName, RepDoc#doc{body = {NewRepDocBody}})
    end.


open_rep_doc(DbName, DocId) ->
    case couch_db:open_int(DbName, [?CTX, sys_db]) of
        {ok, Db} ->
            try
                couch_db:open_doc(Db, DocId, [ejson_body])
            after
                couch_db:close(Db)
            end;
        Else ->
            Else
    end.


save_rep_doc(DbName, Doc) ->
    {ok, Db} = couch_db:open_int(DbName, [?CTX, sys_db]),
    try
        couch_db:update_doc(Db, Doc, [])
    catch
        % User can accidently write a VDU which prevents _replicator from
        % updating replication documents. Avoid crashing replicator and thus
        % preventing all other replication jobs on the node from running.
        throw:{forbidden, Reason} ->
            Msg = "~p VDU function preventing doc update to ~s ~s ~p",
            couch_log:error(Msg, [?MODULE, DbName, Doc#doc.id, Reason]),
            {ok, forbidden}
    after
        couch_db:close(Db)
    end.


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


-spec parse_rep_db({[_]} | binary(), binary(), [_]) -> #httpd{} | binary().
parse_rep_db({Props}, Proxy, Options) ->
    ProxyParams = parse_proxy_params(Proxy),
    ProxyURL = case ProxyParams of
        [] -> undefined;
        _ -> binary_to_list(Proxy)
    end,
    Url = maybe_add_trailing_slash(get_value(<<"url">>, Props)),
    {AuthProps} = get_value(<<"auth">>, Props, {[]}),
    {BinHeaders} = get_value(<<"headers">>, Props, {[]}),
    Headers = lists:ukeysort(1, [{?b2l(K), ?b2l(V)} || {K, V} <- BinHeaders]),
    DefaultHeaders = (#httpdb{})#httpdb.headers,
    #httpdb{
        url = Url,
        auth_props = AuthProps,
        headers = lists:ukeymerge(1, Headers, DefaultHeaders),
        ibrowse_options = lists:keysort(1,
            [{socket_options, get_value(socket_options, Options)} |
                ProxyParams ++ ssl_params(Url)]),
        timeout = get_value(connection_timeout, Options),
        http_connections = get_value(http_connections, Options),
        retries = get_value(retries, Options),
        proxy_url = ProxyURL
    };

parse_rep_db(<<"http://", _/binary>> = Url, Proxy, Options) ->
    parse_rep_db({[{<<"url">>, Url}]}, Proxy, Options);

parse_rep_db(<<"https://", _/binary>> = Url, Proxy, Options) ->
    parse_rep_db({[{<<"url">>, Url}]}, Proxy, Options);

parse_rep_db(<<_/binary>>, _Proxy, _Options) ->
    throw({error, <<"Local endpoints not supported since CouchDB 3.x">>});

parse_rep_db(undefined, _Proxy, _Options) ->
    throw({error, <<"Missing replicator database">>}).


-spec maybe_add_trailing_slash(binary() | list()) -> list().
maybe_add_trailing_slash(Url) when is_binary(Url) ->
    maybe_add_trailing_slash(?b2l(Url));
maybe_add_trailing_slash(Url) ->
    case lists:member($?, Url) of
        true ->
            Url;  % skip if there are query params
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
    DefWorkers = config:get("replicator", "worker_processes", "4"),
    DefBatchSize = config:get("replicator", "worker_batch_size", "500"),
    DefConns = config:get("replicator", "http_connections", "20"),
    DefTimeout = config:get("replicator", "connection_timeout", "30000"),
    DefRetries = config:get("replicator", "retries_per_request", "5"),
    UseCheckpoints = config:get("replicator", "use_checkpoints", "true"),
    DefCheckpointInterval = config:get("replicator", "checkpoint_interval",
        "30000"),
    {ok, DefSocketOptions} = couch_util:parse_term(
        config:get("replicator", "socket_options",
            "[{keepalive, true}, {nodelay, false}]")),
    lists:ukeymerge(1, Options, lists:keysort(1, [
        {connection_timeout, list_to_integer(DefTimeout)},
        {retries, list_to_integer(DefRetries)},
        {http_connections, list_to_integer(DefConns)},
        {socket_options, DefSocketOptions},
        {worker_batch_size, list_to_integer(DefBatchSize)},
        {worker_processes, list_to_integer(DefWorkers)},
        {use_checkpoints, list_to_existing_atom(UseCheckpoints)},
        {checkpoint_interval, list_to_integer(DefCheckpointInterval)}
    ])).


-spec convert_options([_]) -> [_].
convert_options([])->
    [];
convert_options([{<<"cancel">>, V} | _R]) when not is_boolean(V)->
    throw({bad_request, <<"parameter `cancel` must be a boolean">>});
convert_options([{<<"cancel">>, V} | R]) ->
    [{cancel, V} | convert_options(R)];
convert_options([{IdOpt, V} | R]) when IdOpt =:= <<"_local_id">>;
        IdOpt =:= <<"replication_id">>; IdOpt =:= <<"id">> ->
    [{id, couch_replicator_ids:convert(V)} | convert_options(R)];
convert_options([{<<"create_target">>, V} | _R]) when not is_boolean(V)->
    throw({bad_request, <<"parameter `create_target` must be a boolean">>});
convert_options([{<<"create_target">>, V} | R]) ->
    [{create_target, V} | convert_options(R)];
convert_options([{<<"create_target_params">>, V} | _R]) when not is_tuple(V) ->
    throw({bad_request,
        <<"parameter `create_target_params` must be a JSON object">>});
convert_options([{<<"create_target_params">>, V} | R]) ->
    [{create_target_params, V} | convert_options(R)];
convert_options([{<<"continuous">>, V} | _R]) when not is_boolean(V)->
    throw({bad_request, <<"parameter `continuous` must be a boolean">>});
convert_options([{<<"continuous">>, V} | R]) ->
    [{continuous, V} | convert_options(R)];
convert_options([{<<"filter">>, V} | R]) ->
    [{filter, V} | convert_options(R)];
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
    {ok, SocketOptions} = couch_util:parse_term(V),
    [{socket_options, SocketOptions} | convert_options(R)];
convert_options([{<<"since_seq">>, V} | R]) ->
    [{since_seq, V} | convert_options(R)];
convert_options([{<<"use_checkpoints">>, V} | R]) ->
    [{use_checkpoints, V} | convert_options(R)];
convert_options([{<<"checkpoint_interval">>, V} | R]) ->
    [{checkpoint_interval, couch_util:to_integer(V)} | convert_options(R)];
convert_options([_ | R]) -> % skip unknown option
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
        _ ->
            throw({bad_request,
                "`doc_ids`,`filter`,`selector` are mutually exclusive"})
    end.


-spec parse_proxy_params(binary() | [_]) -> [_].
parse_proxy_params(ProxyUrl) when is_binary(ProxyUrl) ->
    parse_proxy_params(?b2l(ProxyUrl));
parse_proxy_params([]) ->
    [];
parse_proxy_params(ProxyUrl) ->
    #url{
        host = Host,
        port = Port,
        username = User,
        password = Passwd,
        protocol = Protocol
    } = ibrowse_lib:parse_url(ProxyUrl),
    [
        {proxy_protocol, Protocol},
        {proxy_host, Host},
        {proxy_port, Port}
    ] ++ case is_list(User) andalso is_list(Passwd) of
        false ->
            [];
        true ->
            [{proxy_user, User}, {proxy_password, Passwd}]
        end.


-spec ssl_params([_]) -> [_].
ssl_params(Url) ->
    case ibrowse_lib:parse_url(Url) of
    #url{protocol = https} ->
        Depth = list_to_integer(
            config:get("replicator", "ssl_certificate_max_depth", "3")
        ),
        VerifyCerts = config:get("replicator", "verify_ssl_certificates"),
        CertFile = config:get("replicator", "cert_file", undefined),
        KeyFile = config:get("replicator", "key_file", undefined),
        Password = config:get("replicator", "password", undefined),
        SslOpts = [{depth, Depth} | ssl_verify_options(VerifyCerts =:= "true")],
        SslOpts1 = case CertFile /= undefined andalso KeyFile /= undefined of
            true ->
                case Password of
                    undefined ->
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


-spec ssl_verify_options(true | false) -> [_].
ssl_verify_options(true) ->
    CAFile = config:get("replicator", "ssl_trusted_certificates_file"),
    [{verify, verify_peer}, {cacertfile, CAFile}];
ssl_verify_options(false) ->
    [{verify, verify_none}].


-spec before_doc_update(#doc{}, Db::any(), couch_db:update_type()) -> #doc{}.
before_doc_update(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, _Db, _UpdateType) ->
    Doc;
before_doc_update(#doc{body = {Body}} = Doc, Db, _UpdateType) ->
    #user_ctx{
       roles = Roles,
       name = Name
    } = couch_db:get_user_ctx(Db),
    case lists:member(<<"_replicator">>, Roles) of
    true ->
        Doc;
    false ->
        case couch_util:get_value(?OWNER, Body) of
        undefined ->
            Doc#doc{body = {?replace(Body, ?OWNER, Name)}};
        Name ->
            Doc;
        Other ->
            case (catch couch_db:check_is_admin(Db)) of
            ok when Other =:= null ->
                Doc#doc{body = {?replace(Body, ?OWNER, Name)}};
            ok ->
                Doc;
            _ ->
                throw({forbidden, <<"Can't update replication documents",
                    " from other users.">>})
            end
        end
    end.


-spec after_doc_read(#doc{}, Db::any()) -> #doc{}.
after_doc_read(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, _Db) ->
    Doc;
after_doc_read(#doc{body = {Body}} = Doc, Db) ->
    #user_ctx{name = Name} = couch_db:get_user_ctx(Db),
    case (catch couch_db:check_is_admin(Db)) of
    ok ->
        Doc;
    _ ->
        case couch_util:get_value(?OWNER, Body) of
        Name ->
            Doc;
        _Other ->
            Source = strip_credentials(couch_util:get_value(<<"source">>,
Body)),
            Target = strip_credentials(couch_util:get_value(<<"target">>,
Body)),
            NewBody0 = ?replace(Body, <<"source">>, Source),
            NewBody = ?replace(NewBody0, <<"target">>, Target),
            #doc{revs = {Pos, [_ | Revs]}} = Doc,
            NewDoc = Doc#doc{body = {NewBody}, revs = {Pos - 1, Revs}},
            NewRevId = couch_db:new_revid(NewDoc),
            NewDoc#doc{revs = {Pos, [NewRevId | Revs]}}
        end
    end.


-spec strip_credentials(undefined) -> undefined;
    (binary()) -> binary();
    ({[_]}) -> {[_]}.
strip_credentials(undefined) ->
    undefined;
strip_credentials(Url) when is_binary(Url) ->
    re:replace(Url,
        "http(s)?://(?:[^:]+):[^@]+@(.*)$",
        "http\\1://\\2",
        [{return, binary}]);
strip_credentials({Props}) ->
    {lists:keydelete(<<"headers">>, 1, Props)}.


error_reason({shutdown, Error}) ->
    error_reason(Error);
error_reason({bad_rep_doc, Reason}) ->
    to_binary(Reason);
error_reason({error, {Error, Reason}})
  when is_atom(Error), is_binary(Reason) ->
    to_binary(io_lib:format("~s: ~s", [Error, Reason]));
error_reason({error, Reason}) ->
    to_binary(Reason);
error_reason(Reason) ->
    to_binary(Reason).


-ifdef(TEST).


-include_lib("couch/include/couch_eunit.hrl").


check_options_pass_values_test() ->
    ?assertEqual(check_options([]), []),
    ?assertEqual(check_options([baz, {other, fiz}]), [baz, {other, fiz}]),
    ?assertEqual(check_options([{doc_ids, x}]), [{doc_ids, x}]),
    ?assertEqual(check_options([{filter, x}]), [{filter, x}]),
    ?assertEqual(check_options([{selector, x}]), [{selector, x}]).


check_options_fail_values_test() ->
    ?assertThrow({bad_request, _},
        check_options([{doc_ids, x}, {filter, y}])),
    ?assertThrow({bad_request, _},
        check_options([{doc_ids, x}, {selector, y}])),
    ?assertThrow({bad_request, _},
        check_options([{filter, x}, {selector, y}])),
    ?assertThrow({bad_request, _},
        check_options([{doc_ids, x}, {selector, y}, {filter, z}])).


check_convert_options_pass_test() ->
    ?assertEqual([], convert_options([])),
    ?assertEqual([], convert_options([{<<"random">>, 42}])),
    ?assertEqual([{cancel, true}],
        convert_options([{<<"cancel">>, true}])),
    ?assertEqual([{create_target, true}],
        convert_options([{<<"create_target">>, true}])),
    ?assertEqual([{continuous, true}],
        convert_options([{<<"continuous">>, true}])),
    ?assertEqual([{doc_ids, [<<"id">>]}],
        convert_options([{<<"doc_ids">>, [<<"id">>]}])),
    ?assertEqual([{selector, {key, value}}],
        convert_options([{<<"selector">>, {key, value}}])).


check_convert_options_fail_test() ->
    ?assertThrow({bad_request, _},
        convert_options([{<<"cancel">>, <<"true">>}])),
    ?assertThrow({bad_request, _},
        convert_options([{<<"create_target">>, <<"true">>}])),
    ?assertThrow({bad_request, _},
        convert_options([{<<"continuous">>, <<"true">>}])),
    ?assertThrow({bad_request, _},
        convert_options([{<<"doc_ids">>, not_a_list}])),
    ?assertThrow({bad_request, _},
        convert_options([{<<"selector">>, [{key, value}]}])).

check_strip_credentials_test() ->
    [?assertEqual(Expected, strip_credentials(Body)) || {Expected, Body} <- [
        {
            undefined,
            undefined
        },
        {
            <<"https://remote_server/database">>,
            <<"https://foo:bar@remote_server/database">>
        },
        {
            {[{<<"_id">>, <<"foo">>}]},
            {[{<<"_id">>, <<"foo">>}, {<<"headers">>, <<"bar">>}]}
        },
        {
            {[{<<"_id">>, <<"foo">>}, {<<"other">>, <<"bar">>}]},
            {[{<<"_id">>, <<"foo">>}, {<<"other">>, <<"bar">>}]}
        },
        {
            {[{<<"_id">>, <<"foo">>}]},
            {[{<<"_id">>, <<"foo">>}, {<<"headers">>, <<"baz">>}]}
        }
    ]].


setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    create_vdu(DbName),
    DbName.


teardown(DbName) when is_binary(DbName) ->
    couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.


create_vdu(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        VduFun = <<"function(newdoc, olddoc, userctx) {throw({'forbidden':'fail'})}">>,
        Doc = #doc{
            id = <<"_design/vdu">>,
            body = {[{<<"validate_doc_update">>, VduFun}]}
        },
        {ok, _} = couch_db:update_docs(Db, [Doc]),
        couch_db:ensure_full_commit(Db)
    end).


update_replicator_doc_with_bad_vdu_test_() ->
    {
        setup,
        fun test_util:start_couch/0,
        fun test_util:stop_couch/1,
        {
            foreach, fun setup/0, fun teardown/1,
            [
                fun t_vdu_does_not_crash_on_save/1
            ]
        }
    }.


t_vdu_does_not_crash_on_save(DbName) ->
    ?_test(begin
        Doc = #doc{id = <<"some_id">>, body = {[{<<"foo">>, 42}]}},
        ?assertEqual({ok, forbidden}, save_rep_doc(DbName, Doc))
    end).


local_replication_endpoint_error_test_() ->
     {
        foreach,
        fun () -> meck:expect(config, get,
            fun(_, _, Default) -> Default end)
        end,
        fun (_) -> meck:unload() end,
        [
            t_error_on_local_endpoint()
        ]
    }.


t_error_on_local_endpoint() ->
    ?_test(begin
        RepDoc = {[
            {<<"_id">>, <<"someid">>},
            {<<"source">>, <<"localdb">>},
            {<<"target">>, <<"http://somehost.local/tgt">>}
        ]},
        Expect = <<"Local endpoints not supported since CouchDB 3.x">>,
        ?assertThrow({bad_rep_doc, Expect}, parse_rep_doc_without_id(RepDoc))
    end).

-endif.
