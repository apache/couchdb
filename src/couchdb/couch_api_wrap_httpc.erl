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

-module(couch_api_wrap_httpc).

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").
-include("../ibrowse/ibrowse.hrl").

-export([setup/1]).
-export([send_req/3]).
-export([full_url/2]).

-import(couch_util, [
    get_value/2,
    get_value/3
]).

-define(replace(L, K, V), lists:keystore(K, 1, L, {K, V})).
-define(MAX_WAIT, 5 * 60 * 1000).


setup(#httpdb{httpc_pool = nil, url = Url, http_connections = MaxConns} = Db) ->
    {ok, Pid} = couch_httpc_pool:start_link(Url, [{max_connections, MaxConns}]),
    {ok, Db#httpdb{httpc_pool = Pid}}.


send_req(HttpDb, Params1, Callback) ->
    Params2 = ?replace(Params1, qs,
        [{K, ?b2l(iolist_to_binary(V))} || {K, V} <- get_value(qs, Params1, [])]),
    Params = ?replace(Params2, ibrowse_options,
        lists:keysort(1, get_value(ibrowse_options, Params2, []))),
    {Worker, Response} = send_ibrowse_req(HttpDb, Params),
    process_response(Response, Worker, HttpDb, Params, Callback).


send_ibrowse_req(#httpdb{headers = BaseHeaders} = HttpDb, Params) ->
    Method = get_value(method, Params, get),
    UserHeaders = lists:keysort(1, get_value(headers, Params, [])),
    Headers1 = lists:ukeymerge(1, UserHeaders, BaseHeaders),
    Headers2 = oauth_header(HttpDb, Params) ++ Headers1,
    Url = full_url(HttpDb, Params),
    Body = get_value(body, Params, []),
    case get_value(path, Params) of
    "_changes" ->
        {ok, Worker} = ibrowse:spawn_link_worker_process(Url);
    _ ->
        {ok, Worker} = couch_httpc_pool:get_worker(HttpDb#httpdb.httpc_pool)
    end,
    IbrowseOptions = [
        {response_format, binary}, {inactivity_timeout, HttpDb#httpdb.timeout} |
        lists:ukeymerge(1, get_value(ibrowse_options, Params, []),
            HttpDb#httpdb.ibrowse_options)
    ],
    Response = ibrowse:send_req_direct(
        Worker, Url, Headers2, Method, Body, IbrowseOptions, infinity),
    {Worker, Response}.


process_response({error, sel_conn_closed}, _Worker, HttpDb, Params, Callback) ->
    send_req(HttpDb, Params, Callback);

process_response({error, {'EXIT', {normal, _}}}, _Worker, HttpDb, Params, Cb) ->
    % ibrowse worker terminated because remote peer closed the socket
    % -> not an error
    send_req(HttpDb, Params, Cb);

process_response({ibrowse_req_id, ReqId}, Worker, HttpDb, Params, Callback) ->
    process_stream_response(ReqId, Worker, HttpDb, Params, Callback);

process_response({ok, Code, Headers, Body}, Worker, HttpDb, Params, Callback) ->
    release_worker(Worker, HttpDb),
    case list_to_integer(Code) of
    Ok when Ok =:= 200 ; Ok =:= 201 ; (Ok >= 400 andalso Ok < 500) ->
        EJson = case Body of
        <<>> ->
            null;
        Json ->
            ?JSON_DECODE(Json)
        end,
        Callback(Ok, Headers, EJson);
    R when R =:= 301 ; R =:= 302 ; R =:= 303 ->
        do_redirect(Worker, R, Headers, HttpDb, Params, Callback);
    Error ->
        maybe_retry({code, Error}, Worker, HttpDb, Params, Callback)
    end;

process_response(Error, Worker, HttpDb, Params, Callback) ->
    maybe_retry(Error, Worker, HttpDb, Params, Callback).


process_stream_response(ReqId, Worker, HttpDb, Params, Callback) ->
    receive
    {ibrowse_async_headers, ReqId, Code, Headers} ->
        case list_to_integer(Code) of
        Ok when Ok =:= 200 ; Ok =:= 201 ; (Ok >= 400 andalso Ok < 500) ->
            StreamDataFun = fun() ->
                stream_data_self(HttpDb, Params, Worker, ReqId, Callback)
            end,
            ibrowse:stream_next(ReqId),
            try
                Ret = Callback(Ok, Headers, StreamDataFun),
                release_worker(Worker, HttpDb),
                clean_mailbox_req(ReqId),
                Ret
            catch throw:{maybe_retry_req, Err} ->
                clean_mailbox_req(ReqId),
                maybe_retry(Err, Worker, HttpDb, Params, Callback)
            end;
        R when R =:= 301 ; R =:= 302 ; R =:= 303 ->
            do_redirect(Worker, R, Headers, HttpDb, Params, Callback);
        Error ->
            report_error(Worker, HttpDb, Params, {code, Error})
        end;
    {ibrowse_async_response, ReqId, {error, _} = Error} ->
        maybe_retry(Error, Worker, HttpDb, Params, Callback)
    after HttpDb#httpdb.timeout + 500 ->
        % Note: ibrowse should always reply with timeouts, but this doesn't
        % seem to be always true when there's a very high rate of requests
        % and many open connections.
        maybe_retry(timeout, Worker, HttpDb, Params, Callback)
    end.


clean_mailbox_req(ReqId) ->
    receive
    {ibrowse_async_response, ReqId, _} ->
        clean_mailbox_req(ReqId);
    {ibrowse_async_response_end, ReqId} ->
        clean_mailbox_req(ReqId)
    after 0 ->
        ok
    end.


release_worker(Worker, #httpdb{httpc_pool = Pool}) ->
    ok = couch_httpc_pool:release_worker(Pool, Worker).


maybe_retry(Error, Worker, #httpdb{retries = 0} = HttpDb, Params, _Cb) ->
    report_error(Worker, HttpDb, Params, {error, Error});

maybe_retry(Error, Worker, #httpdb{retries = Retries, wait = Wait} = HttpDb,
    Params, Cb) ->
    release_worker(Worker, HttpDb),
    Method = string:to_upper(atom_to_list(get_value(method, Params, get))),
    Url = couch_util:url_strip_password(full_url(HttpDb, Params)),
    ?LOG_INFO("Retrying ~s request to ~s in ~p seconds due to error ~s",
        [Method, Url, Wait / 1000, error_cause(Error)]),
    ok = timer:sleep(Wait),
    Wait2 = erlang:min(Wait * 2, ?MAX_WAIT),
    send_req(HttpDb#httpdb{retries = Retries - 1, wait = Wait2}, Params, Cb).


report_error(Worker, HttpDb, Params, Error) ->
    Method = string:to_upper(atom_to_list(get_value(method, Params, get))),
    Url = couch_util:url_strip_password(full_url(HttpDb, Params)),
    do_report_error(Url, Method, Error),
    release_worker(Worker, HttpDb),
    exit({http_request_failed, Method, Url, Error}).


do_report_error(Url, Method, {code, Code}) ->
    ?LOG_ERROR("Replicator, request ~s to ~p failed. The received "
        "HTTP error code is ~p", [Method, Url, Code]);

do_report_error(FullUrl, Method, Error) ->
    ?LOG_ERROR("Replicator, request ~s to ~p failed due to error ~s",
        [Method, FullUrl, error_cause(Error)]).


error_cause({error, Cause}) ->
    lists:flatten(io_lib:format("~p", [Cause]));
error_cause(Cause) ->
    lists:flatten(io_lib:format("~p", [Cause])).


stream_data_self(#httpdb{timeout = T} = HttpDb, Params, Worker, ReqId, Cb) ->
    receive
    {ibrowse_async_response, ReqId, {error, Error}} ->
        throw({maybe_retry_req, Error});
    {ibrowse_async_response, ReqId, <<>>} ->
        ibrowse:stream_next(ReqId),
        stream_data_self(HttpDb, Params, Worker, ReqId, Cb);
    {ibrowse_async_response, ReqId, Data} ->
        ibrowse:stream_next(ReqId),
        {Data, fun() -> stream_data_self(HttpDb, Params, Worker, ReqId, Cb) end};
    {ibrowse_async_response_end, ReqId} ->
        {<<>>, fun() -> throw({maybe_retry_req, more_data_expected}) end}
    after T + 500 ->
        % Note: ibrowse should always reply with timeouts, but this doesn't
        % seem to be always true when there's a very high rate of requests
        % and many open connections.
        throw({maybe_retry_req, timeout})
    end.


full_url(#httpdb{url = BaseUrl}, Params) ->
    Path = get_value(path, Params, []),
    QueryArgs = get_value(qs, Params, []),
    BaseUrl ++ Path ++ query_args_to_string(QueryArgs, []).


query_args_to_string([], []) ->
    "";
query_args_to_string([], Acc) ->
    "?" ++ string:join(lists:reverse(Acc), "&");
query_args_to_string([{K, V} | Rest], Acc) ->
    query_args_to_string(Rest, [K ++ "=" ++ couch_httpd:quote(V) | Acc]).


oauth_header(#httpdb{oauth = nil}, _ConnParams) ->
    [];
oauth_header(#httpdb{url = BaseUrl, oauth = OAuth}, ConnParams) ->
    Consumer = {
        OAuth#oauth.consumer_key,
        OAuth#oauth.consumer_secret,
        OAuth#oauth.signature_method
    },
    Method = case get_value(method, ConnParams, get) of
    get -> "GET";
    post -> "POST";
    put -> "PUT";
    head -> "HEAD"
    end,
    QSL = get_value(qs, ConnParams, []),
    OAuthParams = oauth:signed_params(Method,
        BaseUrl ++ get_value(path, ConnParams, []),
        QSL, Consumer, OAuth#oauth.token, OAuth#oauth.token_secret) -- QSL,
    [{"Authorization",
        "OAuth " ++ oauth_uri:params_to_header_string(OAuthParams)}].


do_redirect(Worker, Code, Headers, #httpdb{url = Url} = HttpDb, Params, Cb) ->
    release_worker(Worker, HttpDb),
    RedirectUrl = redirect_url(Headers, Url),
    {HttpDb2, Params2} = after_redirect(RedirectUrl, Code, HttpDb, Params),
    send_req(HttpDb2, Params2, Cb).


redirect_url(RespHeaders, OrigUrl) ->
    MochiHeaders = mochiweb_headers:make(RespHeaders),
    RedUrl = mochiweb_headers:get_value("Location", MochiHeaders),
    #url{
        host = Host,
        host_type = HostType,
        port = Port,
        path = Path,  % includes query string
        protocol = Proto
    } = ibrowse_lib:parse_url(RedUrl),
    #url{
        username = User,
        password = Passwd
    } = ibrowse_lib:parse_url(OrigUrl),
    Creds = case is_list(User) andalso is_list(Passwd) of
    true ->
        User ++ ":" ++ Passwd ++ "@";
    false ->
        []
    end,
    HostPart = case HostType of
    ipv6_address ->
        "[" ++ Host ++ "]";
    _ ->
        Host
    end,
    atom_to_list(Proto) ++ "://" ++ Creds ++ HostPart ++ ":" ++
        integer_to_list(Port) ++ Path.

after_redirect(RedirectUrl, 303, HttpDb, Params) ->
    after_redirect(RedirectUrl, HttpDb, ?replace(Params, method, get));
after_redirect(RedirectUrl, _Code, HttpDb, Params) ->
    after_redirect(RedirectUrl, HttpDb, Params).

after_redirect(RedirectUrl, HttpDb, Params) ->
    Params2 = lists:keydelete(path, 1, lists:keydelete(qs, 1, Params)),
    {HttpDb#httpdb{url = RedirectUrl}, Params2}.
