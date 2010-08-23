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

-module(couch_rep_httpc).
-include("couch_db.hrl").
-include("../ibrowse/ibrowse.hrl").

-export([db_exists/1, db_exists/2, full_url/1, request/1, redirected_request/2,
    redirect_url/2, spawn_worker_process/1, spawn_link_worker_process/1]).

request(#http_db{} = Req) ->
    do_request(Req).

do_request(#http_db{url=Url} = Req) when is_binary(Url) ->
    do_request(Req#http_db{url = ?b2l(Url)});

do_request(Req) ->
    #http_db{
        auth = Auth,
        body = B,
        conn = Conn,
        headers = Headers0,
        method = Method,
        options = Opts,
        qs = QS
    } = Req,
    Url = full_url(Req),
    Headers = case couch_util:get_value(<<"oauth">>, Auth) of
    undefined ->
        Headers0;
    {OAuthProps} ->
        [oauth_header(Url, QS, Method, OAuthProps) | Headers0]
    end,
    Body = case B of
    {Fun, InitialState} when is_function(Fun) ->
        {Fun, InitialState};
    nil ->
        [];
    _Else ->
        iolist_to_binary(?JSON_ENCODE(B))
    end,
    Resp = case Conn of
    nil ->
        ibrowse:send_req(Url, Headers, Method, Body, Opts, infinity);
    _ ->
        ibrowse:send_req_direct(Conn, Url, Headers, Method, Body, Opts, infinity)
    end,
    process_response(Resp, Req).

db_exists(Req) ->
    db_exists(Req, Req#http_db.url).

db_exists(Req, true) ->
    db_exists(Req, Req#http_db.url, true);

db_exists(Req, false) ->
    db_exists(Req, Req#http_db.url, false);

db_exists(Req, CanonicalUrl) ->
    db_exists(Req, CanonicalUrl, false).

db_exists(Req, CanonicalUrl, CreateDB) ->
    #http_db{
        auth = Auth,
        headers = Headers0,
        url = Url
    } = Req,
    HeadersFun = fun(Method) ->
        case couch_util:get_value(<<"oauth">>, Auth) of
        undefined ->
            Headers0;
        {OAuthProps} ->
            [oauth_header(Url, [], Method, OAuthProps) | Headers0]
        end
    end,
    case CreateDB of
        true ->
            catch ibrowse:send_req(Url, HeadersFun(put), put);
        _Else -> ok
    end,
    case catch ibrowse:send_req(Url, HeadersFun(head), head) of
    {ok, "200", _, _} ->
        Req#http_db{url = CanonicalUrl};
    {ok, "301", RespHeaders, _} ->
        RedirectUrl = redirect_url(RespHeaders, Req#http_db.url),
        db_exists(Req#http_db{url = RedirectUrl}, RedirectUrl);
    {ok, "302", RespHeaders, _} ->
        RedirectUrl = redirect_url(RespHeaders, Req#http_db.url),
        db_exists(Req#http_db{url = RedirectUrl}, CanonicalUrl);
    Error ->
        ?LOG_DEBUG("DB at ~s could not be found because ~p", [Url, Error]),
        throw({db_not_found, ?l2b(Url)})
    end.

redirect_url(RespHeaders, OrigUrl) ->
    MochiHeaders = mochiweb_headers:make(RespHeaders),
    RedUrl = mochiweb_headers:get_value("Location", MochiHeaders),
    {url, _, Base, Port, _, _, Path, Proto} = ibrowse_lib:parse_url(RedUrl),
    {url, _, _, _, User, Passwd, _, _} = ibrowse_lib:parse_url(OrigUrl),
    Creds = case is_list(User) andalso is_list(Passwd) of
    true ->
        User ++ ":" ++ Passwd ++ "@";
    false ->
        []
    end,
    atom_to_list(Proto) ++ "://" ++ Creds ++ Base ++ ":" ++
        integer_to_list(Port) ++ Path.

full_url(#http_db{url=Url} = Req) when is_binary(Url) ->
    full_url(Req#http_db{url = ?b2l(Url)});

full_url(#http_db{qs=[]} = Req) ->
    Req#http_db.url ++ Req#http_db.resource;

full_url(Req) ->
    #http_db{
        url = Url,
        resource = Resource,
        qs = QS
    } = Req,
    QStr = lists:map(fun({K,V}) -> io_lib:format("~s=~s",
        [couch_util:to_list(K), couch_util:to_list(V)]) end, QS),
    lists:flatten([Url, Resource, "?", string:join(QStr, "&")]).

process_response({ok, Status, Headers, Body}, Req) ->
    Code = list_to_integer(Status),
    if Code =:= 200; Code =:= 201 ->
        ?JSON_DECODE(maybe_decompress(Headers, Body));
    Code =:= 301; Code =:= 302 ->
        RedirectUrl = redirect_url(Headers, Req#http_db.url),
        do_request(redirected_request(Req, RedirectUrl));
    Code =:= 409 ->
        throw(conflict);
    Code >= 400, Code < 500 ->
        ?JSON_DECODE(maybe_decompress(Headers, Body));
    Code =:= 500; Code =:= 502; Code =:= 503 ->
        #http_db{pause = Pause, retries = Retries} = Req,
        ?LOG_INFO("retrying couch_rep_httpc request in ~p seconds " ++
            % "due to remote server error: ~s~s", [Pause/1000, Req#http_db.url,
            "due to remote server error: ~p Body ~s", [Pause/1000, Code,
            Body]),
        timer:sleep(Pause),
        do_request(Req#http_db{retries = Retries-1, pause = 2*Pause});
    true ->
        exit({http_request_failed, ?l2b(["unhandled response code ", Status])})
    end;

process_response({ibrowse_req_id, Id}, _Req) ->
    {ibrowse_req_id, Id};

process_response({error, _Reason}, #http_db{url=Url, retries=0}) ->
    ?LOG_ERROR("couch_rep_httpc request failed after 10 retries: ~s", [Url]),
    exit({http_request_failed, ?l2b(["failed to replicate ", Url])});
process_response({error, Reason}, Req) ->
    #http_db{
        method = Method,
        retries = Retries,
        pause = Pause
    } = Req,
    ShortReason = case Reason of
    connection_closed ->
        connection_closed;
    {'EXIT', {noproc, _}} ->
        noproc;
    {'EXIT', {normal, _}} ->
        normal;
    Else ->
        Else
    end,
    ?LOG_DEBUG("retrying couch_rep_httpc ~p request in ~p seconds due to " ++
        "{error, ~p}", [Method, Pause/1000, ShortReason]),
    timer:sleep(Pause),
    if Reason == worker_is_dead ->
        C = spawn_link_worker_process(Req),
        do_request(Req#http_db{retries = Retries-1, pause = 2*Pause, conn=C});
    true ->
        do_request(Req#http_db{retries = Retries-1, pause = 2*Pause})
    end.

redirected_request(Req, RedirectUrl) ->
    {Base, QStr, _} = mochiweb_util:urlsplit_path(RedirectUrl),
    QS = mochiweb_util:parse_qs(QStr),
    Hdrs = case couch_util:get_value(<<"oauth">>, Req#http_db.auth) of
    undefined ->
        Req#http_db.headers;
    _Else ->
        lists:keydelete("Authorization", 1, Req#http_db.headers)
    end,
    Req#http_db{url=Base, resource="", qs=QS, headers=Hdrs}.

spawn_worker_process(Req) ->
    Url = ibrowse_lib:parse_url(Req#http_db.url),
    {ok, Pid} = ibrowse_http_client:start(Url),
    Pid.

spawn_link_worker_process(Req) ->
    Url = ibrowse_lib:parse_url(Req#http_db.url),
    {ok, Pid} = ibrowse_http_client:start_link(Url),
    Pid.

maybe_decompress(Headers, Body) ->
    MochiHeaders = mochiweb_headers:make(Headers),
    case mochiweb_headers:get_value("Content-Encoding", MochiHeaders) of
    "gzip" ->
        zlib:gunzip(Body);
    _ ->
        Body
    end.

oauth_header(Url, QS, Action, Props) ->
    % erlang-oauth doesn't like iolists
    QSL = [{couch_util:to_list(K), ?b2l(?l2b(couch_util:to_list(V)))} ||
        {K,V} <- QS],
    ConsumerKey = ?b2l(couch_util:get_value(<<"consumer_key">>, Props)),
    Token = ?b2l(couch_util:get_value(<<"token">>, Props)),
    TokenSecret = ?b2l(couch_util:get_value(<<"token_secret">>, Props)),
    ConsumerSecret = ?b2l(couch_util:get_value(<<"consumer_secret">>, Props)),
    SignatureMethodStr = ?b2l(couch_util:get_value(<<"signature_method">>, Props, <<"HMAC-SHA1">>)),
    SignatureMethodAtom = case SignatureMethodStr of
        "PLAINTEXT" ->
            plaintext;
        "HMAC-SHA1" ->
            hmac_sha1;
        "RSA-SHA1" ->
            rsa_sha1
    end,
    Consumer = {ConsumerKey, ConsumerSecret, SignatureMethodAtom},
    Method = case Action of
        get -> "GET";
        post -> "POST";
        put -> "PUT";
        head -> "HEAD"
    end,
    Params = oauth:signed_params(Method, Url, QSL, Consumer, Token, TokenSecret)
        -- QSL,
    {"Authorization", "OAuth " ++ oauth_uri:params_to_header_string(Params)}.
