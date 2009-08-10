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

-export([db_exists/1, full_url/1, request/1, spawn_worker_process/1,
    spawn_link_worker_process/1]).

request(Req) when is_record(Req, http_db) ->
    do_request(Req).

do_request(#http_db{url=Url} = Req) when is_binary(Url) ->
    do_request(Req#http_db{url = ?b2l(Url)});

do_request(Req) ->
    #http_db{
        auth = Auth,
        headers = Headers0,
        method = Method,
        body = B,
        options = Opts,
        conn = Conn
    } = Req,
    Url = full_url(Req),
    Headers = case proplists:get_value(<<"oauth">>, Auth) of
    undefined ->
        Headers0;
    {OAuthProps} ->
        [oauth_header(Url, Method, OAuthProps) | Headers0]
    end,
    Body = if B =:= nil -> []; true -> iolist_to_binary(?JSON_ENCODE(B)) end,
    Resp = case Conn of
    nil ->
        ibrowse:send_req(Url, Headers, Method, Body, Opts, infinity);
    _ ->
        ibrowse:send_req_direct(Conn, Url, Headers, Method, Body, Opts, infinity)
    end,
    process_response(Resp, Req).

db_exists(Req) ->
    #http_db{
        url = Url,
        headers = Headers
    } = Req,
    case catch ibrowse:send_req(Url, Headers, head) of
    {ok, "200", _, _} ->
        true;
    {ok, "301", Headers, _} ->
        MochiHeaders = mochiweb_headers:make(Headers),
        RedirectUrl = mochiweb_headers:get_value("Location", MochiHeaders),
        db_exists(Req#http_db{url = RedirectUrl});
    Error ->
        ?LOG_DEBUG("DB at ~s could not be found because ~p", [Url, Error]),
        false
    end.

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
    Code =:= 301 ->
        MochiHeaders = mochiweb_headers:make(Headers),
        RedirectUrl = mochiweb_headers:get_value("Location", MochiHeaders),
        do_request(Req#http_db{url = RedirectUrl});
    Code >= 400, Code < 500 ->
        ?JSON_DECODE(maybe_decompress(Headers, Body));
    Code =:= 500; Code =:= 502 ->
        #http_db{pause = Pause, retries = Retries} = Req,
        ?LOG_INFO("retrying couch_rep_httpc request in ~p seconds " ++
            % "due to remote server error: ~s~s", [Pause/1000, Req#http_db.url,
            "due to remote server error: ~p Body ~s", [Pause/1000, Code,
            Body]),
        timer:sleep(Pause),
        do_request(Req#http_db{retries = Retries-1, pause = 2*Pause})
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
        % "{error}", [Method, Pause]),
    timer:sleep(Pause),
    do_request(Req#http_db{retries = Retries-1, pause = 2*Pause}).

spawn_worker_process(Req) ->
    Url = ibrowse_lib:parse_url(Req#http_db.url),
    {ok, Pid} = ibrowse:spawn_worker_process(Url#url.host, Url#url.port),
    Pid.

spawn_link_worker_process(Req) ->
    Url = ibrowse_lib:parse_url(Req#http_db.url),
    {ok, Pid} = ibrowse:spawn_link_worker_process(Url#url.host, Url#url.port),
    Pid.

maybe_decompress(Headers, Body) ->
    MochiHeaders = mochiweb_headers:make(Headers),
    case mochiweb_headers:get_value("Content-Encoding", MochiHeaders) of
    "gzip" ->
        zlib:gunzip(Body);
    _ ->
        Body
    end.

oauth_header(Url, Action, Props) ->
    ConsumerKey = ?b2l(proplists:get_value(<<"consumer_key">>, Props)),
    Token = ?b2l(proplists:get_value(<<"token">>, Props)),
    TokenSecret = ?b2l(proplists:get_value(<<"token_secret">>, Props)),
    ConsumerSecret = ?b2l(proplists:get_value(<<"consumer_secret">>, Props)),
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    Method = case Action of
        get -> "GET";
        post -> "POST";
        put -> "PUT"
    end,
    Params = oauth:signed_params(Method, Url, [], Consumer, Token, TokenSecret),
    {"Authorization", "OAuth " ++ oauth_uri:params_to_header_string(Params)}.
