% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_oauth).

-include("couch_db.hrl").
-include("couch_js_functions.hrl").

-export([oauth_authentication_handler/1, handle_oauth_req/1]).

-define(OAUTH_DDOC_ID, <<"_design/oauth">>).
-define(OAUTH_VIEW_NAME, <<"oauth_credentials">>).

-record(callback_params, {
    consumer,
    token,
    token_secret,
    url,
    signature,
    params,
    username
}).

% OAuth auth handler using per-node user db
oauth_authentication_handler(Req) ->
    serve_oauth(Req, fun oauth_auth_callback/2, true).


oauth_auth_callback(Req, #callback_params{token_secret = undefined}) ->
    couch_httpd:send_error(
         Req, 400, <<"invalid_token">>, <<"Invalid OAuth token.">>);

oauth_auth_callback(#httpd{mochi_req = MochiReq} = Req, CbParams) ->
    Method = atom_to_list(MochiReq:get(method)),
    #callback_params{
        consumer = Consumer,
        token_secret = TokenSecret,
        url = Url,
        signature = Sig,
        params = Params,
        username = User
    } = CbParams,
    case oauth:verify(Sig, Method, Url, Params, Consumer, TokenSecret) of
    true ->
        set_user_ctx(Req, User);
    false ->
        ?LOG_DEBUG("OAuth handler: signature verification failed for user `~p`~n"
            "Received signature is `~p`~n"
            "HTTP method is `~p`~n"
            "URL is `~p`~n"
            "Parameters are `~p`~n"
            "Consumer is `~p`, token secret is `~p`~n"
            "Expected signature was `~p`~n",
            [User, Sig, Method, Url, Params, Consumer, TokenSecret,
                oauth:signature(Method, Url, Params, Consumer, TokenSecret)]),
        Req
    end.


% Look up the consumer key and get the roles to give the consumer
set_user_ctx(_Req, undefined) ->
    throw({bad_request, unknown_oauth_token});
set_user_ctx(Req, Name) ->
    case couch_auth_cache:get_user_creds(Name) of
        nil ->
            ?LOG_DEBUG("OAuth handler: user `~p` credentials not found", [Name]),
            Req;
        User ->
            Roles = couch_util:get_value(<<"roles">>, User, []),
            Req#httpd{user_ctx=#user_ctx{name=Name, roles=Roles}}
    end.

% OAuth request_token
handle_oauth_req(#httpd{path_parts=[_OAuth, <<"request_token">>], method=Method}=Req1) ->
    serve_oauth(Req1, fun(Req, CbParams) ->
        #callback_params{
            consumer = Consumer,
            token_secret = TokenSecret,
            url = Url,
            signature = Sig,
            params = Params
        } = CbParams,
        case oauth:verify(
            Sig, atom_to_list(Method), Url, Params, Consumer, TokenSecret) of
        true ->
            ok(Req, <<"oauth_token=requestkey&oauth_token_secret=requestsecret">>);
        false ->
            invalid_signature(Req)
        end
    end, false);
handle_oauth_req(#httpd{path_parts=[_OAuth, <<"authorize">>]}=Req) ->
    {ok, serve_oauth_authorize(Req)};
handle_oauth_req(#httpd{path_parts=[_OAuth, <<"access_token">>], method='GET'}=Req1) ->
    serve_oauth(Req1, fun(Req, CbParams) ->
        #callback_params{
            consumer = Consumer,
            token = Token,
            url = Url,
            signature = Sig,
            params = Params
        } = CbParams,
        case Token of
        "requestkey" ->
            case oauth:verify(
                Sig, "GET", Url, Params, Consumer, "requestsecret") of
            true ->
                ok(Req,
                    <<"oauth_token=accesskey&oauth_token_secret=accesssecret">>);
            false ->
                invalid_signature(Req)
            end;
        _ ->
            couch_httpd:send_error(
                Req, 400, <<"invalid_token">>, <<"Invalid OAuth token.">>)
        end
    end, false);
handle_oauth_req(#httpd{path_parts=[_OAuth, <<"access_token">>]}=Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").

invalid_signature(Req) ->
    couch_httpd:send_error(Req, 400, <<"invalid_signature">>, <<"Invalid signature value.">>).

% This needs to be protected i.e. force user to login using HTTP Basic Auth or form-based login.
serve_oauth_authorize(#httpd{method=Method}=Req1) ->
    case Method of
        'GET' ->
            % Confirm with the User that they want to authenticate the Consumer
            serve_oauth(Req1, fun(Req, CbParams) ->
                #callback_params{
                    consumer = Consumer,
                    token_secret = TokenSecret,
                    url = Url,
                    signature = Sig,
                    params = Params
                } = CbParams,
                case oauth:verify(
                    Sig, "GET", Url, Params, Consumer, TokenSecret) of
                true ->
                    ok(Req, <<"oauth_token=requestkey&",
                        "oauth_token_secret=requestsecret">>);
                false ->
                    invalid_signature(Req)
                end
            end, false);
        'POST' ->
            % If the User has confirmed, we direct the User back to the Consumer with a verification code
            serve_oauth(Req1, fun(Req, CbParams) ->
                #callback_params{
                    consumer = Consumer,
                    token_secret = TokenSecret,
                    url = Url,
                    signature = Sig,
                    params = Params
                } = CbParams,
                case oauth:verify(
                    Sig, "POST", Url, Params, Consumer, TokenSecret) of
                true ->
                    %redirect(oauth_callback, oauth_token, oauth_verifier),
                    ok(Req, <<"oauth_token=requestkey&",
                        "oauth_token_secret=requestsecret">>);
                false ->
                    invalid_signature(Req)
                end
            end, false);
        _ ->
            couch_httpd:send_method_not_allowed(Req1, "GET,POST")
    end.

serve_oauth(#httpd{mochi_req=MochiReq}=Req, Fun, FailSilently) ->
    % 1. In the HTTP Authorization header as defined in OAuth HTTP Authorization Scheme.
    % 2. As the HTTP POST request body with a content-type of application/x-www-form-urlencoded.
    % 3. Added to the URLs in the query part (as defined by [RFC3986] section 3).
    AuthHeader = case MochiReq:get_header_value("authorization") of
        undefined ->
            "";
        Else ->
            [Head | Tail] = re:split(Else, "\\s", [{parts, 2}, {return, list}]),
            case [string:to_lower(Head) | Tail] of
                ["oauth", Rest] -> Rest;
                _ -> ""
            end
    end,
    HeaderParams = oauth_uri:params_from_header_string(AuthHeader),
    %Realm = couch_util:get_value("realm", HeaderParams),

    % get requested path
    RequestedPath = case MochiReq:get_header_value("x-couchdb-requested-path") of
        undefined ->
            case MochiReq:get_header_value("x-couchdb-vhost-path") of
                undefined ->
                    MochiReq:get(raw_path);
                VHostPath ->
                    VHostPath
            end;
        RequestedPath0 ->
           RequestedPath0
    end,
    {_, QueryString, _} = mochiweb_util:urlsplit_path(RequestedPath),

    Params = proplists:delete("realm", HeaderParams) ++ mochiweb_util:parse_qs(QueryString),

    ?LOG_DEBUG("OAuth Params: ~p", [Params]),
    case couch_util:get_value("oauth_version", Params, "1.0") of
        "1.0" ->
            case couch_util:get_value("oauth_consumer_key", Params, undefined) of
                undefined ->
                    case FailSilently of
                        true -> Req;
                        false -> couch_httpd:send_error(Req, 400, <<"invalid_consumer">>, <<"Invalid consumer.">>)
                    end;
                ConsumerKey ->
                    Url = couch_httpd:absolute_uri(Req, RequestedPath),
                    case get_callback_params(ConsumerKey, Params, Url) of
                        {ok, CallbackParams} ->
                            Fun(Req, CallbackParams);
                        invalid_consumer_token_pair ->
                            couch_httpd:send_error(
                                Req, 400,
                                <<"invalid_consumer_token_pair">>,
                                <<"Invalid consumer and token pair.">>);
                        {error, {Error, Reason}} ->
                            couch_httpd:send_error(Req, 400, Error, Reason)
                    end
            end;
        _ ->
            couch_httpd:send_error(Req, 400, <<"invalid_oauth_version">>, <<"Invalid OAuth version.">>)
    end.


get_callback_params(ConsumerKey, Params, Url) ->
    Token = couch_util:get_value("oauth_token", Params),
    SigMethod = sig_method(Params),
    CbParams0 = #callback_params{
        token = Token,
        signature = couch_util:get_value("oauth_signature", Params),
        params = proplists:delete("oauth_signature", Params),
        url = Url
    },
    case oauth_credentials_info(Token, ConsumerKey) of
    nil ->
        invalid_consumer_token_pair;
    {error, _} = Err ->
        Err;
    {OauthCreds} ->
        User = couch_util:get_value(<<"username">>, OauthCreds, []),
        ConsumerSecret = ?b2l(couch_util:get_value(
            <<"consumer_secret">>, OauthCreds, <<>>)),
        TokenSecret = ?b2l(couch_util:get_value(
            <<"token_secret">>, OauthCreds, <<>>)),
        case (User =:= []) orelse (ConsumerSecret =:= []) orelse
            (TokenSecret =:= []) of
        true ->
            invalid_consumer_token_pair;
        false ->
            CbParams = CbParams0#callback_params{
                consumer = {ConsumerKey, ConsumerSecret, SigMethod},
                token_secret = TokenSecret,
                username = User
            },
            ?LOG_DEBUG("Got OAuth credentials, for ConsumerKey `~p` and "
                "Token `~p`, from the views, User: `~p`, "
                "ConsumerSecret: `~p`, TokenSecret: `~p`",
                [ConsumerKey, Token, User, ConsumerSecret, TokenSecret]),
            {ok, CbParams}
        end
    end.


sig_method(Params) ->
    sig_method_1(couch_util:get_value("oauth_signature_method", Params)).
sig_method_1("PLAINTEXT") ->
    plaintext;
% sig_method_1("RSA-SHA1") ->
%    rsa_sha1;
sig_method_1("HMAC-SHA1") ->
    hmac_sha1;
sig_method_1(_) ->
    undefined.


ok(#httpd{mochi_req=MochiReq}, Body) ->
    {ok, MochiReq:respond({200, [], Body})}.


oauth_credentials_info(Token, ConsumerKey) ->
    case use_auth_db() of
    {ok, Db} ->
        Result = case query_oauth_view(Db, [?l2b(ConsumerKey), ?l2b(Token)]) of
        [] ->
            nil;
        [Creds] ->
            Creds;
        [_ | _] ->
            Reason = iolist_to_binary(
                io_lib:format("Found multiple OAuth credentials for the pair "
                    " (consumer_key: `~p`, token: `~p`)", [ConsumerKey, Token])),
            {error, {<<"oauth_token_consumer_key_pair">>, Reason}}
        end,
        couch_db:close(Db),
        Result;
    nil ->
        {
            case couch_config:get("oauth_consumer_secrets", ConsumerKey) of
            undefined -> [];
            ConsumerSecret -> [{<<"consumer_secret">>, ?l2b(ConsumerSecret)}]
            end
            ++
            case couch_config:get("oauth_token_secrets", Token) of
            undefined -> [];
            TokenSecret -> [{<<"token_secret">>, ?l2b(TokenSecret)}]
            end
            ++
            case couch_config:get("oauth_token_users", Token) of
            undefined -> [];
            User -> [{<<"username">>, ?l2b(User)}]
            end
        }
    end.


use_auth_db() ->
    case couch_config:get("couch_httpd_oauth", "use_users_db", "false") of
    "false" ->
        nil;
    "true" ->
        AuthDb = open_auth_db(),
        {ok, _AuthDb2} = ensure_oauth_views_exist(AuthDb)
    end.


open_auth_db() ->
    DbName = ?l2b(couch_config:get("couch_httpd_auth", "authentication_db")),
    DbOptions = [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}],
    {ok, AuthDb} = couch_db:open_int(DbName, DbOptions),
    AuthDb.


ensure_oauth_views_exist(AuthDb) ->
    case couch_db:open_doc(AuthDb, ?OAUTH_DDOC_ID, []) of
    {ok, _DDoc} ->
        {ok, AuthDb};
    _ ->
        {ok, DDoc} = get_oauth_ddoc(),
        {ok, _Rev} = couch_db:update_doc(AuthDb, DDoc, []),
        {ok, _AuthDb2} = couch_db:reopen(AuthDb)
    end.


get_oauth_ddoc() ->
    Json = {[
        {<<"_id">>, ?OAUTH_DDOC_ID},
        {<<"language">>, <<"javascript">>},
        {<<"views">>,
            {[
                {?OAUTH_VIEW_NAME,
                    {[
                        {<<"map">>, ?OAUTH_MAP_FUN}
                    ]}
                }
            ]}
        }
    ]},
    {ok, couch_doc:from_json_obj(Json)}.


query_oauth_view(Db, Key) ->
    {ok, View, _} = couch_view:get_map_view(
        Db, ?OAUTH_DDOC_ID, ?OAUTH_VIEW_NAME, nil),
    FoldlFun = fun({_Key_DocId, Value}, _, Acc) ->
        {ok, [Value | Acc]}
    end,
    ViewOptions = [
        {start_key, {Key, ?MIN_STR}},
        {end_key, {Key, ?MAX_STR}}
    ],
    {ok, _, Result} = couch_view:fold(View, FoldlFun, [], ViewOptions),
    Result.
