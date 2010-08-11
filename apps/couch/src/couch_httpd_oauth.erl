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

-export([oauth_authentication_handler/1, handle_oauth_req/1, consumer_lookup/2]).

% OAuth auth handler using per-node user db
oauth_authentication_handler(#httpd{mochi_req=MochiReq}=Req) ->
    serve_oauth(Req, fun(URL, Params, Consumer, Signature) ->
        AccessToken = couch_util:get_value("oauth_token", Params),
        case couch_config:get("oauth_token_secrets", AccessToken) of
            undefined ->
                couch_httpd:send_error(Req, 400, <<"invalid_token">>,
                    <<"Invalid OAuth token.">>);
            TokenSecret ->
                ?LOG_DEBUG("OAuth URL is: ~p", [URL]),
                case oauth:verify(Signature, atom_to_list(MochiReq:get(method)), URL, Params, Consumer, TokenSecret) of
                    true ->
                        set_user_ctx(Req, AccessToken);
                    false ->
                        Req
                end
        end
    end, true).

% Look up the consumer key and get the roles to give the consumer
set_user_ctx(Req, AccessToken) ->
    % TODO move to db storage
    Name = case couch_config:get("oauth_token_users", AccessToken) of
        undefined -> throw({bad_request, unknown_oauth_token});
        Value -> ?l2b(Value)
    end,
    case couch_auth_cache:get_user_creds(Name) of
        nil -> Req;
        User ->
            Roles = couch_util:get_value(<<"roles">>, User, []),
            Req#httpd{user_ctx=#user_ctx{name=Name, roles=Roles}}
    end.

% OAuth request_token
handle_oauth_req(#httpd{path_parts=[_OAuth, <<"request_token">>], method=Method}=Req) ->
    serve_oauth(Req, fun(URL, Params, Consumer, Signature) ->
        AccessToken = couch_util:get_value("oauth_token", Params),
        TokenSecret = couch_config:get("oauth_token_secrets", AccessToken),
        case oauth:verify(Signature, atom_to_list(Method), URL, Params, Consumer, TokenSecret) of
            true ->
                ok(Req, <<"oauth_token=requestkey&oauth_token_secret=requestsecret">>);
            false ->
                invalid_signature(Req)
        end
    end, false);
handle_oauth_req(#httpd{path_parts=[_OAuth, <<"authorize">>]}=Req) ->
    {ok, serve_oauth_authorize(Req)};
handle_oauth_req(#httpd{path_parts=[_OAuth, <<"access_token">>], method='GET'}=Req) ->
    serve_oauth(Req, fun(URL, Params, Consumer, Signature) ->
        case oauth:token(Params) of
            "requestkey" ->
                case oauth:verify(Signature, "GET", URL, Params, Consumer, "requestsecret") of
                    true ->
                        ok(Req, <<"oauth_token=accesskey&oauth_token_secret=accesssecret">>);
                    false ->
                        invalid_signature(Req)
                end;
            _ ->
                couch_httpd:send_error(Req, 400, <<"invalid_token">>, <<"Invalid OAuth token.">>)
        end
    end, false);
handle_oauth_req(#httpd{path_parts=[_OAuth, <<"access_token">>]}=Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").

invalid_signature(Req) ->
    couch_httpd:send_error(Req, 400, <<"invalid_signature">>, <<"Invalid signature value.">>).

% This needs to be protected i.e. force user to login using HTTP Basic Auth or form-based login.
serve_oauth_authorize(#httpd{method=Method}=Req) ->
    case Method of
        'GET' ->
            % Confirm with the User that they want to authenticate the Consumer
            serve_oauth(Req, fun(URL, Params, Consumer, Signature) ->
                AccessToken = couch_util:get_value("oauth_token", Params),
                TokenSecret = couch_config:get("oauth_token_secrets", AccessToken),
                case oauth:verify(Signature, "GET", URL, Params, Consumer, TokenSecret) of
                    true ->
                        ok(Req, <<"oauth_token=requestkey&oauth_token_secret=requestsecret">>);
                    false ->
                        invalid_signature(Req)
                end
            end, false);
        'POST' ->
            % If the User has confirmed, we direct the User back to the Consumer with a verification code
            serve_oauth(Req, fun(URL, Params, Consumer, Signature) ->
                AccessToken = couch_util:get_value("oauth_token", Params),
                TokenSecret = couch_config:get("oauth_token_secrets", AccessToken),
                case oauth:verify(Signature, "POST", URL, Params, Consumer, TokenSecret) of
                    true ->
                        %redirect(oauth_callback, oauth_token, oauth_verifier),
                        ok(Req, <<"oauth_token=requestkey&oauth_token_secret=requestsecret">>);
                    false ->
                        invalid_signature(Req)
                end
            end, false);
        _ ->
            couch_httpd:send_method_not_allowed(Req, "GET,POST")
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
    Params = proplists:delete("realm", HeaderParams) ++ MochiReq:parse_qs(),
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
                    SigMethod = couch_util:get_value("oauth_signature_method", Params),
                    case consumer_lookup(ConsumerKey, SigMethod) of
                        none ->
                            couch_httpd:send_error(Req, 400, <<"invalid_consumer">>, <<"Invalid consumer (key or signature method).">>);
                        Consumer ->
                            Signature = couch_util:get_value("oauth_signature", Params),
                            URL = couch_httpd:absolute_uri(Req, MochiReq:get(raw_path)),
                            Fun(URL, proplists:delete("oauth_signature", Params),
                                Consumer, Signature)
                    end
            end;
        _ ->
            couch_httpd:send_error(Req, 400, <<"invalid_oauth_version">>, <<"Invalid OAuth version.">>)
    end.

consumer_lookup(Key, MethodStr) ->
    SignatureMethod = case MethodStr of
        "PLAINTEXT" -> plaintext;
        "HMAC-SHA1" -> hmac_sha1;
        %"RSA-SHA1" -> rsa_sha1;
        _Else -> undefined
    end,
    case SignatureMethod of
        undefined -> none;
        _SupportedMethod ->
            case couch_config:get("oauth_consumer_secrets", Key, undefined) of
                undefined -> none;
                Secret -> {Key, Secret, SignatureMethod}
            end
    end.

ok(#httpd{mochi_req=MochiReq}, Body) ->
    {ok, MochiReq:respond({200, [], Body})}.
