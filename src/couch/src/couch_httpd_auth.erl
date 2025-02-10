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

-module(couch_httpd_auth).

-compile(tuple_calls).

-include_lib("couch/include/couch_db.hrl").

-export([party_mode_handler/1]).

-export([
    default_authentication_handler/1, default_authentication_handler/2,
    special_test_authentication_handler/1
]).
-export([cookie_authentication_handler/1, cookie_authentication_handler/2]).
-export([null_authentication_handler/1]).
-export([proxy_authentication_handler/1, proxy_authentification_handler/1]).
-export([cookie_auth_header/2]).
-export([handle_session_req/1, handle_session_req/2]).

-export([verify_totp/2]).
-export([ensure_cookie_auth_secret/0, make_cookie_time/0]).
-export([maybe_value/3]).

-export([jwt_authentication_handler/1]).

-import(couch_httpd, [
    header_value/2, send_json/2, send_json/4, send_method_not_allowed/2, maybe_decompress/2
]).

-compile({no_auto_import, [integer_to_binary/1, integer_to_binary/2]}).

-define(LOCKOUT_MSG, <<"Account is temporarily locked due to multiple authentication failures">>).

party_mode_handler(Req) ->
    case
        chttpd_util:get_chttpd_config_boolean(
            "require_valid_user", false
        )
    of
        true ->
            throw({unauthorized, <<"Authentication required.">>});
        false ->
            Req#httpd{user_ctx = #user_ctx{}}
    end.

special_test_authentication_handler(Req) ->
    case header_value(Req, "WWW-Authenticate") of
        "X-Couch-Test-Auth " ++ NamePass ->
            % NamePass is a colon separated string: "joe schmoe:a password".
            [Name, Pass] = re:split(NamePass, ":", [{return, list}, {parts, 2}]),
            case {Name, Pass} of
                {"Jan Lehnardt", "apple"} -> ok;
                {"Christopher Lenz", "dog food"} -> ok;
                {"Noah Slater", "biggiesmalls endian"} -> ok;
                {"Chris Anderson", "mp3"} -> ok;
                {"Damien Katz", "pecan pie"} -> ok;
                {_, _} -> throw({unauthorized, <<"Name or password is incorrect.">>})
            end,
            Req#httpd{user_ctx = #user_ctx{name = ?l2b(Name)}};
        _ ->
            % No X-Couch-Test-Auth credentials sent, give admin access so the
            % previous authentication can be restored after the test
            Req#httpd{user_ctx = ?ADMIN_USER}
    end.

basic_name_pw(Req) ->
    AuthorizationHeader = header_value(Req, "Authorization"),
    case AuthorizationHeader of
        "Basic " ++ Base64Value ->
            try
                re:split(
                    base64:decode(Base64Value),
                    ":",
                    [{return, list}, {parts, 2}]
                )
            of
                ["_", "_"] ->
                    % special name and pass to be logged out
                    nil;
                [User, Pass] ->
                    {User, Pass};
                _ ->
                    nil
            catch
                error:function_clause ->
                    throw({bad_request, "Authorization header has invalid base64 value"})
            end;
        _ ->
            nil
    end.

default_authentication_handler(Req) ->
    default_authentication_handler(Req, couch_auth_cache).

default_authentication_handler(Req, AuthModule) ->
    case basic_name_pw(Req) of
        {User, Pass} ->
            case AuthModule:get_user_creds(Req, User) of
                nil ->
                    throw({unauthorized, <<"Name or password is incorrect.">>});
                {ok, UserProps, AuthCtx} ->
                    reject_if_totp(UserProps),
                    UserName = ?l2b(User),
                    Password = ?l2b(Pass),
                    case authenticate(Req, AuthModule, UserName, Password, UserProps) of
                        true ->
                            couch_password_hasher:maybe_upgrade_password_hash(
                                Req,
                                UserName,
                                Password,
                                UserProps,
                                AuthModule,
                                AuthCtx
                            ),
                            Req#httpd{
                                user_ctx = #user_ctx{
                                    name = UserName,
                                    roles = couch_util:get_value(<<"roles">>, UserProps, [])
                                }
                            };
                        false ->
                            authentication_warning(Req, UserName),
                            throw({unauthorized, <<"Name or password is incorrect.">>})
                    end
            end;
        nil ->
            case couch_server:has_admins() of
                true ->
                    Req;
                false ->
                    case
                        chttpd_util:get_chttpd_config_boolean(
                            "require_valid_user", false
                        )
                    of
                        true -> Req;
                        % If no admins, and no user required, then everyone is admin!
                        % Yay, admin party!
                        false -> Req#httpd{user_ctx = ?ADMIN_USER}
                    end
            end
    end.

null_authentication_handler(Req) ->
    Req#httpd{user_ctx = ?ADMIN_USER}.

%% @doc proxy auth handler.
%
% This handler allows creation of a userCtx object from a user authenticated remotly.
% The client just pass specific headers to CouchDB and the handler create the userCtx.
% Headers  name can be defined in local.ini. By default they are :
%
%   * X-Auth-CouchDB-UserName : contain the username, (x_auth_username in
%   couch_httpd_auth section)
%   * X-Auth-CouchDB-Roles : contain the user roles, list of roles separated by a
%   comma (x_auth_roles in couch_httpd_auth section)
%   * X-Auth-CouchDB-Token : token to authenticate the authorization (x_auth_token
%   in couch_httpd_auth section). This token is an hmac-sha1 created from secret key
%   and username. The secret key should be the same in the client and couchdb node.
%   Secret key is the secret key in couch_httpd_auth section of ini. This token is optional
%   if value of proxy_use_secret key in couch_httpd_auth section of ini isn't true.
%
proxy_authentication_handler(Req) ->
    case proxy_auth_user(Req) of
        nil -> Req;
        Req2 -> Req2
    end.

%% @deprecated
proxy_authentification_handler(Req) ->
    proxy_authentication_handler(Req).

proxy_auth_user(Req) ->
    XHeaderUserName = chttpd_util:get_chttpd_auth_config(
        "x_auth_username", "X-Auth-CouchDB-UserName"
    ),
    XHeaderRoles = chttpd_util:get_chttpd_auth_config(
        "x_auth_roles", "X-Auth-CouchDB-Roles"
    ),
    XHeaderToken = chttpd_util:get_chttpd_auth_config(
        "x_auth_token", "X-Auth-CouchDB-Token"
    ),
    case header_value(Req, XHeaderUserName) of
        undefined ->
            nil;
        UserName ->
            Roles =
                case header_value(Req, XHeaderRoles) of
                    undefined -> [];
                    Else -> re:split(Else, "\\s*,\\s*", [trim, {return, binary}])
                end,
            case
                chttpd_util:get_chttpd_auth_config_boolean(
                    "proxy_use_secret", false
                )
            of
                true ->
                    case chttpd_util:get_chttpd_auth_config("secret") of
                        undefined ->
                            Req#httpd{user_ctx = #user_ctx{name = ?l2b(UserName), roles = Roles}};
                        Secret ->
                            HashAlgorithms = couch_util:get_config_hash_algorithms(),
                            Token = header_value(Req, XHeaderToken),
                            VerifyTokens = fun(HashAlg) ->
                                Hmac = couch_util:hmac(HashAlg, Secret, UserName),
                                couch_passwords:verify(couch_util:to_hex(Hmac), Token)
                            end,
                            case lists:any(VerifyTokens, HashAlgorithms) of
                                true ->
                                    Req#httpd{
                                        user_ctx = #user_ctx{
                                            name = ?l2b(UserName),
                                            roles = Roles
                                        }
                                    };
                                false ->
                                    nil
                            end
                    end;
                false ->
                    Req#httpd{user_ctx = #user_ctx{name = ?l2b(UserName), roles = Roles}}
            end
    end.

jwt_authentication_handler(Req) ->
    case header_value(Req, "Authorization") of
        "Bearer " ++ Jwt ->
            RequiredClaims = get_configured_claims(),
            case jwtf:decode(?l2b(Jwt), [alg | RequiredClaims], fun jwtf_keystore:get/2) of
                {ok, {Claims}} ->
                    Roles = get_roles_claim(Claims),
                    case lists:keyfind(<<"sub">>, 1, Claims) of
                        false ->
                            throw({unauthorized, <<"Token missing sub claim.">>});
                        {_, User} ->
                            Req#httpd{
                                user_ctx = #user_ctx{
                                    name = User,
                                    roles = Roles
                                }
                            }
                    end;
                {error, Reason} ->
                    throw(Reason)
            end;
        _ ->
            Req
    end.

tokenize_json_path(Path, SliceStart, [], Result) ->
    Result1 = Result ++ [?l2b(string:slice(Path, SliceStart))],
    [?l2b(string:replace(X, "\\.", ".", all)) || X <- Result1];
tokenize_json_path(Path, SliceStart, [[{Pos, _}] | T], Result) ->
    Slice = string:slice(Path, SliceStart, Pos - SliceStart),
    NewResult = Result ++ [?l2b(Slice)],
    tokenize_json_path(Path, Pos + 1, T, NewResult).

tokenize_json_path(Path, SplitPositions) ->
    tokenize_json_path(Path, 0, SplitPositions, []).

get_roles_claim(Claims) ->
    RolesClaimPath = config:get(
        "jwt_auth", "roles_claim_path"
    ),
    Roles =
        case RolesClaimPath of
            undefined ->
                couch_util:get_value(
                    ?l2b(
                        config:get(
                            "jwt_auth", "roles_claim_name", "_couchdb.roles"
                        )
                    ),
                    Claims,
                    []
                );
            Defined when is_list(Defined) ->
                % find all "." but no "\."
                PathRegex = "(?<!\\\\)\\.",
                MatchPositions =
                    case re:run(RolesClaimPath, PathRegex, [global]) of
                        nomatch -> [];
                        {match, Pos} -> Pos
                    end,
                TokenizedJsonPath = tokenize_json_path(RolesClaimPath, MatchPositions),
                couch_util:get_nested_json_value({Claims}, TokenizedJsonPath)
        end,
    Result =
        case is_list(Roles) of
            true ->
                Roles;
            false ->
                re:split(Roles, "\\s*,\\s*", [trim, {return, binary}])
        end,
    case lists:all(fun erlang:is_binary/1, Result) of
        true ->
            Result;
        false ->
            throw(
                {bad_request, <<"Malformed token">>}
            )
    end.

get_configured_claims() ->
    Claims = config:get("jwt_auth", "required_claims", "exp"),
    Re = "((?<key1>[a-z]+)|{(?<key2>[a-z]+)\s*,\s*\"(?<val>[^\"]+)\"})",
    case re:run(Claims, Re, [global, {capture, [key1, key2, val], binary}]) of
        nomatch when Claims /= "" ->
            couch_log:error("[jwt_auth] required_claims is set to an invalid value.", []),
            throw({misconfigured_server, <<"JWT is not configured correctly">>});
        nomatch ->
            [];
        {match, Matches} ->
            lists:map(fun to_claim/1, Matches)
    end.

to_claim([Key, <<>>, <<>>]) ->
    binary_to_atom(Key, latin1);
to_claim([<<>>, Key, Value]) ->
    {binary_to_atom(Key, latin1), Value}.

cookie_authentication_handler(Req) ->
    cookie_authentication_handler(Req, couch_auth_cache).

cookie_authentication_handler(#httpd{mochi_req = MochiReq} = Req, AuthModule) ->
    case MochiReq:get_cookie_value("AuthSession") of
        undefined ->
            Req;
        [] ->
            Req;
        Cookie ->
            [User, TimeStr, HashStr] =
                try
                    AuthSession = couch_util:decodeBase64Url(Cookie),
                    [_A, _B, _Cs] = re:split(
                        AuthSession,
                        ":",
                        [{return, list}, {parts, 3}]
                    )
                catch
                    _:_Error ->
                        Reason = <<"Malformed AuthSession cookie. Please clear your cookies.">>,
                        throw({bad_request, Reason})
                end,
            % Verify expiry and hash
            CurrentTime = make_cookie_time(),
            HashAlgorithms = couch_util:get_config_hash_algorithms(),
            case chttpd_util:get_chttpd_auth_config("secret") of
                undefined ->
                    couch_log:debug("cookie auth secret is not set", []),
                    Req;
                SecretStr ->
                    Secret = ?l2b(SecretStr),
                    case AuthModule:get_user_creds(Req, User) of
                        nil ->
                            Req;
                        {ok, UserProps, _AuthCtx} ->
                            UserSalt = couch_util:get_value(<<"salt">>, UserProps, <<"">>),
                            FullSecret = <<Secret/binary, UserSalt/binary>>,
                            Hash = ?l2b(HashStr),
                            VerifyHash = fun(HashAlg) ->
                                Hmac = couch_util:hmac(
                                    HashAlg,
                                    FullSecret,
                                    lists:join(":", [User, TimeStr])
                                ),
                                couch_passwords:verify(Hmac, Hash)
                            end,
                            Timeout = chttpd_util:get_chttpd_auth_config_integer(
                                "timeout", 600
                            ),
                            couch_log:debug("timeout ~p", [Timeout]),
                            case (catch erlang:list_to_integer(TimeStr, 16)) of
                                TimeStamp when CurrentTime < TimeStamp + Timeout ->
                                    case lists:any(VerifyHash, HashAlgorithms) of
                                        true ->
                                            TimeLeft = TimeStamp + Timeout - CurrentTime,
                                            couch_log:debug(
                                                "Successful cookie auth as: ~p",
                                                [User]
                                            ),
                                            Req#httpd{
                                                user_ctx = #user_ctx{
                                                    name = ?l2b(User),
                                                    roles = couch_util:get_value(
                                                        <<"roles">>, UserProps, []
                                                    )
                                                },
                                                auth =
                                                    {FullSecret, TimeLeft < Timeout * 0.9}
                                            };
                                        _Else ->
                                            Req
                                    end;
                                _Else ->
                                    Req
                            end
                    end
            end
    end.

cookie_auth_header(#httpd{user_ctx = #user_ctx{name = null}}, _Headers) ->
    [];
cookie_auth_header(
    #httpd{user_ctx = #user_ctx{name = User}, auth = {Secret, _SendCookie = true}} =
        Req,
    Headers
) ->
    % Note: we only set the AuthSession cookie if:
    %  * a valid AuthSession cookie has been received
    %  * we are outside a 10% timeout window
    %  * and if an AuthSession cookie hasn't already been set e.g. by a login
    %    or logout handler.
    % The login and logout handlers need to set the AuthSession cookie
    % themselves.
    CookieHeader = couch_util:get_value("Set-Cookie", Headers, ""),
    Cookies = mochiweb_cookies:parse_cookie(CookieHeader),
    AuthSession = couch_util:get_value("AuthSession", Cookies),
    if
        AuthSession == undefined ->
            TimeStamp = make_cookie_time(),
            [cookie_auth_cookie(Req, User, Secret, TimeStamp)];
        true ->
            []
    end;
cookie_auth_header(_Req, _Headers) ->
    [].

cookie_auth_cookie(Req, User, Secret, TimeStamp) ->
    SessionItems = [User, erlang:integer_to_list(TimeStamp, 16)],
    cookie_auth_cookie(Req, Secret, SessionItems).

cookie_auth_cookie(Req, Secret, SessionItems) when is_list(SessionItems) ->
    SessionData = lists:join(":", SessionItems),
    [HashAlgorithm | _] = couch_util:get_config_hash_algorithms(),
    Hash = couch_util:hmac(HashAlgorithm, Secret, SessionData),
    mochiweb_cookies:cookie(
        "AuthSession",
        couch_util:encodeBase64Url(lists:join(":", [SessionData, Hash])),
        cookie_attributes(Req)
    ).

clear_auth_cookie(Req) ->
    mochiweb_cookies:cookie(
        "AuthSession", "", cookie_attributes(Req)
    ).

cookie_attributes(Req) ->
    Attributes = [path(), http_only(), max_age(), cookie_scheme(Req), cookie_domain(), same_site()],
    lists:flatten(Attributes).

ensure_cookie_auth_secret() ->
    case chttpd_util:get_chttpd_auth_config("secret") of
        undefined ->
            NewSecret = ?b2l(couch_uuids:random()),
            config:set("chttpd_auth", "secret", NewSecret),
            NewSecret;
        Secret ->
            Secret
    end.

% session handlers
% Login handler with user db
handle_session_req(Req) ->
    handle_session_req(Req, couch_auth_cache).

handle_session_req(#httpd{method = 'POST', mochi_req = MochiReq} = Req, AuthModule) ->
    ReqBody = MochiReq:recv_body(),
    Form =
        case MochiReq:get_primary_header_value("content-type") of
            % content type should be json
            "application/x-www-form-urlencoded" ++ _ ->
                mochiweb_util:parse_qs(ReqBody);
            "application/json" ++ _ ->
                {Pairs} = ?JSON_DECODE(maybe_decompress(Req, ReqBody)),
                lists:map(
                    fun({Key, Value}) ->
                        {?b2l(Key), ?b2l(Value)}
                    end,
                    Pairs
                );
            _ ->
                throw(
                    {bad_ctype, <<
                        "Content-Type must be "
                        "'application/x-www-form-urlencoded' or "
                        "'application/json'"
                    >>}
                )
        end,
    UserName = ?l2b(extract_username(Form)),
    Password = ?l2b(couch_util:get_value("password", Form, "")),
    couch_log:debug("Attempt Login: ~s", [UserName]),
    {ok, UserProps, AuthCtx} =
        case AuthModule:get_user_creds(Req, UserName) of
            nil -> {ok, [], nil};
            Result -> Result
        end,
    case authenticate(Req, AuthModule, UserName, Password, UserProps) of
        true ->
            verify_totp(UserProps, Form),
            couch_password_hasher:maybe_upgrade_password_hash(
                Req, UserName, Password, UserProps, AuthModule, AuthCtx
            ),
            % setup the session cookie
            Secret = ?l2b(ensure_cookie_auth_secret()),
            UserSalt = couch_util:get_value(<<"salt">>, UserProps),
            CurrentTime = make_cookie_time(),
            Cookie = cookie_auth_cookie(
                Req, UserName, <<Secret/binary, UserSalt/binary>>, CurrentTime
            ),
            % TODO document the "next" feature in Futon
            {Code, Headers} =
                case couch_httpd:qs_value(Req, "next", nil) of
                    nil ->
                        {200, [Cookie]};
                    Redirect ->
                        {302, [Cookie, {"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
                end,
            send_json(
                Req#httpd{req_body = ReqBody},
                Code,
                Headers,
                {[
                    {ok, true},
                    {name, UserName},
                    {roles, couch_util:get_value(<<"roles">>, UserProps, [])}
                ]}
            );
        false ->
            authentication_warning(Req, UserName),
            % clear the session
            Cookie = clear_auth_cookie(Req),
            {Code, Headers} =
                case couch_httpd:qs_value(Req, "fail", nil) of
                    nil ->
                        {401, [Cookie]};
                    Redirect ->
                        {302, [Cookie, {"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
                end,
            send_json(
                Req,
                Code,
                Headers,
                {[{error, <<"unauthorized">>}, {reason, <<"Name or password is incorrect.">>}]}
            )
    end;
% get user info
% GET /_session
handle_session_req(#httpd{method = 'GET', user_ctx = UserCtx} = Req, _AuthModule) ->
    Name = UserCtx#user_ctx.name,
    ForceLogin = couch_httpd:qs_value(Req, "basic", "false"),
    case {Name, ForceLogin} of
        {null, "true"} ->
            throw({unauthorized, <<"Please login.">>});
        {Name, _} ->
            send_json(
                Req,
                {[
                    % remove this ok
                    {ok, true},
                    {<<"userCtx">>,
                        {[
                            {name, Name},
                            {roles, UserCtx#user_ctx.roles}
                        ]}},
                    {info, {
                        [
                            {authentication_handlers, [
                                N
                             || {N, _Fun} <- Req#httpd.authentication_handlers
                            ]}
                        ] ++
                            maybe_value(authenticated, UserCtx#user_ctx.handler, fun(Handler) ->
                                Handler
                            end) ++
                            maybe_value(
                                authentication_db,
                                config:get("chttpd_auth", "authentication_db"),
                                fun(Val) ->
                                    ?l2b(Val)
                                end
                            )
                    }}
                ]}
            )
    end;
% logout by deleting the session
handle_session_req(#httpd{method = 'DELETE'} = Req, _AuthModule) ->
    Cookie = clear_auth_cookie(Req),
    {Code, Headers} =
        case couch_httpd:qs_value(Req, "next", nil) of
            nil ->
                {200, [Cookie]};
            Redirect ->
                {302, [Cookie, {"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
        end,
    send_json(Req, Code, Headers, {[{ok, true}]});
handle_session_req(Req, _AuthModule) ->
    send_method_not_allowed(Req, "GET,HEAD,POST,DELETE").

extract_username(Form) ->
    CouchFormat = couch_util:get_value("name", Form),
    case couch_util:get_value("username", Form, CouchFormat) of
        undefined ->
            throw({bad_request, <<"request body must contain a username">>});
        Format when Format =:= CouchFormat ->
            CouchFormat;
        Else1 when CouchFormat == undefined ->
            Else1;
        _Else2 ->
            throw({bad_request, <<"request body contains different usernames">>})
    end.

maybe_value(_Key, undefined, _Fun) -> [];
maybe_value(Key, Else, Fun) -> [{Key, Fun(Else)}].

authenticate(Req, AuthModule, UserName, Password, UserProps) ->
    UserSalt = couch_util:get_value(<<"salt">>, UserProps, <<>>),
    case couch_auth_lockout:is_locked_out(Req, UserName, UserSalt) of
        true ->
            lockout_warning(Req, UserName),
            throw({forbidden, ?LOCKOUT_MSG});
        false ->
            ok
    end,
    case couch_passwords_cache:authenticate(AuthModule, UserName, Password, UserSalt) of
        not_found ->
            case authenticate_int(Password, UserSalt, UserProps) of
                false ->
                    ok = couch_auth_lockout:lockout(Req, UserName, UserSalt),
                    false;
                true ->
                    couch_passwords_cache:insert(AuthModule, UserName, Password, UserSalt),
                    true
            end;
        false ->
            ok = couch_auth_lockout:lockout(Req, UserName, UserSalt),
            false;
        true ->
            true
    end.

authenticate_int(Pass, UserSalt, UserProps) ->
    {PasswordHash, ExpectedHash} =
        case couch_util:get_value(<<"password_scheme">>, UserProps, <<"simple">>) of
            <<"simple">> ->
                authenticate_int_simple(Pass, UserSalt, UserProps);
            <<"pbkdf2">> ->
                authenticate_int_pbkdf2(Pass, UserSalt, UserProps)
        end,
    couch_passwords:verify(PasswordHash, ExpectedHash).

authenticate_int_simple(Pass, UserSalt, UserProps) ->
    {
        couch_passwords:simple(Pass, UserSalt),
        couch_util:get_value(<<"password_sha">>, UserProps, nil)
    }.

authenticate_int_pbkdf2(Pass, UserSalt, UserProps) ->
    PRF = couch_util:get_value(<<"pbkdf2_prf">>, UserProps, <<"sha">>),
    verify_prf(PRF),
    Iterations = couch_util:get_value(<<"iterations">>, UserProps, 10000),
    verify_iterations(Iterations),
    {
        couch_passwords:pbkdf2(
            binary_to_existing_atom(PRF), Pass, UserSalt, Iterations
        ),
        couch_util:get_value(<<"derived_key">>, UserProps, nil)
    }.

verify_iterations(Iterations) when is_integer(Iterations) ->
    Min = chttpd_util:get_chttpd_auth_config_integer("min_iterations", 1),
    Max = chttpd_util:get_chttpd_auth_config_integer("max_iterations", 1000000000),
    case Iterations < Min of
        true ->
            throw({forbidden, <<"Iteration count is too low for this server">>});
        false ->
            ok
    end,
    case Iterations > Max of
        true ->
            throw({forbidden, <<"Iteration count is too high for this server">>});
        false ->
            ok
    end.

verify_prf(PRF) when
    PRF == <<"sha">>;
    PRF == <<"sha224">>;
    PRF == <<"sha256">>;
    PRF == <<"sha384">>;
    PRF == <<"sha512">>
->
    ok;
verify_prf(_PRF) ->
    throw({forbidden, <<"PRF is invalid">>}).

make_cookie_time() ->
    {NowMS, NowS, _} = os:timestamp(),
    NowMS * 1000000 + NowS.

path() ->
    {path, "/"}.

http_only() ->
    {http_only, true}.

cookie_scheme(#httpd{mochi_req = MochiReq}) ->
    case MochiReq:get(scheme) of
        http -> [];
        https -> [{secure, true}]
    end.

max_age() ->
    case
        chttpd_util:get_chttpd_auth_config_boolean(
            "allow_persistent_cookies", true
        )
    of
        false ->
            [];
        true ->
            Timeout = chttpd_util:get_chttpd_auth_config_integer(
                "timeout", 600
            ),
            [{max_age, Timeout}]
    end.

cookie_domain() ->
    Domain = chttpd_util:get_chttpd_auth_config("cookie_domain", ""),
    case Domain of
        "" -> [];
        _ -> [{domain, Domain}]
    end.

same_site() ->
    SameSite = chttpd_util:get_chttpd_auth_config("same_site", ""),
    case string:to_lower(SameSite) of
        "" ->
            [];
        "none" ->
            [{same_site, none}];
        "lax" ->
            [{same_site, lax}];
        "strict" ->
            [{same_site, strict}];
        _ ->
            couch_log:error("invalid config value couch_httpd_auth.same_site: ~p ", [SameSite]),
            []
    end.

reject_if_totp(User) ->
    case get_totp_config(User) of
        undefined ->
            ok;
        _ ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
    end.

verify_totp(User, Form) ->
    case get_totp_config(User) of
        undefined ->
            ok;
        {Props} ->
            Key = couch_base32:decode(couch_util:get_value(<<"key">>, Props)),
            Alg = couch_util:to_existing_atom(
                couch_util:get_value(<<"algorithm">>, Props, <<"sha">>)
            ),
            Len = couch_util:get_value(<<"length">>, Props, 6),
            Token = ?l2b(couch_util:get_value("token", Form, "")),
            verify_token(Alg, Key, Len, Token)
    end.

get_totp_config(User) ->
    couch_util:get_value(<<"totp">>, User).

verify_token(Alg, Key, Len, Token) ->
    Now = make_cookie_time(),
    Tokens = [
        generate_token(Alg, Key, Len, Now - 30),
        generate_token(Alg, Key, Len, Now),
        generate_token(Alg, Key, Len, Now + 30)
    ],
    %% evaluate all tokens in constant time
    Match = lists:foldl(
        fun(T, Acc) -> couch_util:verify(T, Token) or Acc end,
        false,
        Tokens
    ),
    case Match of
        true ->
            ok;
        _ ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
    end.

generate_token(Alg, Key, Len, Timestamp) ->
    integer_to_binary(couch_totp:generate(Alg, Key, Timestamp, 30, Len), Len).

integer_to_binary(Int, Len) when is_integer(Int), is_integer(Len) ->
    Unpadded = erlang:integer_to_binary(Int),
    Padding = binary:copy(<<"0">>, Len),
    Padded = <<Padding/binary, Unpadded/binary>>,
    binary:part(Padded, byte_size(Padded), -Len).

authentication_warning(#httpd{mochi_req = Req}, User) ->
    Peer = Req:get(peer),
    couch_log:warning(
        "~p: Authentication failed for user ~s from ~s",
        [?MODULE, User, Peer]
    ).

lockout_warning(#httpd{mochi_req = Req}, User) ->
    Peer = Req:get(peer),
    couch_log:warning(
        "~p: Authentication rejected for locked-out user ~s from ~s",
        [?MODULE, User, Peer]
    ).
