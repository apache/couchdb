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

-module(chttpd_auth).
-include_lib("couch/include/couch_db.hrl").

-export([special_test_authentication_handler/1, null_authentication_handler/1,
    cookie_authentication_handler/1, default_authentication_handler/1,
    handle_session_req/1, handle_user_req/1, cookie_auth_header/2]).
    
% used by OAuth handler
-export([get_user/1, ensure_users_db_exists/1]).

-import(chttpd, [send_json/2, send_json/4, send_method_not_allowed/2]).

special_test_authentication_handler(Req) ->
    case chttpd:header_value(Req, "WWW-Authenticate") of
    "X-Couch-Test-Auth " ++ NamePass ->
        % NamePass is a colon separated string: "joe schmoe:a password".
        [Name, Pass] = re:split(NamePass, ":", [{return, list}]),
        case {Name, Pass} of
        {"Jan Lehnardt", "apple"} -> ok;
        {"Christopher Lenz", "dog food"} -> ok;
        {"Noah Slater", "biggiesmalls endian"} -> ok;
        {"Chris Anderson", "mp3"} -> ok;
        {"Damien Katz", "pecan pie"} -> ok;
        {_, _} ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
        end,
        Req#httpd{user_ctx=#user_ctx{name=?l2b(Name)}};
    _ ->
        % No X-Couch-Test-Auth credentials sent, give admin access so the
        % previous authentication can be restored after the test
        Req#httpd{user_ctx=#user_ctx{roles=[<<"_admin">>]}}
    end.

null_authentication_handler(Req) ->
    Ctx = #user_ctx{roles=[<<"_reader">>, <<"writer">>, <<"_admin">>]},
    Req#httpd{user_ctx=Ctx}.

default_authentication_handler(Req) ->
    case basic_username_pw(Req) of
    {Username, Password} ->
        case get_user(Username) of
        nil ->
            throw({unauthorized, <<"unknown username">>});
        Props ->
            ExpectedHash = couch_util:get_value(<<"password_sha">>, Props),
            Salt = couch_util:get_value(<<"salt">>, Props),
            PasswordHash = hash_password(?l2b(Password), Salt),
            case couch_util:verify(ExpectedHash, PasswordHash) of
            true ->
                Ctx = #user_ctx{
                    name = couch_util:get_value(<<"username">>, Props),
                    roles = couch_util:get_value(<<"roles">>, Props)
                },
                Req#httpd{user_ctx=Ctx};
            _ ->
                throw({unauthorized, <<"password is incorrect">>})
            end
        end;
    nil ->
        Req
    end.

cookie_authentication_handler(#httpd{path_parts=[<<"_session">>],
        method='POST'} = Req) ->
    % ignore any cookies sent with login request
    Req;
cookie_authentication_handler(Req) ->
    try cookie_auth_user(Req) of
    nil ->
        Req;
    {cookie_auth_failed, _} = X ->
        Req#httpd{auth=X};
    Req2 ->
        Req2
    catch error:_ ->
        Req#httpd{auth={cookie_auth_failed, {invalid_cookie, null}}}
    end.

cookie_auth_header(#httpd{auth={cookie_auth_failed, _}}, Headers) ->
    % check for an AuthSession cookie from login handler
    CookieHeader = couch_util:get_value("Set-Cookie", Headers, ""),
    Cookies = mochiweb_cookies:parse_cookie(CookieHeader),
    AuthSession = couch_util:get_value("AuthSession", Cookies),
    if AuthSession == undefined ->
        [generate_cookie_buster()];
    true ->
        []
    end;
cookie_auth_header(#httpd{user_ctx=#user_ctx{name=null}}, _Headers) ->
    [];
cookie_auth_header(#httpd{user_ctx=Ctx, auth={Secret,true}}, Headers) ->
    % Note: we only set the AuthSession cookie if:
    %  * a valid AuthSession cookie has been received
    %  * we are outside a 10% timeout window
    %  * and if an AuthSession cookie hasn't already been set e.g. by a login
    %    or logout handler.
    % The login and logout handlers set the AuthSession cookie themselves.
    CookieHeader = couch_util:get_value("Set-Cookie", Headers, ""),
    Cookies = mochiweb_cookies:parse_cookie(CookieHeader),
    AuthSession = couch_util:get_value("AuthSession", Cookies),
    if AuthSession == undefined ->
        [generate_cookie(Ctx#user_ctx.name, Secret, timestamp())];
    true ->
        []
    end;
cookie_auth_header(_Req, _Headers) ->
    [].

handle_session_req(#httpd{method='POST', mochi_req=MochiReq, user_ctx=Ctx}=Req) ->
    % login
    Form = parse_form(MochiReq),
    UserName = extract_username(Form),
    case get_user(UserName) of
    nil ->
        throw({forbidden, <<"unknown username">>});
    User ->
        UserSalt = couch_util:get_value(<<"salt">>, User),
        case lists:member(<<"_admin">>, Ctx#user_ctx.roles) of
        true ->
            ok;
        false ->
            Password = extract_password(Form),
            ExpectedHash = couch_util:get_value(<<"password_sha">>, User),
            PasswordHash = hash_password(Password, UserSalt),
            case couch_util:verify(ExpectedHash, PasswordHash) of
            true ->
                ok;
            _Else ->
                throw({forbidden, <<"Name or password is incorrect.">>})
            end
        end,
        Secret = ?l2b(couch_config:get("couch_httpd_auth", "secret")),
        SecretAndSalt = <<Secret/binary, UserSalt/binary>>,
        Cookie = generate_cookie(UserName, SecretAndSalt, timestamp()),
        send_response(Req, [Cookie])
    end;
handle_session_req(#httpd{method='GET', user_ctx=UserCtx}=Req) ->
    % whoami
    #user_ctx{name = Name, roles = Roles} = UserCtx,
    ForceLogin = chttpd:qs_value(Req, "basic", "false"),
    case {Name, ForceLogin} of
    {null, "true"} ->
        throw({unauthorized, <<"Please login.">>});
    _False -> 
        Props = [{name,Name}, {roles,Roles}],
        send_json(Req, {[{ok,true}, {userCtx, {Props}} | Props]})
    end;
handle_session_req(#httpd{method='DELETE'}=Req) ->
    % logout
    send_response(Req, [generate_cookie_buster()]);
handle_session_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD,POST,DELETE").

handle_user_req(#httpd{method='POST'}=Req) ->
    DbName = couch_config:get("couch_httpd_auth", "authentication_db", "users"),
    ensure_users_db_exists(DbName),
    create_user(Req, DbName);
handle_user_req(#httpd{method=Method, path_parts=[_]}=_Req) when
        Method == 'PUT' orelse Method == 'DELETE' ->
    throw({bad_request, <<"Username is missing">>});
handle_user_req(#httpd{method='PUT', path_parts=[_, UserName]}=Req) ->
    DbName = couch_config:get("couch_httpd_auth", "authentication_db", "users"),
    ensure_users_db_exists(DbName),
    update_user(Req, DbName, UserName);
handle_user_req(#httpd{method='DELETE', path_parts=[_, UserName]}=Req) ->
    DbName = couch_config:get("couch_httpd_auth", "authentication_db", "users"),
    ensure_users_db_exists(DbName),
    delete_user(Req, DbName, UserName);
handle_user_req(Req) ->
    send_method_not_allowed(Req, "DELETE,POST,PUT").

get_user(UserName) when is_list(UserName) ->
    get_user(?l2b(UserName));
get_user(UserName) ->
    case couch_config:get("admins", ?b2l(UserName)) of
    "-hashed-" ++ HashedPwdAndSalt ->
        [HashedPwd, Salt] = string:tokens(HashedPwdAndSalt, ","),
        [
            {<<"username">>, UserName},
            {<<"roles">>, [<<"_reader">>, <<"_writer">>, <<"_admin">>]},
            {<<"salt">>, ?l2b(Salt)},
            {<<"password_sha">>, ?l2b(HashedPwd)}
        ];
    _ ->
        try ets:lookup(users_cache, UserName) of
        [{UserName, Props}] ->
            Props;
        [] ->
            load_user_from_db(UserName)
        catch error:badarg ->
            load_user_from_db(UserName)
        end
    end.

load_user_from_db(UserName) ->
    DbName = couch_config:get("couch_httpd_auth", "authentication_db", "users"),
    try fabric:open_doc(DbName, UserName, []) of
    {ok, Doc} ->
        ?LOG_INFO("cache miss on username ~s", [UserName]),
        {Props} = couch_doc:to_json_obj(Doc, []),
        Props;
    _Else ->
        ?LOG_INFO("no record of user ~s", [UserName]),
        nil
    catch error:database_does_not_exist ->
        nil
    end.

ensure_users_db_exists(DbName) ->
    try fabric:get_doc_count(DbName) of
    {ok, N} when is_integer(N) ->
        ok;
    {error, _} ->
        fabric:create_db(DbName, [])
    catch error:database_does_not_exist ->
        fabric:create_db(DbName, [])
    end.

% internal functions

basic_username_pw(Req) ->
    case chttpd:header_value(Req, "Authorization") of
    "Basic " ++ Base64Value ->
        case string:tokens(?b2l(base64:decode(Base64Value)),":") of
        [User, Pass] ->
            {User, Pass};
        [User] ->
            {User, ""};
        _ ->
            nil
        end;
    _ ->
        nil
    end.

cookie_auth_user(#httpd{mochi_req=MochiReq}=Req) ->
    case MochiReq:get_cookie_value("AuthSession") of
    undefined ->
        nil;
    Cookie -> 
        AuthSession = couch_util:decodeBase64Url(Cookie),
        [User, TimeStr | HashParts] = string:tokens(?b2l(AuthSession), ":"),
        % Verify expiry and hash
        case couch_config:get("couch_httpd_auth", "secret") of
        undefined ->
            ?LOG_DEBUG("AuthSession cookie, but no secret in config!", []),
            {cookie_auth_failed, {internal_server_error, null}};
        SecretStr ->
            case get_user(User) of
            nil ->
                Msg = io_lib:format("no record of user ~s", [User]),
                {cookie_auth_failed, {bad_user, ?l2b(Msg)}};
            Result ->
                Secret = ?l2b(SecretStr),
                UserSalt = couch_util:get_value(<<"salt">>, Result),
                FullSecret = <<Secret/binary, UserSalt/binary>>,
                ExpectedHash = crypto:sha_mac(FullSecret, [User, ":", TimeStr]),
                PasswordHash = ?l2b(string:join(HashParts, ":")),
                case couch_util:verify(ExpectedHash, PasswordHash) of
                true ->
                    TimeStamp = erlang:list_to_integer(TimeStr, 16),
                    Timeout = erlang:list_to_integer(couch_config:get(
                        "couch_httpd_auth", "timeout", "600")),
                    CurrentTime = timestamp(),
                    if CurrentTime < TimeStamp + Timeout ->
                        TimeLeft = TimeStamp + Timeout - CurrentTime,
                        Req#httpd{user_ctx=#user_ctx{
                            name=?l2b(User),
                            roles=couch_util:get_value(<<"roles">>, Result, [])
                        }, auth={FullSecret, TimeLeft < Timeout*0.9}};
                    true ->
                        ?LOG_DEBUG("cookie for ~s was expired", [User]),
                        Msg = lists:concat(["Your session has expired after ",
                            Timeout div 60, " minutes of inactivity"]),
                        {cookie_auth_failed, {credentials_expired, ?l2b(Msg)}}
                    end;
                _Else ->
                    Msg = <<"cookie password hash was incorrect">>,
                    {cookie_auth_failed, {bad_password, Msg}}
                end
            end
        end
    end.

create_user(#httpd{method='POST', mochi_req=MochiReq}=Req, Db) ->
    Form = parse_form(MochiReq),
    {UserName, Password} = extract_username_password(Form),
    case get_user(UserName) of
    nil -> 
        Roles = [?l2b(R) || R <- proplists:get_all_values("roles", Form)],
        if Roles /= [] ->
            chttpd:verify_is_server_admin(Req);
        true -> ok end,
        Active = chttpd_view:parse_bool_param(couch_util:get_value("active",
            Form, "true")),
        UserSalt = couch_uuids:random(),
        UserDoc = #doc{
            id = UserName,
            body = {[
                {<<"active">>, Active},
                {<<"email">>, ?l2b(couch_util:get_value("email", Form, ""))},
                {<<"password_sha">>, hash_password(Password, UserSalt)},
                {<<"roles">>, Roles},
                {<<"salt">>, UserSalt},
                {<<"type">>, <<"user">>},
                {<<"username">>, UserName}
            ]}
        },
        {ok, _Rev} = fabric:update_doc(Db, UserDoc, []),
        ?LOG_DEBUG("User ~s (~s) with password, ~s created.", [UserName,
            UserName, Password]),
        send_response(Req);
    _Result -> 
        ?LOG_DEBUG("Can't create ~s: already exists", [UserName]),
         throw({forbidden, <<"User already exists.">>})
    end.

delete_user(#httpd{user_ctx=UserCtx}=Req, Db, UserName) ->
    case get_user(UserName) of
    nil ->
        throw({not_found, <<"User doesn't exist">>});
    User ->
        case lists:member(<<"_admin">>,UserCtx#user_ctx.roles) of
        true ->
            ok;
        false when UserCtx#user_ctx.name == UserName ->
            ok;
        false ->
            throw({forbidden, <<"You aren't allowed to delete the user">>})
        end,
        {Pos,Rev} = couch_doc:parse_rev(couch_util:get_value(<<"_rev">>,User)),
        UserDoc = #doc{
            id = UserName,
            revs = {Pos, [Rev]},
            deleted = true
        },
        {ok, _Rev} = fabric:update_doc(Db, UserDoc, []),
        send_response(Req)
    end.

extract_username(Form) ->
    CouchFormat = couch_util:get_value("name", Form),
    try ?l2b(couch_util:get_value("username", Form, CouchFormat))
    catch error:badarg ->
        throw({bad_request, <<"user accounts must have a username">>})
    end.

extract_password(Form) ->
    try ?l2b(couch_util:get_value("password", Form))
    catch error:badarg ->
        throw({bad_request, <<"user accounts must have a password">>})
    end.

extract_username_password(Form) ->
    CouchFormat = couch_util:get_value("name", Form),
    try
        {?l2b(couch_util:get_value("username", Form, CouchFormat)),
         ?l2b(couch_util:get_value("password", Form))}
    catch error:badarg ->
        Msg = <<"user accounts must have a username and password">>,
        throw({bad_request, Msg})
    end.

generate_cookie_buster() ->
    T0 = calendar:now_to_datetime({0,86400,0}),
    Opts = [{max_age,0}, {path,"/"}, {local_time,T0}],
    mochiweb_cookies:cookie("AuthSession", "", Opts).

generate_cookie(User, Secret, TimeStamp) ->
    SessionData = ?b2l(User) ++ ":" ++ erlang:integer_to_list(TimeStamp, 16),
    Hash = crypto:sha_mac(Secret, SessionData),
    Cookie = couch_util:encodeBase64Url(SessionData ++ ":" ++ ?b2l(Hash)),
    % TODO add {secure, true} to options when SSL is detected
    mochiweb_cookies:cookie("AuthSession", Cookie, [{path, "/"}]).

hash_password(Password, Salt) ->
    ?l2b(couch_util:to_hex(crypto:sha(<<Password/binary, Salt/binary>>))).

parse_form(MochiReq) ->
    case MochiReq:get_primary_header_value("content-type") of
    "application/x-www-form-urlencoded" ++ _ ->
        ReqBody = MochiReq:recv_body(),
        mochiweb_util:parse_qs(ReqBody);
    _ ->
        throw({bad_request, <<"you must specify "
            "application/x-www-form-urlencoded as the primary content-type">>})
    end.

send_response(Req) ->
    send_response(Req, []).

send_response(Req, ExtraHeaders) ->
    {Code, Headers} = case chttpd:qs_value(Req, "next", nil) of
    nil -> {200, []};
    Redirect ->
        {302, [{"Location", chttpd:absolute_uri(Req, Redirect)}]}
    end,
    send_json(Req, Code, Headers ++ ExtraHeaders, {[{ok, true}]}).

timestamp() ->
    {MegaSeconds, Seconds, _} = erlang:now(),
    MegaSeconds * 1000000 + Seconds.

update_user(#httpd{mochi_req=MochiReq, user_ctx=UserCtx}=Req, Db, UserName) ->
    case get_user(UserName) of
    nil ->
        throw({not_found, <<"User doesn't exist">>});
    User ->
        Form = parse_form(MochiReq),
        NewPassword = ?l2b(couch_util:get_value("password", Form, "")),
        OldPassword = ?l2b(couch_util:get_value("old_password", Form, "")),

        UserSalt = couch_util:get_value(<<"salt">>, User),
        CurrentPasswordHash = couch_util:get_value(<<"password_sha">>, User),

        Roles = [?l2b(R) || R <- proplists:get_all_values("roles", Form)],
        if Roles /= [] ->
            chttpd:verify_is_server_admin(Req);
        true -> ok end,

        PasswordHash = case NewPassword of
        <<>> ->
            CurrentPasswordHash;
        _Else ->
            case lists:member(<<"_admin">>,UserCtx#user_ctx.roles) of
            true ->
                hash_password(NewPassword, UserSalt);
            false when UserCtx#user_ctx.name == UserName ->
                %% for user we test old password before allowing change
                case hash_password(OldPassword, UserSalt) of
                CurrentPasswordHash ->
                    hash_password(NewPassword, UserSalt);
                _ ->
                    throw({forbidden, <<"Old password is incorrect.">>})
                end;
            _ ->
                Msg = <<"You aren't allowed to change this password.">>,
                throw({forbidden, Msg})
            end
        end,

        Active = chttpd_view:parse_bool_param(couch_util:get_value("active",
            Form, "true")),
        {Pos,Rev} = couch_doc:parse_rev(couch_util:get_value(<<"_rev">>,User)),
        UserDoc = #doc{
            id = UserName,
            revs = {Pos,[Rev]},
            body = {[
                {<<"active">>, Active},
                {<<"email">>, ?l2b(couch_util:get_value("email", Form, ""))},
                {<<"password_sha">>, PasswordHash},
                {<<"roles">>, Roles},
                {<<"salt">>, UserSalt},
                {<<"type">>, <<"user">>},
                {<<"username">>, UserName}
            ]}
        },
        {ok, _Rev} = fabric:update_doc(Db, UserDoc, []),
        ?LOG_DEBUG("User ~s updated.", [UserName]),
        send_response(Req)
    end.
