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
-include("couch_db.hrl").

-export([default_authentication_handler/1,special_test_authentication_handler/1]).
-export([cookie_authentication_handler/1]).
-export([null_authentication_handler/1]).
-export([cookie_auth_header/2]).
-export([handle_session_req/1]).
-export([handle_user_req/1]).
-export([ensure_users_db_exists/1, get_user/1]).

-import(couch_httpd, [header_value/2, send_json/2,send_json/4, send_method_not_allowed/2]).

special_test_authentication_handler(Req) ->
    case header_value(Req, "WWW-Authenticate") of
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

basic_username_pw(Req) ->
    AuthorizationHeader = header_value(Req, "Authorization"),
    case AuthorizationHeader of
    "Basic " ++ Base64Value ->
        case string:tokens(?b2l(couch_util:decodeBase64(Base64Value)),":") of
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

default_authentication_handler(Req) ->
    case basic_username_pw(Req) of
    {User, Pass} ->
        case couch_server:is_admin(User, Pass) of
        true ->
            Req#httpd{user_ctx=#user_ctx{name=?l2b(User), roles=[<<"_admin">>]}};
        false ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
        end;
    nil ->
        case couch_server:has_admins() of
        true ->
            Req;
        false ->
            case couch_config:get("couch_httpd_auth", "require_valid_user", "false") of
                "true" -> Req;
                % If no admins, and no user required, then everyone is admin!
                % Yay, admin party!
                _ -> Req#httpd{user_ctx=#user_ctx{roles=[<<"_admin">>]}}
            end
        end
    end.

null_authentication_handler(Req) ->
    Req#httpd{user_ctx=#user_ctx{roles=[<<"_admin">>]}}.

% Cookie auth handler using per-node user db
cookie_authentication_handler(Req) ->
    case cookie_auth_user(Req) of
    % Fall back to default authentication handler
    nil -> default_authentication_handler(Req);
    Req2 -> Req2
    end.

% Cookie auth handler using per-db user db
% cookie_authentication_handler(#httpd{path_parts=Path}=Req) ->
%     case Path of
%     [DbName|_] ->
%         case cookie_auth_user(Req, DbName) of
%         nil -> default_authentication_handler(Req);
%         Req2 -> Req2
%         end;
%     _Else ->
%         % Fall back to default authentication handler
%         default_authentication_handler(Req)
%     end.

% maybe we can use hovercraft to simplify running this view query
get_user(UserName) ->
    % In the future this will be pluggable. For now we check the .ini first,
    % then fall back to querying the db.
    case couch_config:get("admins", ?b2l(UserName)) of
    "-hashed-" ++ HashedPwdAndSalt ->
        [HashedPwd, Salt] = string:tokens(HashedPwdAndSalt, ","),
        [{<<"roles">>, [<<"_admin">>]},
          {<<"salt">>, ?l2b(Salt)},
          {<<"password_sha">>, ?l2b(HashedPwd)}];
    _ ->
        DesignId = <<"_design/_auth">>,
        ViewName = <<"users">>,
        % if the design doc or the view doesn't exist, then make it
        DbName = couch_config:get("couch_httpd_auth", "authentication_db"),
        {ok, Db} = ensure_users_db_exists(?l2b(DbName)),

        ensure_users_view_exists(Db, DesignId, ViewName),

        case (catch couch_view:get_map_view(Db, DesignId, ViewName, nil)) of
        {ok, View, _Group} ->
            FoldFun = fun({_, Value}, _, {_}) -> {stop, Value} end,
            {ok, _, {Result}} = couch_view:fold(View, FoldFun, {nil},
                    [{start_key, {UserName, ?MIN_STR}},{end_key, {UserName, ?MAX_STR}}]),
            Result;
        {not_found, _Reason} ->
            nil
            % case (catch couch_view:get_reduce_view(Db, DesignId, ViewName, nil)) of
            % {ok, _ReduceView, _Group} ->
            %     not_implemented;
            % {not_found, _Reason} ->
            %     nil
            % end
        end
    end.
    
ensure_users_db_exists(DbName) ->
    case couch_db:open(DbName, [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}]) of
    {ok, Db} ->
        {ok, Db};
    _Error -> 
        ?LOG_ERROR("Create the db ~p", [DbName]),
        {ok, Db} = couch_db:create(DbName, [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}]),
        ?LOG_ERROR("Created the db ~p", [DbName]),
        {ok, Db}
    end.
    
ensure_users_view_exists(Db, DDocId, VName) -> 
    try couch_httpd_db:couch_doc_open(Db, DDocId, nil, []) of
        _Foo -> ok
    catch 
        _:Error -> 
            ?LOG_ERROR("create the design document ~p : ~p", [DDocId, Error]),
            % create the design document
            {ok, AuthDesign} = auth_design_doc(DDocId, VName),
            {ok, _Rev} = couch_db:update_doc(Db, AuthDesign, []),
            ?LOG_ERROR("created the design document", []),
            ok
    end.

auth_design_doc(DocId, VName) ->
    DocProps = [
        {<<"_id">>, DocId},
        {<<"language">>,<<"javascript">>},
        {<<"views">>,
            {[{VName,
                {[{<<"map">>,
                    <<"function (doc) {\n if (doc.type == \"user\") {\n        emit(doc.username, doc);\n}\n}">>
                }]}
            }]}
        }],
    {ok, couch_doc:from_json_obj({DocProps})}.
    

user_doc(DocId, Username, UserSalt, PasswordHash, Email, Active, Roles) ->
    user_doc(DocId, Username, UserSalt, PasswordHash, Email, Active, Roles, nil).
user_doc(DocId, Username, UserSalt, PasswordHash, Email, Active, Roles, Rev) ->
    DocProps = [
        {<<"_id">>, DocId},
        {<<"type">>, <<"user">>},
        {<<"username">>, Username},
        {<<"password_sha">>, PasswordHash},
        {<<"salt">>, UserSalt},
        {<<"email">>, Email},
        {<<"active">>, Active},
        {<<"roles">>, Roles}],
    DocProps1 = case Rev of
    nil -> DocProps;
    _Rev -> 
        [{<<"_rev">>, Rev}] ++ DocProps
    end,
    {ok, couch_doc:from_json_obj({DocProps1})}.

cookie_auth_user(#httpd{mochi_req=MochiReq}=Req) ->
    case MochiReq:get_cookie_value("AuthSession") of
    undefined -> nil;
    [] -> nil;
    Cookie -> 
        AuthSession = couch_util:decodeBase64Url(Cookie),
        [User, TimeStr | HashParts] = string:tokens(?b2l(AuthSession), ":"),
        % Verify expiry and hash
        {NowMS, NowS, _} = erlang:now(),
        CurrentTime = NowMS * 1000000 + NowS,
        case couch_config:get("couch_httpd_auth", "secret", nil) of
        nil -> nil;
        SecretStr ->
            Secret = ?l2b(SecretStr),
            case get_user(?l2b(User)) of
            nil -> nil;
            Result ->
                UserSalt = proplists:get_value(<<"salt">>, Result, <<"">>),
                FullSecret = <<Secret/binary, UserSalt/binary>>,
                ExpectedHash = crypto:sha_mac(FullSecret, User ++ ":" ++ TimeStr),
                Hash = ?l2b(string:join(HashParts, ":")),
                Timeout = to_int(couch_config:get("couch_httpd_auth", "timeout", 600)),
                ?LOG_DEBUG("timeout ~p", [Timeout]),
                case (catch erlang:list_to_integer(TimeStr, 16)) of
                    TimeStamp when CurrentTime < TimeStamp + Timeout
                    andalso ExpectedHash == Hash ->
                        TimeLeft = TimeStamp + Timeout - CurrentTime,
                        ?LOG_DEBUG("Successful cookie auth as: ~p", [User]),
                        Req#httpd{user_ctx=#user_ctx{
                            name=?l2b(User),
                            roles=proplists:get_value(<<"roles">>, Result, [])
                        }, auth={FullSecret, TimeLeft < Timeout*0.9}};
                    _Else ->
                        nil
                end
            end
        end
    end.

cookie_auth_header(#httpd{user_ctx=#user_ctx{name=null}}, _Headers) -> [];
cookie_auth_header(#httpd{user_ctx=#user_ctx{name=User}, auth={Secret, true}}, Headers) ->
    % Note: we only set the AuthSession cookie if:
    %  * a valid AuthSession cookie has been received
    %  * we are outside a 10% timeout window
    %  * and if an AuthSession cookie hasn't already been set e.g. by a login
    %    or logout handler.
    % The login and logout handlers need to set the AuthSession cookie
    % themselves.
    CookieHeader = proplists:get_value("Set-Cookie", Headers, ""),
    Cookies = mochiweb_cookies:parse_cookie(CookieHeader),
    AuthSession = proplists:get_value("AuthSession", Cookies),
    if AuthSession == undefined ->
        {NowMS, NowS, _} = erlang:now(),
        TimeStamp = NowMS * 1000000 + NowS,
        [cookie_auth_cookie(?b2l(User), Secret, TimeStamp)];
    true ->
        []
    end;
cookie_auth_header(_Req, _Headers) -> [].

cookie_auth_cookie(User, Secret, TimeStamp) ->
    SessionData = User ++ ":" ++ erlang:integer_to_list(TimeStamp, 16),
    Hash = crypto:sha_mac(Secret, SessionData),
    mochiweb_cookies:cookie("AuthSession",
        couch_util:encodeBase64Url(SessionData ++ ":" ++ ?b2l(Hash)),
        [{path, "/"}, {http_only, true}]). % TODO add {secure, true} when SSL is detected

hash_password(Password, Salt) ->
    ?l2b(couch_util:to_hex(crypto:sha(<<Password/binary, Salt/binary>>))).

% Login handler with user db
handle_login_req(#httpd{method='POST', mochi_req=MochiReq}=Req) ->
    ReqBody = MochiReq:recv_body(),
    Form = case MochiReq:get_primary_header_value("content-type") of
        "application/x-www-form-urlencoded" ++ _ ->
            mochiweb_util:parse_qs(ReqBody);
        _ ->
            []
    end,
    UserName = ?l2b(proplists:get_value("username", Form, "")),
    Password = ?l2b(proplists:get_value("password", Form, "")),
    User = case get_user(UserName) of
        nil -> [];
        Result -> Result
    end,
    UserSalt = proplists:get_value(<<"salt">>, User, <<>>),
    PasswordHash = hash_password(Password, UserSalt),
    case proplists:get_value(<<"password_sha">>, User, nil) of
        ExpectedHash when ExpectedHash == PasswordHash ->
            Secret = ?l2b(couch_config:get("couch_httpd_auth", "secret", nil)),
            {NowMS, NowS, _} = erlang:now(),
            CurrentTime = NowMS * 1000000 + NowS,
            Cookie = cookie_auth_cookie(?b2l(UserName), <<Secret/binary, UserSalt/binary>>, CurrentTime),
            {Code, Headers} = case couch_httpd:qs_value(Req, "next", nil) of
                nil ->
                    {200, [Cookie]};
                Redirect ->
                    {302, [Cookie, {"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
            end,
            send_json(Req#httpd{req_body=ReqBody}, Code, Headers,
                {[{ok, true}]});
        _Else ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
    end.

% Session Handler

handle_session_req(#httpd{method='POST'}=Req) ->
    handle_login_req(Req);
handle_session_req(#httpd{method='GET', user_ctx=UserCtx}=Req) ->
    % whoami
    Name = UserCtx#user_ctx.name,
    Roles = UserCtx#user_ctx.roles,
    ForceLogin = couch_httpd:qs_value(Req, "basic", "false"),
    case {Name, ForceLogin} of
        {null, "true"} ->
            throw({unauthorized, <<"Please login.">>});
        _False -> ok
    end,
    send_json(Req, {[
        {ok, true},
        {name, Name},
        {roles, Roles}
    ]});
handle_session_req(#httpd{method='DELETE'}=Req) ->
    % logout
    Cookie = mochiweb_cookies:cookie("AuthSession", "", [{path, "/"}, {http_only, true}]),
    {Code, Headers} = case couch_httpd:qs_value(Req, "next", nil) of
        nil ->
            {200, [Cookie]};
        Redirect ->
            {302, [Cookie, {"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
    end,
    send_json(Req, Code, Headers, {[{ok, true}]});
handle_session_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD,POST,DELETE").
    
create_user_req(#httpd{method='POST', mochi_req=MochiReq}=Req, Db) ->
    ReqBody = MochiReq:recv_body(),
    Form = case MochiReq:get_primary_header_value("content-type") of
        "application/x-www-form-urlencoded" ++ _ ->
            ?LOG_INFO("body parsed ~p", [mochiweb_util:parse_qs(ReqBody)]),
            mochiweb_util:parse_qs(ReqBody);
        _ ->
            []
    end,
    Roles = proplists:get_all_values("roles", Form),
    UserName = ?l2b(proplists:get_value("username", Form, "")),
    Password = ?l2b(proplists:get_value("password", Form, "")),
    Email = ?l2b(proplists:get_value("email", Form, "")),
    Active = couch_httpd_view:parse_bool_param(proplists:get_value("active", Form, "true")),
    case get_user(UserName) of
    nil -> 
        Roles1 = case Roles of
        [] -> Roles;
        _ ->
            ok = couch_httpd:verify_is_server_admin(Req),
            [?l2b(R) || R <- Roles]
        end,
            
        UserSalt = couch_uuids:random(),
        PasswordHash = hash_password(Password, UserSalt),
        DocId = couch_uuids:random(),
        {ok, UserDoc} = user_doc(DocId, UserName, UserSalt, PasswordHash, Email, Active, Roles1),
        {ok, _Rev} = couch_db:update_doc(Db, UserDoc, []),
        ?LOG_DEBUG("User ~s (~s) with password, ~s created.", [?b2l(UserName), ?b2l(DocId), ?b2l(Password)]),
        {Code, Headers} = case couch_httpd:qs_value(Req, "next", nil) of
            nil ->
                {200, []};
            Redirect ->
                {302, [{"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
        end,
        send_json(Req, Code, Headers, {[{ok, true}]});
    _Result -> 
        ?LOG_DEBUG("Can't create ~s: already exists", [?b2l(UserName)]),
         throw({forbidden, <<"User already exists.">>})
    end.

update_user_req(#httpd{method='PUT', mochi_req=MochiReq, user_ctx=UserCtx}=Req, Db, UserName) ->
    Name = UserCtx#user_ctx.name,
    UserRoles = UserCtx#user_ctx.roles,
    case User = get_user(UserName) of
    nil ->
        throw({not_found, <<"User doesn't exist">>});
    _Result ->
        ReqBody = MochiReq:recv_body(),
        Form = case MochiReq:get_primary_header_value("content-type") of
            "application/x-www-form-urlencoded" ++ _ ->
                mochiweb_util:parse_qs(ReqBody);
            _ ->
                []
        end,
        Roles = proplists:get_all_values("roles", Form),
        Password = ?l2b(proplists:get_value("password", Form, "")),
        Email = ?l2b(proplists:get_value("email", Form, "")),
        Active = couch_httpd_view:parse_bool_param(proplists:get_value("active", Form, "true")),
        OldPassword = proplists:get_value("old_password", Form, ""),
        OldPassword1 = ?l2b(OldPassword),
        UserSalt = proplists:get_value(<<"salt">>, User, <<>>),
        OldRev = proplists:get_value(<<"_rev">>, User, <<>>),
        DocId = proplists:get_value(<<"_id">>, User, <<>>),
        CurrentPasswordHash = proplists:get_value(<<"password_sha">>, User, nil),
        
        
        Roles1 = case Roles of
        [] -> Roles;
        _ ->
            ok = couch_httpd:verify_is_server_admin(Req),
            [?l2b(R) || R <- Roles]
        end,
        
        PasswordHash = case lists:member(<<"_admin">>, UserRoles) of
        true ->
            case Password of
                <<>> -> CurrentPasswordHash;
                _Else ->
                    hash_password(Password, UserSalt)
	    end;
        false when Name =:= UserName ->
            %% for user we test old password before allowing change
            case Password of
                <<>> -> 
                    CurrentPasswordHash;
                _P when OldPassword =:= [] ->
                    throw({forbidden, <<"Old password is incorrect.">>});
                _Else ->
                    OldPasswordHash = hash_password(OldPassword1, UserSalt),
                    ?LOG_DEBUG("~p == ~p", [CurrentPasswordHash, OldPasswordHash]),
                    case CurrentPasswordHash of
                        ExpectedHash when ExpectedHash =:= OldPasswordHash ->
                            hash_password(Password, UserSalt);
                        _ ->
                            throw({forbidden, <<"Old password is incorrect.">>})
		    end
                end;
        _ ->
            throw({forbidden, <<"You aren't allowed to change this password.">>})
        end, 
        {ok, UserDoc} = user_doc(DocId, UserName, UserSalt, PasswordHash, Email, Active, Roles1, OldRev),
        {ok, _Rev} = couch_db:update_doc(Db, UserDoc, []),
        ?LOG_DEBUG("User ~s (~s)updated.", [?b2l(UserName), ?b2l(DocId)]),
        {Code, Headers} = case couch_httpd:qs_value(Req, "next", nil) of
        nil -> {200, []};
        Redirect ->
            {302, [{"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
        end,
        send_json(Req, Code, Headers, {[{ok, true}]})
    end.

handle_user_req(#httpd{method='POST'}=Req) ->
    DbName = couch_config:get("couch_httpd_auth", "authentication_db"),
    ensure_users_db_exists(?l2b(DbName)),
    {ok, Db} = couch_db:open(?l2b(DbName),
			     [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}]),
    create_user_req(Req, Db);
handle_user_req(#httpd{method='PUT', path_parts=[_]}=_Req) ->
    throw({bad_request, <<"Username is missing">>});
handle_user_req(#httpd{method='PUT', path_parts=[_, UserName]}=Req) ->
    DbName = couch_config:get("couch_httpd_auth", "authentication_db"),
    ensure_users_db_exists(?l2b(DbName)),
    {ok, Db} = couch_db:open(?l2b(DbName),
			     [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}]),
    update_user_req(Req, Db, UserName);
handle_user_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "POST,PUT").

to_int(Value) when is_binary(Value) ->
    to_int(?b2l(Value)); 
to_int(Value) when is_list(Value) ->
    list_to_integer(Value);
to_int(Value) when is_integer(Value) ->
    Value.

% % Login handler
% handle_login_req(#httpd{method='POST'}=Req) ->
%     DbName = couch_config:get("couch_httpd_auth", "authentication_db"),
%     case couch_db:open(?l2b(DbName), [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}]) of
%         {ok, Db} -> handle_login_req(Req, Db)
%     end;
% handle_login_req(Req) ->
%     send_method_not_allowed(Req, "POST").
% 
% % Logout handler
% handle_logout_req(#httpd{method='POST'}=Req) ->
%     Cookie = mochiweb_cookies:cookie("AuthSession", "", [{path, "/"}, {http_only, true}]),
%     {Code, Headers} = case couch_httpd:qs_value(Req, "next", nil) of
%         nil ->
%             {200, [Cookie]};
%         Redirect ->
%             {302, [Cookie, {"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
%     end,
%     send_json(Req, Code, Headers, {[{ok, true}]});
% handle_logout_req(Req) ->
%     send_method_not_allowed(Req, "POST").
