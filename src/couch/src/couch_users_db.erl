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

-module(couch_users_db).

-export([before_doc_update/3, after_doc_read/2, strip_non_public_fields/1]).

-include_lib("couch/include/couch_db.hrl").

-define(NAME, <<"name">>).
-define(PASSWORD, <<"password">>).
-define(DERIVED_KEY, <<"derived_key">>).
-define(PASSWORD_SCHEME, <<"password_scheme">>).
-define(SIMPLE, <<"simple">>).
-define(PASSWORD_SHA, <<"password_sha">>).
-define(PBKDF2, <<"pbkdf2">>).
-define(PBKDF2_PRF, <<"pbkdf2_prf">>).
-define(PRESERVE_SALT, <<"preserve_salt">>).
-define(ITERATIONS, <<"iterations">>).
-define(SALT, <<"salt">>).
-define(REQUIREMENT_ERROR, "Password does not conform to requirements.").
-define(PASSWORD_SERVER_ERROR, "Server cannot hash passwords at this time.").

% If the request's userCtx identifies an admin
%   -> save_doc (see below)
%
% If the request's userCtx.name is null:
%   -> save_doc
%   // this is an anonymous user registering a new document
%   // in case a user doc with the same id already exists, the anonymous
%   // user will get a regular doc update conflict.
% If the request's userCtx.name doesn't match the doc's name
%   -> 404 // Not Found
% Else
%   -> save_doc
before_doc_update(Doc, Db, _UpdateType) ->
    #user_ctx{name = Name} = couch_db:get_user_ctx(Db),
    DocName = get_doc_name(Doc),
    case (catch couch_db:check_is_admin(Db)) of
        ok ->
            save_doc(Doc);
        _ when Name =:= DocName orelse Name =:= null ->
            save_doc(Doc);
        _ ->
            throw(not_found)
    end.

% If newDoc.password == null || newDoc.password == undefined:
%   ->
%   noop
% Else -> // calculate password hash server side
%    newDoc.password_sha = hash_pw(newDoc.password + salt)
%    newDoc.salt = salt
%    newDoc.password = null
save_doc(#doc{body = {Body}} = Doc) ->
    %% Support both schemes to smooth migration from legacy scheme
    Scheme = chttpd_util:get_chttpd_auth_config("password_scheme", "pbkdf2"),

    % We preserve the salt value if requested (for a hashing upgrade, typically)
    % in order to avoid conflicts if multiple nodes try to upgrade at the same time
    % and to avoid invalidating existing session cookies (since the password did not
    % change).
    PreserveSalt = couch_util:get_value(?PRESERVE_SALT, Body, false),
    Salt =
        case PreserveSalt of
            true ->
                % use existing salt, if present.
                couch_util:get_value(?SALT, Body, couch_uuids:random());
            false ->
                couch_uuids:random()
        end,

    case {couch_util:get_value(?PASSWORD, Body), Scheme} of
        % server admins don't have a user-db password entry
        {null, _} ->
            Doc;
        {undefined, _} ->
            Doc;
        % deprecated
        {ClearPassword, "simple"} ->
            ok = validate_password(ClearPassword),
            PasswordSha = couch_passwords:simple(ClearPassword, Salt),
            Body0 = remove_password_fields(Body),
            Body1 = [
                {?PASSWORD_SCHEME, ?SIMPLE},
                {?SALT, Salt},
                {?PASSWORD_SHA, PasswordSha}
                | Body0
            ],
            Doc#doc{body = {Body1}};
        {ClearPassword, "pbkdf2"} ->
            ok = validate_password(ClearPassword),
            PRF = chttpd_util:get_chttpd_auth_config("pbkdf2_prf", "sha256"),
            Iterations = chttpd_util:get_chttpd_auth_config_integer(
                "iterations", 50000
            ),
            DerivedKey = couch_passwords:pbkdf2(
                list_to_existing_atom(PRF), ClearPassword, Salt, Iterations
            ),
            Body0 = remove_password_fields(Body),
            Body1 = [
                {?PASSWORD_SCHEME, ?PBKDF2},
                {?PBKDF2_PRF, ?l2b(PRF)},
                {?SALT, Salt},
                {?ITERATIONS, Iterations},
                {?DERIVED_KEY, DerivedKey}
                | Body0
            ],
            Doc#doc{body = {Body1}};
        {_ClearPassword, Scheme} ->
            couch_log:error("[couch_httpd_auth] password_scheme value of '~p' is invalid.", [Scheme]),
            throw({forbidden, ?PASSWORD_SERVER_ERROR})
    end.

remove_password_fields(Props) ->
    lists:filter(fun not_password_field/1, Props).

not_password_field({Key, _Value}) ->
    not lists:member(Key, [
        ?DERIVED_KEY,
        ?ITERATIONS,
        ?PASSWORD,
        ?PASSWORD_SCHEME,
        ?PASSWORD_SHA,
        ?PRESERVE_SALT,
        ?PBKDF2_PRF,
        ?SALT
    ]).

% Validate if a new password matches all RegExp in the password_regexp setting.
% Throws if not.
% In this function the [couch_httpd_auth] password_regexp config is parsed.
validate_password(ClearPassword) ->
    case chttpd_util:get_chttpd_auth_config("password_regexp", "") of
        "" ->
            ok;
        "[]" ->
            ok;
        ValidateConfig ->
            RequirementList =
                case couch_util:parse_term(ValidateConfig) of
                    {ok, RegExpList} when is_list(RegExpList) ->
                        RegExpList;
                    {ok, NonListValue} ->
                        couch_log:error(
                            "[couch_httpd_auth] password_regexp value of '~p'"
                            " is not a list.",
                            [NonListValue]
                        ),
                        throw({forbidden, ?PASSWORD_SERVER_ERROR});
                    {error, ErrorInfo} ->
                        couch_log:error(
                            "[couch_httpd_auth] password_regexp value of '~p'"
                            " could not get parsed. ~p",
                            [ValidateConfig, ErrorInfo]
                        ),
                        throw({forbidden, ?PASSWORD_SERVER_ERROR})
                end,
            % Check the password on every RegExp.
            lists:foreach(
                fun(RegExpTuple) ->
                    case get_password_regexp_and_error_msg(RegExpTuple) of
                        {ok, RegExp, PasswordErrorMsg} ->
                            check_password(ClearPassword, RegExp, PasswordErrorMsg);
                        {error} ->
                            couch_log:error(
                                "[couch_httpd_auth] password_regexp part of '~p' "
                                "is not a RegExp string or "
                                "a RegExp and Reason tuple.",
                                [RegExpTuple]
                            ),
                            throw({forbidden, ?PASSWORD_SERVER_ERROR})
                    end
                end,
                RequirementList
            ),
            ok
    end.

% Get the RegExp out of the tuple and combine the the error message.
% First is with a Reason string.
get_password_regexp_and_error_msg({RegExp, Reason}) when
    is_list(RegExp) andalso is_list(Reason) andalso
        length(Reason) > 0
->
    {ok, RegExp, lists:concat([?REQUIREMENT_ERROR, " ", Reason])};
% With a not correct Reason string.
get_password_regexp_and_error_msg({RegExp, _Reason}) when is_list(RegExp) ->
    {ok, RegExp, ?REQUIREMENT_ERROR};
% Without a Reason string.
get_password_regexp_and_error_msg({RegExp}) when is_list(RegExp) ->
    {ok, RegExp, ?REQUIREMENT_ERROR};
% If the RegExp is only a list/string.
get_password_regexp_and_error_msg(RegExp) when is_list(RegExp) ->
    {ok, RegExp, ?REQUIREMENT_ERROR};
% Not correct RegExpValue.
get_password_regexp_and_error_msg(_) ->
    {error}.

% Check the password if it matches a RegExp.
check_password(Password, RegExp, ErrorMsg) ->
    case re:run(Password, RegExp, [{capture, none}]) of
        match ->
            ok;
        _ ->
            throw({bad_request, ErrorMsg})
    end.

% If the doc is a design doc
%   If the request's userCtx identifies an admin
%     -> return doc
%   Else
%     -> 403 // Forbidden
% If the request's userCtx identifies an admin
%   -> return doc
% If the request's userCtx.name doesn't match the doc's name
%   -> 404 // Not Found
% Else
%   -> return doc
after_doc_read(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, Db) ->
    case (catch couch_db:check_is_admin(Db)) of
        ok ->
            Doc;
        _ ->
            throw(
                {forbidden, <<"Only administrators can view design docs in the users database.">>}
            )
    end;
after_doc_read(Doc, Db) ->
    #user_ctx{name = Name} = couch_db:get_user_ctx(Db),
    DocName = get_doc_name(Doc),
    case (catch couch_db:check_is_admin(Db)) of
        ok ->
            Doc;
        _ when Name =:= DocName ->
            Doc;
        _ ->
            Doc1 = strip_non_public_fields(Doc),
            case Doc1 of
                #doc{body = {[]}} ->
                    throw(not_found);
                _ ->
                    Doc1
            end
    end.

get_doc_name(#doc{id = <<"org.couchdb.user:", Name/binary>>}) ->
    Name;
get_doc_name(_) ->
    undefined.

strip_non_public_fields(#doc{body = {Props}} = Doc) ->
    Public = re:split(
        chttpd_util:get_chttpd_auth_config("public_fields", ""),
        "\\s*,\\s*",
        [{return, binary}]
    ),
    Doc#doc{body = {[{K, V} || {K, V} <- Props, lists:member(K, Public)]}}.
