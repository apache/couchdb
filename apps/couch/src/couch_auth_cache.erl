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

-module(couch_auth_cache).

-export([
    get_user_creds/1,
    get_user_creds/2,
    update_user_creds/3,
    get_admin/1,
    add_roles/2,
    auth_design_doc/1,
    ensure_users_db_exists/0
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_js_functions.hrl").

-spec get_user_creds(UserName :: string() | binary()) ->
    {ok, Credentials :: list(), term()} | nil.

get_user_creds(UserName) ->
    get_user_creds(nil, UserName).

-spec get_user_creds(Req :: #httpd{} | nil, UserName :: string() | binary()) ->
    {ok, Credentials :: list(), term()} | nil.

get_user_creds(Req, UserName) when is_list(UserName) ->
    get_user_creds(Req, ?l2b(UserName));
get_user_creds(_Req, UserName) ->
    UserCreds =
        case get_admin(UserName) of
            nil ->
                get_from_db(UserName);
            Props ->
                case get_from_db(UserName) of
                    nil ->
                        Props;
                    UserProps when is_list(UserProps) ->
                        add_roles(Props, couch_util:get_value(<<"roles">>, UserProps))
                end
        end,
    validate_user_creds(UserCreds).

update_user_creds(_Req, UserDoc, _AuthCtx) ->
    ok = ensure_users_db_exists(),
    couch_util:with_db(users_db(), fun(UserDb) ->
        {ok, _NewRev} = couch_db:update_doc(UserDb, UserDoc, []),
        ok
    end).

add_roles(Props, ExtraRoles) ->
    CurrentRoles = couch_util:get_value(<<"roles">>, Props),
    lists:keyreplace(<<"roles">>, 1, Props, {<<"roles">>, CurrentRoles ++ ExtraRoles}).

get_admin(UserName) when is_binary(UserName) ->
    get_admin(?b2l(UserName));
get_admin(UserName) when is_list(UserName) ->
    case config:get("admins", UserName) of
        "-hashed-" ++ HashedPwdAndSalt ->
            % the name is an admin, now check to see if there is a user doc
            % which has a matching name, salt, and password_sha
            [HashedPwd, Salt] = string:tokens(HashedPwdAndSalt, ","),
            make_admin_doc(HashedPwd, Salt);
        "-pbkdf2-" ++ HashedPwdSaltAndIterations ->
            [HashedPwd, Salt, Iterations] = string:tokens(HashedPwdSaltAndIterations, ","),
            make_admin_doc(HashedPwd, Salt, Iterations);
        _Else ->
            nil
    end.

make_admin_doc(HashedPwd, Salt) ->
    [
        {<<"roles">>, [<<"_admin">>]},
        {<<"salt">>, ?l2b(Salt)},
        {<<"password_scheme">>, <<"simple">>},
        {<<"password_sha">>, ?l2b(HashedPwd)}
    ].

make_admin_doc(DerivedKey, Salt, Iterations) ->
    [
        {<<"roles">>, [<<"_admin">>]},
        {<<"salt">>, ?l2b(Salt)},
        {<<"iterations">>, list_to_integer(Iterations)},
        {<<"password_scheme">>, <<"pbkdf2">>},
        {<<"derived_key">>, ?l2b(DerivedKey)}
    ].

get_from_db(UserName) ->
    ok = ensure_users_db_exists(),
    couch_util:with_db(users_db(), fun(Db) ->
        DocId = <<"org.couchdb.user:", UserName/binary>>,
        try
            {ok, Doc} = couch_db:open_doc(Db, DocId, [conflicts]),
            {DocProps} = couch_doc:to_json_obj(Doc, []),
            DocProps
        catch
            _:_Error ->
                nil
        end
    end).

validate_user_creds(nil) ->
    nil;
validate_user_creds(UserCreds) ->
    case couch_util:get_value(<<"_conflicts">>, UserCreds) of
        undefined ->
            ok;
        _ConflictList ->
            throw(
                {unauthorized,
                    <<"User document conflicts must be resolved before the document",
                        " is used for authentication purposes.">>}
            )
    end,
    {ok, UserCreds, nil}.

users_db() ->
    DbNameList = config:get("couch_httpd_auth", "authentication_db", "_users"),
    ?l2b(DbNameList).

ensure_users_db_exists() ->
    Options = [?ADMIN_CTX, nologifmissing],
    case couch_db:open(users_db(), Options) of
        {ok, Db} ->
            ensure_auth_ddoc_exists(Db, <<"_design/_auth">>),
            couch_db:close(Db);
        _Error ->
            {ok, Db} = couch_db:create(users_db(), Options),
            ok = ensure_auth_ddoc_exists(Db, <<"_design/_auth">>),
            couch_db:close(Db)
    end,
    ok.

ensure_auth_ddoc_exists(Db, DDocId) ->
    case couch_db:open_doc(Db, DDocId) of
        {not_found, _Reason} ->
            {ok, AuthDesign} = auth_design_doc(DDocId),
            {ok, _Rev} = couch_db:update_doc(Db, AuthDesign, []);
        {ok, Doc} ->
            {Props} = couch_doc:to_json_obj(Doc, []),
            case couch_util:get_value(<<"validate_doc_update">>, Props, []) of
                ?AUTH_DB_DOC_VALIDATE_FUNCTION ->
                    ok;
                _ ->
                    Props1 = lists:keyreplace(
                        <<"validate_doc_update">>,
                        1,
                        Props,
                        {<<"validate_doc_update">>, ?AUTH_DB_DOC_VALIDATE_FUNCTION}
                    ),
                    couch_db:update_doc(Db, couch_doc:from_json_obj({Props1}), [])
            end
    end,
    ok.

auth_design_doc(DocId) ->
    DocProps = [
        {<<"_id">>, DocId},
        {<<"language">>, <<"javascript">>},
        {<<"validate_doc_update">>, ?AUTH_DB_DOC_VALIDATE_FUNCTION}
    ],
    {ok, couch_doc:from_json_obj({DocProps})}.
