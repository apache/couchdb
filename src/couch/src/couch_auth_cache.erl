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
    get_admin/1,
    add_roles/2,
    auth_design_doc/1
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
    get_admin(UserName).

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

auth_design_doc(DocId) ->
    DocProps = [
        {<<"_id">>, DocId},
        {<<"language">>, <<"javascript">>},
        {<<"validate_doc_update">>, ?AUTH_DB_DOC_VALIDATE_FUNCTION}
    ],
    {ok, couch_doc:from_json_obj({DocProps})}.
