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

-export([before_doc_update/2, after_doc_read/2]).

-include("couch_db.hrl").

-define(NAME, <<"name">>).
-define(PASSWORD, <<"password">>).
-define(PASSWORD_SHA, <<"password_sha">>).
-define(SALT, <<"salt">>).
-define(replace(L, K, V), lists:keystore(K, 1, L, {K, V})).

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
before_doc_update(Doc, #db{user_ctx = UserCtx} = Db) ->
    #user_ctx{name=Name} = UserCtx,
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
save_doc(#doc{body={Body}} = Doc) ->
    case couch_util:get_value(?PASSWORD, Body) of
    null -> % server admins don't have a user-db password entry
        Doc;
    undefined ->
        Doc;
    ClearPassword ->
        Salt = ?b2l(couch_uuids:random()),
        PasswordSha = couch_util:to_hex(crypto:sha(?b2l(ClearPassword) ++ Salt)),
        Body1 = ?replace(Body, ?PASSWORD_SHA, ?l2b(PasswordSha)),
        Body2 = ?replace(Body1, ?SALT, ?l2b(Salt)),
        Body3 = proplists:delete(?PASSWORD, Body2),
        Doc#doc{body={Body3}}
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
        throw({forbidden,
        <<"Only administrators can view design docs in the users database.">>})
    end;
after_doc_read(Doc, #db{user_ctx = UserCtx} = Db) ->
    #user_ctx{name=Name} = UserCtx,
    DocName = get_doc_name(Doc),
    case (catch couch_db:check_is_admin(Db)) of
    ok ->
        Doc;
    _ when Name =:= DocName ->
        Doc;
    _ ->
        throw(not_found)
    end.

get_doc_name(#doc{body={Body}}) ->
    couch_util:get_value(?NAME, Body).
