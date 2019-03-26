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

-module(fabric_security).


-export([
    check_is_admin/2,
    check_is_member/2,

    is_admin/2,
    is_member/2
]).


-include_lib("couch/include/couch_db.hrl").


check_is_admin(DbName, UserCtx) ->
    case is_admin(DbName, UserCtx) of
        true ->
            ok;
        false ->
            Reason = <<"You are not a db or server admin.">>,
            throw_security_error(UserCtx, Reason)
    end.


check_is_member(DbName, UserCtx) ->
    case is_member(DbName, UserCtx) of
        true ->
            ok;
        false ->
            throw_security_error(UserCtx)
    end.


is_admin(DbName, UserCtx) when is_binary(DbName) ->
    {SecProps} = fabric2:get_security(DbName),
    is_admin(SecProps, UserCtx);

is_admin(SecProps, UserCtx) ->
    % Need to re-consider couch_db_plugin:check_is_admin/1
    {Admins} = get_admins(SecProps),
    is_authorized(Admins, UserCtx).


is_member(DbName, UserCtx) when is_binary(DbName) ->
    {SecProps} = fabric2:get_security(DbName),
    is_member(SecProps, UserCtx);

is_member(SecProps, UserCtx) ->
    case is_admin(SecProps, UserCtx) of
        true ->
            true;
        false ->
            case is_public_db(SecProps) of
                true ->
                    true;
                false ->
                    {Members} = get_members(SecProps),
                    is_authorized(Members, UserCtx)
            end
    end.


is_authorized(Group, UserCtx) ->
    #user_ctx{
        name = UserName,
        roles = UserRoles
    } = UserCtx,
    Names = couch_util:get_value(<<"names">>, Group, []),
    Roles = couch_util:get_value(<<"roles">>, Group, []),
    case check_security(roles, UserRoles, [<<"_admin">> | Roles]) of
        true ->
            true;
        false ->
            check_security(names, UserName, Names)
    end.


check_security(roles, [], _) ->
    false;
check_security(roles, UserRoles, Roles) ->
    UserRolesSet = ordsets:from_list(UserRoles),
    RolesSet = ordsets:from_list(Roles),
    not ordsets:is_disjoint(UserRolesSet, RolesSet);
check_security(names, _, []) ->
    false;
check_security(names, null, _) ->
    false;
check_security(names, UserName, Names) ->
    lists:member(UserName, Names).


throw_security_error(#user_ctx{name=null}=UserCtx) ->
    Reason = <<"You are not authorized to access this db.">>,
    throw_security_error(UserCtx, Reason);
throw_security_error(#user_ctx{name=_}=UserCtx) ->
    Reason = <<"You are not allowed to access this db.">>,
    throw_security_error(UserCtx, Reason).


throw_security_error(#user_ctx{}=UserCtx, Reason) ->
    Error = security_error_type(UserCtx),
    throw({Error, Reason}).


security_error_type(#user_ctx{name=null}) ->
    unauthorized;
security_error_type(#user_ctx{name=_}) ->
    forbidden.


is_public_db(SecProps) ->
    {Members} = get_members(SecProps),
    Names = couch_util:get_value(<<"names">>, Members, []),
    Roles = couch_util:get_value(<<"roles">>, Members, []),
    Names =:= [] andalso Roles =:= [].


get_admins(SecProps) ->
    couch_util:get_value(<<"admins">>, SecProps, {[]}).


get_members(SecProps) ->
    % we fallback to readers here for backwards compatibility
    case couch_util:get_value(<<"members">>, SecProps) of
        undefined ->
            couch_util:get_value(<<"readers">>, SecProps, {[]});
        Members ->
            Members
    end.
