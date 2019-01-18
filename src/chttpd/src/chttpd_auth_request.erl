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

-module(chttpd_auth_request).
-export([authorize_request/1]).
-include_lib("couch/include/couch_db.hrl").

authorize_request(#httpd{auth=Auth, user_ctx=Ctx} = Req) ->
    try
	authorize_request_int(Req)
    catch
	throw:{forbidden, Msg} ->
	    case {Auth, Ctx} of
		{{cookie_auth_failed, {Error, Reason}}, _} ->
		    throw({forbidden, {Error, Reason}});
		{_, #user_ctx{name=null}} ->
		    throw({unauthorized, Msg});
		{_, _} ->
		    throw({forbidden, Msg})
	    end
    end.

authorize_request_int(#httpd{path_parts=[]}=Req) ->
    Req;
authorize_request_int(#httpd{path_parts=[<<"favicon.ico">>|_]}=Req) ->
    Req;
authorize_request_int(#httpd{path_parts=[<<"_all_dbs">>|_]}=Req) ->
   case config:get_boolean("chttpd", "admin_only_all_dbs", false) of
       true -> require_admin(Req);
       false -> Req
   end;
authorize_request_int(#httpd{path_parts=[<<"_dbs_info">>|_]}=Req) ->
    Req;
authorize_request_int(#httpd{path_parts=[<<"_replicator">>], method='PUT'}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[<<"_replicator">>], method='DELETE'}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[<<"_replicator">>,<<"_all_docs">>|_]}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[<<"_replicator">>,<<"_changes">>|_]}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[<<"_replicator">>|_]}=Req) ->
    db_authorization_check(Req);
authorize_request_int(#httpd{path_parts=[<<"_users">>], method='PUT'}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[<<"_users">>], method='DELETE'}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[<<"_users">>,<<"_all_docs">>|_]}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[<<"_users">>,<<"_changes">>|_]}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[<<"_users">>|_]}=Req) ->
    db_authorization_check(Req);
authorize_request_int(#httpd{path_parts=[<<"_", _/binary>>|_]}=Req) ->
    server_authorization_check(Req);
authorize_request_int(#httpd{path_parts=[_DbName], method='PUT'}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[_DbName], method='DELETE'}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[_DbName, <<"_compact">>|_]}=Req) ->
    require_db_admin(Req);
authorize_request_int(#httpd{path_parts=[_DbName, <<"_view_cleanup">>]}=Req) ->
    require_db_admin(Req);
authorize_request_int(#httpd{path_parts=[_DbName, <<"_sync_shards">>]}=Req) ->
    require_admin(Req);
authorize_request_int(#httpd{path_parts=[_DbName|_]}=Req) ->
    db_authorization_check(Req).


server_authorization_check(#httpd{path_parts=[<<"_up">>]}=Req) ->
    Req;
server_authorization_check(#httpd{path_parts=[<<"_uuids">>]}=Req) ->
    Req;
server_authorization_check(#httpd{path_parts=[<<"_session">>]}=Req) ->
    Req;
server_authorization_check(#httpd{path_parts=[<<"_replicate">>]}=Req) ->
    Req;
server_authorization_check(#httpd{path_parts=[<<"_stats">>]}=Req) ->
    Req;
server_authorization_check(#httpd{path_parts=[<<"_active_tasks">>]}=Req) ->
    Req;
server_authorization_check(#httpd{path_parts=[<<"_dbs_info">>]}=Req) ->
    Req;
server_authorization_check(#httpd{method=Method, path_parts=[<<"_utils">>|_]}=Req)
  when Method =:= 'HEAD' orelse Method =:= 'GET' ->
    Req;
server_authorization_check(#httpd{path_parts=[<<"_", _/binary>>|_]}=Req) ->
    require_admin(Req).

db_authorization_check(#httpd{path_parts=[DbName|_],user_ctx=Ctx}=Req) ->
    {_} = fabric:get_security(DbName, [{user_ctx, Ctx}]),
    Req.

require_admin(Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Req.

require_db_admin(#httpd{path_parts=[DbName|_],user_ctx=Ctx}=Req) ->
    Sec = fabric:get_security(DbName, [{user_ctx, Ctx}]),

    case is_db_admin(Ctx,Sec) of
        true -> Req;
        false ->  throw({unauthorized, <<"You are not a server or db admin.">>})
    end.

is_db_admin(#user_ctx{name=UserName,roles=UserRoles}, {Security}) ->
    {Admins} = couch_util:get_value(<<"admins">>, Security, {[]}),
    Names = couch_util:get_value(<<"names">>, Admins, []),
    Roles = couch_util:get_value(<<"roles">>, Admins, []),
    case check_security(roles, UserRoles, [<<"_admin">> | Roles]) of
        true -> true;
        false -> check_security(names, UserName, Names)
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
