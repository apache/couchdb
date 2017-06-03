% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(setup_httpd).
-include_lib("couch/include/couch_db.hrl").

-export([handle_setup_req/1]).

handle_setup_req(#httpd{method='POST'}=Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    couch_httpd:validate_ctype(Req, "application/json"),
    Setup = get_body(Req),
    couch_log:notice("Setup: ~p~n", [Setup]),
    Action = binary_to_list(couch_util:get_value(<<"action">>, Setup, <<"missing">>)),
    case handle_action(Action, Setup) of
    ok ->
        chttpd:send_json(Req, 201, {[{ok, true}]});
    {error, Message} ->
        couch_httpd:send_error(Req, 400, <<"bad_request">>, Message)
    end;
handle_setup_req(#httpd{method='GET'}=Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    case setup:is_cluster_enabled() of
        no ->
            chttpd:send_json(Req, 200, {[{state, cluster_disabled}]});
        ok ->
            case setup:has_cluster_system_dbs() of
                no ->
                    chttpd:send_json(Req, 200, {[{state, cluster_enabled}]});
                ok ->
                    chttpd:send_json(Req, 200, {[{state, cluster_finished}]})
            end
    end;
handle_setup_req(#httpd{}=Req) ->
    chttpd:send_method_not_allowed(Req, "GET,POST").


get_options(Options, Setup) ->
    ExtractValues = fun({Tag, Option}, OptionsAcc) ->
        case couch_util:get_value(Option, Setup) of
            undefined -> OptionsAcc;
            Value -> [{Tag, Value} | OptionsAcc]
        end
    end,
    lists:foldl(ExtractValues, [], Options).

handle_action("enable_cluster", Setup) ->
    Options = get_options([
        {username, <<"username">>},
        {password, <<"password">>},
        {bind_address, <<"bind_address">>},
        {port, <<"port">>},
        {remote_node, <<"remote_node">>},
        {remote_current_user, <<"remote_current_user">>},
        {remote_current_password, <<"remote_current_password">>},
        {node_count, <<"node_count">>}
    ], Setup),
    case setup:enable_cluster(Options) of
        {error, cluster_enabled} ->
            {error, <<"Cluster is already enabled">>};
        _ -> ok
    end;


handle_action("finish_cluster", Setup) ->
    couch_log:notice("finish_cluster: ~p~n", [Setup]),
    case setup:finish_cluster() of
        {error, cluster_finished} ->
            {error, <<"Cluster is already finished">>};
        Else ->
            couch_log:notice("Else: ~p~n", [Else]),
            ok
    end;

handle_action("add_node", Setup) ->
    couch_log:notice("add_node: ~p~n", [Setup]),

    Options = get_options([
        {username, <<"username">>},
        {password, <<"password">>},
        {host, <<"host">>},
        {port, <<"port">>},
        {name, <<"name">>}
    ], Setup),
    case setup:add_node(Options) of
        {error, cluster_not_enabled} ->
            {error, <<"Cluster is not enabled.">>};
        {error, {conn_failed, {error, econnrefused}}} ->
            {error, <<"Add node failed. Invalid Host and/or Port.">>};
        {error, wrong_credentials} ->
            {error, <<"Add node failed. Invalid admin credentials,">>};
        {error, Message} ->
            {error, Message};
        _ -> ok
    end;

handle_action("remove_node", Setup) ->
    couch_log:notice("remove_node: ~p~n", [Setup]);

handle_action("receive_cookie", Setup) ->
    couch_log:notice("receive_cookie: ~p~n", [Setup]),
    Options = get_options([
       {cookie, <<"cookie">>}
    ], Setup),
    case setup:receive_cookie(Options) of
        {error, Error} ->
            {error, Error};
        _ -> ok
    end;

handle_action(_, _) ->
    couch_log:notice("invalid_action: ~n", []),
    {error, <<"Invalid Action'">>}.


get_body(Req) ->
    case catch couch_httpd:json_body_obj(Req) of
    {Body} ->
        Body;
    Else ->
        couch_log:notice("Body Fail: ~p~n", [Else]),
        couch_httpd:send_error(Req, 400, <<"bad_request">>, <<"Missing JSON body'">>)
    end.
