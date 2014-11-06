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

-export([handle_setup_req/1]).

handle_setup_req(Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    % TBD uncomment after devving
    %couch_httpd:validate_ctype(Req, "application/json"),
    Setup = get_body(Req),
    io:format("~nSetup: ~p~n", [Setup]),
    Action = binary_to_list(couch_util:get_value(<<"action">>, Setup, <<"missing">>)),
    case handle_action(Action, Setup) of
    ok ->
        chttpd:send_json(Req, 201, {[{ok, true}]});
    {error, Message} ->
        couch_httpd:send_error(Req, 400, <<"bad_request">>, Message)
    end.


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
        {port, <<"port">>}
    ], Setup),
    case setup:enable_cluster(Options) of
        {error, cluster_enabled} ->
            {error, <<"Cluster is already enabled">>};
        _ -> ok
    end;


handle_action("finish_cluster", Setup) ->
    io:format("~nfinish_cluster: ~p~n", [Setup]),
    case etup:finish_cluster() of
        {error, cluster_finished} ->
            {error, <<"Cluster is already finished">>};
        _ -> ok
    end;

handle_action("add_node", Setup) ->
    io:format("~nadd_node: ~p~n", [Setup]),

    Options = get_options([
        {username, <<"username">>},
        {password, <<"password">>},
        {host, <<"host">>},
        {port, <<"port">>}
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
    io:format("~nremove_node: ~p~n", [Setup]);

handle_action("receive_cookie", Setup) ->
    io:format("~nreceive_cookie: ~p~n", [Setup]),
    Options = get_options([
       {cookue, <<"cookie">>}
    ], Setup),
    case setup:receive_cookie(Options) of
        {error, Error} ->
            {error, Error};
        _ -> ok
    end;

handle_action(_, _) ->
    io:format("~ninvalid_action: ~n", []),
    {error, <<"Invalid Action'">>}.


get_body(Req) ->
    case catch couch_httpd:json_body_obj(Req) of
    {Body} ->
        Body;
    Else ->
        io:format("~nBody Fail: ~p~n", [Else]),
        couch_httpd:send_error(Req, 400, <<"bad_request">>, <<"Missing JSON body'">>)
    end.
