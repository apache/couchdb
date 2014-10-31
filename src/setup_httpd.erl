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


handle_action("enable_cluster", Setup) ->
    io:format("~nenable_cluster: ~p~n", [Setup]);
handle_action("finish_cluster", Setup) ->
    io:format("~nfinish_cluster: ~p~n", [Setup]);
handle_action("add_node", Setup) ->
    io:format("~nadd_node: ~p~n", [Setup]);
handle_action("remove_node", Setup) ->
    io:format("~nremove_node: ~p~n", [Setup]);
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
