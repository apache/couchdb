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
-module(couch_plugins_httpd).

-export([handle_req/1]).

-include_lib("couch_db.hrl").

handle_req(#httpd{method='POST'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    couch_httpd:validate_ctype(Req, "application/json"),

    {PluginSpec} = couch_httpd:json_body_obj(Req),
    Url = binary_to_list(couch_util:get_value(<<"url">>, PluginSpec)),
    Name = binary_to_list(couch_util:get_value(<<"name">>, PluginSpec)),
    Version = binary_to_list(couch_util:get_value(<<"version">>, PluginSpec)),
    Delete = couch_util:get_value(<<"delete">>, PluginSpec),
    {Checksums0} = couch_util:get_value(<<"checksums">>, PluginSpec),
    Checksums = parse_checksums(Checksums0),

    Plugin = {Name, Url, Version, Checksums},
    case do_install(Delete, Plugin) of
    ok ->
        couch_httpd:send_json(Req, 202, {[{ok, true}]});
    Error ->
        ?LOG_DEBUG("Plugin Spec: ~p", [PluginSpec]),
        couch_httpd:send_error(Req, {bad_request, Error})
    end;
handle_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "POST").

do_install(false, Plugin) ->
    couch_plugins:install(Plugin);
do_install(true, Plugin) ->
    couch_plugins:uninstall(Plugin).

parse_checksums(Checksums) ->
    lists:map(fun({K, {V}}) ->
        {binary_to_list(K), parse_checksums(V)};
      ({K, V}) ->
         {binary_to_list(K), binary_to_list(V)}
    end, Checksums).