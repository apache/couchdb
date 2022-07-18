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

-module(chttpd_xframe_options).

-export([
    header/2,
    header/3
]).

-define(DENY, "DENY").
-define(SAMEORIGIN, "SAMEORIGIN").
-define(ALLOWFROM, "ALLOW-FROM ").

-include_lib("couch/include/couch_db.hrl").

% X-Frame-Options protects against clickjacking by limiting whether a response can be used in a
% <frame>, <iframe> or <object>.

header(Req, Headers) ->
    header(Req, Headers, get_xframe_config(Req)).

header(Req, Headers, Config) ->
    case lists:keyfind(enabled, 1, Config) of
        {enabled, true} ->
            generate_xframe_header(Req, Headers, Config);
        _ ->
            Headers
    end.

generate_xframe_header(Req, Headers, Config) ->
    XframeOption =
        case lists:keyfind(same_origin, 1, Config) of
            {same_origin, true} ->
                ?SAMEORIGIN;
            _ ->
                check_host(Req, Config)
        end,
    [{"X-Frame-Options", XframeOption} | Headers].

check_host(#httpd{mochi_req = MochiReq} = Req, Config) ->
    Host = couch_httpd_vhost:host(MochiReq),
    case Host of
        [] ->
            ?DENY;
        Host ->
            FullHost = chttpd:absolute_uri(Req, ""),
            AcceptedHosts = get_accepted_hosts(Config),
            AcceptAll = ["*"] =:= AcceptedHosts,
            case AcceptAll orelse lists:member(FullHost, AcceptedHosts) of
                true -> ?ALLOWFROM ++ FullHost;
                false -> ?DENY
            end
    end.

get_xframe_config(#httpd{xframe_config = undefined}) ->
    EnableXFrame = chttpd_util:get_chttpd_config_boolean(
        "enable_xframe_options", false
    ),
    SameOrigin = config:get("x_frame_options", "same_origin", "false") =:= "true",
    AcceptedHosts =
        case config:get("x_frame_options", "hosts") of
            undefined -> [];
            Hosts -> split_list(Hosts)
        end,
    [
        {enabled, EnableXFrame},
        {same_origin, SameOrigin},
        {hosts, AcceptedHosts}
    ];
get_xframe_config(#httpd{xframe_config = Config}) ->
    Config.

get_accepted_hosts(Config) ->
    case lists:keyfind(hosts, 1, Config) of
        false -> [];
        {hosts, AcceptedHosts} -> AcceptedHosts
    end.

split_list(S) ->
    re:split(S, "\\s*,\\s*", [trim, {return, list}]).
