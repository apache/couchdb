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

-module(couch_secondary_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, couch_secondary_services}, ?MODULE, []).

init([]) ->
    SecondarySupervisors = [
        {couch_plugin_event, {gen_event, start_link, [{local, couch_plugin}]}, permanent,
            brutal_kill, worker, dynamic}
    ],
    Daemons =
        [
            {query_servers, {couch_proc_manager, start_link, []}},
            {vhosts, {couch_httpd_vhost, start_link, []}},
            {uuids, {couch_uuids, start, []}}
        ] ++ couch_index_servers(),

    MaybeHttp =
        case http_enabled() of
            true ->
                [{httpd, {couch_httpd, start_link, []}}];
            false ->
                couch_httpd:set_auth_handlers(),
                []
        end,

    MaybeHttps =
        case https_enabled() of
            true -> [{httpsd, {chttpd, start_link, [https]}}];
            false -> []
        end,

    Children =
        SecondarySupervisors ++
            [
                begin
                    {Module, Fun, Args} = Spec,

                    {Name, {Module, Fun, Args}, permanent, brutal_kill, worker, [Module]}
                end
             || {Name, Spec} <-
                    Daemons ++ MaybeHttp ++ MaybeHttps,
                Spec /= ""
            ],
    {ok, {{one_for_one, 50, 3600}, couch_epi:register_service(couch_db_epi, Children)}}.

http_enabled() ->
    config:get_boolean("httpd", "enable", false).

https_enabled() ->
    % 1. [ssl] enable = true | false
    % 2. if [daemons] httpsd == {chttpd, start_link, [https]} -> pretend true as well
    SSLEnabled = config:get_boolean("ssl", "enable", false),
    LegacySSL = config:get("daemons", "httpsd"),
    LegacySSLEnabled = LegacySSL =:= "{chttpd, start_link, [https]}",

    SSLEnabled orelse LegacySSLEnabled.

couch_index_servers() ->
    N = couch_index_server:num_servers(),
    [couch_index_server(I) || I <- lists:seq(1, N)].

couch_index_server(N) ->
    Name = couch_index_server:server_name(N),
    {Name, {couch_index_server, start_link, [N]}}.
