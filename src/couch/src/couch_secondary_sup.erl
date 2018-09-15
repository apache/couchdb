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
    supervisor:start_link({local,couch_secondary_services}, ?MODULE, []).

init([]) ->
    SecondarySupervisors = [
        {couch_plugin_event,
            {gen_event, start_link, [{local, couch_plugin}]},
            permanent,
            brutal_kill,
            worker,
            dynamic}
    ],
    Daemons = [
        {"index_server", "{couch_index_server, start_link, []}"},
        {"query_servers", "{couch_proc_manager, start_link, []}"},
        {"vhosts", "{couch_httpd_vhost, start_link, []}"},
        {"httpd", "{couch_httpd, start_link, []}"},
        {"uuids", "{couch_uuids, start, []}"},
        {"auth_cache", "{couch_auth_cache, start_link, []}"},
        {"compaction_daemon", "{couch_compaction_daemon, start_link, []}"}
    ],

    Children = SecondarySupervisors ++ [
        begin
            {ok, {Module, Fun, Args}} = couch_util:parse_term(SpecStr),

            {list_to_atom(Name),
                {Module, Fun, Args},
                permanent,
                brutal_kill,
                worker,
                [Module]}
        end
        || {Name, SpecStr}
        <- Daemons, SpecStr /= ""],
    {ok, {{one_for_one, 50, 3600},
        couch_epi:register_service(couch_db_epi, Children)}}.
