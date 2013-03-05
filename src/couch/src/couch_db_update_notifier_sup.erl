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

%
% This causes an OS process to spawned and it is notified every time a database
% is updated.
%
% The notifications are in the form of a the database name sent as a line of
% text to the OS processes stdout.
%

-module(couch_db_update_notifier_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, config_change/3]).

start_link() ->
    supervisor:start_link({local, couch_db_update_notifier_sup},
        couch_db_update_notifier_sup, []).

init([]) ->
    ok = couch_config:register(fun ?MODULE:config_change/3),

    UpdateNotifierExes = couch_config:get("update_notification"),

    {ok,
        {{one_for_one, 10, 3600},
            lists:map(fun({Name, UpdateNotifierExe}) ->
                {Name,
                {couch_db_update_notifier, start_link, [UpdateNotifierExe]},
                    permanent,
                    1000,
                    supervisor,
                    [couch_db_update_notifier]}
                end, UpdateNotifierExes)}}.

%% @doc when update_notification configuration changes, terminate the process
%%      for that notifier and start a new one with the updated config
config_change("update_notification", Id, Exe) ->
    ChildSpec = {
        Id,
        {couch_db_update_notifier, start_link, [Exe]},
        permanent,
        1000,
        supervisor,
        [couch_db_update_notifier]
    },
    supervisor:terminate_child(couch_db_update_notifier_sup, Id),
    supervisor:delete_child(couch_db_update_notifier_sup, Id),
    supervisor:start_child(couch_db_update_notifier_sup, ChildSpec).

