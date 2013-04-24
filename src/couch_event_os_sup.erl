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


% This causes an OS process to spawned and it is notified every time a database
% is updated.
%
% The notifications are in the form of a the database name sent as a line of
% text to the OS processes stdout.


-module(couch_event_os_sup).
-behaviour(supervisor).
-behaviour(config_listener).


-export([
    start_link/0,
    init/1
]).

-export([
    handle_config_change/5
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    ok = config:listen_for_changes(?MODULE, nil),

    UpdateNotifierExes = config:get("update_notification"),
    Children = [child(Id, Exe) || {Id, Exe} <- UpdateNotifierExes],

    {ok, {
        {one_for_one, 10, 3600},
        Children
    }.


handle_config_change("update_notification", Id, deleted, _, _) ->
    supervisor:terminate_child(?MODULE, Id),
    supervisor:delete_child(?MODULE, Id),
    {ok, nil};
handle_config_change("update_notification", Id, Exe, _, _) when is_list(Exe) ->
    supervisor:terminate_child(?MODULE, Id),
    supervisor:delete_child(?MODULE, Id),
    supervisor:start_child(?MODULE, child(Id, Exe)),
    {ok, nil};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.


child(Id, Arg) ->
    {
        Id,
        {couch_event_os_listener, start_link, [Arg]},
        permanent,
        1000,
        supervisor,
        [couch_event_os_listener]
    }.
