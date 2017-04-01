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

-vsn(2).

-export([
    start_link/0,
    init/1
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    UpdateNotifierExes = config:get("update_notification"),
    Children = [
        {
            config_listener_mon,
            {config_listener_mon, start_link, [?MODULE, nil]},
            permanent,
            5000,
            worker,
            [config_listener_mon]
        }
    | [child(Id, Exe) || {Id, Exe} <- UpdateNotifierExes]],

    {ok, {
        {one_for_one, 10, 3600},
        Children
    }}.


handle_config_change("update_notification", Id, deleted, _, _) ->
    supervisor:terminate_child(?MODULE, Id),
    supervisor:delete_child(?MODULE, Id),
    {ok, nil};
handle_config_change("update_notification", Id, Exe, _, _) when is_list(Exe) ->
    supervisor:start_child(?MODULE, child(Id, Exe)),
    {ok, nil};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_Server, _Reason, _State) ->
    ok.

child(Id, Arg) ->
    {
        Id,
        {couch_event_os_listener, start_link, [Arg]},
        permanent,
        1000,
        supervisor,
        [couch_event_os_listener]
    }.
