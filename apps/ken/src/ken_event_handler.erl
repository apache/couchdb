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

-module(ken_event_handler).
-behaviour(couch_event_listener).

-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_event/3,
    handle_cast/2,
    handle_info/2
]).

start_link() ->
    couch_event_listener:start_link(?MODULE, nil, [all_dbs]).

%% couch_event_listener callbacks

init(_) ->
    {ok, nil}.

terminate(_Reason, _State) ->
    ok.

handle_event(DbName, updated, State) ->
    ken:add(DbName),
    {ok, State};
handle_event(DbName, deleted, State) ->
    ken:remove(DbName),
    {ok, State};
handle_event(DbName, ddoc_updated, State) ->
    ken:add_all_shards(DbName),
    {ok, State};
handle_event(_DbName, _Event, State) ->
    {ok, State}.

handle_cast(_Msg, State) ->
    {ok, State}.

handle_info(_Msg, State) ->
    {ok, State}.
