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

-module(couch_event).

-export([
    notify/2
]).

-export([
    listen/4,
    link_listener/4,
    stop_listener/1
]).

-export([
    register/2,
    register_many/2,
    register_all/1,
    unregister/1
]).

-define(SERVER, couch_event_server).

notify(DbName, Event) ->
    gen_server:cast(?SERVER, {notify, DbName, Event}).

listen(Module, Function, State, Options) ->
    couch_event_listener_mfa:enter_loop(Module, Function, State, Options).

link_listener(Module, Function, State, Options) ->
    couch_event_listener_mfa:start_link(Module, Function, State, Options).

stop_listener(Pid) ->
    couch_event_listener_mfa:stop(Pid).

register(Pid, DbName) ->
    gen_server:call(?SERVER, {register, Pid, [DbName]}).

register_many(Pid, DbNames) when is_list(DbNames) ->
    gen_server:call(?SERVER, {register, Pid, DbNames}).

register_all(Pid) ->
    gen_server:call(?SERVER, {register, Pid, [all_dbs]}).

unregister(Pid) ->
    gen_server:call(?SERVER, {unregister, Pid}).
