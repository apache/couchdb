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

-module(couch_index_epi).

-behaviour(couch_epi_plugin).

-export([
    app/0,
    providers/0,
    services/0,
    data_subscriptions/0,
    data_providers/0,
    processes/0,
    notify/3
]).

app() ->
    couch_index.

providers() ->
    [
        {couch_db, couch_index_plugin_couch_db}
    ].

services() ->
    [
        {couch_index, couch_index_plugin}
    ].

data_subscriptions() ->
    [].

data_providers() ->
    [].

processes() ->
    [].

notify(_Key, _Old, _New) ->
    ok.
