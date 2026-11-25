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

% Plugins to add metrics to the result of couch_prometheus:scrape/0
%
% Enabled in config like:
%
%   [prometheus_plugins]
%   my_plugin_module = true
%
% A plugin which crashes, or returns anything else, is logged and skipped. The
% rest of the scrape is not affected.

-module(couch_prometheus_plugin).

-export([
    collect/0
]).

-callback metrics() -> [binary() | [binary()]].

collect() ->
    lists:flatmap(fun metrics/1, plugins()).

plugins() ->
    Fun = fun
        ({Module, "true"}, Acc) -> [list_to_atom(Module) | Acc];
        ({_, _}, Acc) -> Acc
    end,
    lists:sort(lists:foldl(Fun, [], config:get("prometheus_plugins"))).

metrics(Mod) ->
    try
        Lines = lists:flatten(Mod:metrics()),
        true = lists:all(fun is_binary/1, Lines),
        Lines
    catch
        Tag:Error:Stack ->
            LogMsg = "~p: prometheus plugin ~p failed: ~p:~p ~p",
            couch_log:error(LogMsg, [?MODULE, Mod, Tag, Error, Stack]),
            []
    end.
