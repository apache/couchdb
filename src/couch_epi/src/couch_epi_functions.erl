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

-module(couch_epi_functions).

-include("couch_epi.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([interval/1, data/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

interval(_) ->
    undefined.

data(Specs) ->
    Defs = [{A, definitions(M)} || {A, #couch_epi_spec{value = M}} <- Specs],
    Modules = lists:flatten([M || {_App, #couch_epi_spec{value = M}} <- Specs]),
    {ok, couch_epi_functions_gen:hash(Modules), group(Defs)}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

definitions(Module) when is_atom(Module) ->
    definitions([Module]);
definitions(Modules) ->
    Blacklist = [{module_info, 0}, {module_info, 1}],
    [{M, M:module_info(exports) -- Blacklist} || M <- Modules].

group(KV) ->
    Dict = lists:foldr(fun({K,V}, D) ->
        dict:append_list(K, V, D)
    end, dict:new(), KV),
    [{K, lists:reverse(V)} || {K, V} <- dict:to_list(Dict)].
