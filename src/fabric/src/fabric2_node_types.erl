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

-module(fabric2_node_types).


-export([
    is_type/1
]).


is_type(Type) when is_atom(Type) ->
    case {from_os_env(Type), from_app_env(Type)} of
        {V, _} when is_boolean(V) ->
            V;
        {undefined, V} when is_boolean(V) ->
            V;
        {undefined, undefined} ->
            % When not defined anywhere assume `true`, that is by default a
            % node will perform all the background tasks
            true
    end.


from_os_env(Type) when is_atom(Type) ->
    StrType = erlang:atom_to_list(Type),
    StrTypeUpper = string:to_upper(StrType),
    case os:getenv("COUCHDB_NODE_TYPE_" ++ StrTypeUpper) of
        false ->
            undefined;
        Str when is_list(Str) ->
            case string:to_lower(Str) of
                "false" -> false;
                _ -> true
            end
    end.


from_app_env(Type) when is_atom(Type) ->
    case application:get_env(fabric, node_types) of
        undefined ->  undefined;
        {ok, Props} when is_list(Props) -> proplists:get_value(Type, Props)
    end.
