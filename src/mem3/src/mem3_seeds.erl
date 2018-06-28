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

-module(mem3_seeds).

-export([
    get_seeds/0
]).

get_seeds() ->
    case config:get("cluster", "seedlist") of
        undefined ->
            [];
        List ->
            Nodes = string:split(List, ",", all),
            [list_to_atom(Node) || Node <- Nodes] -- [node()]
    end.
