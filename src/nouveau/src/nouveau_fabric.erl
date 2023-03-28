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

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% inspired by dreyfus_fabric.erl but better

-module(nouveau_fabric).
-export([get_json_docs/2]).

get_json_docs(DbName, DocIds) ->
    fabric:all_docs(DbName, fun callback/2, [], [{keys, DocIds}, {include_docs, true}]).

callback({meta, _}, Acc) ->
    {ok, Acc};
callback({error, Reason}, _Acc) ->
    {error, Reason};
callback({row, Row}, Acc) ->
    case lists:keyfind(doc, 1, Row) of
        false ->
            {ok, [not_found | Acc]};
        {doc, Doc} ->
            {ok, [Doc | Acc]}
    end;
callback(complete, Acc) ->
    {ok, lists:reverse(Acc)};
callback(timeout, _Acc) ->
    {error, timeout}.
