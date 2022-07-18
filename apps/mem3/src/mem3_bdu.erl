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

-module(mem3_bdu).

-export([
    before_doc_update/3
]).

-include_lib("couch/include/couch_db.hrl").

-spec before_doc_update(#doc{}, Db :: any(), couch_db:update_type()) -> #doc{}.
before_doc_update(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, _Db, _UpdateType) ->
    % Skip design docs
    Doc;
before_doc_update(#doc{deleted = true} = Doc, _Db, _UpdateType) ->
    % Skip deleted
    Doc;
before_doc_update(#doc{} = Doc, _Db, replicated_changes) ->
    % Skip internal replicator updates
    Doc;
before_doc_update(#doc{} = Doc, _Db, _UpdateType) ->
    Body1 = couch_util:json_encode(Doc#doc.body),
    Body2 = couch_util:json_decode(Body1, [return_maps]),
    validate(Body2),
    Doc.

validate(#{} = Body) ->
    validate_key(<<"by_node">>, Body, ["by_node is mandatory"]),
    validate_key(<<"by_range">>, Body, ["by_range is mandatory"]),
    ByNode = maps:get(<<"by_node">>, Body),
    case is_map(ByNode) of
        true -> ok;
        false -> throw({forbidden, ["by_node not an object"]})
    end,
    ByRange = maps:get(<<"by_range">>, Body),
    case is_map(ByRange) of
        true -> ok;
        false -> throw({forbidden, ["by_range not an object"]})
    end,
    % "by_node": {
    %    "node1@xxx.xxx.xxx.xxx": ["00000000-1fffffff",...]
    % ]}
    maps:map(
        fun(Node, Ranges) ->
            validate_by_node(Node, Ranges, ByRange)
        end,
        ByNode
    ),
    % "by_range": {
    %   "00000000-1fffffff": ["node1@xxx.xxx.xxx.xxx", ...]
    % ]}
    maps:map(
        fun(Range, Nodes) ->
            validate_by_range(Range, Nodes, ByNode)
        end,
        ByRange
    ).

validate_by_node(Node, Ranges, ByRange) ->
    validate_array(Ranges, ["by_node", Ranges, "value not an array"]),
    lists:foreach(
        fun(Range) ->
            validate_key(Range, ByRange, ["by_range for", Range, "missing"]),
            Nodes = maps:get(Range, ByRange),
            validate_member(Node, Nodes, ["by_range for", Range, "missing", Node])
        end,
        Ranges
    ).

validate_by_range(Range, Nodes, ByNode) ->
    validate_array(Nodes, ["by_range", Nodes, "value not an array"]),
    lists:foreach(
        fun(Node) ->
            validate_key(Node, ByNode, ["by_node for", Node, "missing"]),
            Ranges = maps:get(Node, ByNode),
            validate_member(Range, Ranges, ["by_node for", Node, "missing", Range])
        end,
        Nodes
    ).

validate_array(Val, _ErrMsg) when is_list(Val) ->
    ok;
validate_array(_Val, ErrMsg) ->
    throw({forbidden, errmsg(ErrMsg)}).

validate_key(Key, #{} = Map, ErrMsg) ->
    case maps:is_key(Key, Map) of
        true -> ok;
        false -> throw({forbidden, errmsg(ErrMsg)})
    end.

validate_member(Val, Array, ErrMsg) when is_list(Array) ->
    case lists:member(Val, Array) of
        true -> ok;
        false -> throw({forbidden, errmsg(ErrMsg)})
    end;
validate_member(_Val, _Array, ErrMsg) ->
    throw({forbidden, errmsg(ErrMsg)}).

errmsg(ErrMsg) when is_list(ErrMsg) ->
    list_to_binary(lists:join(" ", ErrMsg)).
