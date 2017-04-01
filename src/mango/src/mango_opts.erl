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

-module(mango_opts).

-export([
    validate_idx_create/1,
    validate_find/1
]).

-export([
    validate/2,

    is_string/1,
    is_boolean/1,
    is_pos_integer/1,
    is_non_neg_integer/1,
    is_object/1,

    validate_idx_name/1,
    validate_selector/1,
    validate_use_index/1,
    validate_bookmark/1,
    validate_sort/1,
    validate_fields/1,
    validate_bulk_delete/1,

    default_limit/0
]).


-include("mango.hrl").


validate_idx_create({Props}) ->
    Opts = [
        {<<"index">>, [
            {tag, def}
        ]},
        {<<"type">>, [
            {tag, type},
            {optional, true},
            {default, <<"json">>},
            {validator, fun is_string/1}
        ]},
        {<<"name">>, [
            {tag, name},
            {optional, true},
            {default, auto_name},
            {validator, fun validate_idx_name/1}
        ]},
        {<<"ddoc">>, [
            {tag, ddoc},
            {optional, true},
            {default, auto_name},
            {validator, fun validate_idx_name/1}
        ]},
        {<<"w">>, [
            {tag, w},
            {optional, true},
            {default, 2},
            {validator, fun is_pos_integer/1}
        ]}
    ],
    validate(Props, Opts).


validate_find({Props}) ->
    Opts = [
        {<<"selector">>, [
            {tag, selector},
            {validator, fun validate_selector/1}
        ]},
        {<<"use_index">>, [
            {tag, use_index},
            {optional, true},
            {default, []},
            {validator, fun validate_use_index/1}
        ]},
        {<<"bookmark">>, [
            {tag, bookmark},
            {optional, true},
            {default, <<>>},
            {validator, fun validate_bookmark/1}
        ]},
        {<<"limit">>, [
            {tag, limit},
            {optional, true},
            {default, default_limit()},
            {validator, fun is_non_neg_integer/1}
        ]},
        {<<"skip">>, [
            {tag, skip},
            {optional, true},
            {default, 0},
            {validator, fun is_non_neg_integer/1}
        ]},
        {<<"sort">>, [
            {tag, sort},
            {optional, true},
            {default, []},
            {validator, fun validate_sort/1}
        ]},
        {<<"fields">>, [
            {tag, fields},
            {optional, true},
            {default, []},
            {validator, fun validate_fields/1}
        ]},
        {<<"r">>, [
            {tag, r},
            {optional, true},
            {default, 1},
            {validator, fun mango_opts:is_pos_integer/1}
        ]},
        {<<"conflicts">>, [
            {tag, conflicts},
            {optional, true},
            {default, false},
            {validator, fun mango_opts:is_boolean/1}
        ]}
    ],
    validate(Props, Opts).


validate_bulk_delete({Props}) ->
    Opts = [
        {<<"docids">>, [
            {tag, docids},
            {validator, fun validate_bulk_docs/1}
        ]},
        {<<"w">>, [
            {tag, w},
            {optional, true},
            {default, 2},
            {validator, fun is_pos_integer/1}
        ]}
    ],
    validate(Props, Opts).


validate(Props, Opts) ->
    case mango_util:assert_ejson({Props}) of
        true ->
            ok;
        false ->
            ?MANGO_ERROR({invalid_ejson, {Props}})
    end,
    {Rest, Acc} = validate_opts(Opts, Props, []),
    case Rest of
        [] ->
            ok;
        [{BadKey, _} | _] ->
            ?MANGO_ERROR({invalid_key, BadKey})
    end,
    {ok, Acc}.


is_string(Val) when is_binary(Val) ->
    {ok, Val};
is_string(Else) ->
    ?MANGO_ERROR({invalid_string, Else}).


is_boolean(true) ->
    {ok, true};
is_boolean(false) ->
    {ok, false};
is_boolean(Else) ->
    ?MANGO_ERROR({invalid_boolean, Else}).


is_pos_integer(V) when is_integer(V), V > 0 ->
    {ok, V};
is_pos_integer(Else) ->
    ?MANGO_ERROR({invalid_pos_integer, Else}).


is_non_neg_integer(V) when is_integer(V), V >= 0 ->
    {ok, V};
is_non_neg_integer(Else) ->
    ?MANGO_ERROR({invalid_non_neg_integer, Else}).


is_object({Props}) ->
    true = mango_util:assert_ejson({Props}),
    {ok, {Props}};
is_object(Else) ->
    ?MANGO_ERROR({invalid_object, Else}).


validate_idx_name(auto_name) ->
    {ok, auto_name};
validate_idx_name(Else) ->
    is_string(Else).


validate_selector({Props}) ->
    Norm = mango_selector:normalize({Props}),
    {ok, Norm};
validate_selector(Else) ->
    ?MANGO_ERROR({invalid_selector_json, Else}).


%% We re-use validate_use_index to make sure the index names are valid
validate_bulk_docs(Docs) when is_list(Docs) ->
    lists:foreach(fun validate_use_index/1, Docs),
    {ok, Docs};
validate_bulk_docs(Else) ->
    ?MANGO_ERROR({invalid_bulk_docs, Else}).


validate_use_index(IndexName) when is_binary(IndexName) ->
    case binary:split(IndexName, <<"/">>) of
        [DesignId] ->
            {ok, [DesignId]};
        [<<"_design">>, DesignId] ->
            {ok, [DesignId]};
        [DesignId, ViewName] ->
            {ok, [DesignId, ViewName]};
        [<<"_design">>, DesignId, ViewName] ->
            {ok, [DesignId, ViewName]};
        _ ->
            ?MANGO_ERROR({invalid_index_name, IndexName})
    end;
validate_use_index(null) ->
    {ok, []};
validate_use_index([]) ->
    {ok, []};
validate_use_index([DesignId]) when is_binary(DesignId) ->
    {ok, [DesignId]};
validate_use_index([DesignId, ViewName])
        when is_binary(DesignId), is_binary(ViewName) ->
    {ok, [DesignId, ViewName]};
validate_use_index(Else) ->
    ?MANGO_ERROR({invalid_index_name, Else}).


validate_bookmark(null) ->
    {ok, nil};
validate_bookmark(<<>>) ->
    {ok, nil};
validate_bookmark(Bin) when is_binary(Bin) ->
    {ok, Bin};
validate_bookmark(Else) ->
    ?MANGO_ERROR({invalid_bookmark, Else}).


validate_sort(Value) ->
    mango_sort:new(Value).


validate_fields(Value) ->
    mango_fields:new(Value).


validate_opts([], Props, Acc) ->
    {Props, lists:reverse(Acc)};
validate_opts([{Name, Desc} | Rest], Props, Acc) ->
    {tag, Tag} = lists:keyfind(tag, 1, Desc),
    case lists:keytake(Name, 1, Props) of
        {value, {Name, Prop}, RestProps} ->
            NewAcc = [{Tag, validate_opt(Name, Desc, Prop)} | Acc],
            validate_opts(Rest, RestProps, NewAcc);
        false ->
            NewAcc = [{Tag, validate_opt(Name, Desc, undefined)} | Acc],
            validate_opts(Rest, Props, NewAcc)
    end.


validate_opt(_Name, [], Value) ->
    Value;
validate_opt(Name, Desc0, undefined) ->
    case lists:keytake(optional, 1, Desc0) of
        {value, {optional, true}, Desc1} ->
            {value, {default, Value}, Desc2} = lists:keytake(default, 1, Desc1),
            false = (Value == undefined),
            validate_opt(Name, Desc2, Value);
        _ ->
            ?MANGO_ERROR({missing_required_key, Name})
    end;
validate_opt(Name, [{tag, _} | Rest], Value) ->
    % Tags aren't really validated
    validate_opt(Name, Rest, Value);
validate_opt(Name, [{optional, _} | Rest], Value) ->
    % A value was specified for an optional value
    validate_opt(Name, Rest, Value);
validate_opt(Name, [{default, _} | Rest], Value) ->
    % A value was specified for an optional value
    validate_opt(Name, Rest, Value);
validate_opt(Name, [{assert, Value} | Rest], Value) ->
    validate_opt(Name, Rest, Value);
validate_opt(Name, [{assert, Expect} | _], Found) ->
    ?MANGO_ERROR({invalid_value, Name, Expect, Found});
validate_opt(Name, [{validator, Fun} | Rest], Value) ->
    case Fun(Value) of
        {ok, Validated} ->
            validate_opt(Name, Rest, Validated);
        false ->
            ?MANGO_ERROR({invalid_value, Name, Value})
    end.


default_limit() ->
    config:get_integer("mango", "default_limit", 25).
