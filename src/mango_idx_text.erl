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

-module(mango_idx_text).


-export([
    validate_new/1,
    validate_fields/1,
    validate_index_def/1,
    add/2,
    remove/2,
    from_ddoc/1,
    to_json/1,
    columns/1,
    is_usable/2,
    get_default_field_options/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").
-include("mango_idx.hrl").


validate_new(#idx{}=Idx) ->
    {ok, Def} = do_validate(Idx#idx.def),
    {ok, Idx#idx{def=Def}}.


validate_index_def(IndexInfo) ->
    do_validate(IndexInfo).


add(#doc{body={Props0}}=DDoc, Idx) ->
    Texts1 = case proplists:get_value(<<"indexes">>, Props0) of
        {Texts0} -> Texts0;
        _ -> []
    end,
    NewText = make_text(Idx),
    Texts2 = lists:keystore(element(1, NewText), 1, Texts1, NewText),
    Props1 = lists:keystore(<<"indexes">>, 1, Props0, {<<"indexes">>,
        {Texts2}}),
    {ok, DDoc#doc{body={Props1}}}.


remove(#doc{body={Props0}}=DDoc, Idx) ->
    Texts1 = case proplists:get_value(<<"indexes">>, Props0) of
        {Texts0} ->
            Texts0;
        _ ->
            ?MANGO_ERROR({index_not_found, Idx#idx.name})
    end,
    Texts2 = lists:keydelete(Idx#idx.name, 1, Texts1),
    if Texts2 /= Texts1 -> ok; true ->
        ?MANGO_ERROR({index_not_found, Idx#idx.name})
    end,
    Props1 = case Texts2 of
        [] ->
            lists:keydelete(<<"indexes">>, 1, Props0);
        _ ->
            lists:keystore(<<"indexes">>, 1, Props0, {<<"indexes">>, {Texts2}})
    end,
    {ok, DDoc#doc{body={Props1}}}.


from_ddoc({Props}) ->
    case lists:keyfind(<<"indexes">>, 1, Props) of
        {<<"indexes">>, {Texts}} when is_list(Texts) ->
            lists:flatmap(fun({Name, {VProps}}) ->
                case validate_ddoc(VProps) of
                    invalid_ddoc ->
                        [];
                    Def ->
                        I = #idx{
                        type = <<"text">>,
                        name = Name,
                        def = Def
                        },
                        [I]
                end
            end, Texts);
        _ ->
            []
    end.


to_json(Idx) ->
    {[
        {ddoc, Idx#idx.ddoc},
        {name, Idx#idx.name},
        {type, Idx#idx.type},
        {def, {def_to_json(Idx#idx.def)}}
    ]}.


columns(Idx) ->
    {Props} = Idx#idx.def,
    {<<"fields">>, Fields} = lists:keyfind(<<"fields">>, 1, Props),
    case Fields of
        <<"all_fields">> ->
            all_fields;
        _ ->
            {DFProps} = couch_util:get_value(<<"default_field">>, Props, {[]}),
            Enabled = couch_util:get_value(<<"enabled">>, DFProps, true),
            Default = case Enabled of
                true -> [<<"$default">>];
                false -> []
            end,
            Default ++ lists:map(fun({FProps}) ->
                {_, Name} = lists:keyfind(<<"name">>, 1, FProps),
                {_, Type} = lists:keyfind(<<"type">>, 1, FProps),
                iolist_to_binary([Name, ":", Type])
            end, Fields)
    end.


is_usable(Idx, Selector) ->
    case columns(Idx) of
        all_fields ->
            true;
        Cols ->
            Fields = indexable_fields(Selector),
            sets:is_subset(sets:from_list(Fields), sets:from_list(Cols))
    end.


do_validate({Props}) ->
    {ok, Opts} = mango_opts:validate(Props, opts()),
    {ok, {Opts}};
do_validate(Else) ->
    ?MANGO_ERROR({invalid_index_text, Else}).


def_to_json({Props}) ->
    def_to_json(Props);
def_to_json([]) ->
    [];
def_to_json([{<<"fields">>, <<"all_fields">>} | Rest]) ->
    [{<<"fields">>, []} | def_to_json(Rest)];
def_to_json([{fields, Fields} | Rest]) ->
    [{<<"fields">>, fields_to_json(Fields)} | def_to_json(Rest)];
def_to_json([{<<"fields">>, Fields} | Rest]) ->
    [{<<"fields">>, fields_to_json(Fields)} | def_to_json(Rest)];
def_to_json([{Key, Value} | Rest]) ->
    [{Key, Value} | def_to_json(Rest)].


fields_to_json([]) ->
    [];
fields_to_json([{[{<<"name">>, Name}, {<<"type">>, Type0}]} | Rest]) ->
    Type = validate_field_type(Type0),
    [{[{Name, Type}]} | fields_to_json(Rest)];
fields_to_json([{[{<<"type">>, Type0}, {<<"name">>, Name}]} | Rest]) ->
    Type = validate_field_type(Type0),
    [{[{Name, Type}]} | fields_to_json(Rest)].


validate_field_type(<<"string">>) ->
    <<"string">>;
validate_field_type(<<"number">>) ->
    <<"number">>;
validate_field_type(<<"boolean">>) ->
    <<"boolean">>.


validate_fields(<<"all_fields">>) ->
    {ok, all_fields};
validate_fields(Fields) ->
    try fields_to_json(Fields) of
        _ ->
            mango_fields:new(Fields)
    catch error:function_clause ->
        ?MANGO_ERROR({invalid_index_fields_definition, Fields})
    end.


validate_ddoc(VProps) ->
    try
        Def = proplists:get_value(<<"index">>, VProps),
        validate_index_def(Def),
        Def
    catch Error:Reason ->
        couch_log:error("Invalid Index Def ~p: Error. ~p, Reason: ~p",
            [VProps, Error, Reason]),
        invalid_ddoc
    end.


opts() ->
    [
        {<<"default_analyzer">>, [
            {tag, default_analyzer},
            {optional, true},
            {default, <<"keyword">>}
        ]},
        {<<"default_field">>, [
            {tag, default_field},
            {optional, true},
            {default, {[]}}
        ]},
         {<<"selector">>, [
            {tag, selector},
            {optional, true},
            {default, {[]}},
            {validator, fun mango_opts:validate_selector/1}
        ]},
        {<<"fields">>, [
            {tag, fields},
            {optional, true},
            {default, []},
            {validator, fun ?MODULE:validate_fields/1}
        ]}
    ].


make_text(Idx) ->
    Text= {[
        {<<"index">>, Idx#idx.def},
        {<<"analyzer">>, construct_analyzer(Idx#idx.def)}
    ]},
    {Idx#idx.name, Text}.


get_default_field_options(Props) ->
    Default = couch_util:get_value(default_field, Props, {[]}),
    case Default of
        Bool when is_boolean(Bool) ->
            {Bool, <<"standard">>};
        {[]} ->
            {true, <<"standard">>};
        {Opts}->
            Enabled = couch_util:get_value(<<"enabled">>, Opts, true),
            Analyzer = couch_util:get_value(<<"analyzer">>, Opts,
                <<"standard">>),
            {Enabled, Analyzer}
    end.


construct_analyzer({Props}) ->
    DefaultAnalyzer = couch_util:get_value(default_analyzer, Props,
        <<"keyword">>),
    {DefaultField, DefaultFieldAnalyzer} = get_default_field_options(Props),
    DefaultAnalyzerDef = case DefaultField of
        true ->
            [{<<"$default">>, DefaultFieldAnalyzer}];
        _ ->
            []
    end,
    case DefaultAnalyzerDef of
        [] ->
            <<"keyword">>;
        _ ->
            {[
                {<<"name">>, <<"perfield">>},
                {<<"default">>, DefaultAnalyzer},
                {<<"fields">>, {DefaultAnalyzerDef}}
            ]}
    end.


indexable_fields(Selector) ->
    TupleTree = mango_selector_text:convert([], Selector),
    indexable_fields([], TupleTree).


indexable_fields(Fields, {op_and, Args}) when is_list(Args) ->
    lists:foldl(fun(Arg, Fields0) -> indexable_fields(Fields0, Arg) end,
        Fields, Args);

indexable_fields(Fields, {op_or, Args}) when is_list(Args) ->
    lists:foldl(fun(Arg, Fields0) -> indexable_fields(Fields0, Arg) end,
        Fields, Args);

indexable_fields(Fields, {op_not, {ExistsQuery, Arg}}) when is_tuple(Arg) ->
    Fields0 = indexable_fields(Fields, ExistsQuery),
    indexable_fields(Fields0, Arg);

indexable_fields(Fields, {op_insert, Arg}) when is_binary(Arg) ->
    Fields;

indexable_fields(Fields, {op_field, {Name, _}}) ->
    [iolist_to_binary(Name) | Fields];

%% In this particular case, the lucene index is doing a field_exists query
%% so it is looking at all sorts of combinations of field:* and field.*
%% We don't add the field because we cannot pre-determine what field will exist.
%% Hence we just return Fields and make it less restrictive.
indexable_fields(Fields, {op_fieldname, {_, _}}) ->
    Fields;

%% Similar idea to op_fieldname but with fieldname:null
indexable_fields(Fields, {op_null, {_, _}}) ->
    Fields;

indexable_fields(Fields, {op_default, _}) ->
    [<<"$default">> | Fields].
