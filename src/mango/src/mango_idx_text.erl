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
    validate_new/2,
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


validate_new(#idx{}=Idx, Db) ->
    {ok, Def} = do_validate(Idx#idx.def),
    maybe_reject_index_all_req(Def, Db),
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
    ok = validate_field_name(Name),
    Type = validate_field_type(Type0),
    [{[{Name, Type}]} | fields_to_json(Rest)];
fields_to_json([{[{<<"type">>, Type0}, {<<"name">>, Name}]} | Rest]) ->
    ok = validate_field_name(Name),
    Type = validate_field_type(Type0),
    [{[{Name, Type}]} | fields_to_json(Rest)].


%% In the future, we can possibly add more restrictive validation.
%% For now, let's make sure the field name is not blank.
validate_field_name(<<"">>) ->
    throw(invalid_field_name);
validate_field_name(Else) when is_binary(Else)->
    ok;
validate_field_name(_) ->
    throw(invalid_field_name).


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
        ?MANGO_ERROR({invalid_index_fields_definition, Fields});
    throw:invalid_field_name ->
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
        ]},
        {<<"index_array_lengths">>, [
            {tag, index_array_lengths},
            {optional, true},
            {default, true},
            {validator, fun mango_opts:is_boolean/1}
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

%% For queries that use array element access or $in operations, two
%% fields get generated by mango_selector_text:convert. At index
%% definition time, only one field gets defined. In this situation, we
%% remove the extra generated field so that the index can be used. For
%% all other situations, we include the fields as normal.
indexable_fields(Fields, {op_or, [{op_field, Field0},
        {op_field, {[Name | _], _}} = Field1]}) ->
    case lists:member(<<"[]">>, Name) of
        true ->
            indexable_fields(Fields, Field1);
        false ->
            Fields1 = indexable_fields(Fields, {op_field, Field0}),
            indexable_fields(Fields1, Field1)
    end;
indexable_fields(Fields, {op_or, Args}) when is_list(Args) ->
    lists:foldl(fun(Arg, Fields0) -> indexable_fields(Fields0, Arg) end,
        Fields, Args);

indexable_fields(Fields, {op_not, {ExistsQuery, Arg}}) when is_tuple(Arg) ->
    Fields0 = indexable_fields(Fields, ExistsQuery),
    indexable_fields(Fields0, Arg);

indexable_fields(Fields, {op_insert, Arg}) when is_binary(Arg) ->
    Fields;

%% fieldname.[]:length is not a user defined field.
indexable_fields(Fields, {op_field, {[_, <<":length">>], _}}) ->
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


maybe_reject_index_all_req({Def}, Db) ->
    DbName = couch_db:name(Db),
    #user_ctx{name = User} = couch_db:get_user_ctx(Db),
    Fields = couch_util:get_value(fields, Def),
    case {Fields, forbid_index_all()} of
        {all_fields, "true"} ->
            ?MANGO_ERROR(index_all_disabled);
        {all_fields, "warn"} ->
            couch_log:warning("User ~p is indexing all fields in db ~p",
                [User, DbName]);
        _ ->
            ok
    end.


forbid_index_all() ->
    config:get("mango", "index_all_disabled", "false").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


setup() ->
    test_util:start_couch(),
    meck:expect(couch_log, warning, 2,
        fun(_,_) ->
            throw({test_error, logged_warning})
        end),
    %default index all def that generates {fields, all_fields}
    Index = #idx{def={[]}},
    DbName = <<"testdb">>,
    UserCtx = #user_ctx{name = <<"u1">>},
    {ok, Db} = couch_db:clustered_db(DbName, UserCtx),
    {Index, Db}.


teardown(_) ->
    ok = config:delete("mango", "index_all_disabled"),
    test_util:stop_couch().


index_all_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun forbid_index_all/1,
            fun default_and_false_index_all/1,
            fun warn_index_all/1
        ]

    }.


forbid_index_all({Idx, Db}) ->
    ok = config:set("mango", "index_all_disabled", "true"),
    ?_assertThrow({mango_error, ?MODULE, index_all_disabled},
        validate_new(Idx, Db)
    ).


default_and_false_index_all({Idx, Db}) ->
    {ok, #idx{def={Def}}} = validate_new(Idx, Db),
    Fields = couch_util:get_value(fields, Def),
    ?_assertEqual(all_fields, Fields),
    ok = config:set("mango", "index_all_disabled", "false"),
    {ok, #idx{def={Def2}}} = validate_new(Idx, Db),
    Fields2 = couch_util:get_value(fields, Def2),
    ?_assertEqual(all_fields, Fields2).


warn_index_all({Idx, Db}) ->
    ok = config:set("mango", "index_all_disabled", "warn"),
    ?_assertThrow({test_error, logged_warning}, validate_new(Idx, Db)).


-endif.
