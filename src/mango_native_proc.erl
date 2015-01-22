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

-module(mango_native_proc).
-behavior(gen_server).


-export([
    start_link/0,
    set_timeout/2,
    prompt/2
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-record(st, {
    indexes = [],
    timeout = 5000
}).


-record(tacc, {
    fields = all_fields,
    path = []
}).


start_link() ->
    gen_server:start_link(?MODULE, [], []).


set_timeout(Pid, TimeOut) when is_integer(TimeOut), TimeOut > 0 ->
    gen_server:call(Pid, {set_timeout, TimeOut}).


prompt(Pid, Data) ->
    gen_server:call(Pid, {prompt, Data}).


init(_) ->
    {ok, #st{}}.


terminate(_Reason, _St) ->
    ok.


handle_call({set_timeout, TimeOut}, _From, St) ->
    {reply, ok, St#st{timeout=TimeOut}};

handle_call({prompt, [<<"reset">>]}, _From, St) ->
    {reply, true, St#st{indexes=[]}};

handle_call({prompt, [<<"reset">>, _QueryConfig]}, _From, St) ->
    {reply, true, St#st{indexes=[]}};

handle_call({prompt, [<<"add_fun">>, IndexInfo]}, _From, St) ->
    Indexes = St#st.indexes ++ [IndexInfo],
    NewSt = St#st{indexes = Indexes},
    {reply, true, NewSt};

handle_call({prompt, [<<"map_doc">>, Doc]}, _From, St) ->
    {reply, map_doc(St, mango_json:to_binary(Doc)), St};

handle_call({prompt, [<<"reduce">>, _, _]}, _From, St) ->
    {reply, null, St};

handle_call({prompt, [<<"rereduce">>, _, _]}, _From, St) ->
    {reply, null, St};

handle_call({prompt, [<<"index_doc">>, Doc]}, _From, St) ->
    {reply, index_doc(St, mango_json:to_binary(Doc)), St};

handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.


handle_cast(garbage_collect, St) ->
    erlang:garbage_collect(),
    {noreply, St};

handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


map_doc(#st{indexes=Indexes}, Doc) ->
    lists:map(fun(Idx) -> get_index_entries(Idx, Doc) end, Indexes).


index_doc(#st{indexes=Indexes}, Doc) ->
    lists:map(fun(Idx) -> get_text_entries(Idx, Doc) end, Indexes).


get_index_entries({IdxProps}, Doc) ->
    {Fields} = couch_util:get_value(<<"fields">>, IdxProps),
    Values = lists:map(fun({Field, _Dir}) ->
        case mango_doc:get_field(Doc, Field) of
            not_found -> not_found;
            bad_path -> not_found;
            Else -> Else
        end
    end, Fields),
    case lists:member(not_found, Values) of
        true ->
            [];
        false ->
            [[Values, null]]
    end.


get_text_entries({IdxProps}, Doc) ->
    Selector = case couch_util:get_value(<<"selector">>, IdxProps) of
        [] -> {[]};
        Else -> Else
    end,
    case should_index(Selector, Doc) of
        true ->
            get_text_entries0(IdxProps, Doc);
        false ->
            []
    end.


get_text_entries0(IdxProps, Doc) ->
    DefaultEnabled = get_default_enabled(IdxProps),
    FieldsList = get_text_field_list(IdxProps),
    TAcc = #tacc{fields = FieldsList},
    Fields0 = get_text_field_values(Doc, TAcc),
    Fields = if not DefaultEnabled -> Fields0; true ->
        add_default_text_field(Fields0)
    end,
    FieldNames = get_field_names(Fields, []),
    Converted = convert_text_fields(Fields),
    FieldNames ++ Converted.


get_text_field_values({Props}, TAcc) when is_list(Props) ->
    get_text_field_values_obj(Props, TAcc, []);

get_text_field_values(Values, TAcc) when is_list(Values) ->
    NewPath = ["[]" | TAcc#tacc.path],
    NewTAcc = TAcc#tacc{path = NewPath},
    % We bypass make_text_field and directly call make_text_field_name
    % because the length field name is not part of the path.
    LengthFieldName = make_text_field_name(NewTAcc#tacc.path, <<"length">>),
    LengthField = [{escape_name_parts(LengthFieldName), <<"length">>,
        length(Values)}],
    get_text_field_values_arr(Values, NewTAcc, LengthField);

get_text_field_values(Bin, TAcc) when is_binary(Bin) ->
    make_text_field(TAcc, <<"string">>, Bin);

get_text_field_values(Num, TAcc) when is_number(Num) ->
    make_text_field(TAcc, <<"number">>, Num);

get_text_field_values(Bool, TAcc) when is_boolean(Bool) ->
    make_text_field(TAcc, <<"boolean">>, Bool);

get_text_field_values(null, TAcc) ->
    make_text_field(TAcc, <<"null">>, true).


get_text_field_values_obj([], _, FAcc) ->
    FAcc;
get_text_field_values_obj([{Key, Val} | Rest], TAcc, FAcc) ->
    NewPath = [Key | TAcc#tacc.path],
    NewTAcc = TAcc#tacc{path = NewPath},
    Fields = get_text_field_values(Val, NewTAcc),
    get_text_field_values_obj(Rest, TAcc, Fields ++ FAcc).


get_text_field_values_arr([], _, FAcc) ->
    FAcc;
get_text_field_values_arr([Value | Rest], TAcc, FAcc) ->
    Fields = get_text_field_values(Value, TAcc),
    get_text_field_values_arr(Rest, TAcc, Fields ++ FAcc).


get_default_enabled(Props) ->
    case couch_util:get_value(<<"default_field">>, Props, {[]}) of
        Bool when is_boolean(Bool) ->
            Bool;
        {[]} ->
            true;
        {Opts}->
            couch_util:get_value(<<"enabled">>, Opts, true)
    end.


add_default_text_field(Fields) ->
    DefaultFields = add_default_text_field(Fields, []),
    DefaultFields ++ Fields.


add_default_text_field([], Acc) ->
    Acc;
add_default_text_field([{_Name, <<"string">>, Value} | Rest], Acc) ->
    NewAcc = [{<<"$default">>, <<"string">>, Value} | Acc],
    add_default_text_field(Rest, NewAcc);
add_default_text_field([_ | Rest], Acc) ->
    add_default_text_field(Rest, Acc).


%% index of all field names
get_field_names([], FAcc) ->
    FAcc;
get_field_names([{Name, _Type, _Value} | Rest], FAcc) ->
    case lists:member([<<"$fieldnames">>, Name, []], FAcc) of
        true ->
            get_field_names(Rest, FAcc);
        false ->
            get_field_names(Rest, [[<<"$fieldnames">>, Name, []] | FAcc])
    end.


convert_text_fields([]) ->
    [];
convert_text_fields([{Name, _Type, Value} | Rest]) ->
    [[Name, Value, []] | convert_text_fields(Rest)].


should_index(Selector, Doc) ->
    % We should do this
    NormSelector = mango_selector:normalize(Selector),
    Matches = mango_selector:match(NormSelector, Doc),
    IsDesign = case mango_doc:get_field(Doc, <<"_id">>) of
        <<"_design/", _/binary>> -> true;
        _ -> false
    end,
    Matches and not IsDesign.


get_text_field_list(IdxProps) ->
    case couch_util:get_value(<<"fields">>, IdxProps) of
        Fields when is_list(Fields) ->
            lists:flatmap(fun get_text_field_info/1, Fields);
        _ ->
            all_fields
    end.


get_text_field_info({Props}) ->
    Name = couch_util:get_value(<<"name">>, Props),
    Type0 = couch_util:get_value(<<"type">>, Props),
    if not is_binary(Name) -> []; true ->
        Type = get_text_field_type(Type0),
        [iolist_to_binary([Name, ":", Type])]
    end.


get_text_field_type(<<"number">>) ->
    <<"number">>;
get_text_field_type(<<"boolean">>) ->
    <<"boolean">>;
get_text_field_type(_) ->
    <<"string">>.


make_text_field(TAcc, Type, Value) ->
    FieldName = make_text_field_name(TAcc#tacc.path, Type),
    Fields = case TAcc#tacc.fields of
        Fields0 when is_list(Fields0) ->
            % for field names such as "name\\.first"
            PFields = [mango_doc:parse_field(F) || F <- Fields0],
            [iolist_to_binary(mango_util:join(".", P)) || {ok, P} <- PFields];
        _ ->
            all_fields
    end,
    % need to convert the fieldname to binary but not escape it in order
    % to compare with the user field names.
    BName = iolist_to_binary(FieldName),
    case Fields == all_fields orelse lists:member(BName, Fields) of
        true ->
            [{escape_name_parts(FieldName), Type, Value}];
        false ->
            []
    end.


make_text_field_name([P | Rest], Type) ->
    make_text_field_name0(Rest, [P, ":", Type]).

make_text_field_name0([], Name) ->
    Name;
make_text_field_name0([P | Rest], Name) ->
    make_text_field_name0(Rest, [P, "." | Name]).


escape_name_parts(Name) ->
    EscapedName = lists:map(fun(N) ->
        case N of
            "." ->
                ".";
            Else ->
                mango_util:lucene_escape_field(Else)
        end
    end, Name),
    iolist_to_binary(EscapedName).
