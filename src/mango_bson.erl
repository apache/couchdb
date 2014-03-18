-module(mango_bson).


-export([
    to_ejson/1,
    to_ejson/2,
    
    from_ejson/1
]).


-define(V_FLOAT, 16#01).
-define(V_STRING, 16#02).
-define(V_DOC, 16#03).
-define(V_ARRAY, 16#04).
-define(V_BINARY, 16#05).
-define(V_UNDEFINED, 16#06).
-define(V_OBJID, 16#07).
-define(V_BOOLEAN, 16#08).
-define(V_DATETIME, 16#09).
-define(V_NULL, 16#0A).
-define(V_REGEX, 16#0B).
-define(V_DBPOINTER, 16#0C).
-define(V_CODE, 16#0D).
-define(V_SYMBOL, 16#0E).
-define(V_CODE_W_S, 16#0F).
-define(V_INT32, 16#10).
-define(V_TIMESTAMP, 16#11).
-define(V_INT64, 16#12).
-define(V_MIN, 16#FF).
-define(V_MAX, 16#7F).


to_ejson(Data) ->
    to_json(Data, []).


to_ejson(Data, Opts) ->
    ReturnRest = lists:member(return_rest, Opts),
    case parse(Data) of
        {ok, Value, Rest} when ReturnRest ->
            {ok, Value, Rest};
        {ok, Value, <<>>} ->
            {ok, Value};
        {ok, _Value, Garbage} ->
            {error, {trailing_data, Garbage}};
        Error ->
            Error
    end.


from_ejson(Docs) when is_list(Docs) ->
    render(Docs, []);
frome_ejson(Doc) when tuple_size(Doc) == 1 ->
    render([Doc], []).


parse(Data) ->
    case Data of
        <<Size:4/little-integer, Rest/binary>> ->
            case Size =< size(Rest) of
                true ->
                    case parse_props(Rest, []) of
                        {ok, Props, <<0:1/integer, Rest/binary>>} ->
                            {ok, {Props}, Rest};
                        {ok, _, _} ->
                            {error, missing_final_null_byte};
                        Error ->
                            Error
                    end;
                false ->
                    {error, truncated_doc}
            end;
        _ ->
            {error, truncated_size}
    end.


parse_props(Data, Acc) ->
    case Data of
        <<0:1/integer, Rest/binary>> ->
            {ok, lists:reverse(Acc), Rest};
        <<Type:1/integer, Rest0/binary>> ->
            case parse_cstring(Rest0, 0) of
                {ok, Key, Rest1} ->
                    case parse_val(Type, Rest1) of
                        {ok, Val, Rest2} ->
                            parse_props(Rest2, [{Key, Val} | Acc]);
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        <<>> ->
            {error, truncated_list}
    end.


parse_val(?V_FLOAT, Data) ->
    case Data of
        <<Val:8/float, Rest/binary>> ->
            {Val, Rest};
        _ ->
            {error, truncated_float}
    end;
parse_val(?V_STRING, Data) ->
    parse_string(Data);
parse_val(?V_DOC, Data) ->
    parse(Data);
parse_val(?V_ARRAY, Data) ->
    case parse(Data) of
        {ok, {Props}, Rest} ->
            Vals = [V || {_, V} <- Props],
            {ok, Vals, Rest};
        Error ->
            Error
    end;
parse_val(?V_BINARY, Data) ->
    case Data of
        <<Size:4/little-integer, Type:1/integer, Rest0/binary>> ->
            case Size =< size(Rest0) of
                true ->
                    <<Val0:Size/binary, Rest1/binary>> = Rest0,
                    Val = {[
                        {<<"$binary">>, base64:encode(Val0)},
                        {<<"$type">>, to_hex(Type)}
                    ]},
                    {ok, Val, Rest1};
                false ->
                    {error, truncated_binary}
            end;
        _ ->
            {error, truncated_binary_size}
    end;
parse_val(?V_UNDEFINED, Data) ->
    {ok, {[{<<"$undefined">>, true}]}, Data};
parse_val(?V_OBJID, Data) ->
    parse_objid(Data);
parse_val(?V_BOOLEAN, Data) ->
    case Data of
        <<0:1/integer, Rest/binary>> ->
            {ok, false, Rest};
        <<1:1/integer, Rest/binary>> ->
            {ok, true, Rest};
        _ ->
            {error, truncated_boolean}
    end;
parse_val(?V_DATETIME, Data) ->
    case Data of
        <<Val0:8/little-integer, Rest/binary>> ->
            Val = {[{<<"$date">>, Val0}]},
            {ok, Val, Rest};
        _ ->
            {error, truncated_datetime}
    end;
parse_val(?V_NULL, Data) ->
    {ok, null, Data};
parse_val(?V_REGEX, Data) ->
    case parse_cstring(Data, 0) of
        {ok, Pattern, Rest0} ->
            case parse_cstring(Rest0, 0) of
                {ok, Flags, Rest1} ->
                    Val = {[
                        {<<"$regex">>, Pattern},
                        {<<"$options">>, Flags}
                    ]},
                    {ok, Val, Rest1};
                _ ->
                    {error, truncated_regex_flags}
            end;
        _ ->
            {error, truncated_regex_pattern}
    end;
parse_val(?V_DBPOINTER, Data) ->
    case parse_string(Data) of
        {ok, Ref, Rest0} ->
            case parse_objid(Rest0) of
                {ok, ObjId, Rest1} ->
                    Val = {[
                        {<<"$ref">>, Ref},
                        {<<"$id">>, ObjId}
                    ]},
                    {ok, Val, Rest1};
                _ ->
                    {error, truncated_ref_id}
            end;
        _ ->
            {error, truncated_ref_name}
    end;
parse_val(?V_CODE, Data) ->
    case parse_string(Data) of
        {ok, Code, Rest} ->
            {ok, {[{<<"$code">>, Code}]}, Rest};
        Error ->
            Error
    end;
parse_val(?V_SYMBOL, Data) ->
    parse_string(Data);
parse_val(?V_CODE_W_S, Data) ->
    % Yuck...
    case Data of
        <<Size:4/little-integer, Rest0/binary>> ->
            case parse_string(Rest0) of
                {ok, Code, Rest1} ->
                    case parse(Rest1) of
                        {ok, Scope, Rest2} ->
                            Val = {[
                                {<<"$code">>, Code},
                                {<<"$scope">>, Scope}
                            ]},
                            {ok, Val, Rest2};
                        Error->
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            {error, truncated_code_w_s_size}
    end;
parse_val(?V_INT32, Data) ->
    case Data of
        <<Val:4/little-integer, Rest/binary>> ->
            {ok, Val, Rest};
        _ ->
            {error, truncated_int32}
    end;
parse_val(?V_TIMESTAMP, Data) ->
    case Data of
        <<TS:4/little-integer, Inc:4/little-integer, Rest/binary>> ->
            Val = {[
                {<<"$timestamp">>, {[
                    {<<"t">>, TS},
                    {<<"i">>, Inc}
                ]}}
            ]},
            {ok, Val, Rest};
        _ ->
            {error, truncated_timestamp}
    end;
parse_val(?V_INT64, Data) ->
    case Data of
        <<Val:8/little-integer, Rest/binary>> ->
            {ok, Val, Rest};
        _ ->
            {error, truncated_int64}
    end;
parse_val(?V_MIN, Data) ->
    {ok, {[{<<"$minKey">>, 1}]}, Data};
parse_val(?V_MAX, Data) ->
    {ok, {[{<<"$maxKey">>, 1}]}, Data};
parse_val(Type, _Data) ->
    {error, {unknown_type, Type}}.


render([Doc | Rest], Acc) ->
    AsBin = render_doc(Doc),
    render(Rest, NewAcc).


render_doc({[{<<"$binary">>, Bin}, {<<"$type">>, Type}]}) ->
    binary;
render_doc({[{<<"$type">>, Type}, {<<"$binary">>, Bin}]}) ->
    binary;
render_doc({[{<<"$undefined">>, true}]}) ->
    undefined;
render_doc({[{<<"$id">>, ObjId}]}) ->
    objid;
render_doc({[{<<"$date">>, Date}]}) ->
    datetime;
render_doc({[{<<"$regex">>, Regex}, {<<"$options">>, Opts}]}) ->
    regex;
render_doc({[{<<"$options">>, Opts}, {<<"$regex">>, RegEx}]}) ->
    regex;
render_doc({[{<<"$ref">>, Ref}, {<<"$id">>, Id}]}) ->
    dbref;
render_doc({[{<<"$id">>, Id}, {<<"$ref">>, Ref}]}) ->
    dbref;
render_doc({[{<<"$timestamp">>, Doc}]}) ->
    timestamp;
render_doc({[{<<"$code">>, Code}]}) ->
    code;
render_doc({[{<<"$code">>, Code}, {<<"$scope">>, Scop}]}) ->
    code_w_s;
render_doc({[{<<"$scope">>, Scope}, {<<"$code">>, Code}]}) ->
    code_w_s;
render_doc({[{<<"$minKey">>, 1}]}) ->
    minkey;
render_doc({[{<<"$maxKey">>, 1}]}) ->
    maxkey;
render_doc({Props}) ->
    render_props(Props, []).


render_props([{Key, Val} | Rest], Acc) ->
    KeyBin = render_cstring(Key),
    {Type, ValBin} = render_val(Val),
    

parse_cstring(Data, O) ->
    case Data of
        <<Key:O/binary, 0, Rest/binary>> ->
            {ok, Key, Rest};
        <<_:O/binary>> ->
            {error, truncated_key};
        _ ->
            parse_key(Data, O)
    end.


render_cstring(Bin) when is_binary(Bin) ->
    ok.


parse_string(Data) ->
    case Data of
        <<Size:4/little-integer, Rest0/binary>> ->
            case Size =< size(Rest0) of
                true ->
                    <<Val:Size/binary, Rest1/binary>> = Rest0,
                    {ok, Val, Rest1};
                false ->
                    {error, truncated_string}
            end;
        _ ->
            {error, truncated_string_size}
    end.


render_string(Data) ->
    ok.


parse_objid(Data) ->
    case 12 =< size(Data) of
        true ->
            <<Id:12/binary, Rest/binary>> = Data,
            Val = {[{<<"$id">>, to_hex(Id)}]},
            {ok, Val, Rest};
        false ->
            {error, truncated_object_id}
    end.


render_objid(ObjId) when is_binary(ObjId), size(ObjId) == 12 ->
    ok.


to_hex(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
to_hex(<<V:1/integer, Rest/binary>>, Acc) ->
    NewAcc = [hex_dig(V rem 16), hex_dig(V div 16) | Acc],
    to_hex(Rest, NewAcc).


hex_dig(N) when N < 10 ->
    $0 + N;
hex_dig(N) ->
    $A + (N - 10).
