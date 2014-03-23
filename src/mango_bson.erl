-module(mango_bson).


-export([
    to_ejson/1,
    to_ejson/2,

    from_ejson/1,
    
    current_time/0
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
    to_ejson(Data, []).


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
from_ejson(Doc) when tuple_size(Doc) == 1 ->
    render([Doc], []).


current_time() ->
    {Mega, Secs, Micro} = os:timestamp(),
    InMicro = Mega * 1000000000000 + Secs * 1000000 + Micro,
    InMilli = InMicro div 1000,
    {[{<<"$date">>, InMilli}]}.


parse(Data) ->
    case Data of
        <<Size:32/little-integer, Rest0/binary>> ->
            case Size - 4 =< size(Rest0) of
                true ->
                    case parse_props(Rest0, []) of
                        {ok, Props, Rest1} ->
                            {ok, {Props}, Rest1};
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
        % We can detect the end of the document when we hit
        % the final null byte instead of a type.
        <<0:8/integer, Rest/binary>> ->
            {ok, lists:reverse(Acc), Rest};
        <<Type:8/integer, Rest0/binary>> ->
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
        <<Val/float, Rest/binary>> ->
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
        <<Size:32/little-integer, Type:8/integer, Rest0/binary>> ->
            case Size =< size(Rest0) of
                true ->
                    <<Val0:Size/binary, Rest1/binary>> = Rest0,
                    Val = {[
                        {<<"$binary">>, base64:encode(Val0)},
                        {<<"$type">>, to_hex(<<Type:8/integer>>)}
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
        <<0:8/integer, Rest/binary>> ->
            {ok, false, Rest};
        <<1:8/integer, Rest/binary>> ->
            {ok, true, Rest};
        _ ->
            {error, truncated_boolean}
    end;
parse_val(?V_DATETIME, Data) ->
    case Data of
        <<Val0:64/little-integer, Rest/binary>> ->
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
        <<_Size:32/little-integer, Rest0/binary>> ->
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
        <<Val:32/little-integer, Rest/binary>> ->
            {ok, Val, Rest};
        _ ->
            {error, truncated_int32}
    end;
parse_val(?V_TIMESTAMP, Data) ->
    case Data of
        <<TS:32/little-integer, Inc:32/little-integer, Rest/binary>> ->
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
        <<Val:32/little-integer, Rest/binary>> ->
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


render([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
render([Doc | Rest], Acc) ->
    {?V_DOC, AsBin0} = render_doc(Doc),
    AsBin = <<AsBin0/binary, 0:8/integer>>,
    % We add 4 for the size value itself
    Size = 4 + size(AsBin),
    render(Rest, [<<Size:32/little-integer, AsBin/binary>> | Acc]).


render_doc({[{<<"$binary">>, Bin}, {<<"$type">>, Type}]=P}) ->
    case is_binary(Bin) andalso is_integer(Type) of
        true ->
            Size = size(Bin),
            Data = <<
                Size:32/little-integer,
                Type:1/integer,
                Bin/binary
            >>,
            {?V_BINARY, Data};
        false ->
            render_props(P, [])
    end;
render_doc({[{<<"$type">>, Type}, {<<"$binary">>, Bin}]}) ->
    render_doc({[{<<"$binary">>, Bin}, {<<"$type">>, Type}]});
render_doc({[{<<"$undefined">>, true}]}) ->
    {?V_UNDEFINED, <<>>};
render_doc({[{<<"$id">>, ObjId}]=P}) ->
    case is_binary(ObjId) andalso size(ObjId) == 24 of
        true ->
            {?V_OBJID, dehex(ObjId)};
        false ->
            render_props(P, [])
    end;
render_doc({[{<<"$date">>, Date}]=P}) ->
    case is_integer(Date) of
        true ->
            {?V_DATETIME, <<Date:64/little-integer>>};
        false ->
            render_props(P, [])
    end;
render_doc({[{<<"$regex">>, Regex}, {<<"$options">>, Opts}]=P}) ->
    case is_binary(Regex) andalso is_binary(Opts) of
        true ->
            RStr = render_cstring(Regex),
            OStr = render_cstring(Opts),
            {?V_REGEX, <<RStr/binary, OStr/binary>>};
        false ->
            render_props(P, [])
    end;
render_doc({[{<<"$options">>, Opts}, {<<"$regex">>, RegEx}]}) ->
    render_doc({[{<<"$regex">>, RegEx}, {<<"$options">>, Opts}]});
render_doc({[{<<"$ref">>, Ref}, {<<"$id">>, Id}]=P}) ->
    case is_binary(Ref) andalso is_binary(Id) of
        true ->
            RStr = render_string(Ref),
            IDStr = dehex(Id),
            {?V_DBPOINTER, <<RStr/binary, IDStr/binary>>};
        false ->
            render_props(P, [])
    end;
render_doc({[{<<"$id">>, Id}, {<<"$ref">>, Ref}]}) ->
    render_doc({[{<<"$ref">>, Ref}, {<<"$id">>, Id}]});
render_doc({[{<<"$timestamp">>, Doc}]=P}) ->
    case Doc of
        {[{<<"t">>, T}, {<<"i">>, I}]} when is_integer(T), is_integer(I) ->
            {?V_TIMESTAMP, <<I:32/little-integer, T:32/little-integer>>};
        {[{<<"i">>, I}, {<<"t">>, T}]} when is_integer(T), is_integer(I) ->
            {?V_TIMESTAMP, <<I:32/little-integer, T:32/little-integer>>};
        _ ->
            parse_props(P, [])
    end;
render_doc({[{<<"$code">>, Code}]=P}) ->
    case is_binary(Code) of
        true ->
            {?V_CODE, render_string(Code)};
        false ->
            parse_props(P, [])
    end;
render_doc({[{<<"$code">>, Code}, {<<"$scope">>, Scope}]=P}) ->
    case is_binary(Code) andalso is_tuple(Scope) andalso size(Scope) == 1 of
        true ->
            CStr = render_string(Code),
            SStr = render_doc(Scope),
            {?V_CODE_W_S, <<CStr/binary, SStr/binary>>};
        false ->
            render_props(P, [])
    end;
render_doc({[{<<"$scope">>, Scope}, {<<"$code">>, Code}]}) ->
    render_doc({[{<<"$code">>, Code}, {<<"$scope">>, Scope}]});
render_doc({[{<<"$minKey">>, 1}]}) ->
    {?V_MIN, <<>>};
render_doc({[{<<"$maxKey">>, 1}]}) ->
    {?V_MAX, <<>>};
render_doc({Props}) ->
    render_props(Props, []).


render_props([], Acc) ->
    {?V_DOC, iolist_to_binary(lists:reverse(Acc))};
render_props([{Key, Val} | Rest], Acc) ->
    KeyBin = render_cstring(Key),
    {Type, ValBin} = render_val(Val),
    Entry = <<Type:8/integer, KeyBin/binary, ValBin/binary>>,
    render_props(Rest, [Entry | Acc]).


render_val(Num) when is_number(Num) ->
    % Handles ?V_FLOAT, ?V_INT32, ?V_INT64
    render_number(Num);
render_val(true) ->
    {?V_BOOLEAN, <<1>>};
render_val(false) ->
    {?V_BOOLEAN, <<0>>};
render_val(null) ->
    {?V_NULL, <<>>};
render_val(Bin) when is_binary(Bin) ->
    {?V_STRING, render_string(Bin)};
render_val({Props}) when is_list(Props) ->
    render_doc({Props});
render_val(Vals) when is_list(Vals) ->
    Keys = lists:map(fun(I) ->
        list_to_binary(integer_to_list(I))
    end, lists:seq(1, length(Vals) - 1)),
    Props = lists:zip(Keys, Vals),
    {?V_ARRAY, render_doc({Props})}.


parse_cstring(Data, O) ->
    case Data of
        <<Key:O/binary, 0, Rest/binary>> ->
            {ok, Key, Rest};
        <<_:O/binary>> ->
            {error, truncated_key};
        _ ->
            parse_cstring(Data, O+1)
    end.


render_cstring(Bin) when is_binary(Bin) ->
    case check_embedded_null(Bin, 0) of
        true ->
            throw({invalid_cstring, embedded_null});
        false ->
            <<Bin/binary, 0:8/integer>>
    end.


parse_string(Data) ->
    case Data of
        <<Size:32/little-integer, Rest0/binary>> ->
            case Size =< size(Rest0) of
                true ->
                    ValSize = Size - 1, % Trailing null byte
                    <<Val:ValSize/binary, 0:8/integer, Rest1/binary>> = Rest0,
                    try
                        % Assert that we got valid UTF-8
                        xmerl_ucs:from_utf8(binary_to_list(Val)),
                        {ok, Val, Rest1}
                    catch exit:{ucs,_} ->
                        {error, invalid_utf8}
                    end;
                false ->
                    {error, truncated_string}
            end;
        _ ->
            {error, truncated_string_size}
    end.


render_string(Data) when is_binary(Data) ->
    % Assume UTF-8 here?
    Size = size(Data) + 1,
    <<Size:32/little-integer, Data/binary, 0:8/integer>>.


parse_objid(Data) ->
    case 12 =< size(Data) of
        true ->
            <<Id:12/binary, Rest/binary>> = Data,
            Val = {[{<<"$id">>, to_hex(Id)}]},
            {ok, Val, Rest};
        false ->
            {error, truncated_object_id}
    end.


render_number(N) when is_float(N) ->
    {?V_FLOAT, <<N/float>>};
render_number(N) when N >= -2147483648, N =< 2147483647 ->
    {?V_INT32, <<N:32/little-integer>>};
render_number(N) when N >= -9223372036854775808, N =< 9223372036854775807 ->
    {?V_INT64, <<N:64/little-integer>>};
render_number(N) ->
    throw({invalid_integer, N}).


check_embedded_null(Bin, N) when N >= size(Bin) ->
    false;
check_embedded_null(Bin, N) ->
    case Bin of
        <<_:N/binary, 0, _/binary>> ->
            true;
        _ ->
            check_embedded_null(Bin, N+1)
    end.


to_hex(Bin) ->
    to_hex(Bin, []).


to_hex(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
to_hex(<<V:8/integer, Rest/binary>>, Acc) ->
    NewAcc = [hex_dig(V rem 16), hex_dig(V div 16) | Acc],
    to_hex(Rest, NewAcc).


hex_dig(N) when N >= 0, N < 10 ->
    $0 + N;
hex_dig(N) when N >= 10, N < 16 ->
    $A + (N - 10);
hex_dig(N) ->
    throw({invalid_hex_value, N}).


dehex(Bin) ->
    dehex(Bin, []).


dehex(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
dehex(<<A:8/integer, B:8/integer, Rest/binary>>, Acc) ->
    Val = (hex_val(A) bsl 4) bor (hex_val(B)),
    dehex(Rest, [Val | Acc]).


hex_val(N) when N >= $A, N =< $F ->
    (N - $A) + 10;
hex_val(N) when N >= $a, N =< $f ->
    (N - $a) + 10;
hex_val(N) when N - $0 > 0, N - $0 < 10 ->
    N - $0;
hex_val(N) ->
    throw({invalid_hex_character, N}).
