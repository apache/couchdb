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

-module(json_stream_parse).


-export([events/2, to_ejson/1, collect_object/2]).

-define(IS_WS(X), (X == $\  orelse X == $\t orelse X == $\n orelse X == $\r)).
-define(IS_DELIM(X), (X == $} orelse X == $] orelse X == $,)).
-define(IS_DIGIT(X), (X >= $0 andalso X =< $9)).



% Parses the json into events.
%
% The DataFun param is a function that produces the data for parsing. When
% called it must yield a tuple, or the atom done. The first element in the
% tuple is the data itself, and the second element is a function to be called
% next to get the next chunk of data in the stream.
%
% The EventFun is called everytime a json element is parsed. It must produce
% a new function to be called for the next event.
%
% Events happen each time a new element in the json string is parsed.
% For simple value types, the data itself is returned:
% Strings
% Integers
% Floats
% true
% false
% null
%
% For arrays, the start of the array is signaled by the event array_start
% atom. The end is signaled by array_end. The events before the end are the
% values, or nested values.
%
% For objects, the start of the object is signaled by the event object_start
% atom. The end is signaled by object_end. Each key is signaled by
% {key, KeyString}, and the following event is the value, or start of the
% value (array_start, object_start).
%
events(Data,EventFun) when is_list(Data)->
    events(list_to_binary(Data),EventFun);
events(Data,EventFun) when is_binary(Data)->
    events(fun() -> {Data, fun() -> done end} end,EventFun);
events(DataFun,EventFun) ->
    parse_one(DataFun, EventFun, <<>>).

% converts the JSON directly to the erlang represention of Json
to_ejson(DF) ->
    {_DF2, EF, _Rest} = events(DF, fun(Ev) -> collect_events(Ev, []) end),
    [[EJson]] = make_ejson(EF(get_results), [[]]),
    EJson.


% This function is used to return complete objects while parsing streams.
%
% Return this function from inside an event function right after getting an
% object_start event. It then collects the remaining events for that object
% and converts it to the erlang represention of Json.
%
% It then calls your ReturnControl function with the erlang object. Your
% return control function then should yield another event function.
%
% This example stream parses an array of objects, calling
% fun do_something_with_the_object/1 for each object.
%
%    ev_array(array_start) ->
%        fun(Ev) -> ev_object_loop(Ev) end.
%
%    ev_object_loop(object_start) ->
%        fun(Ev) ->
%            json_stream_parse:collect_object(Ev,
%                fun(Obj) ->
%                    do_something_with_the_object(Obj),
%                    fun(Ev2) -> ev_object_loop(Ev2) end
%                end)
%        end;
%    ev_object_loop(array_end) ->
%        ok
%    end.
%
%    % invoke the parse
%    main() ->
%        ...
%        events(Data, fun(Ev) -> ev_array(Ev) end).

collect_object(Ev, ReturnControl) ->
    collect_object(Ev, 0, ReturnControl, [object_start]).



% internal methods

parse_one(DF,EF,Acc) ->
    case toke(DF, Acc) of
    none ->
        none;
    {Token, DF2, Rest} ->
        case Token of
        "{" ->
            EF2 = EF(object_start),
            {DF3, EF3, Rest2} = parse_object(DF2, EF2, Rest),
            {DF3, EF3(object_end), Rest2};
        "[" ->
            EF2 = EF(array_start),
            {DF3, EF3, Rest2} = parse_array(DF2, EF2, Rest),
            {DF3, EF3(array_end), Rest2};
        Int when is_integer(Int)->
            {DF2, EF(Int), Rest};
        Float when is_float(Float)->
            {DF2, EF(Float), Rest};
        Atom when is_atom(Atom)->
            {DF2, EF(Atom), Rest};
        String when is_binary(String)->
            {DF2, EF(String), Rest};
        _OtherToken ->
            err(unexpected_token)
        end
    end.

must_parse_one(DF,EF,Acc,Error)->
    case parse_one(DF, EF, Acc) of
    none ->
        err(Error);
    Else ->
        Else
    end.

must_toke(DF, Data, Error) ->
    case toke(DF, Data) of
    none ->
        err(Error);
    Result ->
        Result
    end.

toke(DF, <<>>) ->
    case DF() of
    done ->
        none;
    {Data, DF2} ->
        toke(DF2, Data)
    end;
toke(DF, <<C,Rest/binary>>) when ?IS_WS(C)->
    toke(DF, Rest);
toke(DF, <<${,Rest/binary>>) ->
    {"{", DF, Rest};
toke(DF, <<$},Rest/binary>>) ->
    {"}", DF, Rest};
toke(DF, <<$[,Rest/binary>>) ->
    {"[", DF, Rest};
toke(DF, <<$],Rest/binary>>) ->
    {"]", DF, Rest};
toke(DF, <<$",Rest/binary>>) ->
    toke_string(DF,Rest,[]);
toke(DF, <<$,,Rest/binary>>) ->
    {",", DF, Rest};
toke(DF, <<$:,Rest/binary>>) ->
    {":", DF, Rest};
toke(DF, <<$-,Rest/binary>>) ->
    {<<C,_/binary>> = Data, DF2} = must_df(DF,1,Rest,expected_number),
    case ?IS_DIGIT(C) of
    true ->
        toke_number_leading(DF2, Data, "-");
    false ->
        err(expected_number)
    end;
toke(DF, <<C,_/binary>> = Data) when ?IS_DIGIT(C) ->
    toke_number_leading(DF, Data, []);
toke(DF, <<$t,Rest/binary>>) ->
    {Data, DF2} = must_match(<<"rue">>, DF, Rest),
    {true, DF2, Data};
toke(DF, <<$f,Rest/binary>>) ->
    {Data, DF2} = must_match(<<"alse">>, DF, Rest),
    {false, DF2, Data};
toke(DF, <<$n,Rest/binary>>) ->
    {Data, DF2} = must_match(<<"ull">>, DF, Rest),
    {null, DF2, Data};
toke(_, _) ->
    err(bad_token).


must_match(Pattern, DF, Data) ->
    Size = size(Pattern),
    case must_df(DF, Size, Data, bad_token) of
    {<<Pattern:Size/binary,Data2/binary>>, DF2} ->
        {Data2, DF2};
    {_, _} ->
        err(bad_token)
    end.

must_df(DF,Error)->
    case DF() of
    done ->
        err(Error);
    {Data, DF2} ->
        {Data, DF2}
    end.


must_df(DF,NeedLen,Acc,Error)->
    if size(Acc) >= NeedLen ->
        {Acc, DF};
    true ->
        case DF() of
        done ->
            err(Error);
        {Data, DF2} ->
            must_df(DF2, NeedLen, <<Acc/binary, Data/binary>>, Error)
        end
    end.


parse_object(DF,EF,Acc) ->
    case must_toke(DF, Acc, unterminated_object) of
    {String, DF2, Rest} when is_binary(String)->
        EF2 = EF({key,String}),
        case must_toke(DF2,Rest,unterminated_object) of
        {":", DF3, Rest2} ->
            {DF4, EF3, Rest3} = must_parse_one(DF3, EF2, Rest2, expected_value),
            case must_toke(DF4,Rest3, unterminated_object) of
            {",", DF5, Rest4} ->
                parse_object(DF5, EF3, Rest4);
            {"}", DF5, Rest4} ->
                {DF5, EF3, Rest4};
            {_, _, _} ->
                err(unexpected_token)
            end;
        _Else ->
            err(expected_colon)
        end;
    {"}", DF2, Rest} ->
        {DF2, EF, Rest};
    {_, _, _} ->
        err(unexpected_token)
    end.

parse_array0(DF,EF,Acc) ->
    case toke(DF, Acc) of
    none ->
        err(unterminated_array);
    {",", DF2, Rest} ->
        parse_array(DF2,EF,Rest);
    {"]", DF2, Rest} ->
        {DF2,EF,Rest};
    _ ->
        err(unexpected_token)
    end.

parse_array(DF,EF,Acc) ->
    case toke(DF, Acc) of
    none ->
         err(unterminated_array);
    {Token, DF2, Rest} ->
        case Token of
        "{" ->
            EF2 = EF(object_start),
            {DF3, EF3, Rest2} = parse_object(DF2, EF2, Rest),
            parse_array0(DF3, EF3(object_end), Rest2);
        "[" ->
            EF2 = EF(array_start),
            {DF3, EF3, Rest2} = parse_array(DF2, EF2, Rest),
            parse_array0(DF3, EF3(array_end), Rest2);
        Int when is_integer(Int)->
            parse_array0(DF2, EF(Int), Rest);
        Float when is_float(Float)->
            parse_array0(DF2, EF(Float), Rest);
        Atom when is_atom(Atom)->
            parse_array0(DF2, EF(Atom), Rest);
        String when is_binary(String)->
            parse_array0(DF2, EF(String), Rest);
        "]" ->
            {DF2, EF, Rest};
        _ ->
            err(unexpected_token)
        end
    end.


toke_string(DF, <<>>, Acc) ->
    {Data, DF2} = must_df(DF, unterminated_string),
    toke_string(DF2, Data, Acc);
toke_string(DF, <<$\\,$",Rest/binary>>, Acc) ->
    toke_string(DF, Rest, [$" | Acc]);
toke_string(DF, <<$\\,$\\,Rest/binary>>, Acc) ->
    toke_string(DF, Rest, [$\\ | Acc]);
toke_string(DF, <<$\\,$/,Rest/binary>>, Acc) ->
    toke_string(DF, Rest, [$/ | Acc]);
toke_string(DF, <<$\\,$b,Rest/binary>>, Acc) ->
    toke_string(DF, Rest, [$\b | Acc]);
toke_string(DF, <<$\\,$f,Rest/binary>>, Acc) ->
    toke_string(DF, Rest, [$\f | Acc]);
toke_string(DF, <<$\\,$n,Rest/binary>>, Acc) ->
    toke_string(DF, Rest, [$\n | Acc]);
toke_string(DF, <<$\\,$r,Rest/binary>>, Acc) ->
    toke_string(DF, Rest, [$\r | Acc]);
toke_string(DF, <<$\\,$t,Rest/binary>>, Acc) ->
    toke_string(DF, Rest, [$\t | Acc]);
toke_string(DF, <<$\\,$u,Rest/binary>>, Acc) ->
    {<<A,B,C,D,Data/binary>>, DF2} = must_df(DF,4,Rest,missing_hex),
    UTFChar = erlang:list_to_integer([A, B, C, D], 16),
    if UTFChar == 16#FFFF orelse UTFChar == 16#FFFE ->
        err(invalid_utf_char);
    true ->
        ok
    end,
    Chars = xmerl_ucs:to_utf8(UTFChar),
    toke_string(DF2, Data, lists:reverse(Chars) ++ Acc);
toke_string(DF, <<$\\>>, Acc) ->
    {Data, DF2} = must_df(DF, unterminated_string),
    toke_string(DF2, <<$\\,Data/binary>>, Acc);
toke_string(_DF, <<$\\, _/binary>>, _Acc) ->
    err(bad_escape);
toke_string(DF, <<$", Rest/binary>>, Acc) ->
    {list_to_binary(lists:reverse(Acc)), DF, Rest};
toke_string(DF, <<C, Rest/binary>>, Acc) ->
    toke_string(DF, Rest, [C | Acc]).


toke_number_leading(DF, <<Digit,Rest/binary>>, Acc)
        when ?IS_DIGIT(Digit) ->
    toke_number_leading(DF, Rest, [Digit | Acc]);
toke_number_leading(DF, <<C,_/binary>>=Rest, Acc)
        when ?IS_WS(C) orelse ?IS_DELIM(C) ->
    {list_to_integer(lists:reverse(Acc)), DF, Rest};
toke_number_leading(DF, <<>>, Acc) ->
    case DF() of
    done ->
         {list_to_integer(lists:reverse(Acc)), fun() -> done end, <<>>};
    {Data, DF2} ->
        toke_number_leading(DF2, Data, Acc)
    end;
toke_number_leading(DF, <<$., Rest/binary>>, Acc) ->
    toke_number_trailing(DF, Rest, [$.|Acc]);
toke_number_leading(DF, <<$e, Rest/binary>>, Acc) ->
    toke_number_exponent(DF, Rest, [$e, $0, $.|Acc]);
toke_number_leading(DF, <<$E, Rest/binary>>, Acc) ->
    toke_number_exponent(DF, Rest, [$e, $0, $.|Acc]);
toke_number_leading(_, _, _) ->
    err(unexpected_character_in_number).

toke_number_trailing(DF, <<Digit,Rest/binary>>, Acc)
        when ?IS_DIGIT(Digit) ->
    toke_number_trailing(DF, Rest, [Digit | Acc]);
toke_number_trailing(DF, <<C,_/binary>>=Rest, Acc)
        when ?IS_WS(C) orelse ?IS_DELIM(C) ->
    {list_to_float(lists:reverse(Acc)), DF, Rest};
toke_number_trailing(DF, <<>>, Acc) ->
    case DF() of
    done ->
        {list_to_float(lists:reverse(Acc)), fun() -> done end, <<>>};
    {Data, DF2} ->
        toke_number_trailing(DF2, Data, Acc)
    end;
toke_number_trailing(DF, <<"e", Rest/binary>>, [C|_]=Acc) when C /= $. ->
    toke_number_exponent(DF, Rest, [$e|Acc]);
toke_number_trailing(DF, <<"E", Rest/binary>>, [C|_]=Acc) when C /= $. ->
    toke_number_exponent(DF, Rest, [$e|Acc]);
toke_number_trailing(_, _, _) ->
    err(unexpected_character_in_number).


toke_number_exponent(DF, <<Digit,Rest/binary>>, Acc) when ?IS_DIGIT(Digit) ->
    toke_number_exponent(DF, Rest, [Digit | Acc]);
toke_number_exponent(DF, <<Sign,Rest/binary>>, [$e|_]=Acc)
        when Sign == $+ orelse Sign == $- ->
    toke_number_exponent(DF, Rest, [Sign | Acc]);
toke_number_exponent(DF, <<C,_/binary>>=Rest, Acc)
        when ?IS_WS(C) orelse ?IS_DELIM(C) ->
    {list_to_float(lists:reverse(Acc)), DF, Rest};
toke_number_exponent(DF, <<>>, Acc) ->
    case DF() of
    done ->
        {list_to_float(lists:reverse(Acc)), fun() -> done end, <<>>};
    {Data, DF2} ->
        toke_number_exponent(DF2, Data, Acc)
    end;
toke_number_exponent(_, _, _) ->
        err(unexpected_character_in_number).


err(Error)->
    throw({parse_error,Error}).


make_ejson([], Stack) ->
    Stack;
make_ejson([array_start | RevEvs], [ArrayValues, PrevValues | RestStack]) ->
    make_ejson(RevEvs, [[ArrayValues | PrevValues] | RestStack]);
make_ejson([array_end | RevEvs], Stack) ->
    make_ejson(RevEvs, [[] | Stack]);
make_ejson([object_start | RevEvs], [ObjValues, PrevValues | RestStack]) ->
    make_ejson(RevEvs, [[{ObjValues} | PrevValues] | RestStack]);
make_ejson([object_end | RevEvs], Stack) ->
    make_ejson(RevEvs, [[] | Stack]);
make_ejson([{key, String} | RevEvs], [[PrevValue|RestObject] | RestStack] = _Stack) ->
    make_ejson(RevEvs, [[{String, PrevValue}|RestObject] | RestStack]);
make_ejson([Value | RevEvs], [Vals | RestStack] = _Stack) ->
    make_ejson(RevEvs, [[Value | Vals] | RestStack]).

collect_events(get_results, Acc) ->
    Acc;
collect_events(Ev, Acc) ->
    fun(NextEv) -> collect_events(NextEv, [Ev | Acc]) end.


collect_object(object_end, 0, ReturnControl, Acc) ->
    [[Obj]] = make_ejson([object_end | Acc], [[]]),
    ReturnControl(Obj);
collect_object(object_end, NestCount, ReturnControl, Acc) ->
    fun(Ev) ->
        collect_object(Ev, NestCount - 1, ReturnControl, [object_end | Acc])
    end;
collect_object(object_start, NestCount, ReturnControl, Acc) ->
    fun(Ev) ->
        collect_object(Ev, NestCount + 1, ReturnControl, [object_start | Acc])
    end;
collect_object(Ev, NestCount, ReturnControl, Acc) ->
    fun(Ev2) ->
        collect_object(Ev2, NestCount, ReturnControl, [Ev | Acc])
    end.
