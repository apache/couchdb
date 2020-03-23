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

-module(mango_util).


-export([
    open_doc/2,
    open_ddocs/1,
    load_ddoc/2,
    load_ddoc/3,

    defer/3,
    do_defer/3,

    assert_ejson/1,

    to_lower/1,

    enc_dbname/1,
    dec_dbname/1,

    enc_hex/1,
    dec_hex/1,

    lucene_escape_field/1,
    lucene_escape_query_value/1,
    lucene_escape_user/1,
    is_number_string/1,

    has_suffix/2,

    join/2,

    parse_field/1,

    cached_re/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


-define(DIGITS, "(\\p{N}+)").
-define(HEXDIGITS, "([0-9a-fA-F]+)").
-define(EXP, "[eE][+-]?" ++ ?DIGITS).
-define(NUMSTRING,
"[\\x00-\\x20]*" ++ "[+-]?(" ++ "NaN|"
     ++ "Infinity|" ++ "((("
     ++ ?DIGITS
     ++ "(\\.)?("
     ++ ?DIGITS
     ++ "?)("
     ++ ?EXP
     ++ ")?)|"
     ++ "(\\.("
     ++ ?DIGITS
     ++ ")("
     ++ ?EXP
     ++ ")?)|"
     ++ "(("
     ++ "(0[xX]"
     ++ ?HEXDIGITS
     ++ "(\\.)?)|"
     ++ "(0[xX]"
     ++ ?HEXDIGITS
     ++ "?(\\.)"
     ++ ?HEXDIGITS
     ++ ")"
     ++ ")[pP][+-]?" ++ ?DIGITS ++ "))" ++ "[fFdD]?))" ++ "[\\x00-\\x20]*").


open_doc(Db, DocId) ->
    open_doc(Db, DocId, [deleted, ejson_body]).


open_doc(Db, DocId, Options) ->
    fabric2_db:open_doc(Db, DocId, Options).


open_ddocs(Db) ->
    case mango_util:defer(fabric, design_docs, [Db]) of
        {ok, Docs} ->
            {ok, Docs};
        _ ->
            ?MANGO_ERROR(error_loading_ddocs)
    end.


load_ddoc(Db, DDocId) ->
    load_ddoc(Db, DDocId, [deleted, ejson_body]).

load_ddoc(Db, DDocId, DbOpts) ->
    case open_doc(Db, DDocId, DbOpts) of
        {ok, Doc} ->
            {ok, check_lang(Doc)};
        {not_found, missing} ->
            Body = {[
                {<<"language">>, <<"query">>}
            ]},
            {ok, #doc{id = DDocId, body = Body}}
    end.


defer(Mod, Fun, Args) ->
    {Pid, Ref} = erlang:spawn_monitor(?MODULE, do_defer, [Mod, Fun, Args]),
    receive
        {'DOWN', Ref, process, Pid, {mango_defer_ok, Value}} ->
            Value;
        {'DOWN', Ref, process, Pid, {mango_defer_throw, Value}} ->
            erlang:throw(Value);
        {'DOWN', Ref, process, Pid, {mango_defer_error, Value}} ->
            erlang:error(Value);
        {'DOWN', Ref, process, Pid, {mango_defer_exit, Value}} ->
            erlang:exit(Value)
    end.


do_defer(Mod, Fun, Args) ->
    try erlang:apply(Mod, Fun, Args) of
        Resp ->
            erlang:exit({mango_defer_ok, Resp})
    catch
        throw:Error ->
            Stack = erlang:get_stacktrace(),
            couch_log:error("Defered error: ~w~n    ~p", [{throw, Error}, Stack]),
            erlang:exit({mango_defer_throw, Error});
        error:Error ->
            Stack = erlang:get_stacktrace(),
            couch_log:error("Defered error: ~w~n    ~p", [{error, Error}, Stack]),
            erlang:exit({mango_defer_error, Error});
        exit:Error ->
            Stack = erlang:get_stacktrace(),
            couch_log:error("Defered error: ~w~n    ~p", [{exit, Error}, Stack]),
            erlang:exit({mango_defer_exit, Error})
    end.


assert_ejson({Props}) ->
    assert_ejson_obj(Props);
assert_ejson(Vals) when is_list(Vals) ->
    assert_ejson_arr(Vals);
assert_ejson(null) ->
    true;
assert_ejson(true) ->
    true;
assert_ejson(false) ->
    true;
assert_ejson(String) when is_binary(String) ->
    true;
assert_ejson(Number) when is_number(Number) ->
    true;
assert_ejson(_Else) ->
    false.


assert_ejson_obj([]) ->
    true;
assert_ejson_obj([{Key, Val} | Rest]) when is_binary(Key) ->
    case assert_ejson(Val) of
        true ->
            assert_ejson_obj(Rest);
        false ->
            false
    end;
assert_ejson_obj(_Else) ->
    false.


assert_ejson_arr([]) ->
    true;
assert_ejson_arr([Val | Rest]) ->
    case assert_ejson(Val) of
        true ->
            assert_ejson_arr(Rest);
        false ->
            false
    end.


check_lang(#doc{id = Id, deleted = true}) ->
    Body = {[
        {<<"language">>, <<"query">>}
    ]},
    #doc{id = Id, body = Body};
check_lang(#doc{body = {Props}} = Doc) ->
    case lists:keyfind(<<"language">>, 1, Props) of
        {<<"language">>, <<"query">>} ->
            Doc;
        Else ->
            ?MANGO_ERROR({invalid_ddoc_lang, Else})
    end.


to_lower(Key) when is_binary(Key) ->
    KStr = binary_to_list(Key),
    KLower = string:to_lower(KStr),
    list_to_binary(KLower).


enc_dbname(<<>>) ->
    <<>>;
enc_dbname(<<A:8/integer, Rest/binary>>) ->
    Bytes = enc_db_byte(A),
    Tail = enc_dbname(Rest),
    <<Bytes/binary, Tail/binary>>.


enc_db_byte(N) when N >= $a, N =< $z -> <<N>>;
enc_db_byte(N) when N >= $0, N =< $9 -> <<N>>;
enc_db_byte(N) when N == $/; N == $_; N == $- -> <<N>>;
enc_db_byte(N) ->
    H = enc_hex_byte(N div 16),
    L = enc_hex_byte(N rem 16),
    <<$$, H:8/integer, L:8/integer>>.


dec_dbname(<<>>) ->
    <<>>;
dec_dbname(<<$$, _:8/integer>>) ->
    throw(invalid_dbname_encoding);
dec_dbname(<<$$, H:8/integer, L:8/integer, Rest/binary>>) ->
    Byte = (dec_hex_byte(H) bsl 4) bor dec_hex_byte(L),
    Tail = dec_dbname(Rest),
    <<Byte:8/integer, Tail/binary>>;
dec_dbname(<<N:8/integer, Rest/binary>>) ->
    Tail = dec_dbname(Rest),
    <<N:8/integer, Tail/binary>>.


enc_hex(<<>>) ->
    <<>>;
enc_hex(<<V:8/integer, Rest/binary>>) ->
    H = enc_hex_byte(V div 16),
    L = enc_hex_byte(V rem 16),
    Tail = enc_hex(Rest),
    <<H:8/integer, L:8/integer, Tail/binary>>.


enc_hex_byte(N) when N >= 0, N < 10 -> $0 + N;
enc_hex_byte(N) when N >= 10, N < 16 -> $a + (N - 10);
enc_hex_byte(N) -> throw({invalid_hex_value, N}).


dec_hex(<<>>) ->
    <<>>;
dec_hex(<<_:8/integer>>) ->
    throw(invalid_hex_string);
dec_hex(<<H:8/integer, L:8/integer, Rest/binary>>) ->
    Byte = (dec_hex_byte(H) bsl 4) bor dec_hex_byte(L),
    Tail = dec_hex(Rest),
    <<Byte:8/integer, Tail/binary>>.


dec_hex_byte(N) when N >= $0, N =< $9 -> (N - $0);
dec_hex_byte(N) when N >= $a, N =< $f -> (N - $a) + 10;
dec_hex_byte(N) when N >= $A, N =< $F -> (N - $A) + 10;
dec_hex_byte(N) -> throw({invalid_hex_character, N}).



lucene_escape_field(Bin) when is_binary(Bin) ->
    Str = binary_to_list(Bin),
    Enc = lucene_escape_field(Str),
    iolist_to_binary(Enc);
lucene_escape_field([H | T]) when is_number(H), H >= 0, H =< 255 ->
    if
        H >= $a, $z >= H ->
            [H | lucene_escape_field(T)];
        H >= $A, $Z >= H ->
            [H | lucene_escape_field(T)];
        H >= $0, $9 >= H ->
            [H | lucene_escape_field(T)];
        true ->
            Hi = enc_hex_byte(H div 16),
            Lo = enc_hex_byte(H rem 16),
            [$_, Hi, Lo | lucene_escape_field(T)]
        end;
lucene_escape_field([]) ->
    [].


lucene_escape_query_value(IoList) when is_list(IoList) ->
    lucene_escape_query_value(iolist_to_binary(IoList));
lucene_escape_query_value(Bin) when is_binary(Bin) ->
    IoList = lucene_escape_qv(Bin),
    iolist_to_binary(IoList).


% This escapes the special Lucene query characters
% listed below as well as any whitespace.
%
%   + - && || ! ( ) { } [ ] ^ ~ * ? : \ " /
%

lucene_escape_qv(<<>>) -> [];
lucene_escape_qv(<<"&&", Rest/binary>>) ->
    ["\\&&" | lucene_escape_qv(Rest)];
lucene_escape_qv(<<"||", Rest/binary>>) ->
    ["\\||" | lucene_escape_qv(Rest)];
lucene_escape_qv(<<C, Rest/binary>>) ->
    NeedsEscape = "+-(){}[]!^~*?:/\\\" \t\r\n",
    Out = case lists:member(C, NeedsEscape) of
        true -> ["\\", C];
        false -> [C]
    end,
    Out ++ lucene_escape_qv(Rest).


lucene_escape_user(Field) ->
    {ok, Path} = parse_field(Field),
    Escaped = [mango_util:lucene_escape_field(P) || P <- Path],
    iolist_to_binary(join(".", Escaped)).


has_suffix(Bin, Suffix) when is_binary(Bin), is_binary(Suffix) ->
    SBin = size(Bin),
    SSuffix = size(Suffix),
    if SBin < SSuffix -> false; true ->
        PSize = SBin - SSuffix,
        case Bin of
            <<_:PSize/binary, Suffix/binary>> ->
                true;
            _ ->
                false
        end
    end.


join(_Sep, []) ->
    [];
join(_Sep, [Item]) ->
    [Item];
join(Sep, [Item | Rest]) ->
    [Item, Sep | join(Sep, Rest)].


is_number_string(Value) when is_binary(Value) ->
    is_number_string(binary_to_list(Value));
is_number_string(Value) when is_list(Value)->
    MP = cached_re(mango_numstring_re, ?NUMSTRING),
    case re:run(Value, MP) of
        nomatch ->
            false;
        _ ->
            true
    end.


cached_re(Name, RE) ->
    case mochiglobal:get(Name) of
        undefined ->
            {ok, MP} = re:compile(RE),
            ok = mochiglobal:put(Name, MP),
            MP;
        MP ->
            MP
    end.


parse_field(Field) ->
    case binary:match(Field, <<"\\">>, []) of
        nomatch ->
            % Fast path, no regex required
            {ok, check_non_empty(Field, binary:split(Field, <<".">>, [global]))};
        _ ->
            parse_field_slow(Field)
    end.

parse_field_slow(Field) ->
    Path = lists:map(fun
        (P) when P =:= <<>> ->
            ?MANGO_ERROR({invalid_field_name, Field});
        (P) ->
            re:replace(P, <<"\\\\">>, <<>>, [global, {return, binary}])
    end, re:split(Field, <<"(?<!\\\\)\\.">>)),
    {ok, Path}.

check_non_empty(Field, Parts) ->
    case lists:member(<<>>, Parts) of
        true ->
            ?MANGO_ERROR({invalid_field_name, Field});
        false ->
            Parts
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_field_test() ->
    ?assertEqual({ok, [<<"ab">>]}, parse_field(<<"ab">>)),
    ?assertEqual({ok, [<<"a">>, <<"b">>]}, parse_field(<<"a.b">>)),
    ?assertEqual({ok, [<<"a.b">>]}, parse_field(<<"a\\.b">>)),
    ?assertEqual({ok, [<<"a">>, <<"b">>, <<"c">>]}, parse_field(<<"a.b.c">>)),
    ?assertEqual({ok, [<<"a">>, <<"b.c">>]}, parse_field(<<"a.b\\.c">>)),
    Exception = {mango_error, ?MODULE, {invalid_field_name, <<"a..b">>}},
    ?assertThrow(Exception, parse_field(<<"a..b">>)).

is_number_string_test() ->
    ?assert(is_number_string("0")),
    ?assert(is_number_string("1")),
    ?assert(is_number_string("1.0")),
    ?assert(is_number_string("1.0E10")),
    ?assert(is_number_string("0d")),
    ?assert(is_number_string("-1")),
    ?assert(is_number_string("-1.0")),
    ?assertNot(is_number_string("hello")),
    ?assertNot(is_number_string("")),
    ?assertMatch({match, _}, re:run("1.0", mochiglobal:get(mango_numstring_re))).

-endif.
