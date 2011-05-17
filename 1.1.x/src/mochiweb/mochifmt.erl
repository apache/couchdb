%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2008 Mochi Media, Inc.

%% @doc String Formatting for Erlang, inspired by Python 2.6
%%      (<a href="http://www.python.org/dev/peps/pep-3101/">PEP 3101</a>).
%%
-module(mochifmt).
-author('bob@mochimedia.com').
-export([format/2, format_field/2, convert_field/2, get_value/2, get_field/2]).
-export([tokenize/1, format/3, get_field/3, format_field/3]).
-export([bformat/2, bformat/3]).
-export([f/2, f/3]).

-record(conversion, {length, precision, ctype, align, fill_char, sign}).

%% @spec tokenize(S::string()) -> tokens()
%% @doc Tokenize a format string into mochifmt's internal format.
tokenize(S) ->
    {?MODULE, tokenize(S, "", [])}.

%% @spec convert_field(Arg, Conversion::conversion()) -> term()
%% @doc Process Arg according to the given explicit conversion specifier.
convert_field(Arg, "") ->
    Arg;
convert_field(Arg, "r") ->
    repr(Arg);
convert_field(Arg, "s") ->
    str(Arg).

%% @spec get_value(Key::string(), Args::args()) -> term()
%% @doc Get the Key from Args. If Args is a tuple then convert Key to
%%      an integer and get element(1 + Key, Args). If Args is a list and Key
%%      can be parsed as an integer then use lists:nth(1 + Key, Args),
%%      otherwise try and look for Key in Args as a proplist, converting
%%      Key to an atom or binary if necessary.
get_value(Key, Args) when is_tuple(Args) ->
    element(1 + list_to_integer(Key), Args);
get_value(Key, Args) when is_list(Args) ->
    try lists:nth(1 + list_to_integer(Key), Args)
    catch error:_ ->
            {_K, V} = proplist_lookup(Key, Args),
            V
    end.

%% @spec get_field(Key::string(), Args) -> term()
%% @doc Consecutively call get_value/2 on parts of Key delimited by ".",
%%      replacing Args with the result of the previous get_value. This
%%      is used to implement formats such as {0.0}.
get_field(Key, Args) ->
    get_field(Key, Args, ?MODULE).

%% @spec get_field(Key::string(), Args, Module) -> term()
%% @doc Consecutively call Module:get_value/2 on parts of Key delimited by ".",
%%      replacing Args with the result of the previous get_value. This
%%      is used to implement formats such as {0.0}.
get_field(Key, Args, Module) ->
    {Name, Next} = lists:splitwith(fun (C) -> C =/= $. end, Key),
    Res = try Module:get_value(Name, Args)
          catch error:undef -> get_value(Name, Args) end,
    case Next of
        "" ->
            Res;
        "." ++ S1 ->
            get_field(S1, Res, Module)
    end.

%% @spec format(Format::string(), Args) -> iolist()
%% @doc Format Args with Format.
format(Format, Args) ->
    format(Format, Args, ?MODULE).

%% @spec format(Format::string(), Args, Module) -> iolist()
%% @doc Format Args with Format using Module.
format({?MODULE, Parts}, Args, Module) ->
    format2(Parts, Args, Module, []);
format(S, Args, Module) ->
    format(tokenize(S), Args, Module).

%% @spec format_field(Arg, Format) -> iolist()
%% @doc Format Arg with Format.
format_field(Arg, Format) ->
    format_field(Arg, Format, ?MODULE).

%% @spec format_field(Arg, Format, _Module) -> iolist()
%% @doc Format Arg with Format.
format_field(Arg, Format, _Module) ->
    F = default_ctype(Arg, parse_std_conversion(Format)),
    fix_padding(fix_sign(convert2(Arg, F), F), F).

%% @spec f(Format::string(), Args) -> string()
%% @doc Format Args with Format and return a string().
f(Format, Args) ->
    f(Format, Args, ?MODULE).

%% @spec f(Format::string(), Args, Module) -> string()
%% @doc Format Args with Format using Module and return a string().
f(Format, Args, Module) ->
    case lists:member(${, Format) of
        true ->
            binary_to_list(bformat(Format, Args, Module));
        false ->
            Format
    end.

%% @spec bformat(Format::string(), Args) -> binary()
%% @doc Format Args with Format and return a binary().
bformat(Format, Args) ->
    iolist_to_binary(format(Format, Args)).

%% @spec bformat(Format::string(), Args, Module) -> binary()
%% @doc Format Args with Format using Module and return a binary().
bformat(Format, Args, Module) ->
    iolist_to_binary(format(Format, Args, Module)).

%% Internal API

add_raw("", Acc) ->
    Acc;
add_raw(S, Acc) ->
    [{raw, lists:reverse(S)} | Acc].

tokenize([], S, Acc) ->
    lists:reverse(add_raw(S, Acc));
tokenize("{{" ++ Rest, S, Acc) ->
    tokenize(Rest, "{" ++ S, Acc);
tokenize("{" ++ Rest, S, Acc) ->
    {Format, Rest1} = tokenize_format(Rest),
    tokenize(Rest1, "", [{format, make_format(Format)} | add_raw(S, Acc)]);
tokenize("}}" ++ Rest, S, Acc) ->
    tokenize(Rest, "}" ++ S, Acc);
tokenize([C | Rest], S, Acc) ->
    tokenize(Rest, [C | S], Acc).

tokenize_format(S) ->
    tokenize_format(S, 1, []).

tokenize_format("}" ++ Rest, 1, Acc) ->
    {lists:reverse(Acc), Rest};
tokenize_format("}" ++ Rest, N, Acc) ->
    tokenize_format(Rest, N - 1, "}" ++ Acc);
tokenize_format("{" ++ Rest, N, Acc) ->
    tokenize_format(Rest, 1 + N, "{" ++ Acc);
tokenize_format([C | Rest], N, Acc) ->
    tokenize_format(Rest, N, [C | Acc]).

make_format(S) ->
    {Name0, Spec} = case lists:splitwith(fun (C) -> C =/= $: end, S) of
                        {_, ""} ->
                            {S, ""};
                        {SN, ":" ++ SS} ->
                            {SN, SS}
                    end,
    {Name, Transform} = case lists:splitwith(fun (C) -> C =/= $! end, Name0) of
                            {_, ""} ->
                                {Name0, ""};
                            {TN, "!" ++ TT} ->
                                {TN, TT}
                        end,
    {Name, Transform, Spec}.

proplist_lookup(S, P) ->
    A = try list_to_existing_atom(S)
        catch error:_ -> make_ref() end,
    B = try list_to_binary(S)
        catch error:_ -> make_ref() end,
    proplist_lookup2({S, A, B}, P).

proplist_lookup2({KS, KA, KB}, [{K, V} | _])
  when KS =:= K orelse KA =:= K orelse KB =:= K ->
    {K, V};
proplist_lookup2(Keys, [_ | Rest]) ->
    proplist_lookup2(Keys, Rest).

format2([], _Args, _Module, Acc) ->
    lists:reverse(Acc);
format2([{raw, S} | Rest], Args, Module, Acc) ->
    format2(Rest, Args, Module, [S | Acc]);
format2([{format, {Key, Convert, Format0}} | Rest], Args, Module, Acc) ->
    Format = f(Format0, Args, Module),
    V = case Module of
            ?MODULE ->
                V0 = get_field(Key, Args),
                V1 = convert_field(V0, Convert),
                format_field(V1, Format);
            _ ->
                V0 = try Module:get_field(Key, Args)
                     catch error:undef -> get_field(Key, Args, Module) end,
                V1 = try Module:convert_field(V0, Convert)
                     catch error:undef -> convert_field(V0, Convert) end,
                try Module:format_field(V1, Format)
                catch error:undef -> format_field(V1, Format, Module) end
        end,
    format2(Rest, Args, Module, [V | Acc]).

default_ctype(_Arg, C=#conversion{ctype=N}) when N =/= undefined ->
    C;
default_ctype(Arg, C) when is_integer(Arg) ->
    C#conversion{ctype=decimal};
default_ctype(Arg, C) when is_float(Arg) ->
    C#conversion{ctype=general};
default_ctype(_Arg, C) ->
    C#conversion{ctype=string}.

fix_padding(Arg, #conversion{length=undefined}) ->
    Arg;
fix_padding(Arg, F=#conversion{length=Length, fill_char=Fill0, align=Align0,
                               ctype=Type}) ->
    Padding = Length - iolist_size(Arg),
    Fill = case Fill0 of
               undefined ->
                   $\s;
               _ ->
                   Fill0
           end,
    Align = case Align0 of
                undefined ->
                    case Type of
                        string ->
                            left;
                        _ ->
                            right
                    end;
                _ ->
                    Align0
            end,
    case Padding > 0 of
        true ->
            do_padding(Arg, Padding, Fill, Align, F);
        false ->
            Arg
    end.

do_padding(Arg, Padding, Fill, right, _F) ->
    [lists:duplicate(Padding, Fill), Arg];
do_padding(Arg, Padding, Fill, center, _F) ->
    LPadding = lists:duplicate(Padding div 2, Fill),
    RPadding = case Padding band 1 of
                   1 ->
                       [Fill | LPadding];
                   _ ->
                       LPadding
               end,
    [LPadding, Arg, RPadding];
do_padding([$- | Arg], Padding, Fill, sign_right, _F) ->
    [[$- | lists:duplicate(Padding, Fill)], Arg];
do_padding(Arg, Padding, Fill, sign_right, #conversion{sign=$-}) ->
    [lists:duplicate(Padding, Fill), Arg];
do_padding([S | Arg], Padding, Fill, sign_right, #conversion{sign=S}) ->
    [[S | lists:duplicate(Padding, Fill)], Arg];
do_padding(Arg, Padding, Fill, sign_right, #conversion{sign=undefined}) ->
    [lists:duplicate(Padding, Fill), Arg];
do_padding(Arg, Padding, Fill, left, _F) ->
    [Arg | lists:duplicate(Padding, Fill)].

fix_sign(Arg, #conversion{sign=$+}) when Arg >= 0 ->
    [$+, Arg];
fix_sign(Arg, #conversion{sign=$\s}) when Arg >= 0 ->
    [$\s, Arg];
fix_sign(Arg, _F) ->
    Arg.

ctype($\%) -> percent;
ctype($s) -> string;
ctype($b) -> bin;
ctype($o) -> oct;
ctype($X) -> upper_hex;
ctype($x) -> hex;
ctype($c) -> char;
ctype($d) -> decimal;
ctype($g) -> general;
ctype($f) -> fixed;
ctype($e) -> exp.

align($<) -> left;
align($>) -> right;
align($^) -> center;
align($=) -> sign_right.

convert2(Arg, F=#conversion{ctype=percent}) ->
    [convert2(100.0 * Arg, F#conversion{ctype=fixed}), $\%];
convert2(Arg, #conversion{ctype=string}) ->
    str(Arg);
convert2(Arg, #conversion{ctype=bin}) ->
    erlang:integer_to_list(Arg, 2);
convert2(Arg, #conversion{ctype=oct}) ->
    erlang:integer_to_list(Arg, 8);
convert2(Arg, #conversion{ctype=upper_hex}) ->
    erlang:integer_to_list(Arg, 16);
convert2(Arg, #conversion{ctype=hex}) ->
    string:to_lower(erlang:integer_to_list(Arg, 16));
convert2(Arg, #conversion{ctype=char}) when Arg < 16#80 ->
    [Arg];
convert2(Arg, #conversion{ctype=char}) ->
    xmerl_ucs:to_utf8(Arg);
convert2(Arg, #conversion{ctype=decimal}) ->
    integer_to_list(Arg);
convert2(Arg, #conversion{ctype=general, precision=undefined}) ->
    try mochinum:digits(Arg)
    catch error:undef -> io_lib:format("~g", [Arg]) end;
convert2(Arg, #conversion{ctype=fixed, precision=undefined}) ->
    io_lib:format("~f", [Arg]);
convert2(Arg, #conversion{ctype=exp, precision=undefined}) ->
    io_lib:format("~e", [Arg]);
convert2(Arg, #conversion{ctype=general, precision=P}) ->
    io_lib:format("~." ++ integer_to_list(P) ++ "g", [Arg]);
convert2(Arg, #conversion{ctype=fixed, precision=P}) ->
    io_lib:format("~." ++ integer_to_list(P) ++ "f", [Arg]);
convert2(Arg, #conversion{ctype=exp, precision=P}) ->
    io_lib:format("~." ++ integer_to_list(P) ++ "e", [Arg]).

str(A) when is_atom(A) ->
    atom_to_list(A);
str(I) when is_integer(I) ->
    integer_to_list(I);
str(F) when is_float(F) ->
    try mochinum:digits(F)
    catch error:undef -> io_lib:format("~g", [F]) end;
str(L) when is_list(L) ->
    L;
str(B) when is_binary(B) ->
    B;
str(P) ->
    repr(P).

repr(P) when is_float(P) ->
    try mochinum:digits(P)
    catch error:undef -> float_to_list(P) end;
repr(P) ->
    io_lib:format("~p", [P]).

parse_std_conversion(S) ->
    parse_std_conversion(S, #conversion{}).

parse_std_conversion("", Acc) ->
    Acc;
parse_std_conversion([Fill, Align | Spec], Acc)
  when Align =:= $< orelse Align =:= $> orelse Align =:= $= orelse Align =:= $^ ->
    parse_std_conversion(Spec, Acc#conversion{fill_char=Fill,
                                              align=align(Align)});
parse_std_conversion([Align | Spec], Acc)
  when Align =:= $< orelse Align =:= $> orelse Align =:= $= orelse Align =:= $^ ->
    parse_std_conversion(Spec, Acc#conversion{align=align(Align)});
parse_std_conversion([Sign | Spec], Acc)
  when Sign =:= $+ orelse Sign =:= $- orelse Sign =:= $\s ->
    parse_std_conversion(Spec, Acc#conversion{sign=Sign});
parse_std_conversion("0" ++ Spec, Acc) ->
    Align = case Acc#conversion.align of
                undefined ->
                    sign_right;
                A ->
                    A
            end,
    parse_std_conversion(Spec, Acc#conversion{fill_char=$0, align=Align});
parse_std_conversion(Spec=[D|_], Acc) when D >= $0 andalso D =< $9 ->
    {W, Spec1} = lists:splitwith(fun (C) -> C >= $0 andalso C =< $9 end, Spec),
    parse_std_conversion(Spec1, Acc#conversion{length=list_to_integer(W)});
parse_std_conversion([$. | Spec], Acc) ->
    case lists:splitwith(fun (C) -> C >= $0 andalso C =< $9 end, Spec) of
        {"", Spec1} ->
            parse_std_conversion(Spec1, Acc);
        {P, Spec1} ->
            parse_std_conversion(Spec1,
                                 Acc#conversion{precision=list_to_integer(P)})
    end;
parse_std_conversion([Type], Acc) ->
    parse_std_conversion("", Acc#conversion{ctype=ctype(Type)}).


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

tokenize_test() ->
    {?MODULE, [{raw, "ABC"}]} = tokenize("ABC"),
    {?MODULE, [{format, {"0", "", ""}}]} = tokenize("{0}"),
    {?MODULE, [{raw, "ABC"}, {format, {"1", "", ""}}, {raw, "DEF"}]} =
        tokenize("ABC{1}DEF"),
    ok.

format_test() ->
    <<"  -4">> = bformat("{0:4}", [-4]),
    <<"   4">> = bformat("{0:4}", [4]),
    <<"   4">> = bformat("{0:{0}}", [4]),
    <<"4   ">> = bformat("{0:4}", ["4"]),
    <<"4   ">> = bformat("{0:{0}}", ["4"]),
    <<"1.2yoDEF">> = bformat("{2}{0}{1}{3}", {yo, "DE", 1.2, <<"F">>}),
    <<"cafebabe">> = bformat("{0:x}", {16#cafebabe}),
    <<"CAFEBABE">> = bformat("{0:X}", {16#cafebabe}),
    <<"CAFEBABE">> = bformat("{0:X}", {16#cafebabe}),
    <<"755">> = bformat("{0:o}", {8#755}),
    <<"a">> = bformat("{0:c}", {97}),
    %% Horizontal ellipsis
    <<226, 128, 166>> = bformat("{0:c}", {16#2026}),
    <<"11">> = bformat("{0:b}", {3}),
    <<"11">> = bformat("{0:b}", [3]),
    <<"11">> = bformat("{three:b}", [{three, 3}]),
    <<"11">> = bformat("{three:b}", [{"three", 3}]),
    <<"11">> = bformat("{three:b}", [{<<"three">>, 3}]),
    <<"\"foo\"">> = bformat("{0!r}", {"foo"}),
    <<"2008-5-4">> = bformat("{0.0}-{0.1}-{0.2}", {{2008,5,4}}),
    <<"2008-05-04">> = bformat("{0.0:04}-{0.1:02}-{0.2:02}", {{2008,5,4}}),
    <<"foo6bar-6">> = bformat("foo{1}{0}-{1}", {bar, 6}),
    <<"-'atom test'-">> = bformat("-{arg!r}-", [{arg, 'atom test'}]),
    <<"2008-05-04">> = bformat("{0.0:0{1.0}}-{0.1:0{1.1}}-{0.2:0{1.2}}",
                               {{2008,5,4}, {4, 2, 2}}),
    ok.

std_test() ->
    M = mochifmt_std:new(),
    <<"01">> = bformat("{0}{1}", [0, 1], M),
    ok.

records_test() ->
    M = mochifmt_records:new([{conversion, record_info(fields, conversion)}]),
    R = #conversion{length=long, precision=hard, sign=peace},
    long = M:get_value("length", R),
    hard = M:get_value("precision", R),
    peace = M:get_value("sign", R),
    <<"long hard">> = bformat("{length} {precision}", R, M),
    <<"long hard">> = bformat("{0.length} {0.precision}", [R], M),
    ok.

-endif.
