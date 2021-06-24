%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% fork of io_lib_format that uses trunc_io to protect against large terms
%%
%% Renamed to couch_log_format to avoid naming collision with
%% lager_Format.
-module(couch_log_trunc_io_fmt).

-export([format/3, format/4]).

-record(options, {
    chomp = false :: boolean()
}).

format(FmtStr, Args, MaxLen) ->
    format(FmtStr, Args, MaxLen, []).

format([], [], _, _) ->
    "";
format(FmtStr, Args, MaxLen, Opts) when is_atom(FmtStr) ->
    format(atom_to_list(FmtStr), Args, MaxLen, Opts);
format(FmtStr, Args, MaxLen, Opts) when is_binary(FmtStr) ->
    format(binary_to_list(FmtStr), Args, MaxLen, Opts);
format(FmtStr, Args, MaxLen, Opts) when is_list(FmtStr) ->
    case couch_log_util:string_p(FmtStr) of
        true ->
            Options = make_options(Opts, #options{}),
            Cs = collect(FmtStr, Args),
            {Cs2, MaxLen2} = build(Cs, [], MaxLen, Options),
            %% count how many terms remain
            {Count, StrLen} = lists:foldl(
                fun
                    ({_C, _As, _F, _Adj, _P, _Pad, _Enc}, {Terms, Chars}) ->
                        {Terms + 1, Chars};
                    (_, {Terms, Chars}) ->
                        {Terms, Chars + 1}
                end,
                {0, 0},
                Cs2
            ),
            build2(Cs2, Count, MaxLen2 - StrLen);
        false ->
            erlang:error(badarg)
    end;
format(_FmtStr, _Args, _MaxLen, _Opts) ->
    erlang:error(badarg).

collect([$~ | Fmt0], Args0) ->
    {C, Fmt1, Args1} = collect_cseq(Fmt0, Args0),
    [C | collect(Fmt1, Args1)];
collect([C | Fmt], Args) ->
    [C | collect(Fmt, Args)];
collect([], []) ->
    [].

collect_cseq(Fmt0, Args0) ->
    {F, Ad, Fmt1, Args1} = field_width(Fmt0, Args0),
    {P, Fmt2, Args2} = precision(Fmt1, Args1),
    {Pad, Fmt3, Args3} = pad_char(Fmt2, Args2),
    {Encoding, Fmt4, Args4} = encoding(Fmt3, Args3),
    {C, As, Fmt5, Args5} = collect_cc(Fmt4, Args4),
    {{C, As, F, Ad, P, Pad, Encoding}, Fmt5, Args5}.

encoding([$t | Fmt], Args) ->
    {unicode, Fmt, Args};
encoding(Fmt, Args) ->
    {latin1, Fmt, Args}.

field_width([$- | Fmt0], Args0) ->
    {F, Fmt, Args} = field_value(Fmt0, Args0),
    field_width(-F, Fmt, Args);
field_width(Fmt0, Args0) ->
    {F, Fmt, Args} = field_value(Fmt0, Args0),
    field_width(F, Fmt, Args).

field_width(F, Fmt, Args) when F < 0 ->
    {-F, left, Fmt, Args};
field_width(F, Fmt, Args) when F >= 0 ->
    {F, right, Fmt, Args}.

precision([$. | Fmt], Args) ->
    field_value(Fmt, Args);
precision(Fmt, Args) ->
    {none, Fmt, Args}.

field_value([$* | Fmt], [A | Args]) when is_integer(A) ->
    {A, Fmt, Args};
field_value([C | Fmt], Args) when is_integer(C), C >= $0, C =< $9 ->
    field_value([C | Fmt], Args, 0);
field_value(Fmt, Args) ->
    {none, Fmt, Args}.

field_value([C | Fmt], Args, F) when is_integer(C), C >= $0, C =< $9 ->
    field_value(Fmt, Args, 10 * F + (C - $0));
%Default case
field_value(Fmt, Args, F) ->
    {F, Fmt, Args}.

pad_char([$., $* | Fmt], [Pad | Args]) -> {Pad, Fmt, Args};
pad_char([$., Pad | Fmt], Args) -> {Pad, Fmt, Args};
pad_char(Fmt, Args) -> {$\s, Fmt, Args}.

%% collect_cc([FormatChar], [Argument]) ->
%%         {Control,[ControlArg],[FormatChar],[Arg]}.
%%  Here we collect the argments for each control character.
%%  Be explicit to cause failure early.

collect_cc([$w | Fmt], [A | Args]) -> {$w, [A], Fmt, Args};
collect_cc([$p | Fmt], [A | Args]) -> {$p, [A], Fmt, Args};
collect_cc([$W | Fmt], [A, Depth | Args]) -> {$W, [A, Depth], Fmt, Args};
collect_cc([$P | Fmt], [A, Depth | Args]) -> {$P, [A, Depth], Fmt, Args};
collect_cc([$s | Fmt], [A | Args]) -> {$s, [A], Fmt, Args};
collect_cc([$r | Fmt], [A | Args]) -> {$r, [A], Fmt, Args};
collect_cc([$e | Fmt], [A | Args]) -> {$e, [A], Fmt, Args};
collect_cc([$f | Fmt], [A | Args]) -> {$f, [A], Fmt, Args};
collect_cc([$g | Fmt], [A | Args]) -> {$g, [A], Fmt, Args};
collect_cc([$b | Fmt], [A | Args]) -> {$b, [A], Fmt, Args};
collect_cc([$B | Fmt], [A | Args]) -> {$B, [A], Fmt, Args};
collect_cc([$x | Fmt], [A, Prefix | Args]) -> {$x, [A, Prefix], Fmt, Args};
collect_cc([$X | Fmt], [A, Prefix | Args]) -> {$X, [A, Prefix], Fmt, Args};
collect_cc([$+ | Fmt], [A | Args]) -> {$+, [A], Fmt, Args};
collect_cc([$# | Fmt], [A | Args]) -> {$#, [A], Fmt, Args};
collect_cc([$c | Fmt], [A | Args]) -> {$c, [A], Fmt, Args};
collect_cc([$~ | Fmt], Args) when is_list(Args) -> {$~, [], Fmt, Args};
collect_cc([$n | Fmt], Args) when is_list(Args) -> {$n, [], Fmt, Args};
collect_cc([$i | Fmt], [A | Args]) -> {$i, [A], Fmt, Args}.

%% build([Control], Pc, Indentation) -> [Char].
%%  Interpret the control structures. Count the number of print
%%  remaining and only calculate indentation when necessary. Must also
%%  be smart when calculating indentation for characters in format.

build([{$n, _, _, _, _, _, _}], Acc, MaxLen, #options{chomp = true}) ->
    %% trailing ~n, ignore
    {lists:reverse(Acc), MaxLen};
build([{C, As, F, Ad, P, Pad, Enc} | Cs], Acc, MaxLen, O) ->
    {S, MaxLen2} = control(C, As, F, Ad, P, Pad, Enc, MaxLen),
    build(Cs, [S | Acc], MaxLen2, O);
build([$\n], Acc, MaxLen, #options{chomp = true}) ->
    %% trailing \n, ignore
    {lists:reverse(Acc), MaxLen};
build([$\n | Cs], Acc, MaxLen, O) ->
    build(Cs, [$\n | Acc], MaxLen - 1, O);
build([$\t | Cs], Acc, MaxLen, O) ->
    build(Cs, [$\t | Acc], MaxLen - 1, O);
build([C | Cs], Acc, MaxLen, O) ->
    build(Cs, [C | Acc], MaxLen - 1, O);
build([], Acc, MaxLen, _O) ->
    {lists:reverse(Acc), MaxLen}.

build2([{C, As, F, Ad, P, Pad, Enc} | Cs], Count, MaxLen) ->
    {S, Len} = control2(C, As, F, Ad, P, Pad, Enc, MaxLen div Count),
    [S | build2(Cs, Count - 1, MaxLen - Len)];
build2([C | Cs], Count, MaxLen) ->
    [C | build2(Cs, Count, MaxLen)];
build2([], _, _) ->
    [].

%% control(FormatChar, [Argument], FieldWidth, Adjust, Precision, PadChar,
%%         Indentation) -> [Char]
%%  This is the main dispatch function for the various formatting commands.
%%  Field widths and precisions have already been calculated.

control($e, [A], F, Adj, P, Pad, _Enc, L) when is_float(A) ->
    Res = fwrite_e(A, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($f, [A], F, Adj, P, Pad, _Enc, L) when is_float(A) ->
    Res = fwrite_f(A, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($g, [A], F, Adj, P, Pad, _Enc, L) when is_float(A) ->
    Res = fwrite_g(A, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($b, [A], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    Res = unprefixed_integer(A, F, Adj, base(P), Pad, true),
    {Res, L - lists:flatlength(Res)};
control($B, [A], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    Res = unprefixed_integer(A, F, Adj, base(P), Pad, false),
    {Res, L - lists:flatlength(Res)};
control($x, [A, Prefix], F, Adj, P, Pad, _Enc, L) when
    is_integer(A),
    is_atom(Prefix)
->
    Res = prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), true),
    {Res, L - lists:flatlength(Res)};
control($x, [A, Prefix], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    %Check if Prefix a character list
    true = io_lib:deep_char_list(Prefix),
    Res = prefixed_integer(A, F, Adj, base(P), Pad, Prefix, true),
    {Res, L - lists:flatlength(Res)};
control($X, [A, Prefix], F, Adj, P, Pad, _Enc, L) when
    is_integer(A),
    is_atom(Prefix)
->
    Res = prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), false),
    {Res, L - lists:flatlength(Res)};
control($X, [A, Prefix], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    %Check if Prefix a character list
    true = io_lib:deep_char_list(Prefix),
    Res = prefixed_integer(A, F, Adj, base(P), Pad, Prefix, false),
    {Res, L - lists:flatlength(Res)};
control($+, [A], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    Base = base(P),
    Prefix = [integer_to_list(Base), $#],
    Res = prefixed_integer(A, F, Adj, Base, Pad, Prefix, true),
    {Res, L - lists:flatlength(Res)};
control($#, [A], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    Base = base(P),
    Prefix = [integer_to_list(Base), $#],
    Res = prefixed_integer(A, F, Adj, Base, Pad, Prefix, false),
    {Res, L - lists:flatlength(Res)};
control($c, [A], F, Adj, P, Pad, unicode, L) when is_integer(A) ->
    Res = char(A, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($c, [A], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    Res = char(A band 255, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($~, [], F, Adj, P, Pad, _Enc, L) ->
    Res = char($~, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($n, [], F, Adj, P, Pad, _Enc, L) ->
    Res = newline(F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($i, [_A], _F, _Adj, _P, _Pad, _Enc, L) ->
    {[], L};
control($s, [A], F, Adj, P, Pad, _Enc, L) when is_atom(A) ->
    Res = string(atom_to_list(A), F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control(C, A, F, Adj, P, Pad, Enc, L) ->
    %% save this for later - these are all the 'large' terms
    {{C, A, F, Adj, P, Pad, Enc}, L}.

control2($w, [A], F, Adj, P, Pad, _Enc, L) ->
    Term = couch_log_trunc_io:fprint(A, L, [{lists_as_strings, false}]),
    Res = term(Term, F, Adj, P, Pad),
    {Res, lists:flatlength(Res)};
control2($p, [A], _F, _Adj, _P, _Pad, _Enc, L) ->
    Term = couch_log_trunc_io:fprint(A, L, [{lists_as_strings, true}]),
    {Term, lists:flatlength(Term)};
control2($W, [A, Depth], F, Adj, P, Pad, _Enc, L) when is_integer(Depth) ->
    Term = couch_log_trunc_io:fprint(A, L, [{depth, Depth}, {lists_as_strings, false}]),
    Res = term(Term, F, Adj, P, Pad),
    {Res, lists:flatlength(Res)};
control2($P, [A, Depth], _F, _Adj, _P, _Pad, _Enc, L) when is_integer(Depth) ->
    Term = couch_log_trunc_io:fprint(A, L, [{depth, Depth}, {lists_as_strings, true}]),
    {Term, lists:flatlength(Term)};
control2($s, [L0], F, Adj, P, Pad, latin1, L) ->
    List = couch_log_trunc_io:fprint(iolist_to_chars(L0), L, [{force_strings, true}]),
    Res = string(List, F, Adj, P, Pad),
    {Res, lists:flatlength(Res)};
control2($s, [L0], F, Adj, P, Pad, unicode, L) ->
    List = couch_log_trunc_io:fprint(cdata_to_chars(L0), L, [{force_strings, true}]),
    Res = uniconv(string(List, F, Adj, P, Pad)),
    {Res, lists:flatlength(Res)};
control2($r, [R], F, Adj, P, Pad, _Enc, _L) ->
    List = couch_log_formatter:format_reason(R),
    Res = string(List, F, Adj, P, Pad),
    {Res, lists:flatlength(Res)}.

iolist_to_chars([C | Cs]) when is_integer(C), C >= $\000, C =< $\377 ->
    [C | iolist_to_chars(Cs)];
iolist_to_chars([I | Cs]) ->
    [iolist_to_chars(I) | iolist_to_chars(Cs)];
iolist_to_chars([]) ->
    [];
iolist_to_chars(B) when is_binary(B) ->
    binary_to_list(B).

cdata_to_chars([C | Cs]) when is_integer(C), C >= $\000 ->
    [C | cdata_to_chars(Cs)];
cdata_to_chars([I | Cs]) ->
    [cdata_to_chars(I) | cdata_to_chars(Cs)];
cdata_to_chars([]) ->
    [];
cdata_to_chars(B) when is_binary(B) ->
    case catch unicode:characters_to_list(B) of
        L when is_list(L) -> L;
        _ -> binary_to_list(B)
    end.

make_options([], Options) ->
    Options;
make_options([{chomp, Bool} | T], Options) when is_boolean(Bool) ->
    make_options(T, Options#options{chomp = Bool}).

-ifdef(UNICODE_AS_BINARIES).
uniconv(C) ->
    unicode:characters_to_binary(C, unicode).
-else.
uniconv(C) ->
    C.
-endif.
%% Default integer base
base(none) ->
    10;
base(B) when is_integer(B) ->
    B.

%% term(TermList, Field, Adjust, Precision, PadChar)
%%  Output the characters in a term.
%%  Adjust the characters within the field if length less than Max padding
%%  with PadChar.

term(T, none, _Adj, none, _Pad) ->
    T;
term(T, none, Adj, P, Pad) ->
    term(T, P, Adj, P, Pad);
term(T, F, Adj, P0, Pad) ->
    L = lists:flatlength(T),
    P =
        case P0 of
            none -> erlang:min(L, F);
            _ -> P0
        end,
    if
        L > P ->
            adjust(chars($*, P), chars(Pad, F - P), Adj);
        F >= P ->
            adjust(T, chars(Pad, F - L), Adj)
    end.

%% fwrite_e(Float, Field, Adjust, Precision, PadChar)

%Default values
fwrite_e(Fl, none, Adj, none, Pad) ->
    fwrite_e(Fl, none, Adj, 6, Pad);
fwrite_e(Fl, none, _Adj, P, _Pad) when P >= 2 ->
    float_e(Fl, float_data(Fl), P);
fwrite_e(Fl, F, Adj, none, Pad) ->
    fwrite_e(Fl, F, Adj, 6, Pad);
fwrite_e(Fl, F, Adj, P, Pad) when P >= 2 ->
    term(float_e(Fl, float_data(Fl), P), F, Adj, F, Pad).

%Negative numbers
float_e(Fl, Fd, P) when Fl < 0.0 ->
    [$- | float_e(-Fl, Fd, P)];
float_e(_Fl, {Ds, E}, P) ->
    case float_man(Ds, 1, P - 1) of
        {[$0 | Fs], true} -> [[$1 | Fs] | float_exp(E)];
        {Fs, false} -> [Fs | float_exp(E - 1)]
    end.

%% float_man([Digit], Icount, Dcount) -> {[Chars],CarryFlag}.
%%  Generate the characters in the mantissa from the digits with Icount
%%  characters before the '.' and Dcount decimals. Handle carry and let
%%  caller decide what to do at top.

float_man(Ds, 0, Dc) ->
    {Cs, C} = float_man(Ds, Dc),
    {[$. | Cs], C};
float_man([D | Ds], I, Dc) ->
    case float_man(Ds, I - 1, Dc) of
        {Cs, true} when D =:= $9 -> {[$0 | Cs], true};
        {Cs, true} -> {[D + 1 | Cs], false};
        {Cs, false} -> {[D | Cs], false}
    end;
%Pad with 0's
float_man([], I, Dc) ->
    {string:chars($0, I, [$. | string:chars($0, Dc)]), false}.

float_man([D | _], 0) when D >= $5 -> {[], true};
float_man([_ | _], 0) ->
    {[], false};
float_man([D | Ds], Dc) ->
    case float_man(Ds, Dc - 1) of
        {Cs, true} when D =:= $9 -> {[$0 | Cs], true};
        {Cs, true} -> {[D + 1 | Cs], false};
        {Cs, false} -> {[D | Cs], false}
    end;
%Pad with 0's
float_man([], Dc) ->
    {string:chars($0, Dc), false}.

%% float_exp(Exponent) -> [Char].
%%  Generate the exponent of a floating point number. Always include sign.

float_exp(E) when E >= 0 ->
    [$e, $+ | integer_to_list(E)];
float_exp(E) ->
    [$e | integer_to_list(E)].

%% fwrite_f(FloatData, Field, Adjust, Precision, PadChar)

%Default values
fwrite_f(Fl, none, Adj, none, Pad) ->
    fwrite_f(Fl, none, Adj, 6, Pad);
fwrite_f(Fl, none, _Adj, P, _Pad) when P >= 1 ->
    float_f(Fl, float_data(Fl), P);
fwrite_f(Fl, F, Adj, none, Pad) ->
    fwrite_f(Fl, F, Adj, 6, Pad);
fwrite_f(Fl, F, Adj, P, Pad) when P >= 1 ->
    term(float_f(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_f(Fl, Fd, P) when Fl < 0.0 ->
    [$- | float_f(-Fl, Fd, P)];
float_f(Fl, {Ds, E}, P) when E =< 0 ->
    %Prepend enough 0's
    float_f(Fl, {string:chars($0, -E + 1, Ds), 1}, P);
float_f(_Fl, {Ds, E}, P) ->
    case float_man(Ds, E, P) of
        %Handle carry
        {Fs, true} -> "1" ++ Fs;
        {Fs, false} -> Fs
    end.

%% float_data([FloatChar]) -> {[Digit],Exponent}

float_data(Fl) ->
    float_data(float_to_list(Fl), []).

float_data([$e | E], Ds) ->
    {lists:reverse(Ds), list_to_integer(E) + 1};
float_data([D | Cs], Ds) when D >= $0, D =< $9 ->
    float_data(Cs, [D | Ds]);
float_data([_ | Cs], Ds) ->
    float_data(Cs, Ds).

%% fwrite_g(Float, Field, Adjust, Precision, PadChar)
%%  Use the f form if Float is >= 0.1 and < 1.0e4,
%%  and the prints correctly in the f form, else the e form.
%%  Precision always means the # of significant digits.

fwrite_g(Fl, F, Adj, none, Pad) ->
    fwrite_g(Fl, F, Adj, 6, Pad);
fwrite_g(Fl, F, Adj, P, Pad) when P >= 1 ->
    A = abs(Fl),
    E =
        if
            A < 1.0e-1 -> -2;
            A < 1.0e0 -> -1;
            A < 1.0e1 -> 0;
            A < 1.0e2 -> 1;
            A < 1.0e3 -> 2;
            A < 1.0e4 -> 3;
            true -> fwrite_f
        end,
    if
        P =< 1, E =:= -1;
        P - 1 > E, E >= -1 ->
            fwrite_f(Fl, F, Adj, P - 1 - E, Pad);
        P =< 1 ->
            fwrite_e(Fl, F, Adj, 2, Pad);
        true ->
            fwrite_e(Fl, F, Adj, P, Pad)
    end.

%% string(String, Field, Adjust, Precision, PadChar)

string(S, none, _Adj, none, _Pad) ->
    S;
string(S, F, Adj, none, Pad) ->
    string_field(S, F, Adj, lists:flatlength(S), Pad);
string(S, none, _Adj, P, Pad) ->
    string_field(S, P, left, lists:flatlength(S), Pad);
string(S, F, Adj, P, Pad) when F >= P ->
    N = lists:flatlength(S),
    if
        F > P ->
            if
                N > P ->
                    adjust(flat_trunc(S, P), chars(Pad, F - P), Adj);
                N < P ->
                    adjust([S | chars(Pad, P - N)], chars(Pad, F - P), Adj);
                % N == P
                true ->
                    adjust(S, chars(Pad, F - P), Adj)
            end;
        % F == P
        true ->
            string_field(S, F, Adj, N, Pad)
    end.

string_field(S, F, _Adj, N, _Pad) when N > F ->
    flat_trunc(S, F);
string_field(S, F, Adj, N, Pad) when N < F ->
    adjust(S, chars(Pad, F - N), Adj);
% N == F
string_field(S, _, _, _, _) ->
    S.

%% unprefixed_integer(Int, Field, Adjust, Base, PadChar, Lowercase)
%% -> [Char].

unprefixed_integer(Int, F, Adj, Base, Pad, Lowercase) when
    Base >= 2, Base =< 1 + $Z - $A + 10
->
    if
        Int < 0 ->
            S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
            term([$- | S], F, Adj, none, Pad);
        true ->
            S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
            term(S, F, Adj, none, Pad)
    end.

%% prefixed_integer(Int, Field, Adjust, Base, PadChar, Prefix, Lowercase)
%% -> [Char].

prefixed_integer(Int, F, Adj, Base, Pad, Prefix, Lowercase) when
    Base >= 2, Base =< 1 + $Z - $A + 10
->
    if
        Int < 0 ->
            S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
            term([$-, Prefix | S], F, Adj, none, Pad);
        true ->
            S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
            term([Prefix | S], F, Adj, none, Pad)
    end.

%% char(Char, Field, Adjust, Precision, PadChar) -> [Char].

char(C, none, _Adj, none, _Pad) ->
    [C];
char(C, F, _Adj, none, _Pad) ->
    chars(C, F);
char(C, none, _Adj, P, _Pad) ->
    chars(C, P);
char(C, F, Adj, P, Pad) when F >= P ->
    adjust(chars(C, P), chars(Pad, F - P), Adj).

%% newline(Field, Adjust, Precision, PadChar) -> [Char].

newline(none, _Adj, _P, _Pad) -> "\n";
newline(F, right, _P, _Pad) -> chars($\n, F).

%%
%% Utilities
%%

adjust(Data, [], _) -> Data;
adjust(Data, Pad, left) -> [Data | Pad];
adjust(Data, Pad, right) -> [Pad | Data].

%% Flatten and truncate a deep list to at most N elements.
flat_trunc(List, N) when is_integer(N), N >= 0 ->
    flat_trunc(List, N, []).

flat_trunc(L, 0, R) when is_list(L) ->
    lists:reverse(R);
flat_trunc([H | T], N, R) ->
    flat_trunc(T, N - 1, [H | R]);
flat_trunc([], _, R) ->
    lists:reverse(R).

%% A deep version of string:chars/2,3

chars(_C, 0) ->
    [];
chars(C, 1) ->
    [C];
chars(C, 2) ->
    [C, C];
chars(C, 3) ->
    [C, C, C];
chars(C, N) when is_integer(N), (N band 1) =:= 0 ->
    S = chars(C, N bsr 1),
    [S | S];
chars(C, N) when is_integer(N) ->
    S = chars(C, N bsr 1),
    [C, S | S].

%chars(C, N, Tail) ->
%    [chars(C, N)|Tail].

%% Lowercase conversion

cond_lowercase(String, true) ->
    lowercase(String);
cond_lowercase(String, false) ->
    String.

lowercase([H | T]) when is_integer(H), H >= $A, H =< $Z ->
    [(H - $A + $a) | lowercase(T)];
lowercase([H | T]) ->
    [H | lowercase(T)];
lowercase([]) ->
    [].
