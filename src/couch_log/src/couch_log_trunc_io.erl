%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with your Erlang distribution. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Corelatus AB.
%% Portions created by Corelatus are Copyright 2003, Corelatus
%% AB. All Rights Reserved.''
%%
%% @doc Module to print out terms for logging. Limits by length rather than depth.
%%
%% The resulting string may be slightly larger than the limit; the intention
%% is to provide predictable CPU and memory consumption for formatting
%% terms, not produce precise string lengths.
%%
%% Typical use:
%%
%%   trunc_io:print(Term, 500).
%%
%% Source license: Erlang Public License.
%% Original author: Matthias Lang, <tt>matthias@corelatus.se</tt>
%%
%% Various changes to this module, most notably the format/3 implementation
%% were added by Andrew Thompson `<andrew@basho.com>'. The module has been renamed
%% to avoid conflicts with the vanilla module.
%%
%% Module renamed to couch_log_trunc_io to avoid naming collisions with
%% the lager version.

-module(couch_log_trunc_io).
-author('matthias@corelatus.se').
%% And thanks to Chris Newcombe for a bug fix

% interface functions
-export([format/3, format/4, print/2, print/3, fprint/2, fprint/3, safe/2]).
-version("$Id: trunc_io.erl,v 1.11 2009-02-23 12:01:06 matthias Exp $").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type option() ::
    {'depth', integer()}
    | {'lists_as_strings', boolean()}
    | {'force_strings', boolean()}.
-type options() :: [option()].

-record(print_options, {
    %% negative depth means no depth limiting
    depth = -1 :: integer(),
    %% whether to print lists as strings, if possible
    lists_as_strings = true :: boolean(),
    %% force strings, or binaries to be printed as a string,
    %% even if they're not printable
    force_strings = false :: boolean()
}).

format(Fmt, Args, Max) ->
    format(Fmt, Args, Max, []).

format(Fmt, Args, Max, Options) ->
    try
        couch_log_trunc_io_fmt:format(Fmt, Args, Max, Options)
    catch
        _What:_Why ->
            erlang:error(badarg, [Fmt, Args])
    end.

%% @doc Returns an flattened list containing the ASCII representation of the given
%% term.
-spec fprint(term(), pos_integer()) -> string().
fprint(Term, Max) ->
    fprint(Term, Max, []).

%% @doc Returns an flattened list containing the ASCII representation of the given
%% term.
-spec fprint(term(), pos_integer(), options()) -> string().
fprint(T, Max, Options) ->
    {L, _} = print(T, Max, prepare_options(Options, #print_options{})),
    lists:flatten(L).

%% @doc Same as print, but never crashes.
%%
%% This is a tradeoff. Print might conceivably crash if it's asked to
%% print something it doesn't understand, for example some new data
%% type in a future version of Erlang. If print crashes, we fall back
%% to io_lib to format the term, but then the formatting is
%% depth-limited instead of length limited, so you might run out
%% memory printing it. Out of the frying pan and into the fire.
%%
-spec safe(term(), pos_integer()) -> {string(), pos_integer()} | {string()}.
safe(What, Len) ->
    case catch print(What, Len) of
        {L, Used} when is_list(L) -> {L, Used};
        _ -> {"unable to print" ++ io_lib:write(What, 99)}
    end.

%% @doc Returns {List, Length}
-spec print(term(), pos_integer()) -> {iolist(), pos_integer()}.
print(Term, Max) ->
    print(Term, Max, []).

%% @doc Returns {List, Length}
-spec print(term(), pos_integer(), options() | #print_options{}) -> {iolist(), pos_integer()}.
print(Term, Max, Options) when is_list(Options) ->
    %% need to convert the proplist to a record
    print(Term, Max, prepare_options(Options, #print_options{}));
print(Term, _Max, #print_options{force_strings = true}) when
    not is_list(Term), not is_binary(Term), not is_atom(Term)
->
    erlang:error(badarg);
print(_, Max, _Options) when Max < 0 -> {"...", 3};
print(_, _, #print_options{depth = 0}) ->
    {"...", 3};
%% @doc We assume atoms, floats, funs, integers, PIDs, ports and refs never need
%% to be truncated. This isn't strictly true, someone could make an
%% arbitrarily long bignum. Let's assume that won't happen unless someone
%% is being malicious.
%%
print(Atom, _Max, #print_options{force_strings = NoQuote}) when is_atom(Atom) ->
    L = atom_to_list(Atom),
    R =
        case atom_needs_quoting_start(L) andalso not NoQuote of
            true -> lists:flatten([$', L, $']);
            false -> L
        end,
    {R, length(R)};
print(<<>>, _Max, #print_options{depth = 1}) ->
    {"<<>>", 4};
print(Bin, _Max, #print_options{depth = 1}) when is_binary(Bin) ->
    {"<<...>>", 7};
print(<<>>, _Max, Options) ->
    case Options#print_options.force_strings of
        true ->
            {"", 0};
        false ->
            {"<<>>", 4}
    end;
print(Binary, 0, _Options) when is_bitstring(Binary) ->
    {"<<..>>", 6};
print(Bin, Max, _Options) when is_binary(Bin), Max < 2 ->
    {"<<...>>", 7};
print(Binary, Max, Options) when is_binary(Binary) ->
    B = binary_to_list(Binary, 1, lists:min([Max, byte_size(Binary)])),
    {Res, Length} =
        case
            Options#print_options.lists_as_strings orelse
                Options#print_options.force_strings
        of
            true ->
                Depth = Options#print_options.depth,
                MaxSize = (Depth - 1) * 4,
                %% check if we need to truncate based on depth
                In =
                    case
                        Depth > -1 andalso MaxSize < length(B) andalso
                            not Options#print_options.force_strings
                    of
                        true ->
                            string:substr(B, 1, MaxSize);
                        false ->
                            B
                    end,
                MaxLen =
                    case Options#print_options.force_strings of
                        true ->
                            Max;
                        false ->
                            %% make room for the leading doublequote
                            Max - 1
                    end,
                try alist(In, MaxLen, Options) of
                    {L0, Len0} ->
                        case Options#print_options.force_strings of
                            false ->
                                case B /= In of
                                    true ->
                                        {[$", L0, "..."], Len0 + 4};
                                    false ->
                                        {[$" | L0], Len0 + 1}
                                end;
                            true ->
                                {L0, Len0}
                        end
                catch
                    throw:{unprintable, C} ->
                        Index = string:chr(In, C),
                        case
                            Index > 1 andalso Options#print_options.depth =< Index andalso
                                Options#print_options.depth > -1 andalso
                                not Options#print_options.force_strings
                        of
                            true ->
                                %% print first Index-1 characters followed by ...
                                {L0, Len0} = alist_start(
                                    string:substr(In, 1, Index - 1), Max - 1, Options
                                ),
                                {L0 ++ "...", Len0 + 3};
                            false ->
                                list_body(In, Max - 4, dec_depth(Options), true)
                        end
                end;
            _ ->
                list_body(B, Max - 4, dec_depth(Options), true)
        end,
    case Options#print_options.force_strings of
        true ->
            {Res, Length};
        _ ->
            {["<<", Res, ">>"], Length + 4}
    end;
%% bitstrings are binary's evil brother who doesn't end on an 8 bit boundary.
%% This makes printing them extremely annoying, so list_body/list_bodyc has
%% some magic for dealing with the output of bitstring_to_list, which returns
%% a list of integers (as expected) but with a trailing binary that represents
%% the remaining bits.
print({inline_bitstring, B}, _Max, _Options) when is_bitstring(B) ->
    Size = bit_size(B),
    <<Value:Size>> = B,
    ValueStr = integer_to_list(Value),
    SizeStr = integer_to_list(Size),
    {[ValueStr, $:, SizeStr], length(ValueStr) + length(SizeStr) + 1};
print(BitString, Max, Options) when is_bitstring(BitString) ->
    BL =
        case byte_size(BitString) > Max of
            true ->
                binary_to_list(BitString, 1, Max);
            _ ->
                R = erlang:bitstring_to_list(BitString),
                {Bytes, [Bits]} = lists:splitwith(fun erlang:is_integer/1, R),
                %% tag the trailing bits with a special tuple we catch when
                %% list_body calls print again
                Bytes ++ [{inline_bitstring, Bits}]
        end,
    {X, Len0} = list_body(BL, Max - 4, dec_depth(Options), true),
    {["<<", X, ">>"], Len0 + 4};
print(Float, _Max, _Options) when is_float(Float) ->
    %% use the same function io_lib:format uses to print floats
    %% float_to_list is way too verbose.
    L = io_lib_format:fwrite_g(Float),
    {L, length(L)};
print(Fun, Max, _Options) when is_function(Fun) ->
    L = erlang:fun_to_list(Fun),
    case length(L) > Max of
        true ->
            S = erlang:max(5, Max),
            Res = string:substr(L, 1, S) ++ "..>",
            {Res, length(Res)};
        _ ->
            {L, length(L)}
    end;
print(Integer, _Max, _Options) when is_integer(Integer) ->
    L = integer_to_list(Integer),
    {L, length(L)};
print(Pid, _Max, _Options) when is_pid(Pid) ->
    L = pid_to_list(Pid),
    {L, length(L)};
print(Ref, _Max, _Options) when is_reference(Ref) ->
    L = erlang:ref_to_list(Ref),
    {L, length(L)};
print(Port, _Max, _Options) when is_port(Port) ->
    L = erlang:port_to_list(Port),
    {L, length(L)};
print({'$lager_record', Name, Fields}, Max, Options) ->
    Leader = "#" ++ atom_to_list(Name) ++ "{",
    {RC, Len} = record_fields(Fields, Max - length(Leader) + 1, dec_depth(Options)),
    {[Leader, RC, "}"], Len + length(Leader) + 1};
print(Tuple, Max, Options) when is_tuple(Tuple) ->
    {TC, Len} = tuple_contents(Tuple, Max - 2, Options),
    {[${, TC, $}], Len + 2};
print(List, Max, Options) when is_list(List) ->
    case
        Options#print_options.lists_as_strings orelse
            Options#print_options.force_strings
    of
        true ->
            alist_start(List, Max, dec_depth(Options));
        _ ->
            {R, Len} = list_body(List, Max - 2, dec_depth(Options), false),
            {[$[, R, $]], Len + 2}
    end;
print(Map, Max, Options) ->
    case erlang:is_builtin(erlang, is_map, 1) andalso erlang:is_map(Map) of
        true ->
            {MapBody, Len} = map_body(Map, Max - 3, dec_depth(Options)),
            {[$#, ${, MapBody, $}], Len + 3};
        false ->
            error(badarg, [Map, Max, Options])
    end.

%% Returns {List, Length}
tuple_contents(Tuple, Max, Options) ->
    L = tuple_to_list(Tuple),
    list_body(L, Max, dec_depth(Options), true).

%% Format the inside of a list, i.e. do not add a leading [ or trailing ].
%% Returns {List, Length}
list_body([], _Max, _Options, _Tuple) ->
    {[], 0};
list_body(_, Max, _Options, _Tuple) when Max < 4 -> {"...", 3};
list_body(_, _Max, #print_options{depth = 0}, _Tuple) ->
    {"...", 3};
list_body([H], Max, Options = #print_options{depth = 1}, _Tuple) ->
    print(H, Max, Options);
list_body([H | _], Max, Options = #print_options{depth = 1}, Tuple) ->
    {List, Len} = print(H, Max - 4, Options),
    Sep =
        case Tuple of
            true -> $,;
            false -> $|
        end,
    {[List ++ [Sep | "..."]], Len + 4};
list_body([H | T], Max, Options, Tuple) ->
    {List, Len} = print(H, Max, Options),
    {Final, FLen} = list_bodyc(T, Max - Len, Options, Tuple),
    {[List | Final], FLen + Len};
%% improper list
list_body(X, Max, Options, _Tuple) ->
    {List, Len} = print(X, Max - 1, Options),
    {[$|, List], Len + 1}.

list_bodyc([], _Max, _Options, _Tuple) ->
    {[], 0};
list_bodyc(_, Max, _Options, _Tuple) when Max < 5 -> {",...", 4};
list_bodyc(_, _Max, #print_options{depth = 1}, true) ->
    {",...", 4};
list_bodyc(_, _Max, #print_options{depth = 1}, false) ->
    {"|...", 4};
list_bodyc([H | T], Max, #print_options{depth = Depth} = Options, Tuple) ->
    {List, Len} = print(H, Max, dec_depth(Options)),
    {Final, FLen} = list_bodyc(T, Max - Len - 1, dec_depth(Options), Tuple),
    Sep =
        case Depth == 1 andalso not Tuple of
            true -> $|;
            _ -> $,
        end,
    {[Sep, List | Final], FLen + Len + 1};
%% improper list
list_bodyc(X, Max, Options, _Tuple) ->
    {List, Len} = print(X, Max - 1, Options),
    {[$|, List], Len + 1}.

map_body(Map, Max, #print_options{depth = Depth}) when Max < 4; Depth =:= 0 ->
    case erlang:map_size(Map) of
        0 -> {[], 0};
        _ -> {"...", 3}
    end;
map_body(Map, Max, Options) ->
    case maps:to_list(Map) of
        [] ->
            {[], 0};
        [{Key, Value} | Rest] ->
            {KeyStr, KeyLen} = print(Key, Max - 4, Options),
            DiffLen = KeyLen + 4,
            {ValueStr, ValueLen} = print(Value, Max - DiffLen, Options),
            DiffLen2 = DiffLen + ValueLen,
            {Final, FLen} = map_bodyc(Rest, Max - DiffLen2, dec_depth(Options)),
            {[KeyStr, " => ", ValueStr | Final], DiffLen2 + FLen}
    end.

map_bodyc([], _Max, _Options) ->
    {[], 0};
map_bodyc(_Rest, Max, #print_options{depth = Depth}) when Max < 5; Depth =:= 0 ->
    {",...", 4};
map_bodyc([{Key, Value} | Rest], Max, Options) ->
    {KeyStr, KeyLen} = print(Key, Max - 5, Options),
    DiffLen = KeyLen + 5,
    {ValueStr, ValueLen} = print(Value, Max - DiffLen, Options),
    DiffLen2 = DiffLen + ValueLen,
    {Final, FLen} = map_bodyc(Rest, Max - DiffLen2, dec_depth(Options)),
    {[$,, KeyStr, " => ", ValueStr | Final], DiffLen2 + FLen}.

%% The head of a list we hope is ascii. Examples:
%%
%% [65,66,67] -> "ABC"
%% [65,0,67] -> "A"[0,67]
%% [0,65,66] -> [0,65,66]
%% [65,b,66] -> "A"[b,66]
%%
alist_start([], _Max, #print_options{force_strings = true}) ->
    {"", 0};
alist_start([], _Max, _Options) ->
    {"[]", 2};
alist_start(_, Max, _Options) when Max < 4 -> {"...", 3};
alist_start(_, _Max, #print_options{depth = 0}) ->
    {"[...]", 5};
alist_start(L, Max, #print_options{force_strings = true} = Options) ->
    alist(L, Max, Options);
%alist_start([H|_T], _Max, #print_options{depth=1}) when is_integer(H) -> {[$[, H, $|, $., $., $., $]], 7};

% definitely printable
alist_start([H | T], Max, Options) when is_integer(H), H >= 16#20, H =< 16#7e ->
    try alist([H | T], Max - 1, Options) of
        {L, Len} ->
            {[$" | L], Len + 1}
    catch
        throw:{unprintable, _} ->
            {R, Len} = list_body([H | T], Max - 2, Options, false),
            {[$[, R, $]], Len + 2}
    end;
% definitely printable
alist_start([H | T], Max, Options) when is_integer(H), H >= 16#a0, H =< 16#ff ->
    try alist([H | T], Max - 1, Options) of
        {L, Len} ->
            {[$" | L], Len + 1}
    catch
        throw:{unprintable, _} ->
            {R, Len} = list_body([H | T], Max - 2, Options, false),
            {[$[, R, $]], Len + 2}
    end;
alist_start([H | T], Max, Options) when
    H =:= $\t; H =:= $\n; H =:= $\r; H =:= $\v; H =:= $\e; H =:= $\f; H =:= $\b
->
    try alist([H | T], Max - 1, Options) of
        {L, Len} ->
            {[$" | L], Len + 1}
    catch
        throw:{unprintable, _} ->
            {R, Len} = list_body([H | T], Max - 2, Options, false),
            {[$[, R, $]], Len + 2}
    end;
alist_start(L, Max, Options) ->
    {R, Len} = list_body(L, Max - 2, Options, false),
    {[$[, R, $]], Len + 2}.

alist([], _Max, #print_options{force_strings = true}) ->
    {"", 0};
alist([], _Max, _Options) ->
    {"\"", 1};
alist(_, Max, #print_options{force_strings = true}) when Max < 4 -> {"...", 3};
alist(_, Max, #print_options{force_strings = false}) when Max < 5 -> {"...\"", 4};
alist([H | T], Max, Options = #print_options{force_strings = false, lists_as_strings = true}) when
    H =:= $"; H =:= $\\
->
    %% preserve escaping around quotes
    {L, Len} = alist(T, Max - 1, Options),
    {[$\\, H | L], Len + 2};
% definitely printable
alist([H | T], Max, Options) when is_integer(H), H >= 16#20, H =< 16#7e ->
    {L, Len} = alist(T, Max - 1, Options),
    {[H | L], Len + 1};
% definitely printable
alist([H | T], Max, Options) when is_integer(H), H >= 16#a0, H =< 16#ff ->
    {L, Len} = alist(T, Max - 1, Options),
    {[H | L], Len + 1};
alist([H | T], Max, Options) when
    H =:= $\t; H =:= $\n; H =:= $\r; H =:= $\v; H =:= $\e; H =:= $\f; H =:= $\b
->
    {L, Len} = alist(T, Max - 1, Options),
    case Options#print_options.force_strings of
        true ->
            {[H | L], Len + 1};
        _ ->
            {[escape(H) | L], Len + 1}
    end;
alist([H | T], Max, #print_options{force_strings = true} = Options) when is_integer(H) ->
    {L, Len} = alist(T, Max - 1, Options),
    {[H | L], Len + 1};
alist([H | T], Max, Options = #print_options{force_strings = true}) when is_binary(H); is_list(H) ->
    {List, Len} = print(H, Max, Options),
    case (Max - Len) =< 0 of
        true ->
            %% no more room to print anything
            {List, Len};
        false ->
            %% no need to decrement depth, as we're in printable string mode
            {Final, FLen} = alist(T, Max - Len, Options),
            {[List | Final], FLen + Len}
    end;
alist(_, _, #print_options{force_strings = true}) ->
    erlang:error(badarg);
alist([H | _L], _Max, _Options) ->
    throw({unprintable, H});
alist(H, _Max, _Options) ->
    %% improper list
    throw({unprintable, H}).

%% is the first character in the atom alphabetic & lowercase?
atom_needs_quoting_start([H | T]) when H >= $a, H =< $z ->
    atom_needs_quoting(T);
atom_needs_quoting_start(_) ->
    true.

atom_needs_quoting([]) ->
    false;
atom_needs_quoting([H | T]) when
    (H >= $a andalso H =< $z);
    (H >= $A andalso H =< $Z);
    (H >= $0 andalso H =< $9);
    H == $@;
    H == $_
->
    atom_needs_quoting(T);
atom_needs_quoting(_) ->
    true.

-spec prepare_options(options(), #print_options{}) -> #print_options{}.
prepare_options([], Options) ->
    Options;
prepare_options([{depth, Depth} | T], Options) when is_integer(Depth) ->
    prepare_options(T, Options#print_options{depth = Depth});
prepare_options([{lists_as_strings, Bool} | T], Options) when is_boolean(Bool) ->
    prepare_options(T, Options#print_options{lists_as_strings = Bool});
prepare_options([{force_strings, Bool} | T], Options) when is_boolean(Bool) ->
    prepare_options(T, Options#print_options{force_strings = Bool}).

dec_depth(#print_options{depth = Depth} = Options) when Depth > 0 ->
    Options#print_options{depth = Depth - 1};
dec_depth(Options) ->
    Options.

escape($\t) -> "\\t";
escape($\n) -> "\\n";
escape($\r) -> "\\r";
escape($\e) -> "\\e";
escape($\f) -> "\\f";
escape($\b) -> "\\b";
escape($\v) -> "\\v".

record_fields([], _, _) ->
    {"", 0};
record_fields(_, Max, #print_options{depth = D}) when Max < 4; D == 0 ->
    {"...", 3};
record_fields([{Field, Value} | T], Max, Options) ->
    {ExtraChars, Terminator} =
        case T of
            [] ->
                {1, []};
            _ ->
                {2, ","}
        end,
    {FieldStr, FieldLen} = print(Field, Max - ExtraChars, Options),
    {ValueStr, ValueLen} = print(Value, Max - (FieldLen + ExtraChars), Options),
    {Final, FLen} = record_fields(T, Max - (FieldLen + ValueLen + ExtraChars), dec_depth(Options)),
    {[FieldStr ++ "=" ++ ValueStr ++ Terminator | Final], FLen + FieldLen + ValueLen + ExtraChars}.

-ifdef(TEST).
%%--------------------
%% The start of a test suite. So far, it only checks for not crashing.
format_test() ->
    %% simple format strings
    ?assertEqual("foobar", lists:flatten(format("~s", [["foo", $b, $a, $r]], 50))),
    ?assertEqual("[\"foo\",98,97,114]", lists:flatten(format("~p", [["foo", $b, $a, $r]], 50))),
    ?assertEqual("[\"foo\",98,97,114]", lists:flatten(format("~P", [["foo", $b, $a, $r], 10], 50))),
    ?assertEqual(
        "[[102,111,111],98,97,114]", lists:flatten(format("~w", [["foo", $b, $a, $r]], 50))
    ),

    %% complex ones
    ?assertEqual("    foobar", lists:flatten(format("~10s", [["foo", $b, $a, $r]], 50))),
    ?assertEqual("f", lists:flatten(format("~1s", [["foo", $b, $a, $r]], 50))),
    ?assertEqual("[\"foo\",98,97,114]", lists:flatten(format("~22p", [["foo", $b, $a, $r]], 50))),
    ?assertEqual(
        "[\"foo\",98,97,114]", lists:flatten(format("~22P", [["foo", $b, $a, $r], 10], 50))
    ),
    ?assertEqual("**********", lists:flatten(format("~10W", [["foo", $b, $a, $r], 10], 50))),
    ?assertEqual(
        "[[102,111,111],98,97,114]", lists:flatten(format("~25W", [["foo", $b, $a, $r], 10], 50))
    ),
    % Note these next two diverge from io_lib:format; the field width is
    % ignored, when it should be used as max line length.
    ?assertEqual("[\"foo\",98,97,114]", lists:flatten(format("~10p", [["foo", $b, $a, $r]], 50))),
    ?assertEqual(
        "[\"foo\",98,97,114]", lists:flatten(format("~10P", [["foo", $b, $a, $r], 10], 50))
    ),
    ok.

atom_quoting_test() ->
    ?assertEqual("hello", lists:flatten(format("~p", [hello], 50))),
    ?assertEqual("'hello world'", lists:flatten(format("~p", ['hello world'], 50))),
    ?assertEqual("'Hello world'", lists:flatten(format("~p", ['Hello world'], 50))),
    ?assertEqual("hello_world", lists:flatten(format("~p", ['hello_world'], 50))),
    ?assertEqual("'node@127.0.0.1'", lists:flatten(format("~p", ['node@127.0.0.1'], 50))),
    ?assertEqual("node@nohost", lists:flatten(format("~p", [node@nohost], 50))),
    ?assertEqual("abc123", lists:flatten(format("~p", [abc123], 50))),
    ok.

sane_float_printing_test() ->
    ?assertEqual("1.0", lists:flatten(format("~p", [1.0], 50))),
    ?assertEqual("1.23456789", lists:flatten(format("~p", [1.23456789], 50))),
    ?assertEqual("1.23456789", lists:flatten(format("~p", [1.234567890], 50))),
    ?assertEqual("0.3333333333333333", lists:flatten(format("~p", [1 / 3], 50))),
    ?assertEqual("0.1234567", lists:flatten(format("~p", [0.1234567], 50))),
    ok.

float_inside_list_test() ->
    ?assertEqual(
        "[97,38.233913133184835,99]",
        lists:flatten(format("~p", [[$a, 38.233913133184835, $c]], 50))
    ),
    ?assertError(badarg, lists:flatten(format("~s", [[$a, 38.233913133184835, $c]], 50))),
    ok.

quote_strip_test() ->
    ?assertEqual("\"hello\"", lists:flatten(format("~p", ["hello"], 50))),
    ?assertEqual("hello", lists:flatten(format("~s", ["hello"], 50))),
    ?assertEqual("hello", lists:flatten(format("~s", [hello], 50))),
    ?assertEqual("hello", lists:flatten(format("~p", [hello], 50))),
    ?assertEqual("'hello world'", lists:flatten(format("~p", ['hello world'], 50))),
    ?assertEqual("hello world", lists:flatten(format("~s", ['hello world'], 50))),
    ok.

binary_printing_test() ->
    ?assertEqual("<<>>", lists:flatten(format("~p", [<<>>], 50))),
    ?assertEqual("", lists:flatten(format("~s", [<<>>], 50))),
    ?assertEqual("<<..>>", lists:flatten(format("~p", [<<"hi">>], 0))),
    ?assertEqual("<<...>>", lists:flatten(format("~p", [<<"hi">>], 1))),
    ?assertEqual("<<\"hello\">>", lists:flatten(format("~p", [<<$h, $e, $l, $l, $o>>], 50))),
    ?assertEqual("<<\"hello\">>", lists:flatten(format("~p", [<<"hello">>], 50))),
    ?assertEqual("<<104,101,108,108,111>>", lists:flatten(format("~w", [<<"hello">>], 50))),
    ?assertEqual("<<1,2,3,4>>", lists:flatten(format("~p", [<<1, 2, 3, 4>>], 50))),
    ?assertEqual([1, 2, 3, 4], lists:flatten(format("~s", [<<1, 2, 3, 4>>], 50))),
    ?assertEqual("hello", lists:flatten(format("~s", [<<"hello">>], 50))),
    ?assertEqual("hello\nworld", lists:flatten(format("~s", [<<"hello\nworld">>], 50))),
    ?assertEqual("<<\"hello\\nworld\">>", lists:flatten(format("~p", [<<"hello\nworld">>], 50))),
    ?assertEqual(
        "<<\"\\\"hello world\\\"\">>", lists:flatten(format("~p", [<<"\"hello world\"">>], 50))
    ),
    ?assertEqual("<<\"hello\\\\world\">>", lists:flatten(format("~p", [<<"hello\\world">>], 50))),
    ?assertEqual("<<\"hello\\\\\world\">>", lists:flatten(format("~p", [<<"hello\\\world">>], 50))),
    ?assertEqual(
        "<<\"hello\\\\\\\\world\">>", lists:flatten(format("~p", [<<"hello\\\\world">>], 50))
    ),
    ?assertEqual("<<\"hello\\bworld\">>", lists:flatten(format("~p", [<<"hello\bworld">>], 50))),
    ?assertEqual("<<\"hello\\tworld\">>", lists:flatten(format("~p", [<<"hello\tworld">>], 50))),
    ?assertEqual("<<\"hello\\nworld\">>", lists:flatten(format("~p", [<<"hello\nworld">>], 50))),
    ?assertEqual("<<\"hello\\rworld\">>", lists:flatten(format("~p", [<<"hello\rworld">>], 50))),
    ?assertEqual("<<\"hello\\eworld\">>", lists:flatten(format("~p", [<<"hello\eworld">>], 50))),
    ?assertEqual("<<\"hello\\fworld\">>", lists:flatten(format("~p", [<<"hello\fworld">>], 50))),
    ?assertEqual("<<\"hello\\vworld\">>", lists:flatten(format("~p", [<<"hello\vworld">>], 50))),
    ?assertEqual("     hello", lists:flatten(format("~10s", [<<"hello">>], 50))),
    ?assertEqual("[a]", lists:flatten(format("~s", [<<"[a]">>], 50))),
    ?assertEqual("[a]", lists:flatten(format("~s", [[<<"[a]">>]], 50))),

    ok.

bitstring_printing_test() ->
    ?assertEqual(
        "<<1,2,3,1:7>>",
        lists:flatten(
            format(
                "~p",
                [<<1, 2, 3, 1:7>>],
                100
            )
        )
    ),
    ?assertEqual(
        "<<1:7>>",
        lists:flatten(
            format(
                "~p",
                [<<1:7>>],
                100
            )
        )
    ),
    ?assertEqual(
        "<<1,2,3,...>>",
        lists:flatten(
            format(
                "~p",
                [<<1, 2, 3, 1:7>>],
                12
            )
        )
    ),
    ?assertEqual(
        "<<1,2,3,...>>",
        lists:flatten(
            format(
                "~p",
                [<<1, 2, 3, 1:7>>],
                13
            )
        )
    ),
    ?assertEqual(
        "<<1,2,3,1:7>>",
        lists:flatten(
            format(
                "~p",
                [<<1, 2, 3, 1:7>>],
                14
            )
        )
    ),
    ?assertEqual("<<..>>", lists:flatten(format("~p", [<<1:7>>], 0))),
    ?assertEqual("<<...>>", lists:flatten(format("~p", [<<1:7>>], 1))),
    ?assertEqual(
        "[<<1>>,<<2>>]",
        lists:flatten(
            format(
                "~p",
                [[<<1>>, <<2>>]],
                100
            )
        )
    ),
    ?assertEqual("{<<1:7>>}", lists:flatten(format("~p", [{<<1:7>>}], 50))),
    ok.

list_printing_test() ->
    ?assertEqual("[]", lists:flatten(format("~p", [[]], 50))),
    ?assertEqual("[]", lists:flatten(format("~w", [[]], 50))),
    ?assertEqual("", lists:flatten(format("~s", [[]], 50))),
    ?assertEqual("...", lists:flatten(format("~s", [[]], -1))),
    ?assertEqual("[[]]", lists:flatten(format("~p", [[[]]], 50))),
    ?assertEqual("[13,11,10,8,5,4]", lists:flatten(format("~p", [[13, 11, 10, 8, 5, 4]], 50))),
    ?assertEqual("\"\\rabc\"", lists:flatten(format("~p", [[13, $a, $b, $c]], 50))),
    ?assertEqual("[1,2,3|4]", lists:flatten(format("~p", [[1, 2, 3 | 4]], 50))),
    ?assertEqual("[...]", lists:flatten(format("~p", [[1, 2, 3, 4]], 4))),
    ?assertEqual("[1,...]", lists:flatten(format("~p", [[1, 2, 3, 4]], 6))),
    ?assertEqual("[1,...]", lists:flatten(format("~p", [[1, 2, 3, 4]], 7))),
    ?assertEqual("[1,2,...]", lists:flatten(format("~p", [[1, 2, 3, 4]], 8))),
    ?assertEqual("[1|4]", lists:flatten(format("~p", [[1 | 4]], 50))),
    ?assertEqual("[1]", lists:flatten(format("~p", [[1]], 50))),
    ?assertError(badarg, lists:flatten(format("~s", [[1 | 4]], 50))),
    ?assertEqual("\"hello...\"", lists:flatten(format("~p", ["hello world"], 10))),
    ?assertEqual("hello w...", lists:flatten(format("~s", ["hello world"], 10))),
    ?assertEqual("hello world\r\n", lists:flatten(format("~s", ["hello world\r\n"], 50))),
    ?assertEqual("\rhello world\r\n", lists:flatten(format("~s", ["\rhello world\r\n"], 50))),
    ?assertEqual(
        "\"\\rhello world\\r\\n\"", lists:flatten(format("~p", ["\rhello world\r\n"], 50))
    ),
    ?assertEqual(
        "[13,104,101,108,108,111,32,119,111,114,108,100,13,10]",
        lists:flatten(format("~w", ["\rhello world\r\n"], 60))
    ),
    ?assertEqual("...", lists:flatten(format("~s", ["\rhello world\r\n"], 3))),
    ?assertEqual(
        "[22835963083295358096932575511191922182123945984,...]",
        lists:flatten(
            format(
                "~p",
                [
                    [
                        22835963083295358096932575511191922182123945984,
                        22835963083295358096932575511191922182123945984
                    ]
                ],
                9
            )
        )
    ),
    ?assertEqual(
        "[22835963083295358096932575511191922182123945984,...]",
        lists:flatten(
            format(
                "~p",
                [
                    [
                        22835963083295358096932575511191922182123945984,
                        22835963083295358096932575511191922182123945984
                    ]
                ],
                53
            )
        )
    ),
    %%improper list
    ?assertEqual("[1,2,3|4]", lists:flatten(format("~P", [[1 | [2 | [3 | 4]]], 5], 50))),
    ?assertEqual("[1|1]", lists:flatten(format("~P", [[1 | 1], 5], 50))),
    ?assertEqual("[9|9]", lists:flatten(format("~p", [[9 | 9]], 50))),
    ok.

iolist_printing_test() ->
    ?assertEqual(
        "iolist: HelloIamaniolist",
        lists:flatten(
            format(
                "iolist: ~s",
                [[$H, $e, $l, $l, $o, "I", ["am", [<<"an">>], [$i, $o, $l, $i, $s, $t]]]],
                1000
            )
        )
    ),
    ?assertEqual(
        "123...",
        lists:flatten(format("~s", [[<<"123456789">>, "HellIamaniolist"]], 6))
    ),
    ?assertEqual(
        "123456...",
        lists:flatten(format("~s", [[<<"123456789">>, "HellIamaniolist"]], 9))
    ),
    ?assertEqual(
        "123456789H...",
        lists:flatten(format("~s", [[<<"123456789">>, "HellIamaniolist"]], 13))
    ),
    ?assertEqual(
        "123456789HellIamaniolist",
        lists:flatten(format("~s", [[<<"123456789">>, "HellIamaniolist"]], 30))
    ),

    ok.

tuple_printing_test() ->
    ?assertEqual("{}", lists:flatten(format("~p", [{}], 50))),
    ?assertEqual("{}", lists:flatten(format("~w", [{}], 50))),
    ?assertError(badarg, lists:flatten(format("~s", [{}], 50))),
    ?assertEqual("{...}", lists:flatten(format("~p", [{foo}], 1))),
    ?assertEqual("{...}", lists:flatten(format("~p", [{foo}], 2))),
    ?assertEqual("{...}", lists:flatten(format("~p", [{foo}], 3))),
    ?assertEqual("{...}", lists:flatten(format("~p", [{foo}], 4))),
    ?assertEqual("{...}", lists:flatten(format("~p", [{foo}], 5))),
    ?assertEqual("{foo,...}", lists:flatten(format("~p", [{foo, bar}], 6))),
    ?assertEqual("{foo,...}", lists:flatten(format("~p", [{foo, bar}], 7))),
    ?assertEqual("{foo,...}", lists:flatten(format("~p", [{foo, bar}], 9))),
    ?assertEqual("{foo,bar}", lists:flatten(format("~p", [{foo, bar}], 10))),
    ?assertEqual(
        "{22835963083295358096932575511191922182123945984,...}",
        lists:flatten(
            format(
                "~w",
                [
                    {22835963083295358096932575511191922182123945984,
                        22835963083295358096932575511191922182123945984}
                ],
                10
            )
        )
    ),
    ?assertEqual(
        "{22835963083295358096932575511191922182123945984,...}",
        lists:flatten(
            format(
                "~w",
                [
                    {22835963083295358096932575511191922182123945984, bar}
                ],
                10
            )
        )
    ),
    ?assertEqual(
        "{22835963083295358096932575511191922182123945984,...}",
        lists:flatten(
            format(
                "~w",
                [
                    {22835963083295358096932575511191922182123945984,
                        22835963083295358096932575511191922182123945984}
                ],
                53
            )
        )
    ),
    ok.

map_printing_test() ->
    case erlang:is_builtin(erlang, is_map, 1) of
        true ->
            ?assertEqual("#{}", lists:flatten(format("~p", [maps:new()], 50))),
            ?assertEqual("#{}", lists:flatten(format("~p", [maps:new()], 3))),
            ?assertEqual("#{}", lists:flatten(format("~w", [maps:new()], 50))),
            ?assertError(badarg, lists:flatten(format("~s", [maps:new()], 50))),
            ?assertEqual("#{...}", lists:flatten(format("~p", [maps:from_list([{bar, foo}])], 1))),
            ?assertEqual("#{...}", lists:flatten(format("~p", [maps:from_list([{bar, foo}])], 6))),
            ?assertEqual(
                "#{bar => ...}", lists:flatten(format("~p", [maps:from_list([{bar, foo}])], 7))
            ),
            ?assertEqual(
                "#{bar => ...}", lists:flatten(format("~p", [maps:from_list([{bar, foo}])], 9))
            ),
            ?assertEqual(
                "#{bar => foo}", lists:flatten(format("~p", [maps:from_list([{bar, foo}])], 10))
            ),
            ?assertEqual(
                "#{bar => ...,...}",
                lists:flatten(format("~p", [maps:from_list([{bar, foo}, {foo, bar}])], 9))
            ),
            ?assertEqual(
                "#{bar => foo,...}",
                lists:flatten(format("~p", [maps:from_list([{bar, foo}, {foo, bar}])], 10))
            ),
            ?assertEqual(
                "#{bar => foo,...}",
                lists:flatten(format("~p", [maps:from_list([{bar, foo}, {foo, bar}])], 17))
            ),
            ?assertEqual(
                "#{bar => foo,foo => ...}",
                lists:flatten(format("~p", [maps:from_list([{bar, foo}, {foo, bar}])], 18))
            ),
            ?assertEqual(
                "#{bar => foo,foo => ...}",
                lists:flatten(format("~p", [maps:from_list([{bar, foo}, {foo, bar}])], 19))
            ),
            ?assertEqual(
                "#{bar => foo,foo => ...}",
                lists:flatten(format("~p", [maps:from_list([{bar, foo}, {foo, bar}])], 20))
            ),
            ?assertEqual(
                "#{bar => foo,foo => bar}",
                lists:flatten(format("~p", [maps:from_list([{bar, foo}, {foo, bar}])], 21))
            ),
            ?assertEqual(
                "#{22835963083295358096932575511191922182123945984 => ...}",
                lists:flatten(
                    format(
                        "~w",
                        [
                            maps:from_list([
                                {22835963083295358096932575511191922182123945984,
                                    22835963083295358096932575511191922182123945984}
                            ])
                        ],
                        10
                    )
                )
            ),
            ?assertEqual(
                "#{22835963083295358096932575511191922182123945984 => ...}",
                lists:flatten(
                    format(
                        "~w",
                        [
                            maps:from_list([{22835963083295358096932575511191922182123945984, bar}])
                        ],
                        10
                    )
                )
            ),
            ?assertEqual(
                "#{22835963083295358096932575511191922182123945984 => ...}",
                lists:flatten(
                    format(
                        "~w",
                        [
                            maps:from_list([{22835963083295358096932575511191922182123945984, bar}])
                        ],
                        53
                    )
                )
            ),
            ?assertEqual(
                "#{22835963083295358096932575511191922182123945984 => bar}",
                lists:flatten(
                    format(
                        "~w",
                        [
                            maps:from_list([{22835963083295358096932575511191922182123945984, bar}])
                        ],
                        54
                    )
                )
            ),
            ok;
        false ->
            ok
    end.

unicode_test() ->
    ?assertEqual([231, 167, 129], lists:flatten(format("~s", [<<231, 167, 129>>], 50))),
    ?assertEqual([31169], lists:flatten(format("~ts", [<<231, 167, 129>>], 50))),
    ok.

depth_limit_test() ->
    ?assertEqual("{...}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 1], 50))),
    ?assertEqual("{a,...}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 2], 50))),
    ?assertEqual("{a,[...]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 3], 50))),
    ?assertEqual("{a,[b|...]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 4], 50))),
    ?assertEqual("{a,[b,[...]]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 5], 50))),
    ?assertEqual("{a,[b,[c|...]]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 6], 50))),
    ?assertEqual("{a,[b,[c,[...]]]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 7], 50))),
    ?assertEqual("{a,[b,[c,[d]]]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 8], 50))),
    ?assertEqual("{a,[b,[c,[d]]]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 9], 50))),

    ?assertEqual("{a,{...}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 3], 50))),
    ?assertEqual("{a,{b,...}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 4], 50))),
    ?assertEqual("{a,{b,{...}}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 5], 50))),
    ?assertEqual("{a,{b,{c,...}}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 6], 50))),
    ?assertEqual("{a,{b,{c,{...}}}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 7], 50))),
    ?assertEqual("{a,{b,{c,{d}}}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 8], 50))),

    case erlang:is_builtin(erlang, is_map, 1) of
        true ->
            ?assertEqual(
                "#{a => #{...}}",
                lists:flatten(
                    format(
                        "~P",
                        [maps:from_list([{a, maps:from_list([{b, maps:from_list([{c, d}])}])}]), 2],
                        50
                    )
                )
            ),
            ?assertEqual(
                "#{a => #{b => #{...}}}",
                lists:flatten(
                    format(
                        "~P",
                        [maps:from_list([{a, maps:from_list([{b, maps:from_list([{c, d}])}])}]), 3],
                        50
                    )
                )
            ),
            ?assertEqual(
                "#{a => #{b => #{c => d}}}",
                lists:flatten(
                    format(
                        "~P",
                        [maps:from_list([{a, maps:from_list([{b, maps:from_list([{c, d}])}])}]), 4],
                        50
                    )
                )
            ),

            ?assertEqual("#{}", lists:flatten(format("~P", [maps:new(), 1], 50))),
            ?assertEqual(
                "#{...}",
                lists:flatten(format("~P", [maps:from_list([{1, 1}, {2, 2}, {3, 3}]), 1], 50))
            ),
            ?assertEqual(
                "#{1 => 1,...}",
                lists:flatten(format("~P", [maps:from_list([{1, 1}, {2, 2}, {3, 3}]), 2], 50))
            ),
            ?assertEqual(
                "#{1 => 1,2 => 2,...}",
                lists:flatten(format("~P", [maps:from_list([{1, 1}, {2, 2}, {3, 3}]), 3], 50))
            ),
            ?assertEqual(
                "#{1 => 1,2 => 2,3 => 3}",
                lists:flatten(format("~P", [maps:from_list([{1, 1}, {2, 2}, {3, 3}]), 4], 50))
            ),

            ok;
        false ->
            ok
    end,

    ?assertEqual("{\"a\",[...]}", lists:flatten(format("~P", [{"a", ["b", ["c", ["d"]]]}, 3], 50))),
    ?assertEqual(
        "{\"a\",[\"b\",[[...]|...]]}",
        lists:flatten(format("~P", [{"a", ["b", ["c", ["d"]]]}, 6], 50))
    ),
    ?assertEqual(
        "{\"a\",[\"b\",[\"c\",[\"d\"]]]}",
        lists:flatten(format("~P", [{"a", ["b", ["c", ["d"]]]}, 9], 50))
    ),

    ?assertEqual("[...]", lists:flatten(format("~P", [[1, 2, 3], 1], 50))),
    ?assertEqual("[1|...]", lists:flatten(format("~P", [[1, 2, 3], 2], 50))),
    ?assertEqual("[1,2|...]", lists:flatten(format("~P", [[1, 2, 3], 3], 50))),
    ?assertEqual("[1,2,3]", lists:flatten(format("~P", [[1, 2, 3], 4], 50))),

    ?assertEqual("{1,...}", lists:flatten(format("~P", [{1, 2, 3}, 2], 50))),
    ?assertEqual("{1,2,...}", lists:flatten(format("~P", [{1, 2, 3}, 3], 50))),
    ?assertEqual("{1,2,3}", lists:flatten(format("~P", [{1, 2, 3}, 4], 50))),

    ?assertEqual("{1,...}", lists:flatten(format("~P", [{1, 2, 3}, 2], 50))),
    ?assertEqual("[1,2|...]", lists:flatten(format("~P", [[1, 2, <<3>>], 3], 50))),
    ?assertEqual("[1,2,<<...>>]", lists:flatten(format("~P", [[1, 2, <<3>>], 4], 50))),
    ?assertEqual("[1,2,<<3>>]", lists:flatten(format("~P", [[1, 2, <<3>>], 5], 50))),

    ?assertEqual("<<...>>", lists:flatten(format("~P", [<<0, 0, 0, 0>>, 1], 50))),
    ?assertEqual("<<0,...>>", lists:flatten(format("~P", [<<0, 0, 0, 0>>, 2], 50))),
    ?assertEqual("<<0,0,...>>", lists:flatten(format("~P", [<<0, 0, 0, 0>>, 3], 50))),
    ?assertEqual("<<0,0,0,...>>", lists:flatten(format("~P", [<<0, 0, 0, 0>>, 4], 50))),
    ?assertEqual("<<0,0,0,0>>", lists:flatten(format("~P", [<<0, 0, 0, 0>>, 5], 50))),

    %% this is a seriously weird edge case
    ?assertEqual("<<\"   \"...>>", lists:flatten(format("~P", [<<32, 32, 32, 0>>, 2], 50))),
    ?assertEqual("<<\"   \"...>>", lists:flatten(format("~P", [<<32, 32, 32, 0>>, 3], 50))),
    ?assertEqual("<<\"   \"...>>", lists:flatten(format("~P", [<<32, 32, 32, 0>>, 4], 50))),
    ?assertEqual("<<32,32,32,0>>", lists:flatten(format("~P", [<<32, 32, 32, 0>>, 5], 50))),
    ?assertEqual("<<32,32,32,0>>", lists:flatten(format("~p", [<<32, 32, 32, 0>>], 50))),

    %% depth limiting for some reason works in 4 byte chunks on printable binaries?
    ?assertEqual("<<\"hell\"...>>", lists:flatten(format("~P", [<<"hello world">>, 2], 50))),
    ?assertEqual(
        "<<\"abcd\"...>>", lists:flatten(format("~P", [<<$a, $b, $c, $d, $e, 0>>, 2], 50))
    ),

    %% I don't even know...
    ?assertEqual("<<>>", lists:flatten(format("~P", [<<>>, 1], 50))),
    ?assertEqual("<<>>", lists:flatten(format("~W", [<<>>, 1], 50))),

    ?assertEqual("{abc,<<\"abc\\\"\">>}", lists:flatten(format("~P", [{abc, <<"abc\"">>}, 4], 50))),

    ok.

print_terms_without_format_string_test() ->
    ?assertError(badarg, format({hello, world}, [], 50)),
    ?assertError(badarg, format([{google, bomb}], [], 50)),
    ?assertError(badarg, format([$h, $e, $l, $l, $o, 3594], [], 50)),
    ?assertEqual("helloworld", lists:flatten(format([$h, $e, $l, $l, $o, "world"], [], 50))),
    ?assertEqual("hello", lists:flatten(format(<<"hello">>, [], 50))),
    ?assertEqual("hello", lists:flatten(format('hello', [], 50))),
    ?assertError(badarg, format(<<1, 2, 3, 1:7>>, [], 100)),
    ?assertError(badarg, format(65535, [], 50)),
    ok.

improper_io_list_test() ->
    ?assertEqual(">hello", lists:flatten(format('~s', [[$> | <<"hello">>]], 50))),
    ?assertEqual(">hello", lists:flatten(format('~ts', [[$> | <<"hello">>]], 50))),
    ?assertEqual("helloworld", lists:flatten(format('~ts', [[<<"hello">> | <<"world">>]], 50))),
    ok.

-endif.
