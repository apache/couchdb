%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2009 Juan Jose Comellas
%%% @doc Parses command line options with a format similar to that of GNU getopt.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(weatherreport_getopt).
-author('juanjo@comellas.org').

-export([parse/2, usage/2, usage/3, usage/4]).

-export_type([
    arg_type/0,
    arg_value/0,
    arg_spec/0,
    simple_option/0,
    compound_option/0,
    option/0,
    option_spec/0
]).

-define(TAB_LENGTH, 8).
%% Indentation of the help messages in number of tabs.
-define(INDENTATION, 3).

%% Position of each field in the option specification tuple.
-define(OPT_NAME, 1).
-define(OPT_SHORT, 2).
-define(OPT_LONG, 3).
-define(OPT_ARG, 4).
-define(OPT_HELP, 5).

-define(IS_OPT_SPEC(Opt), (tuple_size(Opt) =:= ?OPT_HELP)).

%% Atom indicating the data type that an argument can be converted to.
-type arg_type() :: 'atom' | 'binary' | 'boolean' | 'float' | 'integer' | 'string'.
%% Data type that an argument can be converted to.
-type arg_value() :: atom() | binary() | boolean() | float() | integer() | string().
%% Argument specification.
-type arg_spec() :: arg_type() | {arg_type(), arg_value()} | undefined.
%% Option type and optional default argument.
-type simple_option() :: atom().
-type compound_option() :: {atom(), arg_value()}.
-type option() :: simple_option() | compound_option().
%% Command line option specification.
-type option_spec() :: {
    Name :: atom(),
    Short :: char() | undefined,
    Long :: string() | undefined,
    ArgSpec :: arg_spec(),
    Help :: string() | undefined
}.
%% Output streams
-type output_stream() :: 'standard_io' | 'standard_error'.

%% @doc  Parse the command line options and arguments returning a list of tuples
%%       and/or atoms using the Erlang convention for sending options to a
%%       function.
-spec parse([option_spec()], string() | [string()]) ->
    {ok, {[option()], [string()]}} | {error, {Reason :: atom(), Data :: any()}}.
parse(OptSpecList, CmdLine) ->
    try
        Args =
            if
                is_integer(hd(CmdLine)) ->
                    string:tokens(CmdLine, " \t\n");
                true ->
                    CmdLine
            end,
        parse(OptSpecList, [], [], 0, Args)
    catch
        throw:{error, {_Reason, _Data}} = Error ->
            Error
    end.

-spec parse([option_spec()], [option()], [string()], integer(), [string()]) ->
    {ok, {[option()], [string()]}}.
%% Process the option terminator.
parse(OptSpecList, OptAcc, ArgAcc, _ArgPos, ["--" | Tail]) ->
    %% Any argument present after the terminator is not considered an option.
    {ok, {lists:reverse(append_default_options(OptSpecList, OptAcc)), lists:reverse(ArgAcc, Tail)}};
%% Process long options.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, ["--" ++ OptArg = OptStr | Tail]) ->
    parse_long_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Tail, OptStr, OptArg);
%% Process short options.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, ["-" ++ ([_Char | _] = OptArg) = OptStr | Tail]) ->
    parse_short_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Tail, OptStr, OptArg);
%% Process non-option arguments.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, [Arg | Tail]) ->
    case find_non_option_arg(OptSpecList, ArgPos) of
        {value, OptSpec} when ?IS_OPT_SPEC(OptSpec) ->
            parse(OptSpecList, add_option_with_arg(OptSpec, Arg, OptAcc), ArgAcc, ArgPos + 1, Tail);
        false ->
            parse(OptSpecList, OptAcc, [Arg | ArgAcc], ArgPos, Tail)
    end;
parse(OptSpecList, OptAcc, ArgAcc, _ArgPos, []) ->
    %% Once we have completed gathering the options we add the ones that were
    %% not present but had default arguments in the specification.
    {ok, {lists:reverse(append_default_options(OptSpecList, OptAcc)), lists:reverse(ArgAcc)}}.

%% @doc Parse a long option, add it to the option accumulator and continue
%%      parsing the rest of the arguments recursively.
%%      A long option can have the following syntax:
%%        --foo      Single option 'foo', no argument
%%        --foo=bar  Single option 'foo', argument "bar"
%%        --foo bar  Single option 'foo', argument "bar"
-spec parse_long_option(
    [option_spec()], [option()], [string()], integer(), [string()], string(), string()
) ->
    {ok, {[option()], [string()]}}.
parse_long_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, OptArg) ->
    case split_assigned_arg(OptArg) of
        {Long, Arg} ->
            %% Get option that has its argument within the same string
            %% separated by an equal ('=') character (e.g. "--port=1000").
            parse_long_option_assigned_arg(
                OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, Long, Arg
            );
        Long ->
            case lists:keyfind(Long, ?OPT_LONG, OptSpecList) of
                {Name, _Short, Long, undefined, _Help} ->
                    parse(OptSpecList, [Name | OptAcc], ArgAcc, ArgPos, Args);
                {_Name, _Short, Long, _ArgSpec, _Help} = OptSpec ->
                    %% The option argument string is empty, but the option requires
                    %% an argument, so we look into the next string in the list.
                    %% e.g ["--port", "1000"]
                    parse_long_option_next_arg(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptSpec);
                false ->
                    throw({error, {invalid_option, OptStr}})
            end
    end.

%% @doc Parse an option where the argument is 'assigned' in the same string using
%%      the '=' character, add it to the option accumulator and continue parsing the
%%      rest of the arguments recursively. This syntax is only valid for long options.
-spec parse_long_option_assigned_arg(
    [option_spec()],
    [option()],
    [string()],
    integer(),
    [string()],
    string(),
    string(),
    string()
) ->
    {ok, {[option()], [string()]}}.
parse_long_option_assigned_arg(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, Long, Arg) ->
    case lists:keyfind(Long, ?OPT_LONG, OptSpecList) of
        {_Name, _Short, Long, ArgSpec, _Help} = OptSpec ->
            case ArgSpec of
                undefined ->
                    throw({error, {invalid_option_arg, OptStr}});
                _ ->
                    parse(
                        OptSpecList,
                        add_option_with_assigned_arg(OptSpec, Arg, OptAcc),
                        ArgAcc,
                        ArgPos,
                        Args
                    )
            end;
        false ->
            throw({error, {invalid_option, OptStr}})
    end.

%% @doc Split an option string that may contain an option with its argument
%%      separated by an equal ('=') character (e.g. "port=1000").
-spec split_assigned_arg(string()) -> {Name :: string(), Arg :: string()} | string().
split_assigned_arg(OptStr) ->
    split_assigned_arg(OptStr, OptStr, []).

split_assigned_arg(_OptStr, "=" ++ Tail, Acc) ->
    {lists:reverse(Acc), Tail};
split_assigned_arg(OptStr, [Char | Tail], Acc) ->
    split_assigned_arg(OptStr, Tail, [Char | Acc]);
split_assigned_arg(OptStr, [], _Acc) ->
    OptStr.

%% @doc Retrieve the argument for an option from the next string in the list of
%%      command-line parameters or set the value of the argument from the argument
%%      specification (for boolean and integer arguments), if possible.
parse_long_option_next_arg(
    OptSpecList, OptAcc, ArgAcc, ArgPos, Args, {Name, _Short, _Long, ArgSpec, _Help} = OptSpec
) ->
    ArgSpecType = arg_spec_type(ArgSpec),
    case Args =:= [] orelse is_implicit_arg(ArgSpecType, hd(Args)) of
        true ->
            parse(OptSpecList, add_option_with_implicit_arg(OptSpec, OptAcc), ArgAcc, ArgPos, Args);
        false ->
            [Arg | Tail] = Args,
            try
                parse(
                    OptSpecList, [{Name, to_type(ArgSpecType, Arg)} | OptAcc], ArgAcc, ArgPos, Tail
                )
            catch
                error:_ ->
                    throw({error, {invalid_option_arg, {Name, Arg}}})
            end
    end.

%% @doc Parse a short option, add it to the option accumulator and continue
%%      parsing the rest of the arguments recursively.
%%      A short option can have the following syntax:
%%        -a       Single option 'a', no argument or implicit boolean argument
%%        -a foo   Single option 'a', argument "foo"
%%        -afoo    Single option 'a', argument "foo"
%%        -abc     Multiple options: 'a'; 'b'; 'c'
%%        -bcafoo  Multiple options: 'b'; 'c'; 'a' with argument "foo"
%%        -aaa     Multiple repetitions of option 'a' (only valid for options with integer arguments)
-spec parse_short_option(
    [option_spec()], [option()], [string()], integer(), [string()], string(), string()
) ->
    {ok, {[option()], [string()]}}.
parse_short_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, OptArg) ->
    parse_short_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, first, OptArg).

parse_short_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, OptPos, [Short | Arg]) ->
    case lists:keyfind(Short, ?OPT_SHORT, OptSpecList) of
        {Name, Short, _Long, undefined, _Help} ->
            parse_short_option(
                OptSpecList, [Name | OptAcc], ArgAcc, ArgPos, Args, OptStr, first, Arg
            );
        {_Name, Short, _Long, ArgSpec, _Help} = OptSpec ->
            %% The option has a specification, so it requires an argument.
            case Arg of
                [] ->
                    %% The option argument string is empty, but the option requires
                    %% an argument, so we look into the next string in the list.
                    parse_short_option_next_arg(
                        OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptSpec, OptPos
                    );
                _ ->
                    case is_valid_arg(ArgSpec, Arg) of
                        true ->
                            parse(
                                OptSpecList,
                                add_option_with_arg(OptSpec, Arg, OptAcc),
                                ArgAcc,
                                ArgPos,
                                Args
                            );
                        _ ->
                            NewOptAcc =
                                case OptPos of
                                    first -> add_option_with_implicit_arg(OptSpec, OptAcc);
                                    _ -> add_option_with_implicit_incrementable_arg(OptSpec, OptAcc)
                                end,
                            parse_short_option(
                                OptSpecList, NewOptAcc, ArgAcc, ArgPos, Args, OptStr, next, Arg
                            )
                    end
            end;
        false ->
            throw({error, {invalid_option, OptStr}})
    end;
parse_short_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, _OptStr, _OptPos, []) ->
    parse(OptSpecList, OptAcc, ArgAcc, ArgPos, Args).

%% @doc Retrieve the argument for an option from the next string in the list of
%%      command-line parameters or set the value of the argument from the argument
%%      specification (for boolean and integer arguments), if possible.
parse_short_option_next_arg(
    OptSpecList,
    OptAcc,
    ArgAcc,
    ArgPos,
    Args,
    {Name, _Short, _Long, ArgSpec, _Help} = OptSpec,
    OptPos
) ->
    case Args =:= [] orelse is_implicit_arg(ArgSpec, hd(Args)) of
        true when OptPos =:= first ->
            parse(OptSpecList, add_option_with_implicit_arg(OptSpec, OptAcc), ArgAcc, ArgPos, Args);
        true ->
            parse(
                OptSpecList,
                add_option_with_implicit_incrementable_arg(OptSpec, OptAcc),
                ArgAcc,
                ArgPos,
                Args
            );
        false ->
            [Arg | Tail] = Args,
            try
                parse(OptSpecList, [{Name, to_type(ArgSpec, Arg)} | OptAcc], ArgAcc, ArgPos, Tail)
            catch
                error:_ ->
                    throw({error, {invalid_option_arg, {Name, Arg}}})
            end
    end.

%% @doc Find the option for the discrete argument in position specified in the
%%      Pos argument.
-spec find_non_option_arg([option_spec()], integer()) -> {value, option_spec()} | false.
find_non_option_arg([{_Name, undefined, undefined, _ArgSpec, _Help} = OptSpec | _Tail], 0) ->
    {value, OptSpec};
find_non_option_arg([{_Name, undefined, undefined, _ArgSpec, _Help} | Tail], Pos) ->
    find_non_option_arg(Tail, Pos - 1);
find_non_option_arg([_Head | Tail], Pos) ->
    find_non_option_arg(Tail, Pos);
find_non_option_arg([], _Pos) ->
    false.

%% @doc Append options that were not present in the command line arguments with
%%      their default arguments.
-spec append_default_options([option_spec()], [option()]) -> [option()].
append_default_options([{Name, _Short, _Long, {_Type, DefaultArg}, _Help} | Tail], OptAcc) ->
    append_default_options(
        Tail,
        case lists:keymember(Name, 1, OptAcc) of
            false ->
                [{Name, DefaultArg} | OptAcc];
            _ ->
                OptAcc
        end
    );
%% For options with no default argument.
append_default_options([_Head | Tail], OptAcc) ->
    append_default_options(Tail, OptAcc);
append_default_options([], OptAcc) ->
    OptAcc.

%% @doc Add an option with argument converting it to the data type indicated by the
%%      argument specification.
-spec add_option_with_arg(option_spec(), string(), [option()]) -> [option()].
add_option_with_arg({Name, _Short, _Long, ArgSpec, _Help} = OptSpec, Arg, OptAcc) ->
    case is_valid_arg(ArgSpec, Arg) of
        true ->
            try
                [{Name, to_type(ArgSpec, Arg)} | OptAcc]
            catch
                error:_ ->
                    throw({error, {invalid_option_arg, {Name, Arg}}})
            end;
        false ->
            add_option_with_implicit_arg(OptSpec, OptAcc)
    end.

%% @doc Add an option with argument that was part of an assignment expression
%%      (e.g. "--verbose=3") converting it to the data type indicated by the
%%      argument specification.
-spec add_option_with_assigned_arg(option_spec(), string(), [option()]) -> [option()].
add_option_with_assigned_arg({Name, _Short, _Long, ArgSpec, _Help}, Arg, OptAcc) ->
    try
        [{Name, to_type(ArgSpec, Arg)} | OptAcc]
    catch
        error:_ ->
            throw({error, {invalid_option_arg, {Name, Arg}}})
    end.

%% @doc Add an option that required an argument but did not have one. Some data
%%      types (boolean, integer) allow implicit or assumed arguments.
-spec add_option_with_implicit_arg(option_spec(), [option()]) -> [option()].
add_option_with_implicit_arg({Name, _Short, _Long, ArgSpec, _Help}, OptAcc) ->
    case arg_spec_type(ArgSpec) of
        boolean ->
            %% Special case for boolean arguments: if there is no argument we
            %% set the value to 'true'.
            [{Name, true} | OptAcc];
        integer ->
            %% Special case for integer arguments: if the option had not been set
            %% before we set the value to 1. This is needed to support options like
            %% "-v" to return something like {verbose, 1}.
            [{Name, 1} | OptAcc];
        _ ->
            throw({error, {missing_option_arg, Name}})
    end.

%% @doc Add an option with an implicit or assumed argument.
-spec add_option_with_implicit_incrementable_arg(option_spec() | arg_spec(), [option()]) ->
    [option()].
add_option_with_implicit_incrementable_arg({Name, _Short, _Long, ArgSpec, _Help}, OptAcc) ->
    case arg_spec_type(ArgSpec) of
        boolean ->
            %% Special case for boolean arguments: if there is no argument we
            %% set the value to 'true'.
            [{Name, true} | OptAcc];
        integer ->
            %% Special case for integer arguments: if the option had not been set
            %% before we set the value to 1; if not we increment the previous value
            %% the option had. This is needed to support options like "-vvv" to
            %% return something like {verbose, 3}.
            case OptAcc of
                [{Name, Count} | Tail] ->
                    [{Name, Count + 1} | Tail];
                _ ->
                    [{Name, 1} | OptAcc]
            end;
        _ ->
            throw({error, {missing_option_arg, Name}})
    end.

%% @doc Retrieve the data type form an argument specification.
-spec arg_spec_type(arg_spec()) -> arg_type() | undefined.
arg_spec_type({Type, _DefaultArg}) ->
    Type;
arg_spec_type(Type) when is_atom(Type) ->
    Type.

%% @doc Convert an argument string to its corresponding data type.
-spec to_type(arg_spec() | arg_type(), string()) -> arg_value().
to_type({Type, _DefaultArg}, Arg) ->
    to_type(Type, Arg);
to_type(binary, Arg) ->
    list_to_binary(Arg);
to_type(atom, Arg) ->
    list_to_atom(Arg);
to_type(integer, Arg) ->
    list_to_integer(Arg);
to_type(float, Arg) ->
    list_to_float(Arg);
to_type(boolean, Arg) ->
    LowerArg = string:to_lower(Arg),
    case is_arg_true(LowerArg) of
        true ->
            true;
        _ ->
            case is_arg_false(LowerArg) of
                true ->
                    false;
                false ->
                    erlang:error(badarg)
            end
    end;
to_type(_Type, Arg) ->
    Arg.

-spec is_arg_true(string()) -> boolean().
is_arg_true(Arg) ->
    (Arg =:= "true") orelse (Arg =:= "t") orelse
        (Arg =:= "yes") orelse (Arg =:= "y") orelse
        (Arg =:= "on") orelse (Arg =:= "enabled") orelse
        (Arg =:= "1").

-spec is_arg_false(string()) -> boolean().
is_arg_false(Arg) ->
    (Arg =:= "false") orelse (Arg =:= "f") orelse
        (Arg =:= "no") orelse (Arg =:= "n") orelse
        (Arg =:= "off") orelse (Arg =:= "disabled") orelse
        (Arg =:= "0").

-spec is_valid_arg(arg_spec(), nonempty_string()) -> boolean().
is_valid_arg({Type, _DefaultArg}, Arg) ->
    is_valid_arg(Type, Arg);
is_valid_arg(boolean, Arg) ->
    is_boolean_arg(Arg);
is_valid_arg(integer, Arg) ->
    is_non_neg_integer_arg(Arg);
is_valid_arg(float, Arg) ->
    is_non_neg_float_arg(Arg);
is_valid_arg(_Type, _Arg) ->
    true.

-spec is_implicit_arg(arg_spec(), nonempty_string()) -> boolean().
is_implicit_arg({Type, _DefaultArg}, Arg) ->
    is_implicit_arg(Type, Arg);
is_implicit_arg(boolean, Arg) ->
    not is_boolean_arg(Arg);
is_implicit_arg(integer, Arg) ->
    not is_integer_arg(Arg);
is_implicit_arg(_Type, _Arg) ->
    false.

-spec is_boolean_arg(string()) -> boolean().
is_boolean_arg(Arg) ->
    LowerArg = string:to_lower(Arg),
    is_arg_true(LowerArg) orelse is_arg_false(LowerArg).

-spec is_integer_arg(string()) -> boolean().
is_integer_arg("-" ++ Tail) ->
    is_non_neg_integer_arg(Tail);
is_integer_arg(Arg) ->
    is_non_neg_integer_arg(Arg).

-spec is_non_neg_integer_arg(string()) -> boolean().
is_non_neg_integer_arg([Head | Tail]) when Head >= $0, Head =< $9 ->
    is_non_neg_integer_arg(Tail);
is_non_neg_integer_arg([_Head | _Tail]) ->
    false;
is_non_neg_integer_arg([]) ->
    true.

-spec is_non_neg_float_arg(string()) -> boolean().
is_non_neg_float_arg([Head | Tail]) when (Head >= $0 andalso Head =< $9) orelse Head =:= $. ->
    is_non_neg_float_arg(Tail);
is_non_neg_float_arg([_Head | _Tail]) ->
    false;
is_non_neg_float_arg([]) ->
    true.

%% @doc  Show a message on standard_error indicating the command line options and
%%       arguments that are supported by the program.
-spec usage([option_spec()], string()) -> ok.
usage(OptSpecList, ProgramName) ->
    usage(OptSpecList, ProgramName, standard_error).

%% @doc  Show a message on standard_error or standard_io indicating the command line options and
%%       arguments that are supported by the program.
-spec usage([option_spec()], string(), output_stream() | string()) -> ok.
usage(OptSpecList, ProgramName, OutputStream) when is_atom(OutputStream) ->
    io:format(
        OutputStream,
        "Usage: ~s~s~n~n~s~n",
        [ProgramName, usage_cmd_line(OptSpecList), usage_options(OptSpecList)]
    );
%% @doc  Show a message on standard_error indicating the command line options and
%%       arguments that are supported by the program. The CmdLineTail argument
%%       is a string that is added to the end of the usage command line.
usage(OptSpecList, ProgramName, CmdLineTail) ->
    usage(OptSpecList, ProgramName, CmdLineTail, standard_error).

%% @doc  Show a message on standard_error or standard_io indicating the command line options and
%%       arguments that are supported by the program. The CmdLineTail argument
%%       is a string that is added to the end of the usage command line.
-spec usage([option_spec()], string(), string(), output_stream() | [{string(), string()}]) -> ok.
usage(OptSpecList, ProgramName, CmdLineTail, OutputStream) when is_atom(OutputStream) ->
    io:format(
        OutputStream,
        "Usage: ~s~s ~s~n~n~s~n",
        [ProgramName, usage_cmd_line(OptSpecList), CmdLineTail, usage_options(OptSpecList)]
    );
%% @doc  Show a message on standard_error indicating the command line options and
%%       arguments that are supported by the program. The CmdLineTail and OptionsTail
%%       arguments are a string that is added to the end of the usage command line
%%       and a list of tuples that are added to the end of the options' help lines.
usage(OptSpecList, ProgramName, CmdLineTail, OptionsTail) ->
    usage(OptSpecList, ProgramName, CmdLineTail, OptionsTail, standard_error).

%% @doc  Show a message on standard_error or standard_io indicating the command line options and
%%       arguments that are supported by the program. The CmdLineTail and OptionsTail
%%       arguments are a string that is added to the end of the usage command line
%%       and a list of tuples that are added to the end of the options' help lines.
-spec usage([option_spec()], string(), string(), [{string(), string()}], output_stream()) -> ok.
usage(OptSpecList, ProgramName, CmdLineTail, OptionsTail, OutputStream) ->
    UsageOptions = lists:foldl(
        fun({Prefix, Help}, Acc) ->
            add_option_help(Prefix, Help, Acc)
        end,
        usage_options_reverse(OptSpecList, []),
        OptionsTail
    ),
    io:format(
        OutputStream,
        "Usage: ~s~s ~s~n~n~s~n",
        [
            ProgramName,
            usage_cmd_line(OptSpecList),
            CmdLineTail,
            lists:flatten(lists:reverse(UsageOptions))
        ]
    ).

%% @doc Return a string with the syntax for the command line options and
%%      arguments.
-spec usage_cmd_line([option_spec()]) -> string().
usage_cmd_line(OptSpecList) ->
    usage_cmd_line(OptSpecList, []).

usage_cmd_line([{Name, Short, Long, ArgSpec, _Help} | Tail], Acc) ->
    CmdLine =
        case ArgSpec of
            undefined ->
                if
                    %% For options with short form and no argument.
                    Short =/= undefined ->
                        [$\s, $[, $-, Short, $]];
                    %% For options with only long form and no argument.
                    Long =/= undefined ->
                        [$\s, $[, $-, $-, Long, $]];
                    true ->
                        []
                end;
            _ ->
                if
                    %% For options with short form and argument.
                    Short =/= undefined ->
                        [$\s, $[, $-, Short, $\s, $<, atom_to_list(Name), $>, $]];
                    %% For options with only long form and argument.
                    Long =/= undefined ->
                        [$\s, $[, $-, $-, Long, $\s, $<, atom_to_list(Name), $>, $]];
                    %% For options with neither short nor long form and argument.
                    true ->
                        [$\s, $<, atom_to_list(Name), $>]
                end
        end,
    usage_cmd_line(Tail, [CmdLine | Acc]);
usage_cmd_line([], Acc) ->
    lists:flatten(lists:reverse(Acc)).

%% @doc Return a string with the help message for each of the options and
%%      arguments.
-spec usage_options([option_spec()]) -> string().
usage_options(OptSpecList) ->
    lists:flatten(lists:reverse(usage_options_reverse(OptSpecList, []))).

usage_options_reverse([{Name, Short, Long, _ArgSpec, Help} | Tail], Acc) ->
    Prefix =
        case Long of
            undefined ->
                case Short of
                    %% Neither short nor long form (non-option argument).
                    undefined ->
                        [$<, atom_to_list(Name), $>];
                    %% Only short form.
                    _ ->
                        [$-, Short]
                end;
            _ ->
                case Short of
                    %% Only long form.
                    undefined ->
                        [$-, $- | Long];
                    %% Both short and long form.
                    _ ->
                        [$-, Short, $,, $\s, $-, $- | Long]
                end
        end,
    usage_options_reverse(Tail, add_option_help(Prefix, Help, Acc));
usage_options_reverse([], Acc) ->
    Acc.

%% @doc Add the help message corresponding to an option specification to a list
%%      with the correct indentation.
-spec add_option_help(Prefix :: string(), Help :: string(), Acc :: string()) -> string().
add_option_help(Prefix, Help, Acc) when is_list(Help), Help =/= [] ->
    FlatPrefix = lists:flatten(Prefix),
    case ((?INDENTATION * ?TAB_LENGTH) - 2 - length(FlatPrefix)) of
        TabSize when TabSize > 0 ->
            Tab = lists:duplicate(ceiling(TabSize / ?TAB_LENGTH), $\t),
            [[$\s, $\s, FlatPrefix, Tab, Help, $\n] | Acc];
        _ ->
            % The indentation for the option description is 3 tabs (i.e. 24 characters)
            % IMPORTANT: Change the number of tabs below if you change the
            %            value of the INDENTATION macro.
            [[$\t, $\t, $\t, Help, $\n], [$\s, $\s, FlatPrefix, $\n] | Acc]
    end;
add_option_help(_Opt, _Prefix, Acc) ->
    Acc.

%% @doc Return the smallest integral value not less than the argument.
-spec ceiling(float()) -> integer().
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        % Neg when Neg < 0 ->
        %    T;
        Pos when Pos > 0 ->
            T + 1;
        _ ->
            T
    end.
