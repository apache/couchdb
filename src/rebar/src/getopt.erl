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
-module(getopt).
-author('juanjo@comellas.org').

-export([parse/2, usage/2, usage/3, usage/4, tokenize/1]).
-export([usage_cmd_line/2]).

-define(LINE_LENGTH, 75).
-define(MIN_USAGE_COMMAND_LINE_OPTION_LENGTH, 25).

%% Position of each field in the option specification tuple.
-define(OPT_NAME, 1).
-define(OPT_SHORT, 2).
-define(OPT_LONG, 3).
-define(OPT_ARG, 4).
-define(OPT_HELP, 5).

-define(IS_OPT_SPEC(Opt), (tuple_size(Opt) =:= ?OPT_HELP)).
-define(IS_WHITESPACE(Char), ((Char) =:= $\s orelse (Char) =:= $\t orelse
                              (Char) =:= $\n orelse (Char) =:= $\r)).

%% Atom indicating the data type that an argument can be converted to.
-type arg_type()                                :: 'atom' | 'binary' | 'boolean' | 'float' | 'integer' | 'string'.
%% Data type that an argument can be converted to.
-type arg_value()                               :: atom() | binary() | boolean() | float() | integer() | string().
%% Argument specification.
-type arg_spec()                                :: arg_type() | {arg_type(), arg_value()} | undefined.
%% Option type and optional default argument.
-type simple_option()                           :: atom().
-type compound_option()                         :: {atom(), arg_value()}.
-type option()                                  :: simple_option() | compound_option().
%% Command line option specification.
-type option_spec() :: {
                   Name                         :: atom(),
                   Short                        :: char() | undefined,
                   Long                         :: string() | undefined,
                   ArgSpec                      :: arg_spec(),
                   Help                         :: string() | undefined
                  }.
%% Output streams
-type output_stream()                           :: 'standard_io' | 'standard_error'.

%% For internal use
-type usage_line()                              :: {OptionText :: string(), HelpText :: string()}.
-type usage_line_with_length()                  :: {OptionLength :: non_neg_integer(), OptionText :: string(), HelpText :: string()}.


-export_type([arg_type/0, arg_value/0, arg_spec/0, simple_option/0, compound_option/0, option/0, option_spec/0]).


%% @doc  Parse the command line options and arguments returning a list of tuples
%%       and/or atoms using the Erlang convention for sending options to a
%%       function.
-spec parse([option_spec()], string() | [string()]) ->
                   {ok, {[option()], [string()]}} | {error, {Reason :: atom(), Data :: any()}}.
parse(OptSpecList, CmdLine) when is_list(CmdLine) ->
    try
        Args = if
                   is_integer(hd(CmdLine)) -> tokenize(CmdLine);
                   true                    -> CmdLine
               end,
        parse(OptSpecList, [], [], 0, Args)
    catch
        throw: {error, {_Reason, _Data}} = Error ->
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
-spec parse_long_option([option_spec()], [option()], [string()], integer(), [string()], string(), string()) ->
                               {ok, {[option()], [string()]}}.
parse_long_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, OptArg) ->
    case split_assigned_arg(OptArg) of
        {Long, Arg} ->
            %% Get option that has its argument within the same string
            %% separated by an equal ('=') character (e.g. "--port=1000").
            parse_long_option_assigned_arg(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, Long, Arg);

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
-spec parse_long_option_assigned_arg([option_spec()], [option()], [string()], integer(),
                                     [string()], string(), string(), string()) ->
                                            {ok, {[option()], [string()]}}.
parse_long_option_assigned_arg(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, Long, Arg) ->
    case lists:keyfind(Long, ?OPT_LONG, OptSpecList) of
        {_Name, _Short, Long, ArgSpec, _Help} = OptSpec ->
            case ArgSpec of
                undefined ->
                    throw({error, {invalid_option_arg, OptStr}});
                _ ->
                    parse(OptSpecList, add_option_with_assigned_arg(OptSpec, Arg, OptAcc), ArgAcc, ArgPos, Args)
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
parse_long_option_next_arg(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, {Name, _Short, _Long, ArgSpec, _Help} = OptSpec) ->
    ArgSpecType = arg_spec_type(ArgSpec),
    case Args =:= [] orelse is_implicit_arg(ArgSpecType, hd(Args)) of
        true ->
            parse(OptSpecList, add_option_with_implicit_arg(OptSpec, OptAcc), ArgAcc, ArgPos, Args);
        false ->
            [Arg | Tail] = Args,
            try
                parse(OptSpecList, [{Name, to_type(ArgSpecType, Arg)} | OptAcc], ArgAcc, ArgPos, Tail)
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
-spec parse_short_option([option_spec()], [option()], [string()], integer(), [string()], string(), string()) ->
                                {ok, {[option()], [string()]}}.
parse_short_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, OptArg) ->
    parse_short_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, first, OptArg).

parse_short_option(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptStr, OptPos, [Short | Arg]) ->
    case lists:keyfind(Short, ?OPT_SHORT, OptSpecList) of
        {Name, Short, _Long, undefined, _Help} ->
            parse_short_option(OptSpecList, [Name | OptAcc], ArgAcc, ArgPos, Args, OptStr, first, Arg);

        {_Name, Short, _Long, ArgSpec, _Help} = OptSpec ->
            %% The option has a specification, so it requires an argument.
            case Arg of
                [] ->
                    %% The option argument string is empty, but the option requires
                    %% an argument, so we look into the next string in the list.
                    parse_short_option_next_arg(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, OptSpec, OptPos);

                _ ->
                    case is_valid_arg(ArgSpec, Arg) of
                        true ->
                            parse(OptSpecList, add_option_with_arg(OptSpec, Arg, OptAcc), ArgAcc, ArgPos, Args);
                        _ ->
                            NewOptAcc = case OptPos of
                                            first -> add_option_with_implicit_arg(OptSpec, OptAcc);
                                            _     -> add_option_with_implicit_incrementable_arg(OptSpec, OptAcc)
                                        end,
                            parse_short_option(OptSpecList, NewOptAcc, ArgAcc, ArgPos, Args, OptStr, next, Arg)
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
parse_short_option_next_arg(OptSpecList, OptAcc, ArgAcc, ArgPos, Args, {Name, _Short, _Long, ArgSpec, _Help} = OptSpec, OptPos) ->
    case Args =:= [] orelse is_implicit_arg(ArgSpec, hd(Args)) of
        true when OptPos =:= first ->
            parse(OptSpecList, add_option_with_implicit_arg(OptSpec, OptAcc), ArgAcc, ArgPos, Args);
        true ->
            parse(OptSpecList, add_option_with_implicit_incrementable_arg(OptSpec, OptAcc), ArgAcc, ArgPos, Args);
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
    append_default_options(Tail,
                           case lists:keymember(Name, 1, OptAcc) of
                               false ->
                                   [{Name, DefaultArg} | OptAcc];
                               _ ->
                                   OptAcc
                           end);
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
-spec add_option_with_implicit_incrementable_arg(option_spec() | arg_spec(), [option()]) -> [option()].
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
    io:format(OutputStream, "~s~n~n~s~n",
              [usage_cmd_line(ProgramName, OptSpecList), usage_options(OptSpecList)]);
%% @doc  Show a message on standard_error indicating the command line options and
%%       arguments that are supported by the program. The CmdLineTail argument
%%       is a string that is added to the end of the usage command line.
usage(OptSpecList, ProgramName, CmdLineTail) ->
    usage(OptSpecList, ProgramName, CmdLineTail, standard_error).


%% @doc  Show a message on standard_error or standard_io indicating the command line options and
%%       arguments that are supported by the program. The CmdLineTail argument
%%       is a string that is added to the end of the usage command line.
-spec usage([option_spec()], ProgramName :: string(), CmdLineTail :: string(), output_stream() | [{string(), string()}]) -> ok.
usage(OptSpecList, ProgramName, CmdLineTail, OutputStream) when is_atom(OutputStream) ->
    io:format(OutputStream, "~s~n~n~s~n",
              [usage_cmd_line(ProgramName, OptSpecList, CmdLineTail), usage_options(OptSpecList)]);
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
-spec usage([option_spec()], ProgramName :: string(), CmdLineTail :: string(),
            [{OptionName :: string(), Help :: string()}], output_stream()) -> ok.
usage(OptSpecList, ProgramName, CmdLineTail, OptionsTail, OutputStream) ->
    io:format(OutputStream, "~s~n~n~s~n",
              [usage_cmd_line(ProgramName, OptSpecList, CmdLineTail), usage_options(OptSpecList, OptionsTail)]).


-spec usage_cmd_line(ProgramName :: string(), [option_spec()]) -> iolist().
usage_cmd_line(ProgramName, OptSpecList) ->
    usage_cmd_line(ProgramName, OptSpecList, "").

-spec usage_cmd_line(ProgramName :: string(), [option_spec()], CmdLineTail :: string()) -> iolist().
usage_cmd_line(ProgramName, OptSpecList, CmdLineTail) ->
    Prefix = "Usage: " ++ ProgramName,
    PrefixLength = length(Prefix),
    LineLength = line_length(),
    %% Only align the command line options after the program name when there is
    %% enough room to do so (i.e. at least 25 characters). If not, show the
    %% command line options below the program name with a 2-character indentation.
    if
        (LineLength - PrefixLength) > ?MIN_USAGE_COMMAND_LINE_OPTION_LENGTH ->
            Indentation = lists:duplicate(PrefixLength, $\s),
            [FirstOptLine | OptLines] = usage_cmd_line_options(LineLength - PrefixLength, OptSpecList, CmdLineTail),
            IndentedOptLines = [[Indentation | OptLine] || OptLine <- OptLines],
            [Prefix, FirstOptLine | IndentedOptLines];
        true ->
            IndentedOptLines = [[" " | OptLine] || OptLine <- usage_cmd_line_options(LineLength, OptSpecList, CmdLineTail)],
            [Prefix, $\n, IndentedOptLines]
    end.


%% @doc Return a list of the lines corresponding to the usage command line
%%      already wrapped according to the maximum MaxLineLength.
-spec usage_cmd_line_options(MaxLineLength :: non_neg_integer(), [option_spec()], CmdLineTail :: string()) -> iolist().
usage_cmd_line_options(MaxLineLength, OptSpecList, CmdLineTail) ->
    usage_cmd_line_options(MaxLineLength, OptSpecList ++ string:tokens(CmdLineTail, " "), [], 0, []).

usage_cmd_line_options(MaxLineLength, [OptSpec | Tail], LineAcc, LineAccLength, Acc) ->
    Option = [$\s | lists:flatten(usage_cmd_line_option(OptSpec))],
    OptionLength = length(Option),
    %% We accumulate the options in LineAcc until its length is over the
    %% maximum allowed line length. When that happens, we append the line in
    %% LineAcc to the list with all the lines in the command line (Acc).
    NewLineAccLength = LineAccLength + OptionLength,
    if
        NewLineAccLength < MaxLineLength ->
            usage_cmd_line_options(MaxLineLength, Tail, [Option | LineAcc], NewLineAccLength, Acc);
        true ->
            usage_cmd_line_options(MaxLineLength, Tail, [Option], OptionLength + 1,
                                   [lists:reverse([$\n | LineAcc]) | Acc])
    end;
usage_cmd_line_options(MaxLineLength, [], [_ | _] = LineAcc, _LineAccLength, Acc) ->
    %% If there was a non-empty line in LineAcc when there are no more options
    %% to process, we add it to the list of lines to return.
    usage_cmd_line_options(MaxLineLength, [], [], 0, [lists:reverse(LineAcc) | Acc]);
usage_cmd_line_options(_MaxLineLength, [], [], _LineAccLength, Acc) ->
    lists:reverse(Acc).


-spec usage_cmd_line_option(option_spec()) -> string().
usage_cmd_line_option({_Name, Short, _Long, undefined, _Help}) when Short =/= undefined ->
    %% For options with short form and no argument.
    [$[, $-, Short, $]];
usage_cmd_line_option({_Name, _Short, Long, undefined, _Help}) when Long =/= undefined ->
    %% For options with only long form and no argument.
    [$[, $-, $-, Long, $]];
usage_cmd_line_option({_Name, _Short, _Long, undefined, _Help}) ->
    [];
usage_cmd_line_option({Name, Short, Long, ArgSpec, _Help}) when is_atom(ArgSpec) ->
    %% For options with no default argument.
    if
        %% For options with short form and argument.
        Short =/= undefined -> [$[, $-, Short, $\s, $<, atom_to_list(Name), $>, $]];
        %% For options with only long form and argument.
        Long =/= undefined  -> [$[, $-, $-, Long, $\s, $<, atom_to_list(Name), $>, $]];
        %% For options with neither short nor long form and argument.
        true                -> [$[, $<, atom_to_list(Name), $>, $]]
    end;
usage_cmd_line_option({Name, Short, Long, ArgSpec, _Help}) when is_tuple(ArgSpec) ->
    %% For options with default argument.
    if
        %% For options with short form and default argument.
        Short =/= undefined -> [$[, $-, Short, $\s, $[, $<, atom_to_list(Name), $>, $], $]];
        %% For options with only long form and default argument.
        Long =/= undefined  -> [$[, $-, $-, Long, $\s, $[, $<, atom_to_list(Name), $>, $], $]];
        %% For options with neither short nor long form and default argument.
        true                -> [$[, $<, atom_to_list(Name), $>, $]]
    end;
usage_cmd_line_option(Option) when is_list(Option) ->
    %% For custom options that are added to the command line.
    Option.


%% @doc Return a list of help messages to print for each of the options and arguments.
-spec usage_options([option_spec()]) -> [string()].
usage_options(OptSpecList) ->
    usage_options(OptSpecList, []).


%% @doc Return a list of usage lines to print for each of the options and arguments.
-spec usage_options([option_spec()], [{OptionName :: string(), Help :: string()}]) -> [string()].
usage_options(OptSpecList, CustomHelp) ->
    %% Add the usage lines corresponding to the option specifications.
    {MaxOptionLength0, UsageLines0} = add_option_spec_help_lines(OptSpecList, 0, []),
    %% Add the custom usage lines.
    {MaxOptionLength, UsageLines} = add_custom_help_lines(CustomHelp, MaxOptionLength0, UsageLines0),
    MaxLineLength = line_length(),
    lists:reverse([format_usage_line(MaxOptionLength + 1, MaxLineLength, UsageLine) || UsageLine <- UsageLines]).


-spec add_option_spec_help_lines([option_spec()], PrevMaxOptionLength :: non_neg_integer(), [usage_line_with_length()]) ->
                                        {MaxOptionLength :: non_neg_integer(), [usage_line_with_length()]}.
add_option_spec_help_lines([OptSpec | Tail], PrevMaxOptionLength, Acc) ->
    OptionText = usage_option_text(OptSpec),
    HelpText = usage_help_text(OptSpec),
    {MaxOptionLength, ColsWithLength} = get_max_option_length({OptionText, HelpText}, PrevMaxOptionLength),
    add_option_spec_help_lines(Tail, MaxOptionLength, [ColsWithLength | Acc]);
add_option_spec_help_lines([], MaxOptionLength, Acc) ->
    {MaxOptionLength, Acc}.


-spec add_custom_help_lines([usage_line()], PrevMaxOptionLength :: non_neg_integer(), [usage_line_with_length()]) ->
                                   {MaxOptionLength :: non_neg_integer(), [usage_line_with_length()]}.
add_custom_help_lines([CustomCols | Tail], PrevMaxOptionLength, Acc) ->
    {MaxOptionLength, ColsWithLength} = get_max_option_length(CustomCols, PrevMaxOptionLength),
    add_custom_help_lines(Tail, MaxOptionLength, [ColsWithLength | Acc]);
add_custom_help_lines([], MaxOptionLength, Acc) ->
    {MaxOptionLength, Acc}.


-spec usage_option_text(option_spec()) -> string().
usage_option_text({Name, undefined, undefined, _ArgSpec, _Help}) ->
    %% Neither short nor long form (non-option argument).
    "<" ++ atom_to_list(Name) ++ ">";
usage_option_text({_Name, Short, undefined, _ArgSpec, _Help}) ->
    %% Only short form.
    [$-, Short];
usage_option_text({_Name, undefined, Long, _ArgSpec, _Help}) ->
    %% Only long form.
    [$-, $- | Long];
usage_option_text({_Name, Short, Long, _ArgSpec, _Help}) ->
    %% Both short and long form.
    [$-, Short, $,, $\s, $-, $- | Long].


-spec usage_help_text(option_spec()) -> string().
usage_help_text({_Name, _Short, _Long, {_ArgType, ArgValue}, [_ | _] = Help}) ->
    Help ++ " [default: " ++ default_arg_value_to_string(ArgValue) ++ "]";
usage_help_text({_Name, _Short, _Long, _ArgSpec, Help}) ->
    Help.


%% @doc Calculate the maximum width of the column that shows the option's short
%%      and long form.
-spec get_max_option_length(usage_line(), PrevMaxOptionLength :: non_neg_integer()) ->
                                   {MaxOptionLength :: non_neg_integer(), usage_line_with_length()}.
get_max_option_length({OptionText, HelpText}, PrevMaxOptionLength) ->
    OptionLength = length(OptionText),
    {erlang:max(OptionLength, PrevMaxOptionLength), {OptionLength, OptionText, HelpText}}.


%% @doc Format the usage line that is shown for the options' usage. Each usage
%%      line has 2 columns. The first column shows the options in their short
%%      and long form. The second column shows the wrapped (if necessary) help
%%      text lines associated with each option. e.g.:
%%
%%        -h, --host  Database server host name or IP address; this is the
%%                    hostname of the server where the database is running
%%                    [default: localhost]
%%        -p, --port  Database server port [default: 1000]
%%
-spec format_usage_line(MaxOptionLength :: non_neg_integer(), MaxLineLength :: non_neg_integer(),
                        usage_line_with_length()) -> iolist().
format_usage_line(MaxOptionLength, MaxLineLength, {OptionLength, OptionText, [_ | _] = HelpText})
  when MaxOptionLength < (MaxLineLength div 2) ->
    %% If the width of the column where the options are shown is smaller than
    %% half the width of a console line then we show the help text line aligned
    %% next to its corresponding option, with a separation of at least 2
    %% characters.
    [Head | Tail] = wrap_text_line(MaxLineLength - MaxOptionLength - 3, HelpText),
    FirstLineIndentation = lists:duplicate(MaxOptionLength - OptionLength + 1, $\s),
    Indentation = [$\n | lists:duplicate(MaxOptionLength + 3, $\s)],
    ["  ", OptionText, FirstLineIndentation, Head,
     [[Indentation, Line] || Line <- Tail], $\n];
format_usage_line(_MaxOptionLength, MaxLineLength, {_OptionLength, OptionText, [_ | _] = HelpText}) ->
    %% If the width of the first column is bigger than the width of a console
    %% line, we show the help text on the next line with an indentation of 6
    %% characters.
    HelpLines = wrap_text_line(MaxLineLength - 6, HelpText),
    ["  ", OptionText, [["\n      ", Line] || Line <- HelpLines], $\n];
format_usage_line(_MaxOptionLength, _MaxLineLength, {_OptionLength, OptionText, _HelpText}) ->
    ["  ", OptionText, $\n].


%% @doc Wrap a text line converting it into several text lines so that the
%%      length of each one of them is never over HelpLength characters.
-spec wrap_text_line(Length :: non_neg_integer(), Text :: string()) -> [string()].
wrap_text_line(Length, Text) ->
    wrap_text_line(Length, Text, [], 0, []).

wrap_text_line(Length, [Char | Tail], Acc, Count, CurrentLineAcc) when Count < Length ->
    wrap_text_line(Length, Tail, Acc, Count + 1, [Char | CurrentLineAcc]);
wrap_text_line(Length, [_ | _] = Help, Acc, Count, CurrentLineAcc) ->
    %% Look for the first whitespace character in the current (reversed) line
    %% buffer to get a wrapped line. If there is no whitespace just cut the
    %% line at the position corresponding to the maximum length.
    {NextLineAcc, WrappedLine} = case string:cspan(CurrentLineAcc, " \t") of
                                     WhitespacePos when WhitespacePos < Count ->
                                         lists:split(WhitespacePos, CurrentLineAcc);
                                     _ ->
                                         {[], CurrentLineAcc}
                                 end,
    wrap_text_line(Length, Help, [lists:reverse(WrappedLine) | Acc], length(NextLineAcc), NextLineAcc);
wrap_text_line(_Length, [], Acc, _Count, [_ | _] = CurrentLineAcc) ->
    %% If there was a non-empty line when we reached the buffer, add it to the accumulator
    lists:reverse([lists:reverse(CurrentLineAcc) | Acc]);
wrap_text_line(_Length, [], Acc, _Count, _CurrentLineAcc) ->
    lists:reverse(Acc).


default_arg_value_to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
default_arg_value_to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
default_arg_value_to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
default_arg_value_to_string(Value) when is_float(Value) ->
    float_to_list(Value);
default_arg_value_to_string(Value) ->
    Value.


%% @doc Tokenize a command line string with support for single and double
%%      quoted arguments (needed for arguments that have embedded whitespace).
%%      The function also supports the expansion of environment variables in
%%      both the Unix (${VAR}; $VAR) and Windows (%VAR%) formats. It does NOT
%%      support wildcard expansion of paths.
-spec tokenize(CmdLine :: string()) -> [nonempty_string()].
tokenize(CmdLine) ->
    tokenize(CmdLine, [], []).

-spec tokenize(CmdLine :: string(), Acc :: [string()], ArgAcc :: string()) -> [string()].
tokenize([Sep | Tail], Acc, ArgAcc) when ?IS_WHITESPACE(Sep) ->
    NewAcc = case ArgAcc of
                 [_ | _] ->
                     %% Found separator: add to the list of arguments.
                     [lists:reverse(ArgAcc) | Acc];
                 [] ->
                     %% Found separator with no accumulated argument; discard it.
                     Acc
             end,
    tokenize(Tail, NewAcc, []);
tokenize([QuotationMark | Tail], Acc, ArgAcc) when QuotationMark =:= $"; QuotationMark =:= $' ->
    %% Quoted argument (might contain spaces, tabs, etc.)
    tokenize_quoted_arg(QuotationMark, Tail, Acc, ArgAcc);
tokenize([Char | _Tail] = CmdLine, Acc, ArgAcc) when Char =:= $$; Char =:= $% ->
    %% Unix and Windows environment variable expansion: ${VAR}; $VAR; %VAR%
    {NewCmdLine, Var} = expand_env_var(CmdLine),
    tokenize(NewCmdLine, Acc, lists:reverse(Var, ArgAcc));
tokenize([$\\, Char | Tail], Acc, ArgAcc) ->
    %% Escaped char.
    tokenize(Tail, Acc, [Char | ArgAcc]);
tokenize([Char | Tail], Acc, ArgAcc) ->
    tokenize(Tail, Acc, [Char | ArgAcc]);
tokenize([], Acc, []) ->
    lists:reverse(Acc);
tokenize([], Acc, ArgAcc) ->
    lists:reverse([lists:reverse(ArgAcc) | Acc]).

-spec tokenize_quoted_arg(QuotationMark :: char(), CmdLine :: string(), Acc :: [string()], ArgAcc :: string()) -> [string()].
tokenize_quoted_arg(QuotationMark, [QuotationMark | Tail], Acc, ArgAcc) ->
    %% End of quoted argument
    tokenize(Tail, Acc, ArgAcc);
tokenize_quoted_arg(QuotationMark, [$\\, Char | Tail], Acc, ArgAcc) ->
    %% Escaped char.
    tokenize_quoted_arg(QuotationMark, Tail, Acc, [Char | ArgAcc]);
tokenize_quoted_arg($" = QuotationMark, [Char | _Tail] = CmdLine, Acc, ArgAcc) when Char =:= $$; Char =:= $% ->
    %% Unix and Windows environment variable expansion (only for double-quoted arguments): ${VAR}; $VAR; %VAR%
    {NewCmdLine, Var} = expand_env_var(CmdLine),
    tokenize_quoted_arg(QuotationMark, NewCmdLine, Acc, lists:reverse(Var, ArgAcc));
tokenize_quoted_arg(QuotationMark, [Char | Tail], Acc, ArgAcc) ->
    tokenize_quoted_arg(QuotationMark, Tail, Acc, [Char | ArgAcc]);
tokenize_quoted_arg(_QuotationMark, CmdLine, Acc, ArgAcc) ->
    tokenize(CmdLine, Acc, ArgAcc).


-spec expand_env_var(CmdLine :: nonempty_string()) -> {string(), string()}.
expand_env_var(CmdLine) ->
    case CmdLine of
        "${" ++ Tail ->
            expand_env_var("${", $}, Tail, []);
        "$" ++ Tail ->
            expand_env_var("$", Tail, []);
        "%" ++ Tail ->
            expand_env_var("%", $%, Tail, [])
    end.

-spec expand_env_var(Prefix :: string(), EndMark :: char(), CmdLine :: string(), Acc :: string()) -> {string(), string()}.
expand_env_var(Prefix, EndMark, [Char | Tail], Acc)
  when (Char >= $A andalso Char =< $Z) orelse (Char >= $a andalso Char =< $z) orelse
       (Char >= $0 andalso Char =< $9) orelse (Char =:= $_) ->
    expand_env_var(Prefix, EndMark, Tail, [Char | Acc]);
expand_env_var(Prefix, EndMark, [EndMark | Tail], Acc) ->
    {Tail, get_env_var(Prefix, [EndMark], Acc)};
expand_env_var(Prefix, _EndMark, CmdLine, Acc) ->
    {CmdLine, Prefix ++ lists:reverse(Acc)}.


-spec expand_env_var(Prefix :: string(), CmdLine :: string(), Acc :: string()) -> {string(), string()}.
expand_env_var(Prefix, [Char | Tail], Acc)
  when (Char >= $A andalso Char =< $Z) orelse (Char >= $a andalso Char =< $z) orelse
       (Char >= $0 andalso Char =< $9) orelse (Char =:= $_) ->
    expand_env_var(Prefix, Tail, [Char | Acc]);
expand_env_var(Prefix, CmdLine, Acc) ->
    {CmdLine, get_env_var(Prefix, "", Acc)}.


-spec get_env_var(Prefix :: string(), Suffix :: string(), Acc :: string()) -> string().
get_env_var(Prefix, Suffix, [_ | _] = Acc) ->
    Name = lists:reverse(Acc),
    %% Only expand valid/existing variables.
    case os:getenv(Name) of
        false -> Prefix ++ Name ++ Suffix;
        Value -> Value
    end;
get_env_var(Prefix, Suffix, []) ->
    Prefix ++ Suffix.


-spec line_length() -> non_neg_integer().
line_length() ->
    case io:columns() of
        {ok, Columns} when Columns < ?LINE_LENGTH ->
            Columns - 1;
        _ ->
            ?LINE_LENGTH
    end.
