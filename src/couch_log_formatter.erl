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
%
% @doc The formatting functions in this module are pulled
% from lager's error_logger_lager_h.erl which is available
% under the ASFv2 license.


-module(couch_log_formatter).


-export([
    format/4,
    format/3,
    format/1,

    format_reason/1,
    format_mfa/1,
    format_trace/1,
    format_args/3
]).


-include("couch_log.hrl").


-define(DEFAULT_TRUNCATION, 1024).


format(Level, Pid, Fmt, Args) ->
    #log_entry{
        level = couch_log_util:level_to_atom(Level),
        pid = Pid,
        msg = maybe_truncate(Fmt, Args),
        msg_id = couch_log_util:get_msg_id(),
        time_stamp = couch_log_util:iso8601_timestamp()
    }.


format(Level, Pid, Msg) ->
    #log_entry{
        level = couch_log_util:level_to_atom(Level),
        pid = Pid,
        msg = maybe_truncate(Msg),
        msg_id = couch_log_util:get_msg_id(),
        time_stamp = couch_log_util:iso8601_timestamp()
    }.


format({error, _GL, {Pid, "** Generic server " ++ _, Args}}) ->
    %% gen_server terminate
    [Name, LastMsg, State, Reason] = Args,
    MsgFmt = "gen_server ~w terminated with reason: ~s~n" ++
                "  last msg: ~p~n     state: ~p",
    MsgArgs = [Name, format_reason(Reason), LastMsg, State],
    format(error, Pid, MsgFmt, MsgArgs);

format({error, _GL, {Pid, "** State machine " ++ _, Args}}) ->
    %% gen_fsm terminate
    [Name, LastMsg, StateName, State, Reason] = Args,
    MsgFmt = "gen_fsm ~w in state ~w terminated with reason: ~s~n" ++
                " last msg: ~p~n     state: ~p",
    MsgArgs = [Name, StateName, format_reason(Reason), LastMsg, State],
    format(error, Pid, MsgFmt, MsgArgs);

format({error, _GL, {Pid, "** gen_event handler" ++ _, Args}}) ->
    %% gen_event handler terminate
    [ID, Name, LastMsg, State, Reason] = Args,
    MsgFmt = "gen_event ~w installed in ~w terminated with reason: ~s~n" ++
                "  last msg: ~p~n     state: ~p",
    MsgArgs = [ID, Name, format_reason(Reason), LastMsg, State],
    format(error, Pid, MsgFmt, MsgArgs);

format({error, _GL, {Pid, Fmt, Args}}) ->
    format(error, Pid, Fmt, Args);

format({error_report, _GL, {Pid, std_error, D}}) ->
    format(error, Pid, print_silly_list(D));

format({error_report, _GL, {Pid, supervisor_report, D}}) ->
    case lists:sort(D) of
        [{errorContext, Ctx}, {offender, Off},
                {reason, Reason}, {supervisor, Name}] ->
            Offender = format_offender(Off),
            MsgFmt = "Supervisor ~w had child ~s exit " ++
                        "with reason ~s in context ~w",
            Args = [
                supervisor_name(Name),
                Offender,
                format_reason(Reason),
                Ctx
            ],
            format(error, Pid, MsgFmt, Args);
        _ ->
            format(error, Pid, "SUPERVISOR REPORT " ++ print_silly_list(D))
    end;

format({error_report, _GL, {Pid, crash_report, [Report, Neighbors]}}) ->
    Msg = "CRASH REPORT " ++ format_crash_report(Report, Neighbors),
    format(error, Pid, Msg);

format({warning_msg, _GL, {Pid, Fmt, Args}}) ->
    format(warning, Pid, Fmt, Args);

format({warning_report, _GL, {Pid, std_warning, Report}}) ->
    format(warning, Pid, print_silly_list(Report));

format({info_msg, _GL, {Pid, Fmt, Args}}) ->
    format(info, Pid, Fmt, Args);

format({info_report, _GL, {Pid, std_info, D}}) when is_list(D) ->
    case lists:sort(D) of
        [{application, App}, {exited, Reason}, {type, _Type}] ->
            MsgFmt = "Application ~w exited with reason: ~s",
            format(info, Pid, MsgFmt, [App, format_reason(Reason)]);
        _ ->
            format(info, Pid, print_silly_list(D))
    end;

format({info_report, _GL, {Pid, std_info, D}}) ->
    format(info, Pid, "~w", [D]);

format({info_report, _GL, {Pid, progress, D}}) ->
    case lists:sort(D) of
        [{application, App}, {started_at, Node}] ->
            MsgFmt = "Application ~w started on node ~w",
            format(info, Pid, MsgFmt, [App, Node]);
        [{started, Started}, {supervisor, Name}] ->
            MFA = format_mfa(get_value(mfargs, Started)),
            ChildPid = get_value(pid, Started),
            MsgFmt = "Supervisor ~w started ~s at pid ~w",
            format(debug, Pid, MsgFmt, [supervisor_name(Name), MFA, ChildPid]);
        _ ->
            format(info, Pid, "PROGRESS REPORT " ++ print_silly_list(D))
    end;

format(Event) ->
    format(warning, self(), "Unexpected error_logger event ~w", [Event]).


format_crash_report(Report, Neighbours) ->
    Pid = get_value(pid, Report),
    Name = case get_value(registered_name, Report) of
        undefined ->
            pid_to_list(Pid);
        Atom ->
            io_lib:format("~s (~w)", [Atom, Pid])
    end,
    {Class, Reason, Trace} = get_value(error_info, Report),
    ReasonStr = format_reason({Reason, Trace}),
    Type = case Class of
        exit -> "exited";
        _ -> "crashed"
    end,
    MsgFmt = "Process ~s with ~w neighbors ~s with reason: ~s",
    Args = [Name, length(Neighbours), Type, ReasonStr],
    Msg = io_lib:format(MsgFmt, Args),
    case filter_silly_list(Report, [pid, registered_name, error_info]) of
        [] ->
            Msg;
        Rest ->
            Msg ++ "; " ++ print_silly_list(Rest)
    end.


format_offender(Off) ->
    case get_value(mfargs, Off) of
        undefined ->
            %% supervisor_bridge
            Args = [get_value(mod, Off), get_value(pid, Off)],
            io_lib:format("at module ~w at ~w", Args);
        MFArgs ->
            %% regular supervisor
            MFA = format_mfa(MFArgs),

            %% In 2014 the error report changed from `name' to
            %% `id', so try that first.
            Name = case get_value(id, Off) of
                undefined ->
                    get_value(name, Off);
                Id ->
                    Id
            end,
            Args = [Name, MFA, get_value(pid, Off)],
            io_lib:format("~p started with ~s at ~w", Args)
    end.


format_reason({'function not exported', [{M, F, A} | Trace]}) ->
    ["call to unexported function ", format_mfa({M, F, A}),
        " at ", format_trace(Trace)];

format_reason({'function not exported' = C, [{M, F, A, _Props} | Rest]}) ->
    %% Drop line number from undefined function
    format_reason({C, [{M, F, A} | Rest]});

format_reason({undef, [MFA | Trace]}) ->
    ["call to undefined function ", format_mfa(MFA),
        " at ", format_trace(Trace)];

format_reason({bad_return, {MFA, Val}}) ->
    ["bad return value ", print_val(Val), " from ", format_mfa(MFA)];

format_reason({bad_return_value, Val}) ->
    ["bad return value ", print_val(Val)];

format_reason({{bad_return_value, Val}, MFA}) ->
    ["bad return value ", print_val(Val), " at ", format_mfa(MFA)];

format_reason({{badrecord, Record}, Trace}) ->
    ["bad record ", print_val(Record), " at ", format_trace(Trace)];

format_reason({{case_clause, Val}, Trace}) ->
    ["no case clause matching ", print_val(Val), " at ", format_trace(Trace)];

format_reason({function_clause, [MFA | Trace]}) ->
    ["no function clause matching ", format_mfa(MFA),
        " at ", format_trace(Trace)];

format_reason({if_clause, Trace}) ->
    ["no true branch found while evaluating if expression at ",
        format_trace(Trace)];

format_reason({{try_clause, Val}, Trace}) ->
    ["no try clause matching ", print_val(Val), " at ", format_trace(Trace)];

format_reason({badarith, Trace}) ->
    ["bad arithmetic expression at ", format_trace(Trace)];

format_reason({{badmatch, Val}, Trace}) ->
    ["no match of right hand value ", print_val(Val),
        " at ", format_trace(Trace)];

format_reason({emfile, Trace}) ->
    ["maximum number of file descriptors exhausted, check ulimit -n; ",
        format_trace(Trace)];

format_reason({system_limit, [{M, F, A} | Trace]}) ->
    Limit = case {M, F} of
        {erlang, open_port} ->
            "maximum number of ports exceeded";
        {erlang, spawn} ->
            "maximum number of processes exceeded";
        {erlang, spawn_opt} ->
            "maximum number of processes exceeded";
        {erlang, list_to_atom} ->
            "tried to create an atom larger than 255, or maximum atom count exceeded";
        {ets, new} ->
            "maximum number of ETS tables exceeded";
        _ ->
            format_mfa({M, F, A})
    end,
    ["system limit: ", Limit, " at ", format_trace(Trace)];

format_reason({badarg, [MFA | Trace]}) ->
    ["bad argument in call to ", format_mfa(MFA),
        " at ", format_trace(Trace)];

format_reason({{badarg, Stack}, _}) ->
    format_reason({badarg, Stack});

format_reason({{badarity, {Fun, Args}}, Trace}) ->
    {arity, Arity} = lists:keyfind(arity, 1, erlang:fun_info(Fun)),
    MsgFmt = "function called with wrong arity of ~w instead of ~w at ",
    [io_lib:format(MsgFmt, [length(Args), Arity]), format_trace(Trace)];

format_reason({noproc, MFA}) ->
    ["no such process or port in call to ", format_mfa(MFA)];

format_reason({{badfun, Term}, Trace}) ->
    ["bad function ", print_val(Term), " called at ", format_trace(Trace)];

format_reason({Reason, [{M, F, A} | _] = Trace})
        when is_atom(M), is_atom(F), is_integer(A) ->
    [format_reason(Reason), " at ", format_trace(Trace)];

format_reason({Reason, [{M, F, A, Props} | _] = Trace})
        when is_atom(M), is_atom(F), is_integer(A), is_list(Props) ->
    [format_reason(Reason), " at ", format_trace(Trace)];

format_reason(Reason) ->
    {Str, _} = couch_log_trunc_io:print(Reason, 500),
    Str.


format_mfa({M, F, A}) when is_list(A) ->
    {FmtStr, Args} = format_args(A, [], []),
    io_lib:format("~w:~w(" ++ FmtStr ++ ")", [M, F | Args]);

format_mfa({M, F, A}) when is_integer(A) ->
    io_lib:format("~w:~w/~w", [M, F, A]);

format_mfa({M, F, A, Props}) when is_list(Props) ->
    case get_value(line, Props) of
        undefined ->
            format_mfa({M, F, A});
        Line ->
            [format_mfa({M, F, A}), io_lib:format("(line:~w)", [Line])]
    end;

format_mfa(Trace) when is_list(Trace) ->
    format_trace(Trace);

format_mfa(Other) ->
    io_lib:format("~w", [Other]).


format_trace([MFA]) ->
    [trace_mfa(MFA)];

format_trace([MFA | Rest]) ->
    [trace_mfa(MFA), " <= ", format_trace(Rest)];

format_trace(Other) ->
    io_lib:format("~w", [Other]).


trace_mfa({M, F, A}) when is_list(A) ->
    format_mfa({M, F, length(A)});

trace_mfa({M, F, A, Props}) when is_list(A) ->
    format_mfa({M, F, length(A), Props});

trace_mfa(Other) ->
    format_mfa(Other).


format_args([], FmtAcc, ArgsAcc) ->
    {string:join(lists:reverse(FmtAcc), ", "), lists:reverse(ArgsAcc)};

format_args([H|T], FmtAcc, ArgsAcc) ->
    {Str, _} = couch_log_trunc_io:print(H, 100),
    format_args(T, ["~s" | FmtAcc], [Str | ArgsAcc]).


maybe_truncate(Fmt, Args) ->
    MaxMsgSize = couch_log_config:get(max_message_size),
    couch_log_trunc_io:format(Fmt, Args, MaxMsgSize).


maybe_truncate(Msg) ->
    MaxMsgSize = couch_log_config:get(max_message_size),
    case iolist_size(Msg) > MaxMsgSize of
        true ->
            MsgBin = iolist_to_binary(Msg),
            PrefixSize = MaxMsgSize - 3,
            <<Prefix:PrefixSize/binary, _/binary>> = MsgBin,
            [Prefix, "..."];
        false ->
            Msg
    end.


print_silly_list(L) when is_list(L) ->
    case couch_log_util:string_p(L) of
        true ->
            couch_log_trunc_io:format("~s", [L], ?DEFAULT_TRUNCATION);
        _ ->
            print_silly_list(L, [], [])
    end;

print_silly_list(L) ->
    {Str, _} = couch_log_trunc_io:print(L, ?DEFAULT_TRUNCATION),
    Str.


print_silly_list([], Fmt, Acc) ->
    couch_log_trunc_io:format(string:join(lists:reverse(Fmt), ", "),
        lists:reverse(Acc), ?DEFAULT_TRUNCATION);

print_silly_list([{K, V} | T], Fmt, Acc) ->
    print_silly_list(T, ["~p: ~p" | Fmt], [V, K | Acc]);

print_silly_list([H | T], Fmt, Acc) ->
    print_silly_list(T, ["~p" | Fmt], [H | Acc]).


print_val(Val) ->
    {Str, _} = couch_log_trunc_io:print(Val, 500),
    Str.


filter_silly_list([], _) ->
    [];

filter_silly_list([{K, V} | T], Filter) ->
    case lists:member(K, Filter) of
        true ->
            filter_silly_list(T, Filter);
        false ->
            [{K, V} | filter_silly_list(T, Filter)]
    end;

filter_silly_list([H | T], Filter) ->
    [H | filter_silly_list(T, Filter)].


get_value(Key, Value) ->
    get_value(Key, Value, undefined).

get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {Key, Value} -> Value
    end.

supervisor_name({local, Name}) -> Name;
supervisor_name(Name) -> Name.
