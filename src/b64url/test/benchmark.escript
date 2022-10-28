#!/usr/bin/env escript

-mode(compile).


-export([
    encode/1,
    decode/1,
    run_worker/1
]).


-record(st, {
    parent,
    module,
    workers,
    minsize,
    maxsize,
    duration,
    total_bytes
}).


main([Workers0, MinSize0, MaxSize0, Duration0]) ->
    code:add_path("./ebin"),
    code:add_path("../ebin"),
    Workers = to_int(Workers0),
    MinSize = to_int(MinSize0),
    MaxSize = to_int(MaxSize0),
    Duration = to_int(Duration0),
    if Workers > 0 -> ok; true ->
        die("Worker count must be positive~n")
    end,
    if MinSize > 0 -> ok; true ->
        die("Minimum size must be positive.~n")
    end,
    if MaxSize > 0 -> ok; true ->
        die("Maximum size must be positive.~n")
    end,
    if MinSize < MaxSize -> ok; true ->
        die("Minimum size must be less than maximum size.~n")
    end,
    if Duration > 0 -> ok; true ->
        die("Duration must be positive.~n")
    end,
    St = #st{
        parent = self(),
        workers = Workers,
        minsize = MinSize,
        maxsize = MaxSize,
        duration = Duration
    },
    lists:foreach(fun(M) ->
        run_test(St#st{module=M})
    end, randomize([b64url, ?MODULE]));

main(_) ->
    Args = [escript:script_name()],
    die("usage: ~s num_workers min_size max_size time_per_test~n", Args).


run_test(St) ->
    Workers = spawn_workers(St#st.workers, St),
    start_workers(Workers),
    Results = wait_for_workers(Workers),
    report(St#st.module, St#st.duration, Results).


start_workers(Pids) ->
    lists:foreach(fun(P) ->
        P ! start
    end, Pids).


wait_for_workers(Pids) ->
    lists:map(fun(P) ->
        receive
            {P, TotalBytes} -> TotalBytes
        end
    end, Pids).


report(Module, Duration, TotalByteList) ->
    ModDesc = case Module of
        ?MODULE -> "erl";
        b64url -> "nif"
    end,
    TotalBytes = lists:sum(TotalByteList),
    io:format("~s : ~14b bytes / ~3b seconds = ~14.2f bps~n", [
        ModDesc, TotalBytes, Duration, TotalBytes / Duration]).


spawn_workers(NumWorkers, St) ->
    lists:map(fun(_) ->
        spawn_link(?MODULE, run_worker, [St])
    end, lists:seq(1, NumWorkers)).


run_worker(St) ->
    receive
        start -> ok
    end,
    run_worker(St#st{total_bytes=0}, os:timestamp()).


run_worker(St, Started) ->
    HasRun = timer:now_diff(os:timestamp(), Started),
    case HasRun div 1000000 > St#st.duration of
        true ->
            St#st.parent ! {self(), St#st.total_bytes};
        false ->
            NewSt = do_round_trip(St),
            run_worker(NewSt, Started)
    end.


do_round_trip(St) ->
    Size = St#st.minsize + rand:uniform(St#st.maxsize - St#st.minsize),
    Data = crypto:strong_rand_bytes(Size),
    Encoded = (St#st.module):encode(Data),
    Data = (St#st.module):decode(Encoded),
    St#st{total_bytes=St#st.total_bytes+Size}.


encode(Url) ->
    Url1 = iolist_to_binary(re:replace(base64:encode(Url), "=+$", "")),
    Url2 = iolist_to_binary(re:replace(Url1, "/", "_", [global])),
    iolist_to_binary(re:replace(Url2, "\\+", "-", [global])).


decode(Url64) ->
    Url1 = re:replace(iolist_to_binary(Url64), "-", "+", [global]),
    Url2 = iolist_to_binary(
        re:replace(iolist_to_binary(Url1), "_", "/", [global])
    ),
    Padding = list_to_binary(lists:duplicate((4 - size(Url2) rem 4) rem 4, $=)),
    base64:decode(<<Url2/binary, Padding/binary>>).

randomize(List) ->
    List0 = [{rand:uniform(), L} || L <- List],
    List1 = lists:sort(List0),
    [L || {_, L} <- List1].


to_int(Val) when is_integer(Val) ->
    Val;
to_int(Val) when is_binary(Val) ->
    to_int(binary_to_list(Val));
to_int(Val) when is_list(Val) ->
    try
        list_to_integer(Val)
    catch _:_ ->
        die("Invalid integer: ~w~n", [Val])
    end;
to_int(Val) ->
    die("Invalid integer: ~w~n", [Val]).


die(Message) ->
    die(Message, []).

die(Format, Args) ->
    io:format(Format, Args),
    init:stop().

