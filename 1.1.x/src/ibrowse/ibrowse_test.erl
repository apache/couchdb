%%% File    : ibrowse_test.erl
%%% Author  : Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%% Description : Test ibrowse
%%% Created : 14 Oct 2003 by Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>

-module(ibrowse_test).
-export([
	 load_test/3,
	 send_reqs_1/3,
	 do_send_req/2,
	 unit_tests/0,
	 unit_tests/1,
	 unit_tests_1/2,
	 ue_test/0,
	 ue_test/1,
	 verify_chunked_streaming/0,
	 verify_chunked_streaming/1,
         test_chunked_streaming_once/0,
	 i_do_async_req_list/4,
	 test_stream_once/3,
	 test_stream_once/4,
         test_20122010/0,
         test_20122010/1
	]).

test_stream_once(Url, Method, Options) ->
    test_stream_once(Url, Method, Options, 5000).

test_stream_once(Url, Method, Options, Timeout) ->
    case ibrowse:send_req(Url, [], Method, [], [{stream_to, {self(), once}} | Options], Timeout) of
	{ibrowse_req_id, Req_id} ->
	    case ibrowse:stream_next(Req_id) of
		ok ->
		    test_stream_once(Req_id);
		Err ->
		    Err
	    end;
	Err ->
	    Err
    end.

test_stream_once(Req_id) ->
    receive
	{ibrowse_async_headers, Req_id, StatCode, Headers} ->
	    io:format("Recvd headers~n~p~n", [{ibrowse_async_headers, Req_id, StatCode, Headers}]),
	    case ibrowse:stream_next(Req_id) of
		ok ->
		    test_stream_once(Req_id);
		Err ->
		    Err
	    end;
	{ibrowse_async_response, Req_id, {error, Err}} ->
	    io:format("Recvd error: ~p~n", [Err]);
	{ibrowse_async_response, Req_id, Body_1} ->
	    io:format("Recvd body part: ~n~p~n", [{ibrowse_async_response, Req_id, Body_1}]),
	    case ibrowse:stream_next(Req_id) of
		ok ->
		    test_stream_once(Req_id);
		Err ->
		    Err
	    end;
	{ibrowse_async_response_end, Req_id} ->
	    ok
    end.
%% Use ibrowse:set_max_sessions/3 and ibrowse:set_max_pipeline_size/3 to
%% tweak settings before running the load test. The defaults are 10 and 10.
load_test(Url, NumWorkers, NumReqsPerWorker) when is_list(Url),
                                                  is_integer(NumWorkers),
                                                  is_integer(NumReqsPerWorker),
                                                  NumWorkers > 0,
                                                  NumReqsPerWorker > 0 ->
    proc_lib:spawn(?MODULE, send_reqs_1, [Url, NumWorkers, NumReqsPerWorker]).

send_reqs_1(Url, NumWorkers, NumReqsPerWorker) ->
    Start_time = now(),
    ets:new(pid_table, [named_table, public]),
    ets:new(ibrowse_test_results, [named_table, public]),
    ets:new(ibrowse_errors, [named_table, public, ordered_set]),
    init_results(),
    process_flag(trap_exit, true),
    log_msg("Starting spawning of workers...~n", []),
    spawn_workers(Url, NumWorkers, NumReqsPerWorker),
    log_msg("Finished spawning workers...~n", []),
    do_wait(),
    End_time = now(),
    log_msg("All workers are done...~n", []),
    log_msg("ibrowse_test_results table: ~n~p~n", [ets:tab2list(ibrowse_test_results)]),
    log_msg("Start time: ~1000.p~n", [calendar:now_to_local_time(Start_time)]),
    log_msg("End time  : ~1000.p~n", [calendar:now_to_local_time(End_time)]),
    Elapsed_time_secs = trunc(timer:now_diff(End_time, Start_time) / 1000000),
    log_msg("Elapsed   : ~p~n", [Elapsed_time_secs]),
    log_msg("Reqs/sec  : ~p~n", [round(trunc((NumWorkers*NumReqsPerWorker) / Elapsed_time_secs))]),
    dump_errors().

init_results() ->
    ets:insert(ibrowse_test_results, {crash, 0}),
    ets:insert(ibrowse_test_results, {send_failed, 0}),
    ets:insert(ibrowse_test_results, {other_error, 0}),
    ets:insert(ibrowse_test_results, {success, 0}),
    ets:insert(ibrowse_test_results, {retry_later, 0}),
    ets:insert(ibrowse_test_results, {trid_mismatch, 0}),
    ets:insert(ibrowse_test_results, {success_no_trid, 0}),
    ets:insert(ibrowse_test_results, {failed, 0}),
    ets:insert(ibrowse_test_results, {timeout, 0}),
    ets:insert(ibrowse_test_results, {req_id, 0}).

spawn_workers(_Url, 0, _) ->
    ok;
spawn_workers(Url, NumWorkers, NumReqsPerWorker) ->
    Pid = proc_lib:spawn_link(?MODULE, do_send_req, [Url, NumReqsPerWorker]),
    ets:insert(pid_table, {Pid, []}),
    spawn_workers(Url, NumWorkers - 1, NumReqsPerWorker).

do_wait() ->
    receive
	{'EXIT', _, normal} ->
	    do_wait();
	{'EXIT', Pid, Reason} ->
	    ets:delete(pid_table, Pid),
	    ets:insert(ibrowse_errors, {Pid, Reason}),
	    ets:update_counter(ibrowse_test_results, crash, 1),
	    do_wait();
	Msg ->
	    io:format("Recvd unknown message...~p~n", [Msg]),
	    do_wait()
    after 1000 ->
	    case ets:info(pid_table, size) of
		0 ->
		    done;
		_ ->
		    do_wait()
	    end
    end.

do_send_req(Url, NumReqs) ->
    do_send_req_1(Url, NumReqs).

do_send_req_1(_Url, 0) ->
    ets:delete(pid_table, self());
do_send_req_1(Url, NumReqs) ->
    Counter = integer_to_list(ets:update_counter(ibrowse_test_results, req_id, 1)),
    case ibrowse:send_req(Url, [{"ib_req_id", Counter}], get, [], [], 10000) of
	{ok, _Status, Headers, _Body} ->
	    case lists:keysearch("ib_req_id", 1, Headers) of
		{value, {_, Counter}} ->
		    ets:update_counter(ibrowse_test_results, success, 1);
		{value, _} ->
		    ets:update_counter(ibrowse_test_results, trid_mismatch, 1);
		false ->
		    ets:update_counter(ibrowse_test_results, success_no_trid, 1)
	    end;
	{error, req_timedout} ->
	    ets:update_counter(ibrowse_test_results, timeout, 1);
	{error, send_failed} ->
	    ets:update_counter(ibrowse_test_results, send_failed, 1);
	{error, retry_later} ->
	    ets:update_counter(ibrowse_test_results, retry_later, 1);
	Err ->
	    ets:insert(ibrowse_errors, {now(), Err}),
	    ets:update_counter(ibrowse_test_results, other_error, 1),
	    ok
    end,
    do_send_req_1(Url, NumReqs-1).

dump_errors() ->
    case ets:info(ibrowse_errors, size) of
	0 ->
	    ok;
	_ ->
	    {A, B, C} = now(),
	    Filename = lists:flatten(
			 io_lib:format("ibrowse_errors_~p_~p_~p.txt" , [A, B, C])),
	    case file:open(Filename, [write, delayed_write, raw]) of
		{ok, Iod} ->
		    dump_errors(ets:first(ibrowse_errors), Iod);
		Err ->
		    io:format("failed to create file ~s. Reason: ~p~n", [Filename, Err]),
		    ok
	    end
    end.

dump_errors('$end_of_table', Iod) ->
    file:close(Iod);
dump_errors(Key, Iod) ->
    [{_, Term}] = ets:lookup(ibrowse_errors, Key),
    file:write(Iod, io_lib:format("~p~n", [Term])),
    dump_errors(ets:next(ibrowse_errors, Key), Iod).

%%------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------
-define(TEST_LIST, [{"http://intranet/messenger", get},
		    {"http://www.google.co.uk", get},
		    {"http://www.google.com", get},
		    {"http://www.google.com", options},
                    {"https://mail.google.com", get},
		    {"http://www.sun.com", get},
		    {"http://www.oracle.com", get},
		    {"http://www.bbc.co.uk", get},
		    {"http://www.bbc.co.uk", trace},
		    {"http://www.bbc.co.uk", options},
		    {"http://yaws.hyber.org", get},
		    {"http://jigsaw.w3.org/HTTP/ChunkedScript", get},
		    {"http://jigsaw.w3.org/HTTP/TE/foo.txt", get},
		    {"http://jigsaw.w3.org/HTTP/TE/bar.txt", get},
		    {"http://jigsaw.w3.org/HTTP/connection.html", get},
		    {"http://jigsaw.w3.org/HTTP/cc.html", get},
		    {"http://jigsaw.w3.org/HTTP/cc-private.html", get},
		    {"http://jigsaw.w3.org/HTTP/cc-proxy-revalidate.html", get},
		    {"http://jigsaw.w3.org/HTTP/cc-nocache.html", get},
		    {"http://jigsaw.w3.org/HTTP/h-content-md5.html", get},
		    {"http://jigsaw.w3.org/HTTP/h-retry-after.html", get},
		    {"http://jigsaw.w3.org/HTTP/h-retry-after-date.html", get},
		    {"http://jigsaw.w3.org/HTTP/neg", get},
		    {"http://jigsaw.w3.org/HTTP/negbad", get},
		    {"http://jigsaw.w3.org/HTTP/400/toolong/", get},
		    {"http://jigsaw.w3.org/HTTP/300/", get},
		    {"http://jigsaw.w3.org/HTTP/Basic/", get, [{basic_auth, {"guest", "guest"}}]},
		    {"http://jigsaw.w3.org/HTTP/CL/", get},
		    {"http://www.httpwatch.com/httpgallery/chunked/", get},
                    {"https://github.com", get, [{ssl_options, [{depth, 2}]}]},
                    {local_test_fun, test_20122010, []}
		   ]).

unit_tests() ->
    unit_tests([]).

unit_tests(Options) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    (catch ibrowse_test_server:start_server(8181, tcp)),
    ibrowse:start(),
    Options_1 = Options ++ [{connect_timeout, 5000}],
    {Pid, Ref} = erlang:spawn_monitor(?MODULE, unit_tests_1, [self(), Options_1]),
    receive 
	{done, Pid} ->
	    ok;
	{'DOWN', Ref, _, _, Info} ->
	    io:format("Test process crashed: ~p~n", [Info])
    after 60000 ->
	    exit(Pid, kill),
	    io:format("Timed out waiting for tests to complete~n", [])
    end.

unit_tests_1(Parent, Options) ->
    lists:foreach(fun({local_test_fun, Fun_name, Args}) ->
                          execute_req(local_test_fun, Fun_name, Args);
                     ({Url, Method}) ->
			  execute_req(Url, Method, Options);
		     ({Url, Method, X_Opts}) ->
			  execute_req(Url, Method, X_Opts ++ Options)
		  end, ?TEST_LIST),
    Parent ! {done, self()}.

verify_chunked_streaming() ->
    verify_chunked_streaming([]).

verify_chunked_streaming(Options) ->
    io:format("~nVerifying that chunked streaming is working...~n", []),
    Url = "http://www.httpwatch.com/httpgallery/chunked/",
    io:format("  URL: ~s~n", [Url]),
    io:format("  Fetching data without streaming...~n", []),
    Result_without_streaming = ibrowse:send_req(
				 Url, [], get, [],
				 [{response_format, binary} | Options]),
    io:format("  Fetching data with streaming as list...~n", []),
    Async_response_list = do_async_req_list(
			    Url, get, [{response_format, list} | Options]),
    io:format("  Fetching data with streaming as binary...~n", []),
    Async_response_bin = do_async_req_list(
			   Url, get, [{response_format, binary} | Options]),
    io:format("  Fetching data with streaming as binary, {active, once}...~n", []),
    Async_response_bin_once = do_async_req_list(
                                Url, get, [once, {response_format, binary} | Options]),
    Res1 = compare_responses(Result_without_streaming, Async_response_list, Async_response_bin),
    Res2 = compare_responses(Result_without_streaming, Async_response_list, Async_response_bin_once),
    case {Res1, Res2} of
        {success, success} ->
            io:format("  Chunked streaming working~n", []);
        _ ->
            ok
    end.

test_chunked_streaming_once() ->
    test_chunked_streaming_once([]).

test_chunked_streaming_once(Options) ->
    io:format("~nTesting chunked streaming with the {stream_to, {Pid, once}} option...~n", []),
    Url = "http://www.httpwatch.com/httpgallery/chunked/",
    io:format("  URL: ~s~n", [Url]),
    io:format("  Fetching data with streaming as binary, {active, once}...~n", []),
    case do_async_req_list(Url, get, [once, {response_format, binary} | Options]) of
        {ok, _, _, _} ->
            io:format("  Success!~n", []);
        Err ->
            io:format("  Fail: ~p~n", [Err])
    end.

compare_responses({ok, St_code, _, Body}, {ok, St_code, _, Body}, {ok, St_code, _, Body}) ->
    success;
compare_responses({ok, St_code, _, Body_1}, {ok, St_code, _, Body_2}, {ok, St_code, _, Body_3}) ->
    case Body_1 of
	Body_2 ->
	    io:format("Body_1 and Body_2 match~n", []);
	Body_3 ->
	    io:format("Body_1 and Body_3 match~n", []);
	_ when Body_2 == Body_3 ->
	    io:format("Body_2 and Body_3 match~n", []);
	_ ->
	    io:format("All three bodies are different!~n", [])
    end,
    io:format("Body_1 -> ~p~n", [Body_1]),
    io:format("Body_2 -> ~p~n", [Body_2]),
    io:format("Body_3 -> ~p~n", [Body_3]),
    fail_bodies_mismatch;
compare_responses(R1, R2, R3) ->
    io:format("R1 -> ~p~n", [R1]),
    io:format("R2 -> ~p~n", [R2]),
    io:format("R3 -> ~p~n", [R3]),
    fail.

%% do_async_req_list(Url) ->
%%     do_async_req_list(Url, get).

%% do_async_req_list(Url, Method) ->
%%     do_async_req_list(Url, Method, [{stream_to, self()},
%% 				    {stream_chunk_size, 1000}]).

do_async_req_list(Url, Method, Options) ->
    {Pid,_} = erlang:spawn_monitor(?MODULE, i_do_async_req_list,
				   [self(), Url, Method, 
				    Options ++ [{stream_chunk_size, 1000}]]),
%%    io:format("Spawned process ~p~n", [Pid]),
    wait_for_resp(Pid).

wait_for_resp(Pid) ->
    receive
	{async_result, Pid, Res} ->
	    Res;
	{async_result, Other_pid, _} ->
	    io:format("~p: Waiting for result from ~p: got from ~p~n", [self(), Pid, Other_pid]),
	    wait_for_resp(Pid);
	{'DOWN', _, _, Pid, Reason} ->
	    {'EXIT', Reason};
	{'DOWN', _, _, _, _} ->
	    wait_for_resp(Pid);
	Msg ->
	    io:format("Recvd unknown message: ~p~n", [Msg]),
	    wait_for_resp(Pid)
    after 100000 ->
	  {error, timeout}
    end.

i_do_async_req_list(Parent, Url, Method, Options) ->
    Options_1 = case lists:member(once, Options) of
                    true ->
                        [{stream_to, {self(), once}} | (Options -- [once])];
                    false ->
                        [{stream_to, self()} | Options]
                end,
    Res = ibrowse:send_req(Url, [], Method, [], Options_1),
    case Res of
	{ibrowse_req_id, Req_id} ->
	    Result = wait_for_async_resp(Req_id, Options, undefined, undefined, []),
	    Parent ! {async_result, self(), Result};
	Err ->
	    Parent ! {async_result, self(), Err}
    end.

wait_for_async_resp(Req_id, Options, Acc_Stat_code, Acc_Headers, Body) ->    
    receive
	{ibrowse_async_headers, Req_id, StatCode, Headers} ->
            %% io:format("Recvd headers...~n", []),
            maybe_stream_next(Req_id, Options),
	    wait_for_async_resp(Req_id, Options, StatCode, Headers, Body);
	{ibrowse_async_response_end, Req_id} ->
            %% io:format("Recvd end of response.~n", []),
	    Body_1 = list_to_binary(lists:reverse(Body)),
	    {ok, Acc_Stat_code, Acc_Headers, Body_1};
	{ibrowse_async_response, Req_id, Data} ->
            maybe_stream_next(Req_id, Options),
            %% io:format("Recvd data...~n", []),
	    wait_for_async_resp(Req_id, Options, Acc_Stat_code, Acc_Headers, [Data | Body]);
	{ibrowse_async_response, Req_id, {error, _} = Err} ->
            {ok, Acc_Stat_code, Acc_Headers, Err};
	Err ->
	    {ok, Acc_Stat_code, Acc_Headers, Err}
    after 10000 ->
            {timeout, Acc_Stat_code, Acc_Headers, Body}
    end.

maybe_stream_next(Req_id, Options) ->
    case lists:member(once, Options) of
        true ->
            ibrowse:stream_next(Req_id);
        false ->
            ok
    end.

execute_req(local_test_fun, Method, Args) ->
    io:format("     ~-54.54w: ", [Method]),
    Result = (catch apply(?MODULE, Method, Args)),
    io:format("~p~n", [Result]);
execute_req(Url, Method, Options) ->
    io:format("~7.7w, ~50.50s: ", [Method, Url]),
    Result = (catch ibrowse:send_req(Url, [], Method, [], Options)),
    case Result of
	{ok, SCode, _H, _B} ->
	    io:format("Status code: ~p~n", [SCode]);
	Err ->
	    io:format("~p~n", [Err])
    end.

ue_test() ->
    ue_test(lists:duplicate(1024, $?)).
ue_test(Data) ->
    {Time, Res} = timer:tc(ibrowse_lib, url_encode, [Data]),
    io:format("Time -> ~p~n", [Time]),
    io:format("Data Length -> ~p~n", [length(Data)]),
    io:format("Res Length -> ~p~n", [length(Res)]).
%    io:format("Result -> ~s~n", [Res]).

log_msg(Fmt, Args) ->
    io:format("~s -- " ++ Fmt,
	      [ibrowse_lib:printable_date() | Args]).

%%------------------------------------------------------------------------------
%% 
%%------------------------------------------------------------------------------

test_20122010() ->
    test_20122010("http://localhost:8181").

test_20122010(Url) ->
    {ok, Pid} = ibrowse:spawn_worker_process(Url),
    Expected_resp = <<"1-2-3-4-5-6-7-8-9-10-11-12-13-14-15-16-17-18-19-20-21-22-23-24-25-26-27-28-29-30-31-32-33-34-35-36-37-38-39-40-41-42-43-44-45-46-47-48-49-50-51-52-53-54-55-56-57-58-59-60-61-62-63-64-65-66-67-68-69-70-71-72-73-74-75-76-77-78-79-80-81-82-83-84-85-86-87-88-89-90-91-92-93-94-95-96-97-98-99-100">>,
    Test_parent = self(),
    Fun = fun() ->
                  do_test_20122010(Url, Pid, Expected_resp, Test_parent)
          end,
    Pids = [erlang:spawn_monitor(Fun) || _ <- lists:seq(1,10)],
    wait_for_workers(Pids).

wait_for_workers([{Pid, _Ref} | Pids]) ->
    receive
        {Pid, success} ->
            wait_for_workers(Pids)
    after 60000 ->
            test_failed
    end;
wait_for_workers([]) ->
    success.

do_test_20122010(Url, Pid, Expected_resp, Test_parent) ->
    do_test_20122010(10, Url, Pid, Expected_resp, Test_parent).

do_test_20122010(0, _Url, _Pid, _Expected_resp, Test_parent) ->
    Test_parent ! {self(), success};
do_test_20122010(Rem_count, Url, Pid, Expected_resp, Test_parent) ->
    {ibrowse_req_id, Req_id} = ibrowse:send_req_direct(
                                 Pid,
                                 Url ++ "/ibrowse_stream_once_chunk_pipeline_test",
                                 [], get, [],
                                 [{stream_to, {self(), once}},
                                  {inactivity_timeout, 10000},
                                  {include_ibrowse_req_id, true}]),
    do_trace("~p -- sent request ~1000.p~n", [self(), Req_id]),
    Req_id_str = lists:flatten(io_lib:format("~1000.p",[Req_id])),
    receive
        {ibrowse_async_headers, Req_id, "200", Headers} ->
            case lists:keysearch("x-ibrowse-request-id", 1, Headers) of
                {value, {_, Req_id_str}} ->
                    ok;
                {value, {_, Req_id_1}} ->
                    do_trace("~p -- Sent req-id: ~1000.p. Recvd: ~1000.p~n",
                              [self(), Req_id, Req_id_1]),
                    exit(req_id_mismatch)
            end
    after 5000 ->
            do_trace("~p -- response headers not received~n", [self()]),
            exit({timeout, test_failed})
    end,
    do_trace("~p -- response headers received~n", [self()]),
    ok = ibrowse:stream_next(Req_id),
    case do_test_20122010_1(Expected_resp, Req_id, []) of
        true ->
            do_test_20122010(Rem_count - 1, Url, Pid, Expected_resp, Test_parent);
        false ->
            Test_parent ! {self(), failed}
    end.

do_test_20122010_1(Expected_resp, Req_id, Acc) ->
    receive
        {ibrowse_async_response, Req_id, Body_part} ->
            ok = ibrowse:stream_next(Req_id),
            do_test_20122010_1(Expected_resp, Req_id, [Body_part | Acc]);
        {ibrowse_async_response_end, Req_id} ->
            Acc_1 = list_to_binary(lists:reverse(Acc)),
            Result = Acc_1 == Expected_resp,
            do_trace("~p -- End of response. Result: ~p~n", [self(), Result]),
            Result
    after 1000 ->
            exit({timeout, test_failed})
    end.

do_trace(Fmt, Args) ->
    do_trace(get(my_trace_flag), Fmt, Args).

do_trace(true, Fmt, Args) ->
    io:format("~s -- " ++ Fmt, [ibrowse_lib:printable_date() | Args]);
do_trace(_, _, _) ->
    ok.
