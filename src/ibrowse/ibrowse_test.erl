%%% File    : ibrowse_test.erl
%%% Author  : Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%% Description : Test ibrowse
%%% Created : 14 Oct 2003 by Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>

-module(ibrowse_test).
-vsn('$Id: ibrowse_test.erl,v 1.3 2008/05/21 15:28:11 chandrusf Exp $ ').
-export([
	 load_test/3,
	 send_reqs_1/3,
	 do_send_req/2,
	 unit_tests/0,
	 unit_tests/1,
	 drv_ue_test/0,
	 drv_ue_test/1,
	 ue_test/0,
	 ue_test/1
	]).

-import(ibrowse_lib, [printable_date/0]).

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
    log_msg("Reqs/sec  : ~p~n", [(NumWorkers*NumReqsPerWorker) / Elapsed_time_secs]),
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
		    {"http://jigsaw.w3.org/HTTP/CL/", get}
		   ]).

unit_tests() ->
    unit_tests([]).

unit_tests(Options) ->
    lists:foreach(fun({Url, Method}) ->
			  execute_req(Url, Method, Options);
		     ({Url, Method, X_Opts}) ->
			  execute_req(Url, Method, X_Opts ++ Options)
		  end, ?TEST_LIST).

execute_req(Url, Method) ->
    execute_req(Url, Method, []).

execute_req(Url, Method, Options) ->
    io:format("~s, ~p: ", [Url, Method]),
    Result = (catch ibrowse:send_req(Url, [], Method, [], Options)),
    case Result of 
	{ok, SCode, _H, _B} ->
	    io:format("Status code: ~p~n", [SCode]);
	Err ->
	    io:format("Err -> ~p~n", [Err])
    end.

drv_ue_test() ->
    drv_ue_test(lists:duplicate(1024, 127)).
drv_ue_test(Data) ->
    [{port, Port}| _] = ets:lookup(ibrowse_table, port),
%     erl_ddll:unload_driver("ibrowse_drv"),
%     timer:sleep(1000),
%     erl_ddll:load_driver("../priv", "ibrowse_drv"),
%     Port = open_port({spawn, "ibrowse_drv"}, []),
    {Time, Res} = timer:tc(ibrowse_lib, drv_ue, [Data, Port]),
    io:format("Time -> ~p~n", [Time]),
    io:format("Data Length -> ~p~n", [length(Data)]),
    io:format("Res Length -> ~p~n", [length(Res)]).
%    io:format("Result -> ~s~n", [Res]).

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
