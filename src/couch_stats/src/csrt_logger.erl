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

-module(csrt_logger).

%% Process lifetime logging api
-export([
    get_tracker/0,
    is_logging_enabled/0,
    log_process_lifetime_report/1,
    put_tracker/1,
    stop_tracker/0,
    stop_tracker/1,
    track/1,
    tracker/1
]).

%% Raw API that bypasses is_enabled checks
-export([
    do_lifetime_report/1,
    do_status_report/1,
    do_report/2,
    maybe_report/2
]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%% Config update subscription API
-export([
    subscribe_changes/0,
    handle_config_change/5,
    handle_config_terminate/3
]).

%% Matchers
-export([
    matcher_on_dbname/1,
    matcher_on_docs_read/1,
    matcher_on_docs_written/1,
    matcher_on_rows_read/1,
    matcher_on_worker_changes_processed/1,
    matcher_on_ioq_calls/1,
    matcher_on_nonce/1,
    matcher_on_nonce_comp/1,
    matcher_on_writes/1
]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_stats_resource_tracker.hrl").

-define(MATCHERS_KEY, {?MODULE, all_csrt_matchers}).
-define(CONF_MATCHERS_ENABLED, "csrt_logger.matchers_enabled").
-define(CONF_MATCHERS_THRESHOLD, "csrt_logger.matchers_threshold").

-record(st, {
    matchers = #{}
}).

track(#rctx{pid_ref=PidRef}) ->
    case get_tracker() of
        undefined ->
            Pid = spawn(?MODULE, tracker, [PidRef]),
            put_tracker(Pid),
            Pid;
        Pid when is_pid(Pid) ->
            Pid
    end.

tracker({Pid, _Ref}=PidRef) ->
    MonRef = erlang:monitor(process, Pid),
    receive
        stop ->
            %% TODO: do we need cleanup here?
            log_process_lifetime_report(PidRef),
            csrt_server:destroy_resource(PidRef),
            ok;
        {'DOWN', MonRef, _Type, _0DPid, _Reason0} ->
            %% TODO: should we pass reason to log_process_lifetime_report?
            %% Reason = case Reason0 of
            %%     {shutdown, Shutdown0} ->
            %%         Shutdown = atom_to_binary(Shutdown0),
            %%         <<"shutdown: ", Shutdown/binary>>;
            %%     Reason0 ->
            %%         Reason0
            %% end,
            %% TODO: should we send the induced work delta to the coordinator?
            log_process_lifetime_report(PidRef),
            csrt_server:destroy_resource(PidRef),
            ok
    end.

log_process_lifetime_report(PidRef) ->
    case csrt_util:is_enabled() andalso is_logging_enabled() of
        true ->
            %%case csrt_util:conf_get("logger_type", "csrt_logger") of
            case csrt_util:conf_get("logger_type", "csrt_logger") of
                "csrt_logger" ->
                    maybe_report("csrt-pid-usage-lifetime", PidRef);
                _ ->
                    Rctx = csrt_server:get_resource(PidRef),
                    case should_log(Rctx) of
                       true ->
                            do_lifetime_report(Rctx);
                        _ ->
                            ok
                    end
            end;
        false ->
            ok
    end.

is_logging_enabled() ->
    logging_enabled() =/= false.

logging_enabled() ->
    case csrt_util:conf_get("log_pid_usage_report", "coordinator") of
        "coordinator" ->
            coordinator;
        "true" ->
            true;
        _ ->
            false
    end.

should_log(undefined) ->
    false;
should_log(#rctx{}=Rctx) ->
    should_log(Rctx, logging_enabled()).

should_log(undefined, _) ->
    false;
should_log(#rctx{}, true) ->
    true;
should_log(#rctx{}, false) ->
    false;
should_log(#rctx{type = #coordinator{}}, coordinator) ->
    true;
should_log(#rctx{type = #rpc_worker{mod=fabric_rpc, func=FName}}, _) ->
    case csrt_util:conf_get("log_fabric_rpc") of
        "true" ->
            true;
        undefined ->
            false;
        Name ->
            Name =:= atom_to_list(FName)
    end;
should_log(#rctx{}, _) ->
    false.

find_matches(RContexts, Matchers) when is_list(RContexts) andalso is_map(Matchers) ->
    maps:filter(
        fun(_Name, {_MSpec, CompMSpec}) ->
            catch [] =/= ets:match_spec_run(RContexts, CompMSpec)
        end,
        Matchers
    ).

get_matchers() ->
    persistent_term:get(?MATCHERS_KEY, #{}).

is_match(undefined) ->
    false;
is_match(#rctx{}=Rctx) ->
    is_match(Rctx, get_matchers()).

is_match(undefined, _Matchers) ->
    false;
is_match(#rctx{}=Rctx, Matchers) when is_map(Matchers) ->
    maps:size(find_matches([Rctx], Matchers)) > 0.

maybe_report(ReportName, PidRef) ->
    Rctx = csrt_server:get_resource(PidRef),
    case is_match(Rctx) of
        true ->
            do_report(ReportName, Rctx),
            ok;
        false ->
            ok
    end.

do_lifetime_report(Rctx) ->
    do_report("csrt-pid-usage-lifetime", Rctx).

do_status_report(Rctx) ->
    do_report("csrt-pid-usage-status", Rctx).

do_report(ReportName, #rctx{}=Rctx) ->
    couch_log:report(ReportName, csrt_util:to_json(Rctx)).

%%
%% Process lifetime logging api
%%

get_tracker() ->
    get(?TRACKER_PID).

put_tracker(Pid) when is_pid(Pid) ->
    put(?TRACKER_PID, Pid).

stop_tracker() ->
    stop_tracker(get_tracker()).

stop_tracker(undefined) ->
    ok;
stop_tracker(Pid) when is_pid(Pid) ->
    Pid ! stop.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = initialize_matchers(),
    ok = subscribe_changes(),
    {ok, #st{}}.

handle_call({register, Name, MSpec}, _From, #st{matchers=Matchers}=St) ->
    case add_matcher(Name, MSpec, Matchers) of
        {ok, Matchers1} ->
            set_matchers_term(Matchers1),
            {reply, ok, St#st{matchers=Matchers1}};
        {error, badarg}=Error ->
            {reply, Error, St}
    end;
handle_call(reload_matchers, _From, St) ->
    couch_log:warning("Reloading persistent term matchers", []),
    ok = initialize_matchers(),
    {reply, ok, St};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State, 0}.

handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_Msg, St) ->
    {noreply, St}.

%%
%% Matchers
%%

matcher_on_dbname(DbName)
        when is_binary(DbName) ->
    ets:fun2ms(fun(#rctx{dbname=DbName1} = R) when DbName =:= DbName1 -> R end).

matcher_on_docs_read(Threshold)
        when is_integer(Threshold) andalso Threshold > 0 ->
    ets:fun2ms(fun(#rctx{type=#coordinator{}, docs_read=DocsRead} = R) when DocsRead >= Threshold -> R end).

matcher_on_docs_written(Threshold)
        when is_integer(Threshold) andalso Threshold > 0 ->
    ets:fun2ms(fun(#rctx{type=#coordinator{}, docs_written=DocsRead} = R) when DocsRead >= Threshold -> R end).

matcher_on_rows_read(Threshold)
        when is_integer(Threshold) andalso Threshold > 0 ->
    ets:fun2ms(fun(#rctx{rows_read=DocsRead} = R) when DocsRead >= Threshold -> R end).

matcher_on_writes(Threshold)
        when is_integer(Threshold) andalso Threshold > 0 ->
    ets:fun2ms(fun(#rctx{write_kp_node = WKP, write_kv_node = WKV} = R) when WKP + WKV > Threshold-> R end).

matcher_on_nonce(Nonce) ->
    ets:fun2ms(fun(#rctx{nonce = Nonce1} = R) when Nonce =:= Nonce1 -> R end).

matcher_on_nonce_comp(Nonce) ->
    ets:match_spec_compile(ets:fun2ms(fun(#rctx{nonce = Nonce1} = R) when Nonce =:= Nonce1 -> R end)).

matcher_on_worker_changes_processed(Threshold)
        when is_integer(Threshold) andalso Threshold > 0 ->
    ets:fun2ms(
        fun(
            #rctx{
                changes_processed=Processed,
                changes_returned=Returned
            } = R
        ) when (Processed - Returned) >= Threshold ->
            R
        end
    ).

matcher_on_ioq_calls(Threshold)
        when is_integer(Threshold) andalso Threshold > 0 ->
    ets:fun2ms(fun(#rctx{ioq_calls=IOQCalls} = R) when IOQCalls >= Threshold -> R end).

add_matcher(Name, MSpec, Matchers) ->
    try ets:match_spec_compile(MSpec) of
        CompMSpec ->
            %% TODO: handle already registered name case
            Matchers1 = maps:put(Name, {MSpec, CompMSpec}, Matchers),
            {ok, Matchers1}
    catch
        error:badarg ->
            {error, badarg}
    end.

set_matchers_term(Matchers) when is_map(Matchers) ->
    persistent_term:put({?MODULE, all_csrt_matchers}, Matchers).

initialize_matchers() ->
    DefaultMatchers = [
        {docs_read, fun matcher_on_docs_read/1, 100},
        {dbname, fun matcher_on_dbname/1, <<"foo">>},
        {rows_read, fun matcher_on_rows_read/1, 100},
        {docs_written, fun matcher_on_docs_written/1, 1},
        {btree_writes, fun matcher_on_writes/1, 1},
        %%{view_rows_read, fun matcher_on_rows_read/1, 1000},
        %%{slow_reqs, fun matcher_on_slow_reqs/1, 10000},
        {worker_changes_processed, fun matcher_on_worker_changes_processed/1, 1000},
        {ioq_calls, fun matcher_on_ioq_calls/1, 10000}
    ],
    Matchers = lists:foldl(
        fun({Name0, MatchGenFunc, Threshold0}, Matchers0) when is_atom(Name0) ->
            Name = atom_to_list(Name0),
            case matcher_enabled(Name) of
                true ->
                    Threshold = matcher_threshold(Name, Threshold0),
                    %% TODO: handle errors from Func
                    case add_matcher(Name, MatchGenFunc(Threshold), Matchers0) of
                        {ok, Matchers1} ->
                            Matchers1;
                        {error, badarg} ->
                            couch_log:warning("[~p] Failed to initialize matcher: ~p", [?MODULE, Name]),
                            Matchers0
                    end;
                false ->
                    Matchers0
            end
        end,
        #{},
        DefaultMatchers
    ),
    couch_log:notice("Initialized ~p CSRT Logger matchers", [maps:size(Matchers)]),
    persistent_term:put(?MATCHERS_KEY, Matchers),
    ok.


matcher_enabled(Name) when is_list(Name) ->
    %% TODO: fix
    %% config:get_boolean(?CONF_MATCHERS_ENABLED, Name, false).
    config:get_boolean(?CONF_MATCHERS_ENABLED, Name, true).

matcher_threshold("dbname", DbName) when is_binary(DbName) ->
    %% TODO: toggle Default to undefined to disallow for particular dbname
    %% TODO: sort out list vs binary
    %%config:get_integer(?CONF_MATCHERS_THRESHOLD, binary_to_list(DbName), Default);
    DbName;
matcher_threshold(Name, Default)
        when is_list(Name) andalso is_integer(Default) andalso Default > 0 ->
    config:get_integer(?CONF_MATCHERS_THRESHOLD, Name, Default).

subscribe_changes() ->
    config:listen_for_changes(?MODULE, nil).

handle_config_change(?CONF_MATCHERS_ENABLED, _Key, _Val, _Persist, St) ->
    ok = gen_server:call(?MODULE, reload_matchers, infinity),
    {ok, St};
handle_config_change(?CONF_MATCHERS_THRESHOLD, _Key, _Val, _Persist, St) ->
    ok = gen_server:call(?MODULE, reload_matchers, infinity),
    {ok, St};
handle_config_change(_Sec, _Key, _Val, _Persist, St) ->
    {ok, St}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_, _, _) ->
    erlang:send_after(5000, whereis(?MODULE), restart_config_listener).
