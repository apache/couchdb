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

-module(couch_srt_logger).

-behaviour(gen_server).

%% Process lifetime logging api
-export([
    get_tracker/0,
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
    maybe_report/2,
    should_truncate_reports/0
]).

%% gen_server callbacks
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
    deregister_matcher/1,
    find_all_matches/2,
    find_matches/2,
    get_matcher/1,
    get_matchers/0,
    get_registered_matchers/0,
    is_match/1,
    is_match/2,
    matcher_on_dbname/1,
    matcher_on_docs_read/1,
    matcher_on_docs_written/1,
    matcher_on_rows_read/1,
    matcher_on_changes_processed/1,
    matcher_on_ioq_calls/1,
    matcher_on_nonce/1,
    matcher_on_long_reqs/1,
    register_matcher/2,
    reload_matchers/0
]).

%% Recon API Ports of https://github.com/ferd/recon/releases/tag/2.5.6
-export([
    pid_ref_attrs/1,
    pid_ref_matchspec/1,
    proc_window/3
]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_srt.hrl").

-record(st, {
    registered_matchers = #{}
}).

-spec track(Rctx :: rctx()) -> pid().
track(#rctx{pid_ref = PidRef}) ->
    case get_tracker() of
        undefined ->
            Pid = spawn(?MODULE, tracker, [PidRef]),
            put_tracker(Pid),
            Pid;
        Pid when is_pid(Pid) ->
            Pid
    end.

-spec tracker(PidRef :: pid_ref()) -> ok.
tracker({Pid, _Ref} = PidRef) ->
    MonRef = erlang:monitor(process, Pid),
    receive
        stop ->
            log_process_lifetime_report(PidRef),
            couch_srt_server:destroy_resource(PidRef),
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
            couch_srt_server:destroy_resource(PidRef),
            ok
    end.

-spec register_matcher(Name, MSpec) -> ok | {error, badarg} when
    Name :: string(), MSpec :: ets:match_spec().
register_matcher(Name, MSpec) ->
    gen_server:call(?MODULE, {register, Name, MSpec}, infinity).

-spec deregister_matcher(Name :: string()) -> ok.
deregister_matcher(Name) ->
    gen_server:call(?MODULE, {deregister, Name}, infinity).

-spec log_process_lifetime_report(PidRef :: pid_ref()) -> ok.
log_process_lifetime_report(PidRef) ->
    case couch_srt_util:is_enabled() andalso couch_srt_util:is_enabled_reporting() of
        true ->
            maybe_report("csrt-pid-usage-lifetime", PidRef);
        false ->
            ok
    end.

%% Return a subset of Matchers for each Matcher that matches on Rctxs
-spec find_matches(Rctxs :: [rctx()], Matchers :: matchers()) -> matchers().
find_matches(Rctxs, Matchers) when is_list(Rctxs) andalso is_map(Matchers) ->
    Rctxs1 =
        case couch_srt_util:is_enabled_rpc_reporting() of
            true ->
                Rctxs;
            false ->
                [Rctx || #rctx{type = #coordinator{}} = Rctx <- Rctxs]
        end,
    maps:filter(
        fun(_Name, {_MSpec, CompMSpec}) ->
            (catch ets:match_spec_run(Rctxs1, CompMSpec)) =/= []
        end,
        Matchers
    ).

%% Return a Map of #{MatcherName => SRctxs :: rctxs()} for all MatcherName => Matcher
%% in Matchers where SRctxs is the subset of Rctxs matched by the given Matcher
-spec find_all_matches(Rctxs :: rctxs(), Matchers :: matchers()) -> matcher_matches().
find_all_matches(Rctxs, Matchers) when is_list(Rctxs) andalso is_map(Matchers) ->
    maps:map(
        fun(_Name, {_MSpec, CompMSpec}) ->
            try
                ets:match_spec_run(Rctxs, CompMSpec)
            catch
                _:_ ->
                    []
            end
        end,
        Matchers
    ).

-spec reload_matchers() -> ok.
reload_matchers() ->
    ok = gen_server:call(?MODULE, reload_matchers, infinity).

-spec get_matchers() -> matchers().
get_matchers() ->
    persistent_term:get(?MATCHERS_KEY, #{}).

-spec get_matcher(Name :: matcher_name()) -> maybe_matcher().
get_matcher(Name) ->
    maps:get(Name, get_matchers(), undefined).

-spec get_registered_matchers() -> matchers().
get_registered_matchers() ->
    gen_server:call(?MODULE, get_registered_matchers, infinity).

-spec is_match(Rctx :: maybe_rctx()) -> boolean().
is_match(undefined) ->
    false;
is_match(#rctx{} = Rctx) ->
    is_match(Rctx, get_matchers()).

-spec is_match(Rctx :: maybe_rctx(), Matchers :: matchers()) -> boolean().
is_match(undefined, _Matchers) ->
    false;
is_match(_Rctx, undefined) ->
    false;
is_match(#rctx{} = Rctx, Matchers) when is_map(Matchers) ->
    maps:size(find_matches([Rctx], Matchers)) > 0.

%% Generate a report for the Rctx if it triggers an active Matcher
-spec maybe_report(ReportName :: string(), PidRef :: maybe_pid_ref()) -> ok.
maybe_report(ReportName, PidRef) ->
    Rctx = couch_srt_server:get_resource(PidRef),
    case is_match(Rctx) of
        true ->
            do_report(ReportName, Rctx),
            ok;
        false ->
            ok
    end.

%% Whether or not to remove zero value fields from reports to save on volume
-spec should_truncate_reports() -> boolean().
should_truncate_reports() ->
    config:get_boolean(?CSRT, "should_truncate_reports", true).

-spec do_lifetime_report(Rctx :: rctx()) -> boolean().
do_lifetime_report(Rctx) ->
    do_report("csrt-pid-usage-lifetime", Rctx).

-spec do_status_report(Rctx :: rctx()) -> boolean().
do_status_report(Rctx) ->
    do_report("csrt-pid-usage-status", Rctx).

-spec do_report(ReportName :: string(), Rctx :: rctx()) -> boolean().
do_report(ReportName, #rctx{} = Rctx) ->
    JRctx =
        case {should_truncate_reports(), couch_srt_entry:to_json(Rctx)} of
            {true, JRctx0} ->
                maps:filter(fun(_K, V) -> V > 0 end, JRctx0);
            {false, JRctx0} ->
                JRctx0
        end,
    couch_log:report(ReportName, JRctx).

%%
%% Process lifetime logging api
%%

-spec get_tracker() -> maybe_pid().
get_tracker() ->
    get(?TRACKER_PID).

-spec put_tracker(Pid :: pid()) -> maybe_pid().
put_tracker(Pid) when is_pid(Pid) ->
    put(?TRACKER_PID, Pid).

-spec stop_tracker() -> ok.
stop_tracker() ->
    stop_tracker(get_tracker()).

-spec stop_tracker(Pid :: maybe_pid()) -> ok.
stop_tracker(undefined) ->
    ok;
stop_tracker(Pid) when is_pid(Pid) ->
    Pid ! stop,
    ok.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    St = #st{},
    ok = initialize_matchers(St#st.registered_matchers),
    ok = subscribe_changes(),
    {ok, St}.

handle_call({register, Name, MSpec}, _From, #st{registered_matchers = RMatchers} = St) ->
    case add_matcher(Name, MSpec, RMatchers) of
        {ok, RMatchers1} ->
            ok = initialize_matchers(RMatchers1),
            {reply, ok, St#st{registered_matchers = RMatchers1}};
        {error, {invalid_ms, _, _}} = Error ->
            {reply, Error, St}
    end;
handle_call({deregister, Name}, _From, #st{registered_matchers = RMatchers} = St) ->
    case maps:is_key(Name, RMatchers) of
        false ->
            {reply, {error, missing_matcher}, St};
        true ->
            RMatchers1 = maps:remove(Name, RMatchers),
            ok = initialize_matchers(RMatchers1),
            {reply, ok, St#st{registered_matchers = RMatchers1}}
    end;
handle_call(reload_matchers, _From, St) ->
    couch_log:warning("Reloading persistent term matchers", []),
    ok = initialize_matchers(St#st.registered_matchers),
    {reply, ok, St};
handle_call(get_registered_matchers, _From, St) ->
    {reply, St#st.registered_matchers, St};
handle_call(Msg, From, St) ->
    {stop, {unknown_call, Msg, From}, St}.

handle_cast(Msg, St) ->
    {stop, {unknown_cast, Msg}, St}.

handle_info(restart_config_listener, St) ->
    ok = subscribe_changes(),
    {noreply, St};
handle_info(Msg, St) ->
    {stop, {unknown_info, Msg}, St}.

%%
%% Matchers
%%

-spec matcher_on_dbname(DbName :: dbname()) -> ets:match_spec().
matcher_on_dbname(DbName) when
    is_binary(DbName)
->
    ets:fun2ms(fun(#rctx{dbname = DbName1} = R) when DbName =:= DbName1 -> R end).

-spec matcher_on_dbnames_io_threshold(DbName, Threshold) -> ets:match_spec() when
    DbName :: dbname(), Threshold :: pos_integer().
matcher_on_dbnames_io_threshold(DbName, Threshold) when
    is_binary(DbName)
->
    ets:fun2ms(fun(
        #rctx{
            dbname = DbName1,
            ioq_calls = IOQ,
            get_kv_node = KVN,
            get_kp_node = KPN,
            docs_read = Docs,
            rows_read = Rows
        } = R
    ) when
        DbName =:= DbName1 andalso
            ((IOQ >= Threshold) or (KVN >= Threshold) or (KPN >= Threshold) or (Docs >= Threshold) or
                (Rows >= Threshold))
    ->
        R
    end).

-spec matcher_on_docs_read(Threshold :: pos_integer()) -> ets:match_spec().
matcher_on_docs_read(Threshold) when
    is_integer(Threshold) andalso Threshold > 0
->
    ets:fun2ms(fun(#rctx{docs_read = DocsRead} = R) when DocsRead >= Threshold -> R end).

-spec matcher_on_docs_written(Threshold :: pos_integer()) -> ets:match_spec().
matcher_on_docs_written(Threshold) when
    is_integer(Threshold) andalso Threshold > 0
->
    ets:fun2ms(fun(#rctx{docs_written = DocsWritten} = R) when DocsWritten >= Threshold -> R end).

-spec matcher_on_rows_read(Threshold :: pos_integer()) -> ets:match_spec().
matcher_on_rows_read(Threshold) when
    is_integer(Threshold) andalso Threshold > 0
->
    ets:fun2ms(fun(#rctx{rows_read = RowsRead} = R) when RowsRead >= Threshold -> R end).

-spec matcher_on_nonce(Nonce :: nonce()) -> ets:match_spec().
matcher_on_nonce(Nonce) ->
    ets:fun2ms(fun(#rctx{nonce = Nonce1} = R) when Nonce =:= Nonce1 -> R end).

-spec matcher_on_changes_processed(Threshold :: pos_integer()) -> ets:match_spec().
matcher_on_changes_processed(Threshold) when
    is_integer(Threshold) andalso Threshold > 0
->
    %% HACK: because we overload the use of #rctx.rows_read for
    %% changes_processed, we must specify a direct match against a changes
    %% context. Fow now, just match on #coordinator's
    ets:fun2ms(
        fun(
            #rctx{
                type = #coordinator{mod = chttpd_db, func = handle_changes_req},
                rows_read = Processed,
                changes_returned = Returned
            } = R
        ) when (Processed - Returned) >= Threshold ->
            R
        end
    ).

%% Matcher on requests taking longer than Threshold milliseconds
-spec matcher_on_long_reqs(Threshold :: pos_integer()) -> ets:match_spec().
matcher_on_long_reqs(Threshold) when
    is_integer(Threshold) andalso Threshold > 0
->
    %% Threshold is in milliseconds, but we track erlang:monotonic_time/0
    %% which is in native format, a machine dependent internal representation
    %% so we must convert the provided Threshold from milliseconds to native
    %% representation which then allows us to match against entries where the
    %% time delta between the started_at and updated_at is greater than the
    %% native converted threshold.
    %%
    %% Time warps and is relative and is complicated, so here's an example of
    %% converting 10000 milliseconds into a native time format and back, then
    %% using couch_srt_util:tnow/0 to accurately measure sleeping for 10000 ms.
    %%
    %% (node1@127.0.0.1)5> erlang:convert_time_unit(10000, millisecond, native).
    %% 10000000000
    %% (node1@127.0.0.1)6> erlang:convert_time_unit(10000000000, native, millisecond).
    %% 10000
    %% (node1@127.0.0.1)7> T0 = couch_srt_util:tnow(), timer:sleep(10000), T1 = couch_srt_util:tnow(),
    %%     erlang:convert_time_unit(T1 - T0, native, millisecond).
    %% 10000

    Unit = millisecond,
    NativeThreshold = erlang:convert_time_unit(Threshold, Unit, native),
    ets:fun2ms(
        fun(
            #rctx{
                started_at = Started,
                updated_at = Updated
            } = R
        ) when Updated - Started >= NativeThreshold ->
            R
        end
    ).

-spec matcher_on_ioq_calls(Threshold :: pos_integer()) -> ets:match_spec().
matcher_on_ioq_calls(Threshold) when
    is_integer(Threshold) andalso Threshold > 0
->
    ets:fun2ms(fun(#rctx{ioq_calls = IOQCalls} = R) when IOQCalls >= Threshold -> R end).

-spec pid_ref_matchspec(AttrName :: rctx_field()) -> matcher() | throw(any()).
pid_ref_matchspec(AttrName) ->
    #{field_idx := FieldIdx} = couch_srt_entry:record_info(),
    RctxMatch0 = #rctx{_ = '_'},
    RctxMatch1 = setelement(maps:get(pid_ref, FieldIdx) + 1, RctxMatch0, '$1'),
    RctxMatch = setelement(maps:get(AttrName, FieldIdx) + 1, RctxMatch1, '$2'),
    MatchSpec = [{RctxMatch, [], [{{'$1', '$2'}}]}],
    {MatchSpec, ets:match_spec_compile(MatchSpec)}.

-spec pid_ref_attrs(AttrName :: rctx_field()) -> list() | throw(any()).
pid_ref_attrs(AttrName) ->
    {MatchSpec, _CompMatch} = pid_ref_matchspec(AttrName),
    %% Base fields at least an empty list, but we could add more info here.
    %% The recon typespec is an improper list of the form:
    %%Base = [Name | [{current_function, mfa()} | {initial_call, mfa()}]],
    Base = [],
    [{PidRef, Val, Base} || {PidRef, Val} <- ets:select(?CSRT_ETS, MatchSpec)].

%% This is a recon:proc_window/3 [1] port with the same core logic but
%% recon_lib:proc_attrs/1 replaced with pid_ref_attrs/1, and returning on
%% pid_ref() rather than pid().
%% [1] https://github.com/ferd/recon/blob/c2a76855be3a226a3148c0dfc21ce000b6186ef8/src/recon.erl#L268-L300
-spec proc_window(AttrName, Num, Time) -> term() | throw(any()) when
    AttrName :: rctx_field(), Num :: non_neg_integer(), Time :: pos_integer().
proc_window(AttrName, Num, Time) ->
    Sample = fun() -> pid_ref_attrs(AttrName) end,
    {First, Last} = recon_lib:sample(Time, Sample),
    recon_lib:sublist_top_n_attrs(recon_lib:sliding_window(First, Last), Num).

-spec add_matcher(Name, MSpec, Matchers) ->
    {ok, matchers()} | {error, {invalid_ms, string(), ets:match_spec()}}
when
    Name :: string(), MSpec :: ets:match_spec(), Matchers :: matchers().
add_matcher(Name, MSpec, Matchers) ->
    try ets:match_spec_compile(MSpec) of
        CompMSpec ->
            %% TODO: handle already registered name case
            Matchers1 = maps:put(Name, {MSpec, CompMSpec}, Matchers),
            {ok, Matchers1}
    catch
        error:badarg ->
            {error, {invalid_ms, Name, MSpec}}
    end.

-spec set_matchers_term(Matchers :: matchers()) -> ok.
set_matchers_term(Matchers) when is_map(Matchers) ->
    persistent_term:put(?MATCHERS_KEY, Matchers).

-spec initialize_matchers(RegisteredMatchers :: map()) -> ok.
initialize_matchers(RegisteredMatchers) when is_map(RegisteredMatchers) ->
    %% Standard matchers to conditionally enable
    DefaultMatchers = [
        {docs_read, fun matcher_on_docs_read/1, 1000},
        {rows_read, fun matcher_on_rows_read/1, 1000},
        {docs_written, fun matcher_on_docs_written/1, 500},
        %% long_reqs Threshold in milliseconds
        {long_reqs, fun matcher_on_long_reqs/1, 60000},
        {changes_processed, fun matcher_on_changes_processed/1, 1000},
        {ioq_calls, fun matcher_on_ioq_calls/1, 10000}
    ],

    %% Add enabled Matchers for standard matchers
    Matchers = lists:foldl(
        fun({Name0, MatchGenFunc, Threshold0}, Matchers0) when is_atom(Name0) ->
            Name = atom_to_list(Name0),
            case matcher_enabled(Name) of
                true ->
                    %% Wrap in a try-catch to handle MatcherGen errors
                    try
                        Threshold = matcher_threshold(Name, Threshold0),
                        case add_matcher(Name, MatchGenFunc(Threshold), Matchers0) of
                            {ok, Matchers1} ->
                                Matchers1;
                            {error, {invalid_ms, NameE, MSpecE}} ->
                                couch_log:warning("[~p] Failed to initialize matcher[~p]: ~p", [
                                    ?MODULE, NameE, MSpecE
                                ]),
                                Matchers0
                        end
                    catch
                        _:_ ->
                            Matchers0
                    end;
                false ->
                    couch_log:warning("[~p] Failed to initialize matcher: ~p", [
                        ?MODULE, Name
                    ]),
                    Matchers0
            end
        end,
        #{},
        DefaultMatchers
    ),

    %% Add additional dbnames_io matchers
    Matchers1 = lists:foldl(
        fun({Dbname, Value}, Matchers0) ->
            try list_to_integer(Value) of
                Threshold when Threshold > 0 ->
                    Name = "dbnames_io__" ++ Dbname ++ "__" ++ Value,
                    DbnameB = list_to_binary(Dbname),
                    MSpec = matcher_on_dbnames_io_threshold(DbnameB, Threshold),
                    case add_matcher(Name, MSpec, Matchers0) of
                        {ok, Matchers1} ->
                            Matchers1;
                        {error, {invalid_ms, NameE, MSpecE}} ->
                            couch_log:warning("[~p] Failed to initialize matcher[~p]: ~p", [
                                ?MODULE, NameE, MSpecE
                            ]),
                            Matchers0
                    end;
                _ ->
                    Matchers0
            catch
                error:badarg ->
                    couch_log:warning("[~p] Failed to initialize dbname io matcher on[~p]: ~p", [
                        ?MODULE, Dbname, Value
                    ]),
                    Matchers0
            end
        end,
        Matchers,
        config:get(?CSRT_MATCHERS_DBNAMES)
    ),

    %% Finally, merge in the dynamically registered matchers, with priority
    Matchers2 = maps:merge(Matchers1, RegisteredMatchers),

    couch_log:notice("Initialized ~p CSRT Logger matchers", [maps:size(Matchers2)]),
    set_matchers_term(Matchers2),
    ok.

-spec matcher_enabled(Name :: string()) -> boolean().
matcher_enabled(Name) when is_list(Name) ->
    config:get_boolean(?CSRT_MATCHERS_ENABLED, Name, false).

-spec matcher_threshold(Name, Threshold) -> string() | integer() when
    Name :: string(), Threshold :: pos_integer() | string().
matcher_threshold(Name, Default) when
    is_list(Name) andalso is_integer(Default) andalso Default > 0
->
    config:get_integer(?CSRT_MATCHERS_THRESHOLD, Name, Default).

subscribe_changes() ->
    config:listen_for_changes(?MODULE, nil).

handle_config_change(?CSRT_MATCHERS_ENABLED, _Key, _Val, _Persist, St) ->
    ok = gen_server:call(?MODULE, reload_matchers, infinity),
    {ok, St};
handle_config_change(?CSRT_MATCHERS_THRESHOLD, _Key, _Val, _Persist, St) ->
    ok = gen_server:call(?MODULE, reload_matchers, infinity),
    {ok, St};
handle_config_change(?CSRT_MATCHERS_DBNAMES, _Key, _Val, _Persist, St) ->
    ok = gen_server:call(?MODULE, reload_matchers, infinity),
    {ok, St};
handle_config_change(_Sec, _Key, _Val, _Persist, St) ->
    {ok, St}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_, _, _) ->
    erlang:send_after(5000, whereis(?MODULE), restart_config_listener).
