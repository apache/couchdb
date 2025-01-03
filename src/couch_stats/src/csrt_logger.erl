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

-behaviour(gen_server).
-behaviour(config_listener).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([
    log_level/0,
    log_level/1,
    maybe_report/1,
    maybe_report_old/1,
    get_matchers/0
]).

%% Config update subscription API
-export([
    subscribe_changes/0,
    handle_config_change/5,
    handle_config_terminate/3
]).

%% Matchers
-export([
    matcher_on_docs_read/1,
    matcher_on_worker_changes_processed/1,
    matcher_on_ioq_calls/1,
    matcher_on_nonce/1,
    matcher_on_nonce_comp/1
]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_stats_resource_tracker.hrl").

-record(st, {
    should_track,
    matchers = #{}
}).

-define(CSRT_CONTEXTS, [
   #rpc_worker{mod=fabric_rpc, func=all_docs},
   #rpc_worker{mod=fabric_rpc, func=changes},
   #rpc_worker{mod=fabric_rpc, func=map_view},
   #rpc_worker{mod=fabric_rpc, func=reduce_view},
   #rpc_worker{mod=fabric_rpc, func=get_all_security},
   #rpc_worker{mod=fabric_rpc, func=open_doc},
   #rpc_worker{mod=fabric_rpc, func=update_docs},
   #rpc_worker{mod=fabric_rpc, func=open_shard},
   #rpc_worker{mod=mango_cursor, func=view}
   %% enable all: #worker{}
   %% TODO: add coordinator/etc
   %% #coordinator{mod=chttpd_db:}
]).

-define(CSRT_LOG_LEVELS, [
    {all, 9},
    {workers_too, 4},
    {custom, 2},
    {coordinators, 1},
    {none, 0}
]).

-define(NO_LOG, 0).
-define(DEFAULT_LEVEL, coordinators).

-define(MATCHERS_KEY, {?MODULE, all_csrt_matchers}).
-define(CONF_MATCHERS_ENABLED, "csrt_logger.matchers_enabled").
-define(CONF_MATCHERS_THRESHOLD, "csrt_logger.matchers_threshold").

%%
%% Matchers
%%

matcher_on_docs_read(Threshold)
        when is_integer(Threshold) andalso Threshold > 0 ->
    ets:fun2ms(fun(#rctx{docs_read=DocsRead} = R) when DocsRead >= Threshold -> R end).

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

%%
%% Public API
%%

log_level(all) -> 9;
log_level(all_workers) -> 4;
log_level(rpc_workers) -> 3;
log_level(#rctx{type=#rpc_worker{}}) -> 3;
log_level(custom) -> 2;
log_level(coordinators) -> 1;
log_level(#rctx{type=#coordinator{}}) -> 1;
log_level(none) -> ?NO_LOG;
log_level(_) -> ?NO_LOG.

log_level() ->
    Level = config:get("couch_stats_resource_tracker", "log_level", "coordinators"),
    try
        log_level(list_to_existing_atom(Level))
    catch
        _:_ ->
            log_level(?DEFAULT_LEVEL)
    end.


maybe_report(PidRef) ->
    Rctx = couch_stats_resource_tracker:get_resource(PidRef),
    case is_match(Rctx) of
        true ->
            report(Rctx),
            ok;
        false ->
            ok
    end.

maybe_report_old(PidRef) ->
    Level = log_level(),
    case Level > ?NO_LOG of
        true ->
            Rctx = couch_stats_resource_tracker:get_resource(PidRef),
            RLevel = log_level(Rctx),
            %% io:format("CSRT LOGGER TRIGGERING{~p} ~p =<? ~p~n", [Rctx#rctx.type, RLevel, Level]),
            case RLevel >= Level of
                true ->
                    %% NOTE: we drop the PidRef here, it's now safe to delete the ets entry
                    Msg = {maybe_report, Rctx, RLevel, Level},
                    gen_server:cast(get_server(), Msg);
                false ->
                    ok
            end;
        false ->
            ok
    end.

find_matches(RContexts, Matchers) when is_list(RContexts) andalso is_map(Matchers) ->
    maps:filter(
        fun(_Name, {_MSpec, CompMSpec}) ->
            catch [] =/= ets:match_spec_run(RContexts, CompMSpec)
        end,
        Matchers
    ).

get_matchers() ->
    persistent_term:get(?MATCHERS_KEY, #{}).

is_match(#rctx{}=Rctx) ->
    is_match(Rctx, get_matchers()).

is_match(#rctx{}=Rctx, Matchers) when is_map(Matchers) ->
    maps:size(find_matches([Rctx], Matchers)) > 0.

%% default.ini:
%% [csrt]
%% enabled = false
%%
%% [csrt.should_log]
%% worker.fabric_rpc.all_docs = true
%% worker.fabric_rpc.map_view = true
%% worker.fabric_rpc.reduce_view = true
create_tracking_map() ->
    lists:foldl(fun(Ele, Acc) ->
        Key = case Ele of
            #rpc_worker{mod=M0, func=F0} ->
                M = atom_to_list(M0),
                F = atom_to_list(F0),
                "worker." ++ M ++ "." ++ F
            %% TODO: #coordinator{} ->
        end,
        Enabled = config:get_boolean("csrt.should_log", Key, true),
        maps:put(Ele, Enabled, Acc)
    end, #{}, ?CSRT_CONTEXTS).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = initialize_matchers(),
    ok = subscribe_changes(),
    {ok, #st{should_track=create_tracking_map()}}.

handle_call({register, Name, MSpec}, _From, #st{matchers=Matchers}=St) ->
    case add_matcher(Name, MSpec, Matchers) of
        {ok, Matchers1} ->
            set_matchers_term(Matchers1),
            {reply, ok, St#st{matchers=Matchers1}};
        {error, badarg}=Error ->
            {reply, Error, St}
    end;
handle_call({maybe_report, #rctx{}=Rctx, RLevel, Level}, _From, St) ->
    ok = maybe_report_int(Rctx, RLevel, Level, St),
    {reply, ok, St};
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

%% TODO: should we use originally defined log level? or St#st.level?
%% ShouldLog = log_level(Rctx) > St#st.log_level
maybe_report_int(#rctx{}=Rctx, RLevel, Level, #st{}) when RLevel >= Level ->
    report(Rctx);
maybe_report_int(#rctx{}=Rctx, _RLevel, _Level, #st{matchers=Matchers, should_track=Trackers}) ->
    case is_tracked(Rctx, Trackers) orelse is_match(Rctx, Matchers) of
        true ->
            report(Rctx);
        false ->
            ok
    end.

report(Rctx) ->
    couch_log:report(
        "csrt-pid-usage-lifetime",
        couch_stats_resource_tracker:to_json(Rctx)
    ).

get_server() ->
    %% TODO: shard servers like couch_server/etc
    ?MODULE.

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

is_tracked(#rctx{type=Type}, Trackers) ->
    Res = maps:is_key(Type, Trackers) andalso true =:= maps:get(Type, Trackers),
    Res.

set_matchers_term(Matchers) when is_map(Matchers) ->
    persistent_term:put({?MODULE, all_csrt_matchers}, Matchers).

initialize_matchers() ->
    DefaultMatchers = [
        {docs_read, fun matcher_on_docs_read/1, 1000},
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
    couch_log:warning("Initialized ~p CSRT Logger matchers", [maps:size(Matchers)]),
    persistent_term:put(?MATCHERS_KEY, Matchers),
    ok.


matcher_enabled(Name) when is_list(Name) ->
    %% TODO: fix
    %% config:get_boolean(?CONF_MATCHERS_ENABLED, Name, false).
    config:get_boolean(?CONF_MATCHERS_ENABLED, Name, true).

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
