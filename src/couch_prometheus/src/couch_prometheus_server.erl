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

-module(couch_prometheus_server).

-behaviour(gen_server).

-import(couch_prometheus_util, [
    couch_to_prom/3,
    to_prom/3,
    to_prom_summary/2
]).

-export([
    scrape/0,
    version/0
]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include("couch_prometheus.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(st, {
    metrics,
    refresh
}).

init([]) ->
    Metrics = refresh_metrics(),
    RT = update_refresh_timer(),
    {ok, #st{metrics = Metrics, refresh = RT}}.

scrape() ->
    {ok, Metrics} = gen_server:call(?MODULE, scrape),
    Metrics.

version() ->
    ?PROMETHEUS_VERSION.

handle_call(scrape, _from, #st{metrics = Metrics} = State) ->
    {reply, {ok, Metrics}, State};
handle_call(refresh, _from, #st{refresh = OldRT} = State) ->
    timer:cancel(OldRT),
    Metrics = refresh_metrics(),
    RT = update_refresh_timer(),
    {reply, ok, State#st{metrics = Metrics, refresh = RT}};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(refresh, State) ->
    Metrics = refresh_metrics(),
    RT = update_refresh_timer(),
    {noreply, State#st{metrics = Metrics, refresh = RT}};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

refresh_metrics() ->
    CouchDB = get_couchdb_stats(),
    System = couch_stats_httpd:to_ejson(get_system_stats()),
    couch_prometheus_util:to_bin(
        lists:map(
            fun(Line) ->
                io_lib:format("~s~n", [Line])
            end,
            CouchDB ++ System
        )
    ).

get_couchdb_stats() ->
    Stats = lists:sort(couch_stats:fetch()),
    lists:flatmap(
        fun({Path, Info}) ->
            couch_to_prom(Path, Info, Stats)
        end,
        Stats
    ).

get_system_stats() ->
    lists:flatten([
        get_uptime_stat(),
        get_vm_stats(),
        get_io_stats(),
        get_message_queue_stats(),
        get_run_queue_stats(),
        get_vm_stats(),
        get_ets_stats()
    ]).

get_uptime_stat() ->
    to_prom(uptime_seconds, counter, couch_app:uptime() div 1000).

get_vm_stats() ->
    MemLabels = lists:map(
        fun({Type, Value}) ->
            {[{memory_type, Type}], Value}
        end,
        erlang:memory()
    ),
    {NumGCs, WordsReclaimed, _} = erlang:statistics(garbage_collection),
    CtxSwitches = element(1, erlang:statistics(context_switches)),
    Reds = element(1, erlang:statistics(reductions)),
    ProcCount = erlang:system_info(process_count),
    ProcLimit = erlang:system_info(process_limit),
    [
        to_prom(erlang_memory_bytes, gauge, MemLabels),
        to_prom(erlang_gc_collections_total, counter, NumGCs),
        to_prom(erlang_gc_words_reclaimed_total, counter, WordsReclaimed),
        to_prom(erlang_context_switches_total, counter, CtxSwitches),
        to_prom(erlang_reductions_total, counter, Reds),
        to_prom(erlang_processes, gauge, ProcCount),
        to_prom(erlang_process_limit, gauge, ProcLimit)
    ].

get_io_stats() ->
    {{input, In}, {output, Out}} = erlang:statistics(io),
    [
        to_prom(erlang_io_recv_bytes_total, counter, In),
        to_prom(erlang_io_sent_bytes_total, counter, Out)
    ].

get_message_queue_stats() ->
    Queues = lists:map(
        fun(Name) ->
            case process_info(whereis(Name), message_queue_len) of
                {message_queue_len, N} ->
                    N;
                _ ->
                    0
            end
        end,
        registered()
    ),
    [
        to_prom(erlang_message_queues, gauge, lists:sum(Queues)),
        to_prom(erlang_message_queue_min, gauge, lists:min(Queues)),
        to_prom(erlang_message_queue_max, gauge, lists:max(Queues))
    ].

get_run_queue_stats() ->
    %% Workaround for https://bugs.erlang.org/browse/ERL-1355
    {Normal, Dirty} =
        case erlang:system_info(dirty_cpu_schedulers) > 0 of
            false ->
                {statistics(run_queue), 0};
            true ->
                [DCQ | SQs] = lists:reverse(statistics(run_queue_lengths)),
                {lists:sum(SQs), DCQ}
        end,
    [
        to_prom(erlang_scheduler_queues, gauge, Normal),
        to_prom(erlang_dirty_cpu_scheduler_queues, gauge, Dirty)
    ].

get_ets_stats() ->
    NumTabs = length(ets:all()),
    to_prom(erlang_ets_table, gauge, NumTabs).

update_refresh_timer() ->
    RefreshTime = 1000 * config:get_integer("couch_prometheus", "interval", ?REFRESH_INTERVAL),
    erlang:send_after(RefreshTime, self(), refresh).
