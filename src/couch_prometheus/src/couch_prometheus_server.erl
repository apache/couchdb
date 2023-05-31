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
    to_prom/4,
    to_prom/2,
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

-ifdef(TEST).
-export([
    get_internal_replication_jobs_stat/0
]).
-endif.

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

handle_info(refresh, #st{refresh = OldRT} = State) ->
    timer:cancel(OldRT),
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
        get_io_stats(),
        get_message_queue_stats(),
        get_db_pid_stats(),
        get_run_queue_stats(),
        get_vm_stats(),
        get_ets_stats(),
        get_internal_replication_jobs_stat(),
        get_membership_stat(),
        get_distribution_stats()
    ]).

get_uptime_stat() ->
    to_prom(uptime_seconds, counter, "couchdb uptime", couch_app:uptime() div 1000).

get_internal_replication_jobs_stat() ->
    to_prom(
        internal_replication_jobs,
        gauge,
        "count of internal replication changes to process",
        try
            mem3_sync:get_backlog()
        catch
            _:_ ->
                couch_log:warning("~p mem3_sync down", [?MODULE]),
                0
        end
    ).

get_membership_stat() ->
    % expected nodes
    ClusterNodes = mem3:nodes(),
    % connected nodes
    AllNodes = nodes([this, visible]),
    Labels = [
        {[{nodes, "cluster_nodes"}], length(ClusterNodes)},
        {[{nodes, "all_nodes"}], length(AllNodes)}
    ],
    to_prom(membership, gauge, "count of nodes in the cluster", Labels).

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
        to_prom(
            erlang_memory_bytes,
            gauge,
            "size of memory (in bytes) dynamically allocated by the Erlang emulator",
            MemLabels
        ),
        to_prom(
            erlang_gc_collections_total,
            counter,
            "number of garbage collections by the Erlang emulator",
            NumGCs
        ),
        to_prom(
            erlang_gc_words_reclaimed_total,
            counter,
            "number of words reclaimed by garbage collections",
            WordsReclaimed
        ),
        to_prom(
            erlang_context_switches_total, counter, "total number of context switches", CtxSwitches
        ),
        to_prom(erlang_reductions_total, counter, "total number of reductions", Reds),
        to_prom(erlang_processes, gauge, "the number of Erlang processes", ProcCount),
        to_prom(
            erlang_process_limit,
            gauge,
            "the maximum number of simultaneously existing Erlang processes",
            ProcLimit
        )
    ].

get_io_stats() ->
    {{input, In}, {output, Out}} = erlang:statistics(io),
    [
        to_prom(
            erlang_io_recv_bytes_total,
            counter,
            "the total number of bytes received through ports",
            In
        ),
        to_prom(
            erlang_io_sent_bytes_total, counter, "the total number of bytes output to ports", Out
        )
    ].

get_message_queue_stats() ->
    Queues = chttpd_node:message_queues(),
    QueueLens = lists:map(fun({_, Len}) -> Len end, Queues),
    QueueLenByLabel = lists:map(fun({Name, Len}) -> {[{queue_name, Name}], Len} end, Queues),
    [
        to_prom(
            erlang_message_queues, gauge, "total size of all message queues", lists:sum(QueueLens)
        ),
        to_prom(
            erlang_message_queue_min,
            gauge,
            "minimum size across all message queues",
            lists:min(QueueLens)
        ),
        to_prom(
            erlang_message_queue_max,
            gauge,
            "maximum size across all message queues",
            lists:max(QueueLens)
        ),
        to_prom(erlang_message_queue_size, gauge, "size of message queue", QueueLenByLabel)
    ].

get_db_pid_stats() ->
    {CF, CDU} = chttpd_node:db_pid_stats(),
    [
        pid_to_prom_summary(
            "erlang_message_queue_couch_file",
            "size of message queue across couch_file processes",
            CF
        ),
        pid_to_prom_summary(
            "erlang_message_queue_couch_db_updater",
            "size of message queue across couch_db_updater processes",
            CDU
        )
    ].

pid_to_prom_summary(_, _, []) ->
    [];
pid_to_prom_summary(Metric, Desc, Mailboxes) ->
    Sorted = lists:sort(Mailboxes),
    Count = length(Sorted),
    Quantiles = [
        {[{quantile, <<"0.5">>}], lists:nth(round(Count * 0.5), Sorted)},
        {[{quantile, <<"0.9">>}], lists:nth(round(Count * 0.9), Sorted)},
        {[{quantile, <<"0.99">>}], lists:nth(round(Count * 0.99), Sorted)}
    ],
    SumStat = to_prom(Metric ++ ["_sum"], lists:sum(Sorted)),
    CountStat = to_prom(Metric ++ ["_count"], length(Sorted)),
    MinStat = to_prom(Metric ++ ["_min"], hd(Sorted)),
    MaxStat = to_prom(Metric ++ ["_max"], lists:last(Sorted)),
    to_prom(Metric, summary, Desc, Quantiles) ++ [SumStat, CountStat, MinStat, MaxStat].

get_run_queue_stats() ->
    %% Workaround for https://bugs.erlang.org/browse/ERL-1355
    {SQ, DCQ} = chttpd_node:run_queues(),
    [
        to_prom(erlang_scheduler_queues, gauge, "the total size of all normal run queues", SQ),
        to_prom(
            erlang_dirty_cpu_scheduler_queues,
            gauge,
            "the total size of all dirty CPU scheduler run queues",
            DCQ
        )
    ].

% gets the socket stat for the specified socket,
% inverting the result from inet:getstat/1 to
% return a map keyed on the stat_option and
% with a value representing the node and stat value
% e.g.
% #{
%    recv_oct => [{[{node="node2@127.0.0.1"}], 30609}]
%    recv_cnt => [{[{node="node2@127.0.0.1"}], 123}]
% ...
% }
% where there is an error fetching the socket stats,
% return no result for the specified node.
-spec get_sock_stats({Node, Socket}, MapAcc) ->
    #{OptionValue := [{[{node, Node}], Value}]}
when
    Node :: node(),
    Socket :: inet:socket(),
    OptionValue :: inet:stat_option(),
    Value :: integer(),
    MapAcc :: #{OptionValue := [{[{node, Node}], Value}]}.
get_sock_stats({Node, Socket}, MapAcc) ->
    try inet:getstat(Socket) of
        {ok, Stats} ->
            % For each Key/Value pair in Stats, append
            % an entry for the current Node to the result.
            % This relies on lists:foldl returning the final
            % accumulated map
            lists:foldl(
                fun({StatOption, Value}, Map0) ->
                    maps:update_with(StatOption, fun(V) -> V ++ [{[{node, Node}], Value}] end, Map0)
                end,
                MapAcc,
                Stats
            )
    catch
        _:_ ->
            % no result
            MapAcc
    end.

get_distribution_stats() ->
    % each distribution metric has a different type,
    % so expose each as a different metric with the erlang
    % node as a label.
    % This is the inverse of the structure returned by
    % inet:getstat/1.

    % This fold accumulates a map keyed on the socket
    % stat_option (https://www.erlang.org/doc/man/inet.html#getstat-2)
    % where the value is a list of labels/value pairs for that stat
    % e.g.
    % recv_oct => [{[{node="node2@127.0.0.1"}], 30609}, {[{node="node3@127.0.0.1"}], 28392}]
    % recv_cnt => [{[{node="node2@127.0.0.1"}], 123}, {[{node="node3@127.0.0.1"}], 134}]
    DefaultMap = #{
        recv_oct => [],
        recv_cnt => [],
        recv_max => [],
        recv_avg => [],
        recv_dvi => [],
        send_oct => [],
        send_cnt => [],
        send_max => [],
        send_avg => [],
        send_pend => []
    },
    NodeStats = erlang:system_info(dist_ctrl),
    DistStats = lists:foldl(
        fun get_sock_stats/2,
        DefaultMap,
        NodeStats
    ),
    [
        to_prom(
            erlang_distribution_recv_oct_bytes_total,
            counter,
            "Number of bytes received by the socket.",
            maps:get(recv_oct, DistStats)
        ),
        to_prom(
            erlang_distribution_recv_cnt_packets_total,
            counter,
            "number of packets received by the socket.",
            maps:get(recv_cnt, DistStats)
        ),
        to_prom(
            erlang_distribution_recv_max_bytes,
            gauge,
            "size of the largest packet, in bytes, received by the socket.",
            maps:get(recv_max, DistStats)
        ),
        to_prom(
            erlang_distribution_recv_avg_bytes,
            gauge,
            "average size of packets, in bytes, received by the socket.",
            maps:get(recv_avg, DistStats)
        ),
        to_prom(
            erlang_distribution_recv_dvi_bytes,
            gauge,
            "average packet size deviation, in bytes, received by the socket.",
            maps:get(recv_dvi, DistStats)
        ),
        to_prom(
            erlang_distribution_send_oct_bytes_total,
            counter,
            "Number of bytes sent by the socket.",
            maps:get(send_oct, DistStats)
        ),
        to_prom(
            erlang_distribution_send_cnt_packets_total,
            counter,
            "number of packets sent by the socket.",
            maps:get(send_cnt, DistStats)
        ),
        to_prom(
            erlang_distribution_send_max_bytes,
            gauge,
            "size of the largest packet, in bytes, sent by the socket.",
            maps:get(send_max, DistStats)
        ),
        to_prom(
            erlang_distribution_send_avg_bytes,
            gauge,
            "average size of packets, in bytes, sent by the socket.",
            maps:get(send_avg, DistStats)
        ),
        to_prom(
            erlang_distribution_send_pend_bytes,
            gauge,
            "number of bytes waiting to be sent by the socket.",
            maps:get(send_pend, DistStats)
        )
    ].

get_ets_stats() ->
    NumTabs = length(ets:all()),
    to_prom(erlang_ets_table, gauge, "number of ETS tables", NumTabs).

drain_refresh_messages() ->
    receive
        refresh -> drain_refresh_messages()
    after 0 ->
        ok
    end.

update_refresh_timer() ->
    drain_refresh_messages(),
    RefreshTime = 1000 * config:get_integer("prometheus", "interval", ?REFRESH_INTERVAL),
    erlang:send_after(RefreshTime, self(), refresh).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

drain_refresh_messages_test() ->
    self() ! refresh,
    {messages, Mq0} = erlang:process_info(self(), messages),
    ?assert(lists:member(refresh, Mq0)),
    drain_refresh_messages(),
    {messages, Mq1} = erlang:process_info(self(), messages),
    ?assert(not lists:member(refresh, Mq1)).

-endif.
