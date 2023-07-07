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

-module(couch_stats_util).

-export([
    % Load metrics from apps
    create_metrics/1,
    load_metrics_for_applications/0,
    metrics_changed/2,

    % Get various metric types
    get_counter/2,
    get_gauge/2,
    get_histogram/2,

    % Get histogram interval config settings
    histogram_interval_sec/0,
    histogram_safety_buffer_size_sec/0,
    reset_histogram_interval_sec/0,

    % Manage the main stats (metrics) persistent term map
    replace_stats/1,
    stats/0,
    histograms/1,

    % Fetch stats values
    fetch/3,
    sample/4
]).

-define(DEFAULT_INTERVAL_SEC, 10).

% Histogram types
-define(HIST, histogram).
-define(CNTR, counter).
-define(GAUGE, gauge).

% Safety buffer before and after current window to prevent
% overwrites from the cleaner process
-define(HIST_WRAP_BUFFER_SIZE_SEC, 5).

% Persistent term keys
-define(STATS_KEY, {?MODULE, stats}).
-define(HIST_TIME_INTERVAL_KEY, {?MODULE, hist_time_interval}).

load_metrics_for_applications() ->
    Apps = [element(1, A) || A <- application:loaded_applications()],
    lists:foldl(fun load_metrics_for_application_fold/2, #{}, Apps).

load_metrics_for_application_fold(AppName, #{} = Acc) ->
    case code:priv_dir(AppName) of
        {error, _Error} ->
            Acc;
        Dir ->
            case file:consult(Dir ++ "/stats_descriptions.cfg") of
                {ok, Descriptions} ->
                    DescMap = maps:map(
                        fun(_, TypeDesc) ->
                            Type = proplists:get_value(type, TypeDesc, counter),
                            Desc = proplists:get_value(desc, TypeDesc, <<>>),
                            {Type, Desc}
                        end,
                        maps:from_list(Descriptions)
                    ),
                    maps:merge(Acc, DescMap);
                {error, _Error} ->
                    Acc
            end
    end.

metrics_changed(#{} = Map1, #{} = Map2) when map_size(Map1) =/= map_size(Map2) ->
    % If their sizes are differently they are obvioulsy not the same
    true;
metrics_changed(#{} = Map1, #{} = Map2) when map_size(Map1) =:= map_size(Map2) ->
    % If their intersection size is not the same as their individual size
    % they are also not the same
    map_size(maps:intersect(Map1, Map2)) =/= map_size(Map1).

get_counter(Name, #{} = Stats) ->
    get_metric(Name, ?CNTR, Stats).

get_gauge(Name, #{} = Stats) ->
    get_metric(Name, ?GAUGE, Stats).

get_histogram(Name, #{} = Stats) ->
    get_metric(Name, ?HIST, Stats).

get_metric(Name, Type, Stats) when is_atom(Type), is_map(Stats) ->
    case maps:get(Name, Stats, unknown_metric) of
        {FoundType, Metric, _Desc} when FoundType =:= Type ->
            {ok, Metric};
        {OtherType, _, _} ->
            error_logger:error_msg("invalid metric: ~p ~p =/= ~p", [Name, Type, OtherType]),
            {error, invalid_metric};
        unknown_metric ->
            error_logger:error_msg("unknown metric: ~p", [Name]),
            {error, unknown_metric}
    end.

histogram_interval_sec() ->
    case persistent_term:get(?HIST_TIME_INTERVAL_KEY, not_cached) of
        not_cached ->
            Time = config:get_integer("stats", "interval", ?DEFAULT_INTERVAL_SEC),
            persistent_term:put(?HIST_TIME_INTERVAL_KEY, Time),
            Time;
        Val when is_integer(Val) ->
            Val
    end.

reset_histogram_interval_sec() ->
    persistent_term:erase(?HIST_TIME_INTERVAL_KEY).

histogram_safety_buffer_size_sec() ->
    ?HIST_WRAP_BUFFER_SIZE_SEC.

histogram_total_size_sec() ->
    % Add a safety buffer before and after the window couch_stats_server will
    % periodically clear.
    histogram_interval_sec() * 2 + ?HIST_WRAP_BUFFER_SIZE_SEC * 2.

replace_stats(#{} = Stats) ->
    persistent_term:put(?STATS_KEY, Stats).

stats() ->
    persistent_term:get(?STATS_KEY, #{}).

histograms(Stats) ->
    maps:filter(fun(_, {Type, _, _}) -> Type =:= ?HIST end, Stats).

create_metrics(MetricsDefs) ->
    maps:fold(fun create_fold/3, #{}, MetricsDefs).

create_fold(Name, {?CNTR, Desc}, #{} = Acc) ->
    Acc#{Name => {?CNTR, couch_stats_counter:new(), Desc}};
create_fold(Name, {?GAUGE, Desc}, #{} = Acc) ->
    Acc#{Name => {?GAUGE, couch_stats_gauge:new(), Desc}};
create_fold(Name, {?HIST, Desc}, #{} = Acc) ->
    TotalSizeSec = histogram_total_size_sec(),
    Acc#{Name => {?HIST, couch_stats_histogram:new(TotalSizeSec), Desc}};
create_fold(Name, Unknown, #{} = _Acc) ->
    throw({unknown_metric, {Name, Unknown}}).

fetch(#{} = Stats, Now, Interval) when is_integer(Now), is_integer(Interval) ->
    {Result, _, _} = maps:fold(fun fetch_fold/3, {[], Now, Interval}, Stats),
    Result.

fetch_fold(Name, {?CNTR, Ctx, Desc}, {Entries, Now, Interval}) ->
    Entry = [
        {value, couch_stats_counter:read(Ctx)},
        {type, ?CNTR},
        {desc, Desc}
    ],
    {[{Name, Entry} | Entries], Now, Interval};
fetch_fold(Name, {?GAUGE, Ctx, Desc}, {Entries, Now, Interval}) ->
    Entry = [
        {value, couch_stats_gauge:read(Ctx)},
        {type, ?GAUGE},
        {desc, Desc}
    ],
    {[{Name, Entry} | Entries], Now, Interval};
fetch_fold(Name, {?HIST, Ctx, Desc}, {Entries, Now, Interval}) ->
    Stats = couch_stats_histogram:stats(Ctx, Now, Interval),
    Entry = [
        {value, Stats},
        {type, ?HIST},
        {desc, Desc}
    ],
    {[{Name, Entry} | Entries], Now, Interval}.

sample(Name, #{} = Stats, Time, Ticks) when is_integer(Time), is_integer(Ticks) ->
    case maps:get(Name, Stats, unknown_metric) of
        {?CNTR, Ctx, _Desc} ->
            couch_stats_counter:read(Ctx);
        {?GAUGE, Ctx, _Desc} ->
            couch_stats_gauge:read(Ctx);
        {?HIST, Ctx, _Desc} ->
            couch_stats_histogram:stats(Ctx, Time, Ticks);
        unknown_metric ->
            throw(unknown_metric)
    end.
