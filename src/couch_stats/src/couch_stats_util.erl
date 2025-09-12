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
    load/0,

    % Get various metric types
    get_counter/2,
    get_gauge/2,
    get_histogram/2,

    % Get histogram interval config settings
    histogram_interval_sec/0,
    histogram_safety_buffer_size_sec/0,

    % Get the stats persistent term map
    stats/0,
    histograms/1,

    % Fetch stats values
    fetch/3,
    sample/4
]).

-include_lib("stdlib/include/assert.hrl").

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

% Don't waste time looking for stats definition in some built-in and dependency
% apps. This doesn't have to be an exhaustive list, it's just to avoid doing
% extra work.
%
-define(SKIP_APPS, [
    asn1,
    b64url,
    compiler,
    cowlib,
    crypto,
    gun,
    ibrowse,
    inets,
    jiffy,
    kernel,
    meck,
    mochiweb,
    os_mon,
    public_key,
    rebar,
    rebar3,
    recon,
    runtime_tools,
    sasl,
    snappy,
    ssl,
    stdlib,
    syntax_tools,
    xmerl
]).

load() ->
    Definitions = load_metrics_for_applications(),
    Stats = create_metrics(Definitions),
    persistent_term:put(?STATS_KEY, Stats),
    Stats.

load_metrics_for_applications() ->
    Apps = [element(1, A) || A <- application:loaded_applications()],
    Apps1 = [A || A <- Apps, not lists:member(A, ?SKIP_APPS)],
    lists:foldl(fun load_metrics_for_application_fold/2, #{}, Apps1).

load_metrics_for_application_fold(AppName, #{} = Acc) ->
    % For an existing application we should always be able to compute its
    % priv_dir path, even though the directory itself may not exist or may not
    % be accessible.
    Dir = code:priv_dir(AppName),
    ?assert(is_list(Dir), "Could not get application priv_dir " ++ atom_to_list(AppName)),
    Path = filename:join(Dir, "stats_descriptions.cfg"),
    % Expect some apps not to have stats descriptions and priv_dir paths to not even exist
    case file:consult(Path) of
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
        {error, enoent} ->
            Acc;
        {error, enotdir} ->
            Acc;
        {error, Error} ->
            % Bail if we can't load stats for any other reason
            error({couch_stats_load_error, Path, Error})
    end.

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
    config:get_integer("stats", "interval", ?DEFAULT_INTERVAL_SEC).

histogram_safety_buffer_size_sec() ->
    ?HIST_WRAP_BUFFER_SIZE_SEC.

histogram_total_size_sec() ->
    % Add a safety buffer before and after the window couch_stats_server will
    % periodically clear.
    histogram_interval_sec() * 2 + ?HIST_WRAP_BUFFER_SIZE_SEC * 2.

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
