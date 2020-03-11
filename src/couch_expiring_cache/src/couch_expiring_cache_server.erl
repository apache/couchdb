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

-module(couch_expiring_cache_server).

-behaviour(gen_server).

-callback start_link() -> {ok, pid()} | ignore | {error, term()}.

-export([
    start_link/2
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-define(DEFAULT_BATCH_SIZE, 1000).
-define(DEFAULT_PERIOD_MSEC, 5000).
-define(DEFAULT_MAX_JITTER_MSEC, 1000).


-include_lib("couch_expiring_cache/include/couch_expiring_cache.hrl").


start_link(Name, Opts) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Opts#{name => Name}, []).


init(Opts) ->
    DefaultCacheName = atom_to_binary(maps:get(name, Opts), utf8),
    Period = maps:get(period, Opts, ?DEFAULT_PERIOD_MSEC),
    MaxJitter = maps:get(max_jitter, Opts, ?DEFAULT_MAX_JITTER_MSEC),
    {ok, #{
        cache_name => maps:get(cache_name, Opts, DefaultCacheName),
        batch_size => maps:get(batch_size, Opts, ?DEFAULT_BATCH_SIZE),
        period => Period,
        max_jitter => MaxJitter,
        timer_ref => schedule_remove_expired(Period, MaxJitter),
        oldest_ts => 0,
        elapsed => 0,
        largest_elapsed => 0,
        lag => 0}}.


terminate(_, _) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info(remove_expired, St) ->
    #{
        cache_name := Name,
        batch_size := BatchSize,
        period := Period,
        max_jitter := MaxJitter,
        oldest_ts := OldestTS0,
        largest_elapsed := LargestElapsed
    } = St,

    NowTS = erlang:system_time(?TIME_UNIT),
    OldestTS = max(OldestTS0,
        couch_expiring_cache_fdb:clear_expired_range(Name, NowTS, BatchSize)),
    Elapsed = erlang:system_time(?TIME_UNIT) - NowTS,

    {noreply, St#{
        timer_ref := schedule_remove_expired(Period, MaxJitter),
        oldest_ts := OldestTS,
        elapsed := Elapsed,
        largest_elapsed := max(Elapsed, LargestElapsed),
        lag := NowTS - OldestTS}};


handle_info({Ref, ready}, St) when is_reference(Ref) ->
    % Prevent crashing server and application
    LogMsg = "~p : spurious erlfdb future ready message ~p",
    couch_log:error(LogMsg, [?MODULE, Ref]),
    {noreply, St};


handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


%% Private


schedule_remove_expired(Timeout, MaxJitter) ->
    Jitter = max(Timeout div 2, MaxJitter),
    Wait = Timeout + rand:uniform(max(1, Jitter)),
    erlang:send_after(Wait, self(), remove_expired).
