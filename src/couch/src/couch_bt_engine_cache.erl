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

-module(couch_bt_engine_cache).

-include_lib("stdlib/include/ms_transform.hrl").

% Main API
%
-export([
    insert/2,
    insert/3,
    reset/1,
    lookup/1,
    info/0,
    tables/0
]).

% Supervision and start API
%
-export([
    create_tables/0,
    sup_children/0,
    start_link/1,
    init/1
]).

-define(DEFAULT_SIZE, 67108864).
-define(DEFAULT_LEAVE_PERCENT, 30).
-define(INTERVAL_MSEC, 3000).
% How often cleaners check if the ets size increases. This is used in cases
% when initially, at the start the cleaning interval, ets table size is below
% "leave percent". Then we skip cleaning all the 0 usage entries. However, if a
% new set of requests come in soon after, but before the next clean-up interval
% they'll fail since the cache would be full. To be able to react quicker and
% make room, cleaners poll table sizes a bit more often.
-define(CLEANUP_INTERVAL_MSEC, 100).
% 1 bsl 58, power of 2 that's still an immediate integer
-define(MAX_PRIORITY, 288230376151711744).
-define(PTERM_KEY, {?MODULE, caches}).
% Metrics
-define(HITS, hits).
-define(MISSES, misses).
-define(FULL, full).

-record(cache, {tid, max_size}).

% Main API

insert(Key, Term) ->
    insert(Key, Term, 1).

insert(Key, Term, Priority) when is_integer(Priority) ->
    Priority1 = min(?MAX_PRIORITY, max(0, Priority)),
    case get_cache(Key) of
        #cache{tid = Tid, max_size = Max} ->
            case ets:info(Tid, memory) < Max of
                true ->
                    case ets:insert_new(Tid, {Key, Priority1, Term}) of
                        true ->
                            true;
                        false ->
                            bump_usage(Tid, Key),
                            false
                    end;
                false ->
                    bump_metric(?FULL),
                    false
            end;
        undefined ->
            false
    end.

reset(Key) ->
    case get_cache(Key) of
        #cache{tid = Tid} -> reset_usage(Tid, Key);
        undefined -> true
    end.

lookup(Key) ->
    case get_cache(Key) of
        #cache{tid = Tid} ->
            case ets:lookup_element(Tid, Key, 3, undefined) of
                undefined ->
                    bump_metric(?MISSES),
                    undefined;
                Term ->
                    bump_usage(Tid, Key),
                    bump_metric(?HITS),
                    Term
            end;
        undefined ->
            undefined
    end.

info() ->
    case persistent_term:get(?PTERM_KEY, undefined) of
        Caches when is_tuple(Caches) ->
            SizeMem = [info(C) || C <- tuple_to_list(Caches)],
            MaxMem = [Max || #cache{max_size = Max} <- tuple_to_list(Caches)],
            {Sizes, Mem} = lists:unzip(SizeMem),
            #{
                size => lists:sum(Sizes),
                memory => lists:sum(Mem),
                max_memory => lists:sum(MaxMem) * wordsize(),
                full => sample_metric(?FULL),
                hits => sample_metric(?HITS),
                misses => sample_metric(?MISSES),
                shard_count => shard_count()
            };
        undefined ->
            #{}
    end.

tables() ->
    case persistent_term:get(?PTERM_KEY, undefined) of
        Caches when is_tuple(Caches) ->
            [Tid || #cache{tid = Tid} <- tuple_to_list(Caches)];
        undefined ->
            []
    end.

% Supervisor helper functions

create_tables() ->
    BtCaches = [new() || _ <- lists:seq(1, shard_count())],
    persistent_term:put(?PTERM_KEY, list_to_tuple(BtCaches)).

sup_children() ->
    [sup_child(I) || I <- lists:seq(1, shard_count())].

sup_child(N) ->
    Name = list_to_atom("couch_bt_engine_cache_" ++ integer_to_list(N)),
    #{id => Name, start => {?MODULE, start_link, [N]}, shutdown => brutal_kill}.

% Process start and main loop

start_link(N) when is_integer(N) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [N])}.

init(N) ->
    Caches = persistent_term:get(?PTERM_KEY),
    Cache = #cache{tid = Tid} = element(N, Caches),
    ets:delete_all_objects(Tid),
    loop(Cache).

loop(#cache{tid = Tid} = Cache) ->
    decay(Tid),
    Next = now_msec() + wait_interval(?INTERVAL_MSEC),
    clean(Cache, Next - ?CLEANUP_INTERVAL_MSEC),
    remove_dead(Cache),
    WaitLeft = max(10, Next - now_msec()),
    timer:sleep(WaitLeft),
    loop(Cache).

% Clean unused procs. If we haven't cleaned any keep polling at a higher
% rate so we react quicker if a new set of entries are added.
%
clean(#cache{tid = Tid} = Cache, Until) ->
    case now_msec() < Until of
        true ->
            case should_clean(Cache) of
                true ->
                    ets:match_delete(Tid, {'_', 0, '_'});
                false ->
                    timer:sleep(wait_interval(?CLEANUP_INTERVAL_MSEC)),
                    clean(Cache, Until)
            end;
        false ->
            false
    end.

should_clean(#cache{tid = Tid, max_size = Max}) ->
    ets:info(Tid, memory) >= Max * leave_percent() / 100.

remove_dead(#cache{tid = Tid}) ->
    All = pids(Tid),
    Alive = sets:filter(fun is_process_alive/1, All),
    Dead = sets:subtract(All, Alive),
    % In OTP 27+ use sets:map/2
    Fun = fun(Pid, _) -> ets:match_delete(Tid, {{Pid, '_'}, '_', '_'}) end,
    sets:fold(Fun, true, Dead).

pids(Tid) ->
    Acc = couch_util:new_set(),
    try
        ets:foldl(fun pids_fold/2, Acc, Tid)
    catch
        error:badarg -> Acc
    end.

pids_fold({{Pid, _}, _, _}, Acc) when is_pid(Pid) ->
    sets:add_element(Pid, Acc);
pids_fold({_, _, _}, Acc) ->
    Acc.

new() ->
    Opts = [public, {write_concurrency, true}, {read_concurrency, true}],
    Max0 = round(max_size() / wordsize() / shard_count()),
    % Some per-table overhead for the table metadata
    Max = Max0 + round(250 * 1024 / wordsize()),
    #cache{tid = ets:new(?MODULE, Opts), max_size = Max}.

get_cache(Term) ->
    case persistent_term:get(?PTERM_KEY, undefined) of
        Caches when is_tuple(Caches) ->
            Index = erlang:phash2(Term, tuple_size(Caches)),
            #cache{} = element(1 + Index, Caches);
        undefined ->
            undefined
    end.

bump_usage(Tid, Key) ->
    % We're updating the second field incrementing it by 1 and clamping it
    % at ?MAX_PRIORITY. We don't set the default for the update_counter
    % specifically to avoid creating bogus entries just from updating the
    % counter, so expect the error:badarg here sometimes.
    UpdateOp = {2, 1, ?MAX_PRIORITY, ?MAX_PRIORITY},
    try
        ets:update_counter(Tid, Key, UpdateOp)
    catch
        error:badarg -> ok
    end.

reset_usage(Tid, Key) ->
    % Reset the value of the usage to 0. Since max value is ?MAX_PRIORITY,
    % subtract that and clamp it at 0. Do not provide a default since if an
    % entry is missing we don't want to create a bogus one from this operation.
    UpdateOp = {2, -?MAX_PRIORITY, 0, 0},
    try
        ets:update_counter(Tid, Key, UpdateOp)
    catch
        error:badarg -> ok
    end.

info(#cache{tid = Tid}) ->
    Memory = ets:info(Tid, memory) * wordsize(),
    Size = ets:info(Tid, size),
    {Size, Memory}.

decay(Tid) ->
    MatchSpec = ets:fun2ms(
        fun({Key, Usage, Term}) when Usage > 0 ->
            {Key, Usage bsr 1, Term}
        end
    ),
    ets:select_replace(Tid, MatchSpec).

shard_count() ->
    % Use a minimum size of 16 even for there are less than 16 schedulers
    % to keep the total tables size a bit smaller
    max(16, erlang:system_info(schedulers)).

wait_interval(Interval) ->
    Jitter = rand:uniform(max(1, Interval bsr 1)),
    Interval + Jitter.

max_size() ->
    config:get_integer("bt_engine_cache", "max_size", ?DEFAULT_SIZE).

leave_percent() ->
    Val = config:get_integer("bt_engine_cache", "leave_percent", ?DEFAULT_LEAVE_PERCENT),
    max(0, min(90, Val)).

now_msec() ->
    erlang:monotonic_time(millisecond).

bump_metric(Metric) when is_atom(Metric) ->
    couch_stats:increment_counter([couchdb, bt_engine_cache, Metric]).

sample_metric(Metric) when is_atom(Metric) ->
    try
        couch_stats:sample([couchdb, bt_engine_cache, Metric])
    catch
        throw:unknown_metric ->
            0
    end.

% ETS sizes are expressed in "words". To get the byte size need to multiply
% memory sizes by the wordsize. On 64 bit systems this should be 8
%
wordsize() ->
    erlang:system_info(wordsize).
