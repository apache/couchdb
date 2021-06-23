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


% This module implements rate limiting based on a variation the additive
% increase / multiplicative decrease feedback control algorithm.
%
%  https://en.wikipedia.org/wiki/Additive_increase/multiplicative_decrease
%
% This is an adaptive algorithm which converges on available channel
% capacity where each participant (client) doesn't a priori know the
% capacity, and participants don't communicate or know about each other (so they
% don't coordinate to divide the capacity among themselves).
%
% The algorithm referenced above estimates a rate, whereas the implemented
% algorithm uses an interval (in milliseconds). It preserves the original
% semantics, that is the failure part is multplicative and the success part is
% additive. The relationship between rate and interval is: rate = 1000 /
% interval.
%
% There are two main API functions:
%
%   success(Key) -> IntervalInMilliseconds
%   failure(Key) -> IntervalInMilliseconds
%
% Key is any term, typically something like {Method, Url}. The result from the
% function is the current period value. Caller then might decide to sleep for
% that amount of time before or after each request.


-module(couch_replicator_rate_limiter).

-behaviour(gen_server).

-export([
   start_link/0
]).

-export([
   init/1,
   terminate/2,
   handle_call/3,
   handle_info/2,
   handle_cast/2,
   code_change/3
]).

-export([
   interval/1,
   max_interval/0,
   failure/1,
   success/1
]).

% Types
-type key() :: any().
-type interval() :: non_neg_integer().
-type msec() :: non_neg_integer().


% Definitions

% Main parameters of the algorithm. The factor is the multiplicative part and
% base interval is the additive.
-define(BASE_INTERVAL, 20).
-define(BACKOFF_FACTOR, 1.2).

% If estimated period exceeds a limit, it is clipped to this value. This
% defines a practical limit of this algorithm. This is driven by real world
% concerns such as having a connection which sleeps for too long and ends up
% with socket timeout errors, or replication jobs which occupy a scheduler
% slot without making any progress.
-define(MAX_INTERVAL, 25000).

% Specify when (threshold) and how much (factor) to decay the estimated period.
% If there is a long pause between consecutive updates, the estimated period
% would become less accurate as more time passes. In such case choose to
% optimistically decay the estimated value. That is assume there a certain
% rate of successful requests happened. (For reference, TCP congestion algorithm
% also handles a variation of this in RFC 5681 under "Restarting Idle
% Connections" section).
-define(TIME_DECAY_FACTOR, 2).
-define(TIME_DECAY_THRESHOLD, 1000).

% Limit the rate of updates applied. This controls the rate of change of the
% estimated value. In colloquial terms it defines how "twitchy" the algorithm
% is. Or, another way to look at it, this is as a poor version of a low pass
% filter. (Some alternative TCP congestion control algorithms, like Westwood+
% use something similar to solve the ACK compression problem).
-define(SENSITIVITY_TIME_WINDOW, 80).


-record(state, {timer}).
-record(rec, {id, backoff, ts}).


-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec interval(key()) -> interval().
interval(Key) ->
    {Interval, _Timestamp} = interval_and_timestamp(Key),
    Interval.


-spec max_interval() -> interval().
max_interval() ->
    ?MAX_INTERVAL.


-spec failure(key()) -> interval().
failure(Key) ->
    {Interval, Timestamp} = interval_and_timestamp(Key),
    update_failure(Key, Interval, Timestamp, now_msec()).


-spec success(key()) -> interval().
success(Key) ->
    {Interval, Timestamp} = interval_and_timestamp(Key),
    update_success(Key, Interval, Timestamp, now_msec()).


% gen_server callbacks

init([]) ->
    couch_replicator_rate_limiter_tables:create(#rec.id),
    {ok, #state{timer = new_timer()}}.


terminate(_Reason, _State) ->
    ok.


handle_call(_Msg, _From, State) ->
    {reply, invalid, State}.


handle_cast(_, State) ->
    {noreply, State}.


handle_info(cleanup, #state{timer = Timer}) ->
    erlang:cancel_timer(Timer),
    TIds = couch_replicator_rate_limiter_tables:tids(),
    [cleanup_table(TId, now_msec() - ?MAX_INTERVAL) || TId <- TIds],
    {noreply, #state{timer = new_timer()}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% Private functions

-spec update_success(any(), interval(), msec(), msec()) -> interval().
update_success(_Key, _Interval, _Timestamp = 0, _Now) ->
    0;  % No ets entry. Keep it that way and don't insert a new one.

update_success(_Key, Interval, Timestamp, Now)
    when Now - Timestamp =< ?SENSITIVITY_TIME_WINDOW ->
    Interval;  % Ignore too frequent updates.

update_success(Key, Interval, Timestamp, Now) ->
    DecayedInterval = time_decay(Now - Timestamp, Interval),
    AdditiveFactor = additive_factor(DecayedInterval),
    NewInterval = DecayedInterval - AdditiveFactor,
    if
        NewInterval =< 0 ->
            Table = couch_replicator_rate_limiter_tables:term_to_table(Key),
            ets:delete(Table, Key),
            0;
        NewInterval =< ?BASE_INTERVAL ->
            insert(Key, ?BASE_INTERVAL, Now);
        NewInterval > ?BASE_INTERVAL ->
            insert(Key, NewInterval, Now)
    end.


-spec update_failure(any(), interval(), msec(), msec()) -> interval().
update_failure(_Key, Interval, Timestamp, Now)
    when Now - Timestamp =< ?SENSITIVITY_TIME_WINDOW ->
    Interval;  % Ignore too frequent updates.

update_failure(Key, Interval, _Timestamp, Now) ->
    Interval1 = erlang:max(Interval, ?BASE_INTERVAL),
    Interval2 = round(Interval1 * ?BACKOFF_FACTOR),
    Interval3 = erlang:min(Interval2, ?MAX_INTERVAL),
    insert(Key, Interval3, Now).


-spec insert(any(), interval(), msec()) -> interval().
insert(Key, Interval, Timestamp) ->
    Entry = #rec{id = Key, backoff = Interval, ts = Timestamp},
    Table = couch_replicator_rate_limiter_tables:term_to_table(Key),
    ets:insert(Table, Entry),
    Interval.


-spec interval_and_timestamp(key()) -> {interval(), msec()}.
interval_and_timestamp(Key) ->
    Table = couch_replicator_rate_limiter_tables:term_to_table(Key),
    case ets:lookup(Table, Key) of
        [] ->
            {0, 0};
        [#rec{backoff = Interval, ts = Timestamp}] ->
            {Interval, Timestamp}
    end.


-spec time_decay(msec(), interval()) -> interval().
time_decay(Dt, Interval) when Dt > ?TIME_DECAY_THRESHOLD ->
    DecayedInterval = Interval - ?TIME_DECAY_FACTOR * Dt,
    erlang:max(round(DecayedInterval), 0);

time_decay(_Dt, Interval) ->
    Interval.


% Calculate additive factor. Ideally it would be a constant but in this case
% it is a step function to help handle larger values as they are approaching
% the backoff limit. Large success values closer to the limit add some
% pressure against the limit, which is useful, as at the backoff limit the
% whole replication job is killed which can be costly in time and temporary work
% lost by those jobs.
-spec additive_factor(interval()) -> interval().
additive_factor(Interval) when Interval > 10000 ->
    ?BASE_INTERVAL * 50;
additive_factor(Interval) when Interval > 1000 ->
    ?BASE_INTERVAL * 5;
additive_factor(Interval) when Interval > 100 ->
    ?BASE_INTERVAL * 2;
additive_factor(_Interval) ->
    ?BASE_INTERVAL.


-spec new_timer() -> reference().
new_timer() ->
    erlang:send_after(?MAX_INTERVAL * 2, self(), cleanup).


-spec now_msec() -> msec().
now_msec() ->
    {Mega, Sec, Micro} = os:timestamp(),
    ((Mega * 1000000) + Sec) * 1000 + Micro div 1000.


-spec cleanup_table(atom(), msec()) -> non_neg_integer().
cleanup_table(Tid, LimitMSec) ->
    Head = #rec{ts = '$1', _ = '_'},
    Guard = {'<', '$1', LimitMSec},
    ets:select_delete(Tid, [{Head, [Guard], [true]}]).
