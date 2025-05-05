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

% Basic leaky bucket and AIMD algorithm [1] to rate limit plugins:
%
%  * rate limiter gen_server periodically refills the configured
%    number of token in each db, shard, and doc buckets.
%
%  * couch_scanner_plugin calls update/2 function when performing operations.
%    It updates it's own backoff value and uses that to determine how much it
%    should sleep for that particular type of operation.
%
% [1] https://en.wikipedia.org/wiki/Additive_increase/multiplicative_decrease
%
% Example of usage:
%
%  initialize:
%    Limiter = couch_scanner_rate_limiter:get(),
%
%  use:
%    bulk_docs(#{docs => [doc1, doc2, doc3]}),
%    {Wait, Limiter1} = couch_scanner_rate_limiter:update(Limiter, doc_write, 3),
%    timer:sleep(Wait)
%       or
%    receive .... after Wait -> ... end
%
%  The Type can be:
%     * db : rate of clustered db opens
%     * shard: rate of shard files opened
%     * doc : rate of document reads
%     * doc_write : rate of document writes (or other per document updates, could be purges, too)
%

-module(couch_scanner_rate_limiter).

-behavior(gen_server).

-export([
    start_link/0,
    get/0,
    update/2,
    update/3
]).

% gen_server callbacks
%
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

% AIMD parameters
%
-define(MULTIPLICATIVE_FACTOR, 1.2).
-define(ADDITIVE_FACTOR, 0.5).

% Initial, minimal and maximum backoffs
%
-define(INIT_BACKOFF, 10).
-define(MIN_BACKOFF, 0.001).
-define(MAX_BACKOFF, 500).

% Limit the rate of the backoff updates This smooths out updates when the
% backoff interval is small (a few milliseconds only).
%
-define(SENSITIVITY_MSEC, 100).

% Default rates
%
-define(DB_RATE_DEFAULT, 25).
-define(SHARD_RATE_DEFAULT, 50).
-define(DOC_RATE_DEFAULT, 1000).
-define(DOC_WRITE_RATE_DEFAULT, 500).

% Atomic ref indices. They start at 1.
-define(INDICES, #{db => 1, shard => 2, doc => 3, doc_write => 4}).

% Record maintained by the clients. Each client will have one of these handles.
% With each update/2 call they will update their own backoff values.
%
-record(client_st, {
    ref,
    % db|shard|doc|doc_write => {Backoff, UpdateTStamp}
    backoffs = #{}
}).

-record(st, {
    ref,
    tref
}).

get() ->
    Ref = gen_server:call(?MODULE, get, infinity),
    NowMSec = erlang:monotonic_time(millisecond),
    Backoffs = maps:from_keys([db, shard, doc, doc_write], {?INIT_BACKOFF, NowMSec}),
    #client_st{ref = Ref, backoffs = Backoffs}.

update(St, Type) ->
    update(St, Type, 1).

update(#client_st{ref = Ref, backoffs = Backoffs} = St, Type, Count) when
    (is_integer(Count) andalso Count >= 0) andalso
        (Type =:= db orelse Type =:= shard orelse Type =:= doc orelse Type =:= doc_write)
->
    AtLimit = atomics:sub_get(Ref, map_get(Type, ?INDICES), Count) =< 0,
    {Backoff, TStamp} = map_get(Type, Backoffs),
    NowMSec = erlang:monotonic_time(millisecond),
    case NowMSec - TStamp > ?SENSITIVITY_MSEC of
        true ->
            Backoff1 = update_backoff(AtLimit, Backoff),
            St1 = St#client_st{backoffs = Backoffs#{Type := {Backoff1, NowMSec}}},
            {floor(Backoff1), St1};
        false ->
            {floor(Backoff), St}
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Gen server callbacks

init(_Args) ->
    St = #st{ref = atomics:new(map_size(?INDICES), [])},
    {ok, refill(St)}.

handle_call(get, _From, #st{} = St) ->
    {reply, St#st.ref, St};
handle_call(refill, _From, #st{} = St) ->
    % Used for testing mainly
    {reply, ok, refill(St)};
handle_call(Msg, _From, #st{} = St) ->
    couch_log:error("~p : unknown call ~p", [?MODULE, Msg]),
    {reply, {error, {invalid_call, Msg}}, St}.

handle_cast(Msg, #st{} = St) ->
    couch_log:error("~p : unknown cast ~p", [?MODULE, Msg]),
    {noreply, St}.

handle_info(refill, #st{} = St) ->
    {noreply, refill(St)};
handle_info(Msg, St) ->
    couch_log:error("~p : unknown info message ~p", [?MODULE, Msg]),
    {noreply, St}.

% Private functions

schedule_refill(#st{tref = TRef} = St) when is_reference(TRef) ->
    erlang:cancel_timer(TRef),
    schedule_refill(St#st{tref = undefined});
schedule_refill(#st{tref = undefined} = St) ->
    TRef = erlang:send_after(1000, self(), refill),
    St#st{tref = TRef}.

refill(#st{ref = Ref} = St) ->
    ok = atomics:put(Ref, map_get(db, ?INDICES), db_limit()),
    ok = atomics:put(Ref, map_get(shard, ?INDICES), shard_limit()),
    ok = atomics:put(Ref, map_get(doc, ?INDICES), doc_limit()),
    ok = atomics:put(Ref, map_get(doc_write, ?INDICES), doc_write_limit()),
    schedule_refill(St).

update_backoff(true, 0) ->
    ?MIN_BACKOFF;
update_backoff(true, Backoff) ->
    min(?MAX_BACKOFF, Backoff * ?MULTIPLICATIVE_FACTOR);
update_backoff(false, Backoff) ->
    max(0, Backoff - ?ADDITIVE_FACTOR).

db_limit() ->
    cfg_int("db_rate_limit", ?DB_RATE_DEFAULT).

shard_limit() ->
    cfg_int("shard_rate_limit", ?SHARD_RATE_DEFAULT).

doc_limit() ->
    cfg_int("doc_rate_limit", ?DOC_RATE_DEFAULT).

doc_write_limit() ->
    cfg_int("doc_write_rate_limit", ?DOC_WRITE_RATE_DEFAULT).

cfg_int(Key, Default) when is_list(Key), is_integer(Default) ->
    config:get_integer("couch_scanner", Key, Default).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_scanner_rate_limiter_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_init),
            ?TDEF_FE(t_update),
            ?TDEF_FE(t_update_multiple),
            ?TDEF_FE(t_refill)
        ]
    }.

t_init(_) ->
    ClientSt = ?MODULE:get(),
    ?assertEqual(ok, refill()),
    ?assertMatch({Val, #client_st{}} when is_number(Val), update(ClientSt, db)),
    ?assertMatch({Val, #client_st{}} when is_number(Val), update(ClientSt, shard)),
    ?assertMatch({Val, #client_st{}} when is_number(Val), update(ClientSt, doc)),
    ?assertMatch({Val, #client_st{}} when is_number(Val), update(ClientSt, doc_write)).

t_update(_) ->
    ClientSt = ?MODULE:get(),
    Fun = fun(_, Acc) ->
        {_, Acc1} = update(Acc, db),
        reset_time(Acc1, db)
    end,
    ClientSt1 = lists:foldl(Fun, ClientSt, lists:seq(1, 500)),
    {Backoff, _} = update(ClientSt1, db),
    ?assertEqual(?MAX_BACKOFF, Backoff).

t_update_multiple(_) ->
    ClientSt = ?MODULE:get(),
    Fun = fun(_, Acc) ->
        {_, Acc1} = update(Acc, doc_write, 100),
        reset_time(Acc1, doc_write)
    end,
    ClientSt1 = lists:foldl(Fun, ClientSt, lists:seq(1, 50)),
    {Backoff, _} = update(ClientSt1, doc_write, 100),
    ?assertEqual(?MAX_BACKOFF, Backoff).

t_refill(_) ->
    ClientSt = ?MODULE:get(),
    Fun = fun(_, Acc) ->
        refill(),
        {_, Acc1} = update(Acc, db),
        reset_time(Acc1, db)
    end,
    ClientSt1 = lists:foldl(Fun, ClientSt, lists:seq(1, 500)),
    {Backoff, _} = update(ClientSt1, db),
    ?assertEqual(0, Backoff).

setup() ->
    meck:new(config, [passthrough]),
    meck:expect(config, get, fun(_, _, Default) -> Default end),
    {ok, Pid} = start_link(),
    Pid.

teardown(Pid) when is_pid(Pid) ->
    gen_server:stop(Pid),
    meck:unload().

refill() ->
    gen_server:call(?MODULE, refill, infinity).

reset_time(#client_st{backoffs = Backoffs} = ClientSt, Type) ->
    #{Type := {Backoff, TStamp}} = Backoffs,
    TStamp1 = TStamp - 1000,
    Backoffs1 = Backoffs#{Type := {Backoff, TStamp1}},
    ClientSt#client_st{backoffs = Backoffs1}.

-endif.
