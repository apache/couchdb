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

-module(csrt).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_stats_resource_tracker.hrl").

%% PidRef API
-export([
    destroy_pid_ref/0,
    destroy_pid_ref/1,
    create_pid_ref/0,
    get_pid_ref/0,
    get_pid_ref/1,
    set_pid_ref/1
]).

%% Context API
-export([
    create_context/2,
    create_coordinator_context/2,
    create_worker_context/3,
    destroy_context/0,
    destroy_context/1,
    get_resource/0,
    get_resource/1,
    set_context_dbname/1,
    set_context_dbname/2,
    set_context_handler_fun/1,
    set_context_handler_fun/2,
    set_context_username/1,
    set_context_username/2
]).

%% Public API
-export([
    is_enabled/0,
    do_report/2,
    maybe_report/2,
    conf_get/1,
    conf_get/2
]).

%% stats collection api
-export([
    accumulate_delta/1,
    docs_written/1,
    inc/1,
    inc/2,
    ioq_called/0,
    make_delta/0,
    maybe_inc/2,
    should_track/1
]).

%% aggregate query api
-export([
    active/0,
    active/1,
    active_coordinators/0,
    active_coordinators/1,
    active_workers/0,
    active_workers/1,
    count_by/1,
    find_by_nonce/1,
    find_by_pid/1,
    find_by_pidref/1,
    find_workers_by_pidref/1,
    group_by/2,
    group_by/3,
    sorted/1,
    sorted_by/1,
    sorted_by/2,
    sorted_by/3
]).

%%
%% PidRef operations
%%

get_pid_ref() ->
    get(?PID_REF).

get_pid_ref(#rctx{pid_ref=PidRef}) ->
    PidRef.

set_pid_ref(PidRef) ->
    erlang:put(?PID_REF, PidRef),
    PidRef.

create_pid_ref() ->
    csrt_server:create_pid_ref().

destroy_pid_ref() ->
    destroy_pid_ref(get_pid_ref()).

%%destroy_pid_ref(undefined) ->
%%    undefined;
destroy_pid_ref(_PidRef) ->
    erase(?PID_REF).

%%
%%
%% Context lifecycle API
%%

%% TODO: shouldn't need this?
%% create_resource(#rctx{} = Rctx) ->
%%     csrt_server:create_resource(Rctx).

create_worker_context(From, {M,F,_A}, Nonce) ->
    case is_enabled() of
        true ->
            Type = #rpc_worker{from=From, mod=M, func=F},
            create_context(Type, Nonce);
        false ->
            false
    end.

create_coordinator_context(#httpd{method=Verb, nonce=Nonce}, Path0) ->
    case is_enabled() of
        true ->
            Path = list_to_binary([$/ | Path0]),
            Type = #coordinator{method=Verb, path=Path},
            create_context(Type, Nonce);
        false ->
            false
    end.

create_context(Type, Nonce) ->
    Rctx = csrt_server:new_context(Type, Nonce),
    %% TODO: which approach
    %% PidRef = csrt_server:pid_ref(Rctx),
    PidRef = get_pid_ref(Rctx),
    set_pid_ref(PidRef),
    erlang:put(?DELTA_TZ, Rctx),
    csrt_server:create_resource(Rctx),
    csrt_logger:track(Rctx),
    PidRef.

set_context_dbname(DbName) ->
    set_context_dbname(DbName, get_pid_ref()).

set_context_dbname(_, undefined) ->
    ok;
set_context_dbname(DbName, PidRef) ->
    is_enabled() andalso csrt_server:set_context_dbname(DbName, PidRef).

set_context_handler_fun(Fun) when is_function(Fun) ->
    case is_enabled() of
        false ->
            ok;
        true ->
            FProps = erlang:fun_info(Fun),
            Mod = proplists:get_value(module, FProps),
            Func = proplists:get_value(name, FProps),
            update_handler_fun(Mod, Func, get_pid_ref())
    end.

set_context_handler_fun(Mod, Func)
        when is_atom(Mod) andalso is_atom(Func)  ->
    case is_enabled() of
        false ->
            ok;
        true ->
            update_handler_fun(Mod, Func, get_pid_ref())
    end.

update_handler_fun(_, _, undefined) ->
    ok;
update_handler_fun(Mod, Func, PidRef) ->
    Rctx = get_resource(PidRef),
    %% TODO: #coordinator{} assumption needs to adapt for other types
    #coordinator{} = Coordinator0 = csrt_server:get_context_type(Rctx),
    Coordinator = Coordinator0#coordinator{mod=Mod, func=Func},
    csrt_server:set_context_type(Coordinator, PidRef),
    ok.

set_context_username(null) ->
    ok;
set_context_username(undefined) ->
    ok;
set_context_username(User) ->
    set_context_username(User, get_pid_ref()).

set_context_username(null, _) ->
    ok;
set_context_username(_, undefined) ->
    ok;
set_context_username(#httpd{user_ctx = Ctx}, PidRef) ->
    set_context_username(Ctx, PidRef);
set_context_username(#user_ctx{name = Name}, PidRef) ->
    set_context_username(Name, PidRef);
set_context_username(UserName, PidRef) ->
    is_enabled() andalso csrt_server:set_context_username(UserName, PidRef).

destroy_context() ->
    destroy_context(get_pid_ref()).

destroy_context(undefined) ->
    ok;
destroy_context({_, _} = PidRef) ->
    csrt_logger:stop_tracker(),
    destroy_pid_ref(PidRef),
    ok.

%%
%% Public API
%%

is_enabled() ->
    csrt_util:is_enabled().

get_resource() ->
    get_resource(get_pid_ref()).

get_resource(PidRef) ->
    csrt_server:get_resource(PidRef).

%% Log a CSRT report if any filters match
maybe_report(ReportName, PidRef) ->
    csrt_logger:maybe_report(ReportName, PidRef).

%% Direct report logic skipping should log filters
do_report(ReportName, PidRef) ->
    csrt_logger:do_report(ReportName, PidRef).

%%
%% Stat collection API
%%

inc(Key) ->
    inc(Key, 1).

inc(Key, N) ->
    case maps:is_key(Key, ?KEYS_TO_FIELDS) of
        true ->
            update_counter(maps:get(Key, ?KEYS_TO_FIELDS), N);
        false ->
            0
    end.


maybe_inc(Stat, Val) ->
    case maps:is_key(Stat, ?STATS_TO_KEYS) of
        true ->
            inc(maps:get(Stat, ?STATS_TO_KEYS), Val);
        false ->
            0
    end.

%% TODO: update stats_descriptions.cfg for relevant apps
should_track([fabric_rpc, all_docs, spawned]) ->
    is_enabled();
should_track([fabric_rpc, changes, spawned]) ->
    is_enabled();
should_track([fabric_rpc, changes, processed]) ->
    is_enabled();
should_track([fabric_rpc, changes, returned]) ->
    is_enabled();
should_track([fabric_rpc, map_view, spawned]) ->
    is_enabled();
should_track([fabric_rpc, reduce_view, spawned]) ->
    is_enabled();
should_track([fabric_rpc, get_all_security, spawned]) ->
    is_enabled();
should_track([fabric_rpc, open_doc, spawned]) ->
    is_enabled();
should_track([fabric_rpc, update_docs, spawned]) ->
    is_enabled();
should_track([fabric_rpc, open_shard, spawned]) ->
    is_enabled();
should_track([mango_cursor, view, all_docs]) ->
    is_enabled();
should_track([mango_cursor, view, idx]) ->
    is_enabled();
should_track(_Metric) ->
    %%io:format("SKIPPING METRIC: ~p~n", [Metric]),
    false.

ioq_called() ->
    inc(ioq_calls).

docs_written(N) ->
    inc(docs_written, N).

accumulate_delta(Delta) when is_map(Delta) ->
    %% TODO: switch to creating a batch of updates to invoke a single
    %% update_counter rather than sequentially invoking it for each field
    is_enabled() andalso maps:foreach(fun inc/2, Delta);
accumulate_delta(undefined) ->
    ok.

make_delta() ->
    TA = case get(?DELTA_TA) of
        undefined ->
            %% Need to handle this better, can't just make a new T0 at T' as
            %% the timestamps will be identical causing a divide by zero error.
            %%
            %% Realistically need to ensure that all invocations of database
            %% operations sets T0 appropriately. Perhaps it's possible to do
            %% this is the couch_db:open chain, and then similarly, in
            %% couch_server, and uhhhh... couch_file, and...
            %%
            %% I think we need some type of approach for establishing a T0 that
            %% doesn't result in outrageous deltas. For now zero out the
            %% microseconds field, or subtract a second on the off chance that
            %% microseconds is zero. I'm not uptodate on the latest Erlang time
            %% libraries and don't remember how to easily get an
            %% `os:timestamp()` out of now() - 100ms or some such.
            %%
            %% I think it's unavoidable that we'll have some codepaths that do
            %% not properly instantiate the T0 at spawn resulting in needing to
            %% do some time of "time warp" or ignoring the timing collection
            %% entirely. Perhaps if we hoisted out the stats collection into
            %% the primary flow of the database and funnel that through all the
            %% function clauses we could then utilize Dialyzer to statically
            %% analyze and assert all code paths that invoke database
            %% operations have properly instantinated a T0 at the appropriate
            %% start time such that we don't have to "fudge" deltas with a
            %% missing start point, but we're a long ways from that happening
            %% so I feel it necessary to address the NULL start time.

            %% Track how often we fail to initiate T0 correctly
            %% Perhaps somewhat naughty we're incrementing stats from within
            %% couch_stats itself? Might need to handle this differently
            %% TODO: determine appropriate course of action here
            %% io:format("~n**********MISSING STARTING DELTA************~n~n", []),
            couch_stats:increment_counter(
                [couchdb, csrt, delta_missing_t0]),
                %%[couch_stats_resource_tracker, delta_missing_t0]),

            case erlang:get(?DELTA_TZ) of
                undefined ->
                    TA0 = make_delta_base(),
                    %% TODO: handline missing deltas, otherwise divide by zero
                    set_delta_a(TA0),
                    TA0;
                TA0 ->
                    TA0
            end;
        #rctx{} = TA0 ->
            TA0
    end,
    TB = get_resource(),
    Delta = make_delta(TA, TB),
    set_delta_a(TB),
    Delta.

make_delta(#rctx{}=TA, #rctx{}=TB) ->
    Delta = #{
        docs_read => TB#rctx.docs_read - TA#rctx.docs_read,
        docs_written => TB#rctx.docs_written - TA#rctx.docs_written,
        js_filter => TB#rctx.js_filter - TA#rctx.js_filter,
        js_filtered_docs => TB#rctx.js_filtered_docs - TA#rctx.js_filtered_docs,
        rows_read => TB#rctx.rows_read - TA#rctx.rows_read,
        changes_returned => TB#rctx.changes_returned - TA#rctx.changes_returned,
        get_kp_node => TB#rctx.get_kp_node - TA#rctx.get_kp_node,
        get_kv_node => TB#rctx.get_kv_node - TA#rctx.get_kv_node,
        db_open => TB#rctx.db_open - TA#rctx.db_open,
        ioq_calls => TB#rctx.ioq_calls - TA#rctx.ioq_calls,
        dt => csrt_util:make_dt(TA#rctx.updated_at, TB#rctx.updated_at)
    },
    %% TODO: reevaluate this decision
    %% Only return non zero (and also positive) delta fields
    maps:filter(fun(_K,V) -> V > 0 end, Delta);
make_delta(_, #rctx{}) ->
    #{error => missing_beg_rctx};
make_delta(#rctx{}, _) ->
    #{error => missing_fin_rctx}.

%% TODO: what to do when PidRef=undefined?
make_delta_base(PidRef) ->
    %% TODO: extract user_ctx and db/shard from request
    Now = csrt_util:tnow(),
    #rctx{
        pid_ref = PidRef,
        %% TODO: confirm this subtraction works
        started_at = Now - 100, %% give us 100ms rewind time for missing T0
        updated_at = Now
    }.

make_delta_base() ->
    make_delta_base(get_pid_ref()).

set_delta_a(TA) ->
    erlang:put(?DELTA_TA, TA).

update_counter(Field, Count) ->
    is_enabled() andalso csrt_server:update_counter(get_pid_ref(), Field, Count).


conf_get(Key) ->
    csrt_util:conf_get(Key).


conf_get(Key, Default) ->
    csrt_util:conf_get(Key, Default).

%%
%% aggregate query api
%%

active() ->
    csrt_query:active().

active(Type) ->
    csrt_query:active(Type).

active_coordinators() ->
    csrt_query:active_coordinators().

active_coordinators(Type) ->
    csrt_query:active_coordinators(Type).

active_workers() ->
    csrt_query:active_workers().

active_workers(Type) ->
    csrt_query:active_workers(Type).

count_by(Key) ->
    csrt_query:count_by(Key).

find_by_nonce(Nonce) ->
    csrt_query:find_by_nonce(Nonce).

find_by_pid(Pid) ->
    csrt_query:find_by_pid(Pid).

find_by_pidref(PidRef) ->
    csrt_query:find_by_pidref(PidRef).

find_workers_by_pidref(PidRef) ->
    csrt_query:find_workers_by_pidref(PidRef).

group_by(Key, Val) ->
    csrt_query:group_by(Key, Val).

group_by(Key, Val, Agg) ->
    csrt_query:group_by(Key, Val, Agg).

sorted(Map) ->
    csrt_query:sorted(Map).

sorted_by(Key) ->
    csrt_query:sorted_by(Key).

sorted_by(Key, Val) ->
    csrt_query:sorted_by(Key, Val).

sorted_by(Key, Val, Agg) ->
    csrt_query:sorted_by(Key, Val, Agg).

%%
%% Internal Operations assuming is_enabled() == true
%%


-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_stats_resource_tracker_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_static_map_translations)
        ]
    }.

setup() ->
    test_util:start_couch().

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

t_static_map_translations(_) ->
    ?assert(lists:all(fun(E) -> maps:is_key(E, ?KEYS_TO_FIELDS) end, maps:values(?STATS_TO_KEYS))),
    %% TODO: properly handle ioq_calls field
    ?assertEqual(lists:sort(maps:values(?STATS_TO_KEYS)), lists:delete(docs_written, lists:delete(ioq_calls, lists:sort(maps:keys(?KEYS_TO_FIELDS))))).

-endif.
