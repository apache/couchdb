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

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2
]).

-export([
    log_level/0,
    log_level/1,
    maybe_report/1
]).

-record(st, {
    should_track,
    matchers = #{}
}).

%% TODO: switch to include for record def
-record(rctx, {
    %% Metadata
    started_at,
    updated_at,
    pid_ref,
    mfa,
    nonce,
    from,
    type = unknown, %% unknown/background/system/rpc/coordinator/fabric_rpc/etc_rpc/etc
    dbname,
    username,
    path,

    %% Stats counters
    db_open = 0,
    docs_read = 0,
    rows_read = 0,
    changes_processed = 0,
    changes_returned = 0,
    ioq_calls = 0,
    io_bytes_read = 0,
    io_bytes_written = 0,
    js_evals = 0,
    js_filter = 0,
    js_filtered_docs = 0,
    mango_eval_match = 0,
    %% TODO: switch record definitions to be macro based, eg:
    %% ?COUCH_BT_GET_KP_NODE = 0,
    get_kv_node = 0,
    get_kp_node = 0,
    write_kv_node = 0,
    write_kp_node = 0
}).

-record(rpc_worker, {
    mod,
    func
}).

-record(coordinator, {}).

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
    config:get_atom("couch_stats_resource_tracker", "log_level", none).

maybe_report(PidRef) ->
    Level = log_level(),
    case Level > ?NO_LOG of
        true ->
            Rctx = couch_stats_resource_tracker:get_resource(PidRef),
            RLevel = log_level(Rctx),
            case RLevel =< Level of
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

is_match(#rctx{}=R, Matchers) when is_map(Matchers) ->
    maps:size(find_matches([R], Matchers)) > 0.

%% default.ini:
%% [csrt]
%% enabled = false
%%
%% [csrt.should_log]
%% worker.fabric_rpc.all_docs = true
%% worker.fabric_rpc.map_view = true
%% worker.fabric_rpc.reduce_view = true
create_tracking_map() ->
    lists:fold(fun(Ele, Acc) ->
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
    {ok, #st{should_track=create_tracking_map()}}.

handle_call({register, Name, MSpec}, _From, #st{matchers=Matchers}=St) ->
    case add_matcher(Name, MSpec, Matchers) of
        {ok, Matchers1} ->
            {reply, ok, St#st{matchers=Matchers1}};
        {error, badarg}=Error ->
            {reply, Error, St}
    end;
handle_call({maybe_report, #rctx{}=Rctx, RLevel, Level}, _From, St) ->
    ok = maybe_report_int(Rctx, RLevel, Level, St),
    {reply, ok, St};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State, 0}.

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
    maps:is_key(Type, Trackers) andalso true =:= maps:get(Type, Trackers).
