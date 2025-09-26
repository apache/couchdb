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

% Scanner plugin server.
%
% This gen_server starts, stops and reschedules plugin processes.
%

-module(couch_scanner_server).

-behavior(gen_server).
-behavior(config_listener).

-export([
    start_link/0,
    status/0,
    stop/0,
    resume/0
]).

% gen_server callbacks
%
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

% config listener callbacks
%
-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

-define(INTERVAL_SEC, 15).
-define(MIN_PENALTY_SEC, 30).
-define(MAX_PENALTY_SEC, 8 * 3600).
-define(HEAL_THRESHOLD_SEC, 5 * 60).
-define(TIMEOUT, timeout).

-record(sched, {
    start_time = 0,
    error_count = 0,
    reschedule = 0
}).

-record(st, {
    stopped = false,
    % #{Id => Pid}
    pids = #{},
    % #{Id => #sched{}}
    scheduling = #{},
    tref
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

status() ->
    gen_server:call(?MODULE, status, infinity).

stop() ->
    gen_server:call(?MODULE, stop, infinity).

resume() ->
    gen_server:call(?MODULE, resume, infinity).

% Gen server callbacks

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, subscribe_for_config(#st{}), 0}.

terminate(_Reason, #st{pids = Pids} = St) ->
    ToStop = maps:keys(Pids),
    lists:foldl(fun stop_plugin/2, St, ToStop),
    ok.

handle_call(status, _From, #st{} = St) ->
    #st{pids = Pids, stopped = Stopped} = St,
    Fun = fun(_, Sched) ->
        #{
            start_time => Sched#sched.start_time,
            error_count => Sched#sched.error_count,
            reschedule => Sched#sched.reschedule
        }
    end,
    Scheds = maps:map(Fun, St#st.scheduling),
    {reply, #{pids => Pids, scheduling => Scheds, stopped => Stopped}, St};
handle_call(stop, _From, #st{} = St) ->
    St1 = St#st{stopped = true},
    St2 = stop_in_maintenance(St1),
    {reply, ok, St2};
handle_call(resume, _From, #st{} = St) ->
    St1 = start_stop(St#st{stopped = false}),
    {reply, ok, St1};
handle_call(Msg, _From, #st{} = St) ->
    couch_log:error("~p : unknown call ~p", [?MODULE, Msg]),
    {reply, {error, {invalid_call, Msg}}, St}.

handle_cast(Msg, #st{} = St) ->
    couch_log:error("~p : unknown cast ~p", [?MODULE, Msg]),
    {noreply, St}.

handle_info(?TIMEOUT, #st{} = St) ->
    St1 = cancel_timeout(St),
    St2 = start_stop(St1),
    St3 = schedule_timeout(St2),
    {noreply, St3};
handle_info({'EXIT', Pid, Reason}, #st{pids = Pids} = St) ->
    case maps:filter(fun(_, P) -> P =:= Pid end, Pids) of
        Map when map_size(Map) == 1 ->
            [{Id, Pid}] = maps:to_list(Map),
            St1 = St#st{pids = maps:remove(Id, Pids)},
            {noreply, handle_exit(Id, Reason, St1)};
        Map when map_size(Map) == 0 ->
            {noreply, St}
    end;
handle_info({reschedule, Id}, #st{} = St) ->
    {noreply, reschedule(Id, St)};
handle_info(restart_config_listener, #st{} = St) ->
    {noreply, subscribe_for_config(St)};
handle_info(Msg, St) ->
    couch_log:error("~p : unknown info message ~p", [?MODULE, Msg]),
    {noreply, St}.

% Config changes callbacks

handle_config_change("couchdb", "maintenance_mode", _Val, _Persist, Pid) ->
    Pid ! ?TIMEOUT,
    {ok, Pid};
handle_config_change("couch_scanner", _Key, _Val, _Persist, Pid) ->
    Pid ! ?TIMEOUT,
    {ok, Pid};
handle_config_change("couch_scanner_plugins", _Key, _Val, _Persist, Pid) ->
    Pid ! ?TIMEOUT,
    {ok, Pid};
handle_config_change(Id, "after", _Val, _Persist, Pid) ->
    Pid ! {reschedule, list_to_binary(Id)},
    {ok, Pid};
handle_config_change(Id, "repeat", _Val, _Persist, Pid) ->
    Pid ! {reschedule, list_to_binary(Id)},
    {ok, Pid};
handle_config_change(_Section, _Key, _Val, _Persist, Pid) ->
    {ok, Pid}.

handle_config_terminate(_Server, stop, _Pid) ->
    ok;
handle_config_terminate(_Server, _Reason, Pid) ->
    erlang:send_after(1000, Pid, restart_config_listener).

% Private

subscribe_for_config(#st{} = St) ->
    ok = config:listen_for_changes(?MODULE, self()),
    St.

stop_in_maintenance(#st{pids = Pids} = St) ->
    case map_size(Pids) > 0 of
        true ->
            couch_log:info("~p stopping in maintenance mode", [?MODULE]),
            lists:foldl(fun stop_plugin/2, St, maps:keys(Pids));
        false ->
            St
    end.

start_stop(#st{stopped = Stopped} = St) ->
    case in_maintenance() orelse Stopped of
        true -> stop_in_maintenance(St);
        false -> start_stop_cfg(St)
    end.

start_stop_cfg(#st{pids = Pids, scheduling = Scheduling} = St) ->
    PluginIds = plugins(),
    RunningIds = maps:keys(Pids),
    ToStart = PluginIds -- RunningIds,
    ToStop = RunningIds -- PluginIds,
    St1 = lists:foldl(fun stop_plugin/2, St, ToStop),
    lists:foreach(fun couch_scanner_checkpoint:reset/1, ToStop),
    ToRemove = maps:keys(Scheduling) -- PluginIds,
    St2 = St1#st{scheduling = maps:without(ToRemove, Scheduling)},
    lists:foreach(fun couch_scanner_checkpoint:reset/1, ToRemove),
    lists:foldl(fun start_plugin/2, St2, ToStart).

stop_plugin(Id, #st{} = St) ->
    #st{pids = Pids, scheduling = Scheduling} = St,
    {Pid, Pids1} = maps:take(Id, Pids),
    #{Id := #sched{} = Sched} = Scheduling,
    couch_log:info("~p : stopping ~s", [?MODULE, Id]),
    ok = couch_scanner_plugin:stop(Pid),
    Sched1 = Sched#sched{start_time = 0, reschedule = 0},
    Scheduling1 = Scheduling#{Id := Sched1},
    St#st{pids = Pids1, scheduling = Scheduling1}.

start_plugin(Id, #st{} = St) ->
    #st{pids = Pids, scheduling = Scheduling} = St,
    Sched = maps:get(Id, Scheduling, #sched{}),
    Now = tsec(),
    case Now >= Sched#sched.reschedule of
        true ->
            couch_log:info("~p : starting ~s", [?MODULE, Id]),
            Pids1 = Pids#{Id => couch_scanner_plugin:spawn_link(Id)},
            Sched1 = Sched#sched{
                start_time = Now,
                reschedule = 0
            },
            Scheduling1 = Scheduling#{Id => Sched1},
            St#st{pids = Pids1, scheduling = Scheduling1};
        false ->
            St
    end.

reschedule(Id, #st{scheduling = Scheduling} = St) ->
    % Reschedule if it's a plugin which is not running
    case {is_map_key(Id, St#st.pids), Scheduling} of
        {false, #{Id := #sched{} = Sched}} ->
            Sched1 = Sched#sched{reschedule = tsec(), error_count = 0},
            St#st{scheduling = Scheduling#{Id := Sched1}};
        {_, _} ->
            St
    end.

plugins() ->
    Fun = fun
        ({Module, "true"}, Acc) -> [list_to_binary(Module) | Acc];
        ({_, _}, Acc) -> Acc
    end,
    lists:foldl(Fun, [], config:get("couch_scanner_plugins")).

handle_exit(Id, Reason, #st{} = St) ->
    #st{scheduling = Scheduling} = St,
    #{Id := Sched} = Scheduling,
    Sched1 = sched_exit_update(Id, Sched, Reason),
    St#st{scheduling = Scheduling#{Id := Sched1}}.

sched_exit_update(Id, #sched{} = Sched, {shutdown, {reschedule, TSec}}) ->
    couch_log:notice("~p : ~s rescheduled after ~p", [?MODULE, Id, TSec]),
    Sched#sched{start_time = 0, error_count = 0, reschedule = TSec};
sched_exit_update(Id, #sched{} = Sched, {shutdown, reset}) ->
    couch_log:warning("~p : resetting plugin ~s", [?MODULE, Id]),
    couch_scanner_checkpoint:reset(Id),
    Sched#sched{start_time = 0, error_count = 0, reschedule = tsec()};
sched_exit_update(_Id, #sched{} = Sched, {shutdown, stop}) ->
    Sched#sched{start_time = 0, error_count = 0};
sched_exit_update(Id, #sched{} = Sched, Norm) when
    Norm == shutdown; Norm == normal
->
    couch_log:notice("~p : ~s finished", [?MODULE, Id]),
    Sched#sched{start_time = 0, error_count = 0, reschedule = infinity};
sched_exit_update(Id, #sched{} = Sched, Error) ->
    couch_log:error("~p : ~s exited with error ~p", [?MODULE, Id, Error]),
    #sched{start_time = Start, error_count = ErrorCount} = Sched,
    Sched1 = Sched#sched{start_time = 0},
    Now = tsec(),
    % If process has been running successfully for a while without crashing
    % reset (forgive) its previous errors.
    case Now - Start =< heal_threshold_sec() of
        true -> penalize(Now, Sched1#sched{error_count = 1});
        false -> penalize(Now, Sched1#sched{error_count = ErrorCount + 1})
    end.

penalize(Now, #sched{error_count = ErrorCount} = Sched) ->
    Penalty = min_penalty_sec() * (1 bsl (ErrorCount - 1)),
    Penalty1 = min(Penalty, max_penalty_sec()),
    Sched#sched{reschedule = Now + Penalty1}.

in_maintenance() ->
    "false" /= config:get("couchdb", "maintenance_mode", "false").

tsec() ->
    erlang:system_time(second).

cancel_timeout(#st{tref = undefined} = St) ->
    St;
cancel_timeout(#st{tref = TRef} = St) when is_reference(TRef) ->
    erlang:cancel_timer(TRef),
    St#st{tref = undefined}.

schedule_timeout(#st{tref = undefined} = St) ->
    TRef = erlang:send_after(1000 * interval_sec(), self(), timeout),
    St#st{tref = TRef}.

interval_sec() ->
    cfg_int("interval_sec", ?INTERVAL_SEC).

heal_threshold_sec() ->
    cfg_int("heal_threshold_sec", ?HEAL_THRESHOLD_SEC).

min_penalty_sec() ->
    cfg_int("min_penalty_sec", ?MIN_PENALTY_SEC).

max_penalty_sec() ->
    cfg_int("max_penalty_sec", ?MAX_PENALTY_SEC).

cfg_int(Key, Default) when is_list(Key), is_integer(Default) ->
    config:get_integer("couch_scanner", Key, Default).
