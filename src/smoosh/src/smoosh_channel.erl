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

-module(smoosh_channel).
-behaviour(gen_server).
-vsn(1).
-include_lib("couch/include/couch_db.hrl").

% public api.
-export([start_link/1, close/1, suspend/1, resume/1, get_status/1]).
-export([enqueue/3, last_updated/2, flush/1]).

% gen_server api.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2]).

% records.

-record(state, {
    active=[],
    name,
    waiting=smoosh_priority_queue:new(),
    paused=true,
    starting=[]
}).

% public functions.

start_link(Name) ->
    gen_server:start_link(?MODULE, Name, []).

suspend(ServerRef) ->
    gen_server:call(ServerRef, suspend).

resume(ServerRef) ->
    gen_server:call(ServerRef, resume).

enqueue(ServerRef, Object, Priority) ->
    gen_server:cast(ServerRef, {enqueue, Object, Priority}).

last_updated(ServerRef, Object) ->
    gen_server:call(ServerRef, {last_updated, Object}).

get_status(ServerRef) ->
    gen_server:call(ServerRef, status).

close(ServerRef) ->
    gen_server:call(ServerRef, close).

flush(ServerRef) ->
    gen_server:call(ServerRef, flush).

% gen_server functions.

init(Name) ->
    schedule_unpause(),
    erlang:send_after(60 * 1000, self(), check_window),
    {ok, #state{name=Name}}.

handle_call({last_updated, Object}, _From, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    LastUpdated = smoosh_priority_queue:last_updated(Object, State#state.waiting),
    {reply, LastUpdated, State};

handle_call(suspend, _From, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    #state{active = Active} = State,
    [catch erlang:suspend_process(Pid, [unless_suspending])
        || {_,Pid} <- Active],
    {reply, ok, State#state{paused=true}};

handle_call(resume, _From, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    #state{active = Active} = State,
    [catch erlang:resume_process(Pid) || {_,Pid} <- Active],
    {reply, ok, State#state{paused=false}};

handle_call(status, _From, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    {reply, {ok, [
        {active, length(State#state.active)},
        {starting, length(State#state.starting)},
        {waiting, smoosh_priority_queue:info(State#state.waiting)}
    ]}, State};

handle_call(close, _From, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    {stop, normal, ok, State};

handle_call(flush, _From, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    {reply, ok, State#state{waiting=smoosh_priority_queue:new()}}.

handle_cast({enqueue, _Object, 0}, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    {noreply, State};
handle_cast({enqueue, Object, Priority}, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    {noreply, maybe_start_compaction(add_to_queue(Object, Priority, State))}.

% We accept noproc here due to possibly having monitored a restarted compaction
% pid after it finished.
handle_info({'DOWN', Ref, _, Job, Reason}, State0)  when Reason == normal;
        Reason == noproc ->
    {ok, State} = code_change(nil, State0, nil),
    #state{active=Active, starting=Starting} = State,
    {noreply, maybe_start_compaction(
                State#state{active=lists:keydelete(Job, 2, Active),
                            starting=lists:keydelete(Ref, 1, Starting)})};

handle_info({'DOWN', Ref, _, Job, Reason}, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    #state{active=Active0, starting=Starting0} = State,
    case lists:keytake(Job, 2, Active0) of
        {value, {Key, _Pid}, Active1} ->
            couch_log:warning("exit for compaction of ~p: ~p", [
                smoosh_utils:stringify(Key), Reason]),
            {ok, _} = timer:apply_after(5000, smoosh_server, enqueue, [Key]),
            {noreply, maybe_start_compaction(State#state{active=Active1})};
        false ->
            case lists:keytake(Ref, 1, Starting0) of
                {value, {_, Key}, Starting1} ->
                    couch_log:warning("failed to start compaction of ~p: ~p", [
                        smoosh_utils:stringify(Key), Reason]),
                    {ok, _} = timer:apply_after(5000, smoosh_server, enqueue, [Key]),
                    {noreply, maybe_start_compaction(State#state{starting=Starting1})};
                false ->
                    {noreply, State}
            end
    end;

handle_info({Ref, {ok, Pid}}, State0) when is_reference(Ref) ->
    {ok, State} = code_change(nil, State0, nil),
    case lists:keytake(Ref, 1, State#state.starting) of
        {value, {_, Key}, Starting1} ->
            couch_log:notice("~s: Started compaction for ~s",
                     [State#state.name, smoosh_utils:stringify(Key)]),
            erlang:monitor(process, Pid),
            erlang:demonitor(Ref, [flush]),
            {noreply, State#state{active=[{Key, Pid}|State#state.active],
                                  starting=Starting1}};
        false ->
            {noreply, State}
    end;

handle_info(check_window, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    #state{paused = Paused, name = Name} = State,
    StrictWindow = smoosh_utils:get(Name, "strict_window", "false"),
    FinalState = case {not Paused, smoosh_utils:in_allowed_window(Name)} of
        {false, false} ->
            % already in desired state
            State;
        {true, true} ->
            % already in desired state
            State;
        {false, true} ->
            % resume is always safe even if we did not previously suspend
            {reply, ok, NewState} = handle_call(resume, nil, State),
            NewState;
        {true, false} ->
            if StrictWindow =:= "true" ->
                {reply, ok, NewState} = handle_call(suspend, nil, State),
                NewState;
            true ->
                State#state{paused=true}
            end
    end,
    erlang:send_after(60 * 1000, self(), check_window),
    {noreply, FinalState};

handle_info(pause, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    {noreply, State#state{paused=true}};
handle_info(unpause, State0) ->
    {ok, State} = code_change(nil, State0, nil),
    {noreply, maybe_start_compaction(State#state{paused=false})}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, #state{}=State, _Extra) ->
    {ok, State}.

% private functions.

add_to_queue(Key, Priority, State) ->
    #state{active=Active,waiting=Q} = State,
    case lists:keymember(Key, 1, Active) of
    true ->
        State;
    false ->
        Capacity = list_to_integer(smoosh_utils:get(State#state.name, "capacity", "9999")),
        couch_log:notice(
            "~s: adding ~p to internal compactor queue with priority ~p",
                 [State#state.name, Key, Priority]),
        State#state{
            waiting=smoosh_priority_queue:in(Key, Priority, Priority, Capacity, Q)
        }
    end.

maybe_start_compaction(#state{paused=true}=State) ->
    State;
maybe_start_compaction(State) ->
    Concurrency = list_to_integer(smoosh_utils:get(State#state.name,
        "concurrency", "1")),
    if length(State#state.active) + length(State#state.starting) < Concurrency ->
        case smoosh_priority_queue:out(State#state.waiting) of
        false ->
            State;
        {Key, Priority, Q} ->
            try
                State2 = case start_compact(State, Key) of
                false ->
                    State;
                State1 ->
                    couch_log:notice(
                        "~s: Starting compaction for ~s (priority ~p)",
                        [State#state.name, smoosh_utils:stringify(Key), Priority]),
                    State1
                end,
                maybe_start_compaction(State2#state{waiting=Q})
            catch Class:Exception ->
                couch_log:notice("~s: ~p ~p for ~s",
                    [State#state.name, Class, Exception,
                        smoosh_utils:stringify(Key)]),
                maybe_start_compaction(State#state{waiting=Q})
            end
        end;
    true ->
        State
    end.

start_compact(State, {schema, DbName, GroupId}) ->
    case smoosh_utils:ignore_db({DbName, GroupId}) of
        false ->
            {ok, Pid} = couch_md_index_manager:get_group_pid(DbName,
                GroupId),
            Ref = erlang:monitor(process, Pid),
            Pid ! {'$gen_call', {self(), Ref}, compact},
            State#state{starting=[{Ref, {schema, DbName,
                GroupId}} | State#state.starting]};
        _ ->
            false
    end;

start_compact(State, DbName) when is_list(DbName) ->
    start_compact(State, ?l2b(DbName));
start_compact(State, DbName) when is_binary(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    try start_compact(State, Db) after couch_db:close(Db) end;
start_compact(State, {Shard,GroupId}) ->
    case smoosh_utils:ignore_db({Shard, GroupId}) of
    false ->
        DbName = mem3:dbname(Shard),
        {ok, Pid} = couch_index_server:get_index(
                couch_mrview_index, Shard, GroupId),
        spawn(fun() -> cleanup_index_files(DbName, Shard) end),
        Ref = erlang:monitor(process, Pid),
        Pid ! {'$gen_call', {self(), Ref}, compact},
        State#state{starting=[{Ref, {Shard, GroupId}}|State#state.starting]};
    _ ->
        false
    end;
start_compact(State, Db) ->
    case smoosh_utils:ignore_db(Db) of
    false ->
        DbPid = couch_db:get_pid(Db),
        Key = couch_db:name(Db),
        case couch_db:get_compactor_pid(Db) of
            nil ->
                Ref = erlang:monitor(process, DbPid),
                DbPid ! {'$gen_call', {self(), Ref}, start_compact},
                State#state{starting=[{Ref, Key}|State#state.starting]};
            % database is still compacting so we can just monitor the existing
            % compaction pid
            CPid ->
                couch_log:notice("Db ~s continuing compaction",
                    [smoosh_utils:stringify(Key)]),
                erlang:monitor(process, CPid),
                State#state{active=[{Key, CPid}|State#state.active]}
        end;
    _ ->
        false
    end.

schedule_unpause() ->
    WaitSecs = list_to_integer(config:get("smoosh", "wait_secs", "30")),
    erlang:send_after(WaitSecs * 1000, self(), unpause).

cleanup_index_files(DbName, _Shard) ->
    case config:get("smoosh", "cleanup_index_files", "false") of
    "true" ->
        fabric:cleanup_index_files(DbName);
    _ ->
        ok
    end.
