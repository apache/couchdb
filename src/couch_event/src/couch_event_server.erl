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

-module(couch_event_server).
-behaviour(gen_server).

-export([
    start_link/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(st, {
    by_pid,
    by_dbname
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).

init(_) ->
    erlang:process_flag(message_queue_data, off_heap),
    {ok, #st{
        by_pid = #{},
        by_dbname = #{}
    }}.

handle_call({register, Pid, NewDbNames}, _From, St) ->
    case maps:get(Pid, St#st.by_pid, undefined) of
        undefined ->
            NewRef = monitor(process, Pid),
            {reply, ok, register(St, NewRef, Pid, NewDbNames)};
        {ReuseRef, OldDbNames} ->
            unregister(St, Pid, OldDbNames),
            {reply, ok, register(St, ReuseRef, Pid, NewDbNames)}
    end;
handle_call({unregister, Pid}, _From, #st{by_pid = ByPid} = St) ->
    case maps:get(Pid, ByPid, undefined) of
        undefined ->
            {reply, not_registered, St};
        {Ref, OldDbNames} ->
            demonitor(Ref, [flush]),
            {reply, ok, unregister(St, Pid, OldDbNames)}
    end;
handle_call(Msg, From, St) ->
    couch_log:notice("~s ignoring call ~w from ~w", [?MODULE, Msg, From]),
    {reply, ignored, St}.

handle_cast({notify, DbName, Event}, St) ->
    notify_listeners(St#st.by_dbname, DbName, Event),
    {noreply, St};
handle_cast(Msg, St) ->
    couch_log:notice("~s ignoring cast ~w", [?MODULE, Msg]),
    {noreply, St}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, #st{by_pid = ByPid} = St) ->
    #{Pid := {Ref, OldDbNames}} = ByPid,
    {noreply, unregister(St, Pid, OldDbNames)};
handle_info(Msg, St) ->
    couch_log:notice("~s ignoring info ~w", [?MODULE, Msg]),
    {noreply, St}.

notify_listeners(#{} = ByDbName, DbName, Event) ->
    Msg = {'$couch_event', DbName, Event},
    notify_listeners(maps:get(all_dbs, ByDbName, undefined), Msg),
    notify_listeners(maps:get(DbName, ByDbName, undefined), Msg).

notify_listeners(undefined, _) ->
    ok;
notify_listeners(#{} = Listeners, Msg) ->
    maps:foreach(fun(Pid, _) -> Pid ! Msg end, Listeners).

register(#st{by_pid = ByPid, by_dbname = ByDbName} = St, Ref, Pid, DbNames) ->
    FoldFun = fun(DbName, Acc) -> add_listener(Acc, DbName, Pid) end,
    ByDbName1 = lists:foldl(FoldFun, ByDbName, DbNames),
    St#st{by_pid = ByPid#{Pid => {Ref, DbNames}}, by_dbname = ByDbName1}.

add_listener(#{} = ByDbName, DbName, Pid) ->
    case maps:get(DbName, ByDbName, not_found) of
        #{} = Listeners -> ByDbName#{DbName => Listeners#{Pid => nil}};
        not_found -> ByDbName#{DbName => #{Pid => nil}}
    end.

unregister(#st{by_pid = ByPid, by_dbname = ByDbName} = St, Pid, OldDbNames) ->
    FoldFun = fun(DbName, Acc) -> rem_listener(Acc, DbName, Pid) end,
    ByDbName1 = lists:foldl(FoldFun, ByDbName, OldDbNames),
    St#st{by_pid = maps:remove(Pid, ByPid), by_dbname = ByDbName1}.

rem_listener(#{} = ByDbName, DbName, Pid) ->
    #{DbName := Listeners} = ByDbName,
    Listeners1 = maps:remove(Pid, Listeners),
    case map_size(Listeners1) of
        0 -> maps:remove(DbName, ByDbName);
        _ -> ByDbName#{DbName := Listeners1}
    end.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_event_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_register_proc_basic),
            ?TDEF_FE(t_unregister_proc_basic),
            ?TDEF_FE(t_unregister_on_death),
            ?TDEF_FE(t_notify_basic),
            ?TDEF_FE(t_notify_all_dbs),
            ?TDEF_FE(t_register_multiple),
            ?TDEF_FE(t_reregister),
            ?TDEF_FE(t_invalid_gen_server_messages)
        ]
    }.

setup() ->
    meck:expect(couch_log, notice, fun(_, _) -> ok end),
    {ok, SPid} = start_link(),
    Listeners = [start_listener(), start_listener(), start_listener()],
    {SPid, Listeners}.

teardown({SPid, Listeners}) ->
    [kill_sync(Pid) || Pid <- [SPid | Listeners]],
    meck:unload(),
    ok.

t_register_proc_basic({_, [LPid | _]}) ->
    ?assertEqual(ok, reg(LPid, [db])),
    #st{by_pid = ByPid, by_dbname = ByName} = state(),
    ?assertMatch(#{LPid := {_, [db]}}, ByPid),
    ?assertMatch(#{db := #{LPid := nil}}, ByName).

t_unregister_proc_basic({_, [LPid | _]}) ->
    ?assertEqual(ok, reg(LPid, [db])),
    #st{by_pid = ByPid, by_dbname = ByName} = state(),
    ?assertMatch(#{LPid := {_, [db]}}, ByPid),
    ?assertMatch(#{db := #{LPid := nil}}, ByName),
    ?assertEqual(not_registered, unreg(self())),
    ?assertEqual(ok, unreg(LPid)),
    #st{by_pid = #{}, by_dbname = #{}} = state().

t_unregister_on_death({_, [LPid | _]}) ->
    ?assertEqual(ok, reg(LPid, [db])),
    #st{by_pid = ByPid, by_dbname = ByName} = state(),
    ?assertMatch(#{LPid := {_, [db]}}, ByPid),
    ?assertMatch(#{db := #{LPid := nil}}, ByName),
    kill_sync(LPid),
    test_util:wait(fun() ->
        #st{by_pid = Pids} = state(),
        case map_size(Pids) of
            0 -> ok;
            N when is_integer(N), N > 1 -> wait
        end
    end),
    ?assertEqual(#st{by_pid = #{}, by_dbname = #{}}, state()).

t_notify_basic({_, [LPid | _]}) ->
    reg(LPid, [db]),
    notify(other_db, foo),
    notify(db, bar),
    ?assertEqual([{db, bar}], wait(LPid, 1)).

t_notify_all_dbs({_, [LPid | _]}) ->
    ok = reg(LPid, [all_dbs]),
    notify(db, bar),
    ?assertEqual([{db, bar}], wait(LPid, 1)).

t_register_multiple({_, [L1, L2, L3 | _]}) ->
    reg(L1, [dbx, dby]),
    reg(L2, [dby, all_dbs]),
    reg(L3, [dbx, all_dbs]),
    % all_dbs broadcast works
    notify(dbw, e1),
    ?assertEqual([{dbw, e1}], wait(L2, 1)),
    ?assertEqual([{dbw, e1}], wait(L3, 1)),
    % dbx updated, only dbx and all_dbs are notified
    notify(dbx, e2),
    ?assertEqual([{dbx, e2}], wait(L1, 1)),
    ?assertEqual([{dbx, e2}], wait(L2, 1)),
    % L3 gets 2 notifications once for all_dbs and dbx
    ?assertEqual([{dbx, e2}, {dbx, e2}], wait(L3, 2)),
    unreg(L1),
    unreg(L2),
    unreg(L3),
    #st{by_pid = #{}, by_dbname = #{}} = state().

t_reregister({_, [LPid | _]}) ->
    ?assertEqual(ok, reg(LPid, [dbx, dby])),
    ?assertEqual(ok, reg(LPid, [dbz, dbw])),
    #st{by_pid = ByPid, by_dbname = ByName} = state(),
    ?assertEqual(1, map_size(ByPid)),
    ?assertMatch(#{LPid := {_, [dbz, dbw]}}, ByPid),
    ?assertMatch(
        #{
            dbz := #{LPid := nil},
            dbw := #{LPid := nil}
        },
        ByName
    ).

t_invalid_gen_server_messages(_) ->
    meck:reset(couch_log),
    whereis(?MODULE) ! random_msg,
    gen_server:cast(?MODULE, bad_cast),
    ?assertEqual(ignored, gen_server:call(?MODULE, bad_call)),
    ?assertEqual(3, meck:num_calls(couch_log, notice, 2)).

-record(lst, {
    events = [],
    wait_cnt = 0,
    wait_pid = undefined
}).

kill_sync(Pid) ->
    unlink(Pid),
    Ref = monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    end.

reg(Pid, Dbs) ->
    gen_server:call(?MODULE, {register, Pid, Dbs}).

unreg(Pid) ->
    gen_server:call(?MODULE, {unregister, Pid}).

notify(Db, Event) ->
    gen_server:cast(?MODULE, {notify, Db, Event}).

state() ->
    sys:get_state(?MODULE).

% Listeners receive and save their notification events. {get_events, MinNum,
% self()} call will block until the listener had received at least MinNum
% events. We use this since event notification is asynchronous.

start_listener() ->
    spawn(fun() -> loop(#lst{}) end).

loop(#lst{} = St0) ->
    St = #lst{events = Events, wait_pid = WaitPid} = respond(St0),
    receive
        {'$couch_event', Db, Ev} ->
            loop(St#lst{events = [{Db, Ev} | Events]});
        {get_events, _, WPid} when is_pid(WaitPid) ->
            WPid ! {error, existing_waiter};
        {get_events, WCnt, WPid} ->
            loop(St#lst{wait_cnt = WCnt, wait_pid = WPid})
    end.

respond(#lst{events = Events, wait_cnt = Cnt, wait_pid = Pid} = St) ->
    case {length(Events) >= Cnt, is_pid(Pid)} of
        {true, true} ->
            Pid ! Events,
            St#lst{events = [], wait_cnt = 0, wait_pid = undefined};
        {_, _} ->
            St
    end.

wait(LPid, N) ->
    LPid ! {get_events, N, self()},
    receive
        {error, Error} -> {error, Error};
        Events when is_list(Events) -> Events
    end.

-endif.
