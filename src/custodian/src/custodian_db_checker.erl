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

-module(custodian_db_checker).
-behaviour(gen_server).
-vsn(1).

-export([start_link/0]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
    check_dbs/0
]).

-record(st, {
    checker
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    {ok, restart_checker(#st{})}.

terminate(_Reason, St) ->
    couch_util:shutdown_sync(St#st.checker),
    ok.

handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.

handle_cast(refresh, St) ->
    {noreply, restart_checker(St)};
handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.

handle_info({nodeup, _}, St) ->
    {noreply, restart_checker(St)};
handle_info({nodedown, _}, St) ->
    {noreply, restart_checker(St)};
handle_info({'EXIT', Pid, normal}, #st{checker = Pid} = St) ->
    {noreply, St#st{checker = undefined}};
handle_info({'EXIT', Pid, Reason}, #st{checker = Pid} = St) ->
    couch_log:notice("custodian db checker died ~p", [Reason]),
    {noreply, restart_checker(St#st{checker = undefined})};
handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

restart_checker(#st{checker = undefined} = St) ->
    Pid = spawn_link(fun ?MODULE:check_dbs/0),
    St#st{checker = Pid};
restart_checker(#st{checker = Pid} = St) when is_pid(Pid) ->
    St.

check_dbs() ->
    {ok, DbsDb} = custodian_util:ensure_dbs_exists(),
    try
        Missing = lists:foldl(
            fun(DbName, Count) ->
                case check_db(DbsDb, DbName) of
                    ok -> Count;
                    missing -> Count + 1
                end
            end,
            0,
            get_dbs()
        ),
        case Missing == 0 of
            true -> clear_missing_dbs_alert();
            false -> ok
        end
    after
        couch_db:close(DbsDb)
    end.

check_db(DbsDb, DbName) when is_binary(DbName) ->
    try
        case couch_db:open_doc(DbsDb, DbName, []) of
            {ok, _} ->
                ok;
            _ ->
                send_missing_db_alert(DbName),
                missing
        end
    catch
        _:_ ->
            send_missing_db_alert(DbName),
            missing
    end.

get_dbs() ->
    lists:flatten([
        get_users_db(),
        get_stats_db()
    ]).

get_users_db() ->
    UsersDb = chttpd_auth_cache:dbname(),
    [list_to_binary(UsersDb)].

get_stats_db() ->
    case application:get_env(ioq, stats_db) of
        {ok, DbName} when is_binary(DbName) ->
            [DbName];
        {ok, DbName} when is_list(DbName) ->
            [iolist_to_binary(DbName)];
        _ ->
            []
    end.

send_missing_db_alert(DbName) ->
    couch_log:notice("Missing system database ~s", [DbName]),
    ?CUSTODIAN_MONITOR:send_missing_db_alert(DbName).

clear_missing_dbs_alert() ->
    couch_log:notice("All system databases exist.", []),
    ?CUSTODIAN_MONITOR:clear_missing_dbs_alert().
