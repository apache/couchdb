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

-module(test_util).

-include_lib("couch/include/couch_eunit.hrl").
-include("couch_db.hrl").
-include("couch_db_int.hrl").
-include("couch_bt_engine.hrl").

-export([init_code_path/0]).
-export([source_file/1, build_file/1]).
%% -export([run/2]).

-export([start_couch/0, start_couch/1, start_couch/2, stop_couch/0, stop_couch/1]).
-export([start_config/1, stop_config/1]).
-export([start_applications/1, stop_applications/1]).

-export([stop_sync/1, stop_sync/2, stop_sync/3]).

-export([stop_sync_throw/2, stop_sync_throw/3, stop_sync_throw/4]).

-export([with_process_restart/1, with_process_restart/2, with_process_restart/3]).
-export([wait_process/1, wait_process/2]).
-export([wait/1, wait/2, wait/3]).
-export([wait_value/2, wait_other_value/2]).
-export([with_processes_restart/2, with_processes_restart/4]).
-export([with_couch_server_restart/1]).

-export([start/1, start/2, start/3, stop/1]).

-export([fake_db/1]).

-record(test_context, {mocked = [], started = [], module}).

-define(DEFAULT_APPS, [inets, ibrowse, ssl, config, couch_epi, couch_event, couch]).

srcdir() ->
    code:priv_dir(couch) ++ "/../../".

builddir() ->
    code:priv_dir(couch) ++ "/../../../".

init_code_path() ->
    Paths = [
        "couchdb",
        "jiffy",
        "ibrowse",
        "mochiweb",
        "snappy"
    ],
    lists:foreach(
        fun(Name) ->
            code:add_patha(filename:join([builddir(), "src", Name]))
        end,
        Paths
    ).

source_file(Name) ->
    filename:join([srcdir(), Name]).

build_file(Name) ->
    filename:join([builddir(), Name]).

start_couch() ->
    start_couch(?CONFIG_CHAIN, []).

start_couch(ExtraApps) ->
    start_couch(?CONFIG_CHAIN, ExtraApps).

start_couch(IniFiles, ExtraApps) ->
    load_applications_with_stats(),
    ok = application:set_env(config, ini_files, IniFiles),
    Apps = start_applications(?DEFAULT_APPS ++ ExtraApps),
    ok = config:delete("compactions", "_default", false),
    #test_context{started = Apps}.

stop_couch() ->
    ok = stop_applications(?DEFAULT_APPS).

stop_couch(#test_context{started = Apps}) ->
    stop_applications(Apps);
stop_couch(_) ->
    stop_couch().

with_couch_server_restart(Fun) ->
    Servers = couch_server:names(),
    test_util:with_processes_restart(Servers, Fun).

start_applications(Apps) ->
    StartOrder = calculate_start_order(Apps),
    start_applications(StartOrder, []).

start_applications([], Acc) ->
    lists:reverse(Acc);
start_applications([App | Apps], Acc) when App == kernel; App == stdlib ->
    start_applications(Apps, Acc);
start_applications([App | Apps], Acc) ->
    case application:start(App) of
        {error, {already_started, crypto}} ->
            start_applications(Apps, [crypto | Acc]);
        {error, {already_started, App}} ->
            io:format(standard_error, "Application ~s was left running!~n", [App]),
            application:stop(App),
            start_applications([App | Apps], Acc);
        {error, Reason} ->
            io:format(standard_error, "Cannot start application '~s', reason ~p~n", [App, Reason]),
            throw({error, {cannot_start, App, Reason}});
        ok ->
            start_applications(Apps, [App | Acc])
    end.

stop_applications(Apps) ->
    [application:stop(App) || App <- lists:reverse(Apps)],
    ok.

start_config(Chain) ->
    case config:start_link(Chain) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, OldPid}} ->
            ok = stop_config(OldPid),
            start_config(Chain)
    end.

stop_config(Pid) ->
    Timeout = 1000,
    case stop_sync(Pid, fun() -> config:stop() end, Timeout) of
        timeout ->
            throw({timeout_error, config_stop});
        _Else ->
            ok
    end.

stop_sync(Name) ->
    stop_sync(Name, shutdown).
stop_sync(Name, Reason) ->
    stop_sync(Name, Reason, 5000).

stop_sync(Name, Reason, Timeout) when is_atom(Name) ->
    stop_sync(whereis(Name), Reason, Timeout);
stop_sync(Pid, Reason, Timeout) when is_atom(Reason) and is_pid(Pid) ->
    stop_sync(Pid, fun() -> exit(Pid, Reason) end, Timeout);
stop_sync(Pid, Fun, Timeout) when is_function(Fun) and is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    try
        begin
            catch unlink(Pid),
            Res = (catch Fun()),
            receive
                {'DOWN', MRef, _, _, _} ->
                    Res
            after Timeout ->
                timeout
            end
        end
    after
        erlang:demonitor(MRef, [flush])
    end;
stop_sync(_, _, _) ->
    error(badarg).

stop_sync_throw(Name, Error) ->
    stop_sync_throw(Name, shutdown, Error).
stop_sync_throw(Name, Reason, Error) ->
    stop_sync_throw(Name, Reason, Error, 5000).

stop_sync_throw(Pid, Fun, Error, Timeout) ->
    case stop_sync(Pid, Fun, Timeout) of
        timeout ->
            throw(Error);
        Else ->
            Else
    end.

with_process_restart(Name) ->
    {Pid, true} = with_process_restart(
        Name, fun() -> exit(whereis(Name), shutdown) end
    ),
    Pid.

with_process_restart(Name, Fun) ->
    with_process_restart(Name, Fun, 5000).

with_process_restart(Name, Fun, Timeout) ->
    Res = stop_sync(Name, Fun),
    case wait_process(Name, Timeout) of
        timeout ->
            timeout;
        Pid ->
            {Pid, Res}
    end.

wait_process(Name) ->
    wait_process(Name, 5000).
wait_process(Name, Timeout) ->
    wait(
        fun() ->
            case whereis(Name) of
                undefined ->
                    wait;
                Pid ->
                    Pid
            end
        end,
        Timeout
    ).

wait(Fun) ->
    wait(Fun, 5000, 50).

wait(Fun, Timeout) ->
    wait(Fun, Timeout, 50).

wait(Fun, Timeout, Delay) ->
    Now = now_us(),
    wait(Fun, Timeout * 1000, Delay, Now, Now).

wait(_Fun, Timeout, _Delay, Started, Prev) when Prev - Started > Timeout ->
    timeout;
wait(Fun, Timeout, Delay, Started, _Prev) ->
    case Fun() of
        wait ->
            ok = timer:sleep(Delay),
            wait(Fun, Timeout, Delay, Started, now_us());
        Else ->
            Else
    end.

wait_value(Fun, Value) ->
    wait(fun() ->
        case Fun() of
            Value -> Value;
            _ -> wait
        end
    end).

wait_other_value(Fun, Value) ->
    wait(fun() ->
        case Fun() of
            Value -> wait;
            Other -> Other
        end
    end).

with_processes_restart(Processes, Fun) ->
    with_processes_restart(Processes, Fun, 5000, 50).

with_processes_restart(Names, Fun, Timeout, Delay) ->
    Processes = lists:foldl(
        fun(Name, Acc) ->
            [{Name, whereis(Name)} | Acc]
        end,
        [],
        Names
    ),
    [catch unlink(Pid) || {_, Pid} <- Processes],
    Res = (catch Fun()),
    {wait_start(Processes, Timeout, Delay), Res}.

wait_start(Processses, TimeoutInSec, Delay) ->
    Now = now_us(),
    wait_start(Processses, TimeoutInSec * 1000, Delay, Now, Now, #{}).

wait_start(_, Timeout, _Delay, Started, Prev, _) when Prev - Started > Timeout ->
    timeout;
wait_start([], _Timeout, _Delay, _Started, _Prev, Res) ->
    Res;
wait_start([{Name, Pid} | Rest] = Processes, Timeout, Delay, Started, _Prev, Res) ->
    case whereis(Name) of
        NewPid when is_pid(NewPid) andalso NewPid =/= Pid ->
            wait_start(Rest, Timeout, Delay, Started, now_us(), maps:put(Name, NewPid, Res));
        _ ->
            ok = timer:sleep(Delay),
            wait_start(Processes, Timeout, Delay, Started, now_us(), Res)
    end.

start(Module) ->
    start(Module, [], []).

start(Module, ExtraApps) ->
    start(Module, ExtraApps, []).

start(Module, ExtraApps, Options) ->
    Apps = start_applications([config, couch_log, ioq, couch_epi | ExtraApps]),
    ToMock = [config, couch_stats] -- proplists:get_value(dont_mock, Options, []),
    mock(ToMock),
    #test_context{module = Module, mocked = ToMock, started = Apps}.

stop(#test_context{mocked = Mocked, started = Apps}) ->
    meck:unload(Mocked),
    stop_applications(Apps).

fake_db(Fields0) ->
    {ok, Db, Fields} = maybe_set_engine(Fields0),
    Indexes = lists:zip(
        record_info(fields, db),
        lists:seq(2, record_info(size, db))
    ),
    lists:foldl(
        fun({FieldName, Value}, Acc) ->
            Idx = couch_util:get_value(FieldName, Indexes),
            setelement(Idx, Acc, Value)
        end,
        Db,
        Fields
    ).

maybe_set_engine(Fields0) ->
    case lists:member(engine, Fields0) of
        true ->
            {ok, #db{}, Fields0};
        false ->
            {ok, Header, Fields} = get_engine_header(Fields0),
            Db = #db{engine = {couch_bt_engine, #st{header = Header}}},
            {ok, Db, Fields}
    end.

get_engine_header(Fields) ->
    Keys = [
        disk_version,
        update_seq,
        unused,
        id_tree_state,
        seq_tree_state,
        local_tree_state,
        purge_seq,
        purged_docs,
        security_ptr,
        revs_limit,
        uuid,
        epochs,
        compacted_seq
    ],
    {HeadFields, RestFields} = lists:partition(
        fun({K, _}) -> lists:member(K, Keys) end, Fields
    ),
    Header0 = couch_bt_engine_header:new(),
    Header = couch_bt_engine_header:set(Header0, HeadFields),
    {ok, Header, RestFields}.

now_us() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

mock(Modules) when is_list(Modules) ->
    [mock(Module) || Module <- Modules];
mock(config) ->
    meck:new(config, [passthrough]),
    meck:expect(config, get, fun(_, _) -> undefined end),
    meck:expect(config, get, fun(_, _, Default) -> Default end),
    ok;
mock(couch_stats) ->
    meck:new(couch_stats, [passthrough]),
    meck:expect(couch_stats, increment_counter, fun(_) -> ok end),
    meck:expect(couch_stats, increment_counter, fun(_, _) -> ok end),
    meck:expect(couch_stats, decrement_counter, fun(_) -> ok end),
    meck:expect(couch_stats, decrement_counter, fun(_, _) -> ok end),
    meck:expect(couch_stats, update_histogram, fun(_, _) -> ok end),
    meck:expect(couch_stats, update_gauge, fun(_, _) -> ok end),
    ok.

load_applications_with_stats() ->
    Wildcard = filename:join([?BUILDDIR(), "src/*/priv/stats_descriptions.cfg"]),
    [application:load(stats_file_to_app(File)) || File <- filelib:wildcard(Wildcard)],
    ok.

stats_file_to_app(File) ->
    [_Desc, _Priv, App | _] = lists:reverse(filename:split(File)),
    erlang:list_to_atom(App).

calculate_start_order(Apps) ->
    AllApps = calculate_start_order(sort_apps(Apps), []),
    % AllApps may not be the same list as Apps if we
    % loaded any dependencies. We recurse here when
    % that changes so that our sort_apps function has
    % a global view of all applications to start.
    case lists:usort(AllApps) == lists:usort(Apps) of
        true -> AllApps;
        false -> calculate_start_order(AllApps)
    end.

calculate_start_order([], StartOrder) ->
    lists:reverse(StartOrder);
calculate_start_order([App | RestApps], StartOrder) ->
    NewStartOrder = load_app_deps(App, StartOrder),
    calculate_start_order(RestApps, NewStartOrder).

load_app_deps(App, StartOrder) ->
    case lists:member(App, StartOrder) of
        true ->
            StartOrder;
        false ->
            case application:load(App) of
                ok -> ok;
                {error, {already_loaded, App}} -> ok
            end,
            {ok, Apps} = application:get_key(App, applications),
            Deps =
                case App of
                    kernel -> Apps;
                    stdlib -> Apps;
                    _ -> lists:usort([kernel, stdlib | Apps])
                end,
            NewStartOrder = lists:foldl(
                fun(Dep, Acc) ->
                    load_app_deps(Dep, Acc)
                end,
                StartOrder,
                Deps
            ),
            [App | NewStartOrder]
    end.

sort_apps(Apps) ->
    Weighted = [weight_app(App) || App <- Apps],
    element(2, lists:unzip(lists:sort(Weighted))).

weight_app(couch_log) -> {0.0, couch_log};
weight_app(Else) -> {1.0, Else}.
