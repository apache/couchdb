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

-export([start/1, start/2, start/3, stop/1]).

-export([fake_db/1]).

-record(test_context, {mocked = [], started = [], module}).

-define(DEFAULT_APPS,
        [inets, ibrowse, ssl, config, couch_epi, couch_event, couch]).

srcdir() ->
    code:priv_dir(couch) ++ "/../../".

builddir() ->
    code:priv_dir(couch) ++ "/../../../".

init_code_path() ->
    Paths = [
        "couchdb",
        "jiffy",
        "oauth",
        "ibrowse",
        "mochiweb",
        "snappy"
    ],
    lists:foreach(fun(Name) ->
        code:add_patha(filename:join([builddir(), "src", Name]))
    end, Paths).

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
    #test_context{started = Apps}.

stop_couch() ->
    ok = stop_applications(?DEFAULT_APPS).

stop_couch(#test_context{started = Apps}) ->
    stop_applications(Apps);
stop_couch(_) ->
    stop_couch().

start_applications(Apps) ->
    start_applications(Apps, []).

start_applications([], Acc) ->
    lists:reverse(Acc);
start_applications([App|Apps], Acc) ->
    case application:start(App) of
    {error, {already_started, _}} ->
        start_applications(Apps, Acc);
    {error, {not_started, Dep}} ->
        start_applications([Dep, App | Apps], Acc);
    {error, {not_running, Dep}} ->
        start_applications([Dep, App | Apps], Acc);
    ok ->
        start_applications(Apps, [App|Acc])
    end.

stop_applications(Apps) ->
    [application:stop(App) || App <- lists:reverse(Apps)],
    ok.

start_config(Chain) ->
    case config:start_link(Chain) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, OldPid}}  ->
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
stop_sync(_, _, _) -> error(badarg).

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
        Name, fun() -> exit(whereis(Name), shutdown) end),
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
    wait(fun() ->
       case whereis(Name) of
       undefined ->
          wait;
       Pid ->
          Pid
       end
    end, Timeout).

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

fake_db(Fields) ->
    Indexes = lists:zip(
            record_info(fields, db),
            lists:seq(2, record_info(size, db))
        ),
    lists:foldl(fun({FieldName, Value}, Acc) ->
        Idx = couch_util:get_value(FieldName, Indexes),
        setelement(Idx, Acc, Value)
    end, #db{}, Fields).

now_us() ->
    {MegaSecs, Secs, MicroSecs} = now(),
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
    [_Desc, _Priv, App|_] = lists:reverse(filename:split(File)),
    erlang:list_to_atom(App).
