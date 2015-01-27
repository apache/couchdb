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

-export([init_code_path/0]).
-export([source_file/1, build_file/1]).
%% -export([run/2]).

-export([start_couch/0, start_couch/1, start_couch/2, stop_couch/0, stop_couch/1]).
-export([start_config/1, stop_config/1]).
-export([start_applications/1]).

-export([stop_sync/1, stop_sync/2, stop_sync/3]).


srcdir() ->
    code:priv_dir(couch) ++ "/../../".

builddir() ->
    code:priv_dir(couch) ++ "/../../../".

init_code_path() ->
    Paths = [
        "etap",
        "couchdb",
        "ejson",
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
    ok = application:set_env(config, ini_files, IniFiles),
    ok = lager:start(),
    Ctx = start_applications([inets, ibrowse, ssl, config, couch] ++ ExtraApps),
    couch_stats:reload(),
    Ctx.

stop_couch() ->
    ok = application:stop(couch),
    ok = application:stop(lager),
    ok = application:stop(goldrush),
    ok = application:stop(config),
    ok = application:stop(ssl),
    ok = application:stop(ibrowse),
    ok = application:stop(inets),
    ok.


stop_couch(_) ->
    stop_couch().

start_applications(Apps) ->
    start_applications(Apps, []).

start_applications([], Acc) ->
    lists:reverse(Acc);
start_applications([App|Apps], Acc) ->
    NewAcc =
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
