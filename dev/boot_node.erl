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

-module(boot_node).

-export([start/0]).


start() ->
    monitor_parent(),
    Apps = load_apps(),
    Deps = load_deps(Apps),
    start_all_apps(Deps).


monitor_parent() ->
    {ok, [[PPid]]} = init:get_argument(parent_pid),
    spawn(fun() -> monitor_parent(PPid) end).


monitor_parent(PPid) ->
    timer:sleep(1000),
    case os:cmd("kill -0 " ++ PPid) of
        "" ->
            monitor_parent(PPid);
        _Else ->
            % Assume _Else is a no such process error
            init:stop()
    end.


load_apps() ->
    {ok, [[Config]]} = init:get_argument(reltool_config),
    {ok, Terms} = file:consult(Config),
    load_apps(Terms).


load_apps([]) ->
    erlang:error(failed_to_load_apps);
load_apps([{sys, Terms} | _]) ->
    load_apps(Terms);
load_apps([{rel, "couchdb", _Vsn, Apps} | _]) ->
    Apps;
load_apps([_ | Rest]) ->
    load_apps(Rest).


load_deps(Apps) ->
    load_deps(Apps, dict:new()).


load_deps([], Deps) ->
    Deps;
load_deps([App | Rest], Deps) ->
    load_app(App),
    case application:get_key(App, applications) of
        {ok, AppDeps0} ->
            NewDeps = dict:store(App, AppDeps0, Deps),
            Filter = fun(A) -> not dict:is_key(A, Deps) end,
            AppDeps = lists:filter(Filter, AppDeps0),
            load_deps(AppDeps ++ Rest, NewDeps);
        _ ->
            NewDeps = dict:store(App, [], Deps),
            load_deps(Rest, NewDeps)
    end.


load_app(App) ->
    case application:load(App) of
        ok ->
            case application:get_key(App, modules) of
                {ok, Modules} ->
                    lists:foreach(fun(Mod) ->
                        case load_app_module(Mod) of
                            ok -> ok;
                            E -> io:format("~p = load_app_module(~p)~n", [E, Mod])
                        end
                    end, Modules);
                undefined ->
                    ok
            end;
        {error, {already_loaded, App}} ->
            ok;
        Error ->
            Error
    end.


load_app_module(Mod) ->
    case code:is_loaded(Mod) of
        {file, _} ->
            ok;
        _ ->
            case code:load_file(Mod) of
                {module, Mod} ->
                    ok;
                Error ->
                    Error
            end
    end.


start_all_apps(Deps) ->
    lists:foldl(fun(App, Started) ->
        start_app(App, Deps, Started)
    end, [], dict:fetch_keys(Deps)).


start_app(App, Deps, Started) ->
    case lists:member(App, Started) of
        true ->
            Started;
        false ->
            AppDeps = dict:fetch(App, Deps),
            NowStarted = lists:foldl(fun(Dep, Acc) ->
                start_app(Dep, Deps, Acc)
            end, Started, AppDeps),
            case application:start(App) of
                ok ->
                    [App | NowStarted];
                {error, {already_started,App}} ->
                    % Kernel causes this
                    [App | NowStarted];
                Else ->
                    erlang:error(Else)
            end
    end.
