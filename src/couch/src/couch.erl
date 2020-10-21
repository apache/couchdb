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

-module(couch).

-export([
    start/0,
    stop/0,
    restart/0
]).


deps() ->
    [
        sasl,
        inets,
        crypto,
        public_key,
        ssl,
        ibrowse,
        mochiweb,
        config,
        couch_log
    ].


start() ->
    catch erlang:system_flag(scheduler_bind_type, default_bind),
    case start_apps(deps()) of
        ok ->
            ok = application:start(couch);
        Else ->
            throw(Else)
    end.


stop() ->
    application:stop(couch).


restart() ->
    init:restart().


start_apps([]) ->
    ok;
start_apps([App|Rest]) ->
    case application:start(App) of
    ok ->
       start_apps(Rest);
    {error, {already_started, App}} ->
       start_apps(Rest);
    {error, _Reason} when App =:= public_key ->
       % ignore on R12B5
       start_apps(Rest);
    {error, _Reason} ->
       {error, {app_would_not_start, App}}
    end.
