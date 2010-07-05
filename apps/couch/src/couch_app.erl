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

-module(couch_app).

-behaviour(application).

-include("couch_db.hrl").

-export([start/2, stop/1]).

start(_Type, _Args) ->
    catch erlang:system_flag(scheduler_bind_type, default_bind),
    IniFiles = get_ini_files(),
    couch_server_sup:start_link(IniFiles).

stop(_) ->
    ok.

get_ini_files() ->
    Etc = filename:join(code:root_dir(), "etc"),
    Default = [filename:join(Etc,"default.ini"), filename:join(Etc,"local.ini")],
    case init:get_argument(couch_ini) of
    error ->
        Default;
    {ok, [[]]} ->
        Default;
    {ok, [Values]} ->
        Values
    end.
