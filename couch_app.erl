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

start(_Type, DefaultIniFiles) ->
    IniFiles = get_ini_files(DefaultIniFiles),
    case start_apps([crypto, public_key, sasl, inets, oauth, ssl, ibrowse, mochiweb, os_mon]) of
    ok ->
        couch_server_sup:start_link(IniFiles);
    {error, Reason} ->
        {error, Reason}
    end.

stop(_) ->
    ok.

get_ini_files(Default) ->
    case init:get_argument(couch_ini) of
    error ->
        Default;
    {ok, [[]]} ->
        Default;
    {ok, [Values]} ->
        Values
    end.

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
