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

-module(couch_log).

-export([debug/2, info/2, notice/2, warning/2, error/2, critical/2, alert/2, emergency/2]).
-export([set_level/1]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{debug, 2}, {info, 2}, {notice, 2}, {warning, 2},
    {error, 2}, {critical, 2}, {alert, 2}, {set_level, 1}];
behaviour_info(_) ->
    undefined.

debug(Fmt, Args) ->
    {ok, Backend} = get_backend(),
    catch couch_stats:increment_counter([couch_log, level, debug]),
    Backend:debug(Fmt, Args).

info(Fmt, Args) ->
    {ok, Backend} = get_backend(),
    catch couch_stats:increment_counter([couch_log, level, info]),
    Backend:info(Fmt, Args).

notice(Fmt, Args) ->
    {ok, Backend} = get_backend(),
    catch couch_stats:increment_counter([couch_log, level, notice]),
    Backend:notice(Fmt, Args).

warning(Fmt, Args) ->
    {ok, Backend} = get_backend(),
    catch couch_stats:increment_counter([couch_log, level, warning]),
    Backend:warning(Fmt, Args).

error(Fmt, Args) ->
    {ok, Backend} = get_backend(),
    catch couch_stats:increment_counter([couch_log, level, 'error']),
    Backend:error(Fmt, Args).

critical(Fmt, Args) ->
    {ok, Backend} = get_backend(),
    catch couch_stats:increment_counter([couch_log, level, critical]),
    Backend:critical(Fmt, Args).

alert(Fmt, Args) ->
    {ok, Backend} = get_backend(),
    catch couch_stats:increment_counter([couch_log, level, alert]),
    Backend:alert(Fmt, Args).

emergency(Fmt, Args) ->
    {ok, Backend} = get_backend(),
    catch couch_stats:increment_counter([couch_log, level, emergency]),
    Backend:emergency(Fmt, Args).

set_level(Level) ->
    {ok, Backend} = application:get_env(?MODULE, backend),
    Backend:set_level(Level).

get_backend() ->
    case application:get_env(?MODULE, backend) of
        undefined ->
            ok = application:load(?MODULE),
            get_backend();
        {ok, Backend} ->
            {ok, Backend}
    end.
