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

debug(Fmt, Args) ->
    catch couch_stats:increment_counter([couch_log, level, debug]),
    lager:debug(Fmt, Args).

info(Fmt, Args) ->
    catch couch_stats:increment_counter([couch_log, level, info]),
    lager:info(Fmt, Args).

notice(Fmt, Args) ->
    catch couch_stats:increment_counter([couch_log, level, notice]),
    lager:notice(Fmt, Args).

warning(Fmt, Args) ->
    catch couch_stats:increment_counter([couch_log, level, warning]),
    lager:warning(Fmt, Args).

error(Fmt, Args) ->
    catch couch_stats:increment_counter([couch_log, level, 'error']),
    lager:error(Fmt, Args).

critical(Fmt, Args) ->
    catch couch_stats:increment_counter([couch_log, level, critical]),
    lager:critical(Fmt, Args).

alert(Fmt, Args) ->
    catch couch_stats:increment_counter([couch_log, level, alert]),
    lager:alert(Fmt, Args).

emergency(Fmt, Args) ->
    catch couch_stats:increment_counter([couch_log, level, emergency]),
    lager:emergency(Fmt, Args).
