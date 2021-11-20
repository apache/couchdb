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
%
% This file is primarily based on error_logger_lager_h.erl from
% https://github.com/basho/lager which is available under the
% above marked ASFL v2 license.

-module(couch_log_error_logger_h).

-behaviour(gen_event).

-export([
    init/1,
    terminate/2,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    code_change/3
]).

init(_) ->
    {ok, undefined}.

terminate(_Reason, _St) ->
    ok.

handle_call(_, St) ->
    {ok, ignored, St}.

handle_event(Event, St) ->
    Entry = couch_log_formatter:format(Event),
    ok = couch_log_server:log(Entry),
    {ok, St}.

handle_info(_, St) ->
    {ok, St}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
