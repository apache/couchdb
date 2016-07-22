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

-module(couch_log_monitor).

-behaviour(gen_server).
-vsn(1).


-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-define(HANDLER_MOD, couch_log_error_logger_h).


start_link() ->
    gen_server:start_link(?MODULE, [], []).


init(_) ->
    ok = gen_event:add_sup_handler(error_logger, ?HANDLER_MOD, []),
    {ok, nil}.


terminate(_, _) ->
    ok.


handle_call(_Msg, _From, St) ->
    {reply, ignored, St}.


handle_cast(_Msg, St) ->
    {noreply, St}.


handle_info({gen_event_EXIT, ?HANDLER_MOD, Reason}, St) ->
    {stop, Reason, St};


handle_info(_Msg, St) ->
    {noreply, St}.


code_change(_, State, _) ->
    {ok, State}.
