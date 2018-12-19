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

-module(couch_mrview_update_notifier).

-behaviour(gen_event).

-export([start_link/1, notify/1]).
-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2, code_change/3, stop/1]).

-include_lib("couch/include/couch_db.hrl").

start_link(Exec) ->
    couch_event_sup:start_link(couch_mrview_update, {couch_mrview_update_notifier, make_ref()}, Exec).

notify(Event) ->
    gen_event:notify(couch_mrview_update, Event).

stop(Pid) ->
    couch_event_sup:stop(Pid).

init(Fun) ->
    {ok, Fun}.

terminate(_Reason, _State) ->
    ok.

handle_event(Event, Fun) ->
    Fun(Event),
    {ok, Fun}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info({'EXIT', Pid, Reason}, Pid) ->
    couch_log:error("View update notification process ~p died: ~p", [Pid, Reason]),
    remove_handler.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
