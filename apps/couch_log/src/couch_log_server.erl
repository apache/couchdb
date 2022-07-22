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

-module(couch_log_server).
-behavior(gen_server).

-export([
    start_link/0,
    reconfigure/0,
    log/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-include("couch_log.hrl").

-record(st, {
    writer
}).

-ifdef(TEST).
-define(SEND(Entry), gen_server:call(?MODULE, {log, Entry})).
-else.
-define(SEND(Entry), gen_server:cast(?MODULE, {log, Entry})).
-endif.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

reconfigure() ->
    gen_server:call(?MODULE, reconfigure).

log(Entry) ->
    ?SEND(Entry).

init(_) ->
    couch_util:set_mqd_off_heap(?MODULE),
    process_flag(trap_exit, true),
    {ok, #st{
        writer = couch_log_writer:init()
    }}.

terminate(Reason, St) ->
    ok = couch_log_writer:terminate(Reason, St#st.writer).

handle_call(reconfigure, _From, St) ->
    ok = couch_log_writer:terminate(reconfiguring, St#st.writer),
    {reply, ok, St#st{
        writer = couch_log_writer:init()
    }};
handle_call({log, Entry}, _From, St) ->
    % We re-check if we should log here in case an operator
    % adjusted the log level and then realized it was a bad
    % idea because it filled our message queue.
    case couch_log_util:should_log(Entry) of
        true ->
            NewWriter = couch_log_writer:write(Entry, St#st.writer),
            {reply, ok, St#st{writer = NewWriter}};
        false ->
            {reply, ok, St}
    end;
handle_call(Ignore, From, St) ->
    Args = [?MODULE, Ignore],
    Entry = couch_log_formatter:format(error, ?MODULE, "~s ignored ~p", Args),
    handle_call({log, Entry}, From, St).

handle_cast(Msg, St) ->
    {reply, ok, NewSt} = handle_call(Msg, nil, St),
    {noreply, NewSt}.

handle_info(Msg, St) ->
    {reply, ok, NewSt} = handle_call(Msg, nil, St),
    {noreply, NewSt}.

code_change(_Vsn, St, _Extra) ->
    {ok, St}.
