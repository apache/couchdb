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

-module(ioq).
-behaviour(gen_server).
-behaviour(config_listener).

-export([start_link/0, call/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

% config_listener api
-export([handle_config_change/5]).

-record(state, {
    concurrency=10,
    ratio,
    interactive=queue:new(),
    compaction=queue:new(),
    running=[]
}).

-record(request, {
    fd,
    msg,
    priority,
    from,
    ref
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call(Fd, Msg, Priority) ->
    Request = #request{fd=Fd, msg=Msg, priority=Priority, from=self()},
    try
        gen_server:call(?MODULE, Request, infinity)
    catch
        exit:{noproc,_} ->
            gen_server:call(Fd, Msg, infinity)
    end.

init(_) ->
    ok = config:listen_for_changes(?MODULE, nil),
    State = #state{},
    {ok, read_config(State)}.

read_config(State) ->
    Ratio = list_to_float(config:get("ioq", "ratio", "0.01")),
    State#state{ratio=Ratio}.

handle_call(#request{}=Request, From, State) ->
    {noreply, enqueue_request(Request#request{from=From}, State), 0}.

handle_cast(change, State) ->
    {noreply, read_config(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Ref, Reply}, State) ->
    case lists:keytake(Ref, #request.ref, State#state.running) of
        {value, Request, Remaining} ->
            erlang:demonitor(Ref, [flush]),
            gen_server:reply(Request#request.from, Reply),
            {noreply, State#state{running=Remaining}, 0};
        false ->
            {noreply, State, 0}
    end;

handle_info({'DOWN', Ref, _, _, Reason}, State) ->
    case lists:keytake(Ref, #request.ref, State#state.running) of
        {value, Request, Remaining} ->
            gen_server:reply(Request#request.from, {'EXIT', Reason}),
            {noreply, State#state{running=Remaining}, 0};
        false ->
            {noreply, State, 0}
    end;

handle_info(timeout, State) ->
    {noreply, maybe_submit_request(State)}.

handle_config_change("ioq", _, _, _, _) ->
    {ok, gen_server:cast(?MODULE, change)};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

enqueue_request(#request{priority={db_compact, _}}=Request, #state{}=State) ->
    State#state{compaction=queue:in(Request, State#state.compaction)};
enqueue_request(#request{priority={view_compact, _, _}}=Request, #state{}=State) ->
    State#state{compaction=queue:in(Request, State#state.compaction)};
enqueue_request(#request{}=Request, #state{}=State) ->
    State#state{interactive=queue:in(Request, State#state.interactive)}.

maybe_submit_request(#state{concurrency=Concurrency, running=Running}=State)
  when length(Running) < Concurrency ->
    case make_next_request(State) of
        State ->
            State;
        NewState when length(Running) >= Concurrency - 1 ->
            NewState;
        NewState ->
            maybe_submit_request(NewState)
    end;
maybe_submit_request(State) ->
    State.

make_next_request(#state{}=State) ->
    case {queue:is_empty(State#state.compaction), queue:is_empty(State#state.interactive)} of
        {true, true} ->
            State;
        {true, false} ->
            choose_next_request(#state.interactive, State);
        {false, true} ->
            choose_next_request(#state.compaction, State);
        {false, false} ->
            case random:uniform() < State#state.ratio of
                true ->
                    choose_next_request(#state.compaction, State);
                false ->
                    choose_next_request(#state.interactive, State)
            end
    end.

choose_next_request(Index, State) ->
    case queue:out(element(Index, State)) of
        {empty, _} ->
            State;
        {{value, Request}, Q} ->
            submit_request(Request, setelement(Index, State, Q))
    end.

submit_request(#request{}=Request, #state{}=State) ->
    Ref = erlang:monitor(process, Request#request.fd),
    Request#request.fd ! {'$gen_call', {self(), Ref}, Request#request.msg},
    State#state{running = [Request#request{ref=Ref} | State#state.running]}.
