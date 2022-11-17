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

-export([start_link/0, call/3, call_search/3]).
-export([get_queue_lengths/0]).
-export([get_io_priority/0, set_io_priority/1, maybe_set_io_priority/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

-define(RELISTEN_DELAY, 5000).

-record(state, {
    concurrency,
    ratio,
    interactive = queue:new(),
    background = queue:new(),
    running = []
}).

-record(request, {
    fd,
    msg,
    priority,
    from,
    ref
}).

set_io_priority(Priority) ->
    erlang:put(io_priority, Priority).

get_io_priority() ->
    erlang:get(io_priority).

maybe_set_io_priority(Priority) ->
    case get_io_priority() of
        undefined -> set_io_priority(Priority);
        _ -> ok
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call_search(Fd, Msg, Metadata) ->
    call(Fd, Msg, Metadata).

call(Fd, Msg, Metadata) ->
    Priority = io_class(Msg, Metadata),
    case bypass(Priority) of
        true ->
            gen_server:call(Fd, Msg, infinity);
        false ->
            queued_call(Fd, Msg, Priority)
    end.

get_queue_lengths() ->
    gen_server:call(?MODULE, get_queue_lengths).

bypass(Priority) ->
    case Priority of
        os_process -> config:get_boolean("ioq.bypass", "os_process", true);
        read -> config:get_boolean("ioq.bypass", "read", true);
        write -> config:get_boolean("ioq.bypass", "write", true);
        view_update -> config:get_boolean("ioq.bypass", "view_update", true);
        reshard -> config:get_boolean("ioq.bypass", "reshard", false);
        shard_sync -> config:get_boolean("ioq.bypass", "shard_sync", false);
        compaction -> config:get_boolean("ioq.bypass", "compaction", false);
        _ -> config:get("ioq.bypass", atom_to_list(Priority)) =:= "true"
    end.

io_class({prompt, _}, _) ->
    os_process;
io_class({data, _}, _) ->
    os_process;
io_class(_, {interactive, _}) ->
    read;
io_class(_, {db_update, _}) ->
    write;
io_class(_, {view_update, _, _}) ->
    view_update;
io_class(_, {internal_repl, _}) ->
    shard_sync;
io_class(_, {db_compact, _}) ->
    compaction;
io_class(_, {view_compact, _, _}) ->
    compaction;
io_class(_, {system, _}) ->
    system;
io_class(_, {search, _}) ->
    search;
io_class(_, {search, _, _}) ->
    search;
io_class(_, {reshard, _}) ->
    reshard;
io_class(_, _) ->
    other.

queued_call(Fd, Msg, Priority) ->
    Request = #request{fd = Fd, msg = Msg, priority = Priority, from = self()},
    try
        gen_server:call(?MODULE, Request, infinity)
    catch
        exit:{noproc, _} ->
            gen_server:call(Fd, Msg, infinity)
    end.

init(_) ->
    ok = config:listen_for_changes(?MODULE, nil),
    State = #state{},
    {ok, read_config(State)}.

read_config(State) ->
    Ratio = config:get_float("ioq", "ratio", 0.01),
    Concurrency = config:get_integer("ioq", "concurrency", 10),
    State#state{concurrency = Concurrency, ratio = Ratio}.

handle_call(get_queue_lengths, _From, State) ->
    Response = #{
        interactive => queue:len(State#state.interactive),
        background => queue:len(State#state.background)
    },
    {reply, Response, State, 0};
handle_call(#request{} = Request, From, State) ->
    {noreply, enqueue_request(Request#request{from = From}, State), 0}.

handle_cast(change, State) ->
    {noreply, read_config(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Ref, Reply}, State) ->
    case lists:keytake(Ref, #request.ref, State#state.running) of
        {value, Request, Remaining} ->
            erlang:demonitor(Ref, [flush]),
            gen_server:reply(Request#request.from, Reply),
            {noreply, State#state{running = Remaining}, 0};
        false ->
            {noreply, State, 0}
    end;
handle_info({'DOWN', Ref, _, _, Reason}, State) ->
    case lists:keytake(Ref, #request.ref, State#state.running) of
        {value, Request, Remaining} ->
            gen_server:reply(Request#request.from, {'EXIT', Reason}),
            {noreply, State#state{running = Remaining}, 0};
        false ->
            {noreply, State, 0}
    end;
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(timeout, State) ->
    {noreply, maybe_submit_request(State)}.

handle_config_change("ioq", _, _, _, _) ->
    {ok, gen_server:cast(?MODULE, change)};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_Server, stop, _State) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(?RELISTEN_DELAY, whereis(?MODULE), restart_config_listener).

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

enqueue_request(#request{priority = compaction} = Request, #state{} = State) ->
    State#state{background = queue:in(Request, State#state.background)};
enqueue_request(#request{priority = shard_sync} = Request, #state{} = State) ->
    State#state{background = queue:in(Request, State#state.background)};
enqueue_request(#request{priority = reshard} = Request, #state{} = State) ->
    State#state{background = queue:in(Request, State#state.background)};
enqueue_request(#request{} = Request, #state{} = State) ->
    State#state{interactive = queue:in(Request, State#state.interactive)}.

maybe_submit_request(#state{concurrency = Concurrency, running = Running} = State) when
    length(Running) < Concurrency
->
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

make_next_request(#state{} = State) ->
    case {queue:is_empty(State#state.background), queue:is_empty(State#state.interactive)} of
        {true, true} ->
            State;
        {true, false} ->
            choose_next_request(#state.interactive, State);
        {false, true} ->
            choose_next_request(#state.background, State);
        {false, false} ->
            case couch_rand:uniform() < State#state.ratio of
                true ->
                    choose_next_request(#state.background, State);
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

submit_request(#request{} = Request, #state{} = State) ->
    Ref = erlang:monitor(process, Request#request.fd),
    Request#request.fd ! {'$gen_call', {self(), Ref}, Request#request.msg},
    State#state{running = [Request#request{ref = Ref} | State#state.running]}.
