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

-module(ioq_osq).
-behaviour(gen_server).
-vsn(1).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/0, call/3]).

-record(channel, {
    name,
    q = queue:new()
}).

-record(state, {
    reqs = [],
    min = 2,
    max = 6,
    global_max = 15,
    channels = queue:new()
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WARNING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This server relies on the internal structure of the channels queue as a   %%
%% {list(), list()} to do in-place modifications of some elements.  We are   %%
%% "running on thin ice", as it were.                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


call(Pid, Msg, Priority) ->
    Now = erlang:monotonic_time(),
    Reply = gen_server:call(ioq_osq, {rlimit, nil, Priority, Now}, infinity),
    try
        gen_server:call(Pid, Msg, infinity)
    after
        whereis(ioq_osq) ! Reply
    end.


init([]) ->
    ets:new(osq_counters, [named_table]),
    St = #state{},
    {ok, St#state{
        min = threshold("minimum", St#state.min),
        max = threshold("maximum", St#state.max),
        global_max = threshold("global_maximum", St#state.global_max)
    }}.

handle_call({set_minimum, C}, _From, State) when is_integer(C), C > 0 ->
    {reply, ok, State#state{min = C}};

handle_call({set_maximum, C}, _From, State) when is_integer(C), C > 0 ->
    {reply, ok, State#state{max = C}};

handle_call({set_global_maximum, C}, _From, State) when is_integer(C), C > 0 ->
    {reply, ok, State#state{global_max = C}};

handle_call(get_queue_depths, _From, State) ->
    Channels = [{N, queue:len(Q)} || #channel{name=N, q=Q}
        <- queue:to_list(State#state.channels)],
    {reply, Channels, State};

handle_call(get_requests, _From, State) ->
    {reply, State#state.reqs, State};

handle_call({_, _, {interactive, Shard}, _} = Req, From, State) ->
    {noreply, enqueue_channel(channel_name(Shard), {Req, From}, State)};

handle_call({_, _, {view_update, Shard, _}, _} = Req, From, State) ->
    {noreply, enqueue_channel(channel_name(Shard), {Req, From}, State)};

handle_call({Fd, _, _, _} = Req, From, State) when is_pid(Fd) ->
    {noreply, enqueue_channel(other, {Req, From}, State)};

handle_call({rlimit, _, _, _} = Req, From, State) ->
    {noreply, enqueue_channel(other, {Req, From}, State)};

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Ref, Reply}, #state{reqs = Reqs} = State) ->
    case lists:keyfind(Ref, 3, Reqs) of
    {_, notify, Ref} ->
        erlang:demonitor(Ref, [flush]),
        Reqs2 = lists:keydelete(Ref, 3, Reqs),
        {noreply, make_next_request(State#state{reqs = Reqs2})};
    {_, From, Ref} ->
        erlang:demonitor(Ref, [flush]),
        gen_server:reply(From, Reply),
        Reqs2 = lists:keydelete(Ref, 3, Reqs),
        {noreply, make_next_request(State#state{reqs = Reqs2})};
    false ->
        {noreply, State}
    end;

handle_info({'DOWN', Ref, _, _, Reason}, #state{reqs = Reqs} = State) ->
    case lists:keyfind(Ref, 3, Reqs) of
    {_, notify, Ref} ->
        Reqs2 = lists:keydelete(Ref, 3, Reqs),
        {noreply, make_next_request(State#state{reqs = Reqs2})};
    {_, From, Ref} ->
        gen_server:reply(From, {'EXIT', Reason}),
        Reqs2 = lists:keydelete(Ref, 3, Reqs),
        {noreply, make_next_request(State#state{reqs = Reqs2})};
    false ->
        {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

channel_name(Shard) ->
    try re:split(Shard, "/") of
    [<<"shards">>, _, <<"heroku">>, AppId | _] ->
        <<AppId/binary, ".heroku">>;
    [<<"shards">>, _, Account | _] ->
        Account;
    _ ->
        other
    catch _:_ ->
        other
    end.

find_channel(Account, {A, B}) ->
    case lists:keyfind(Account, 2, A) of
    false ->
        case lists:keyfind(Account, 2, B) of
        false ->
            {new, #channel{name = Account}};
        #channel{} = Channel ->
            {2, Channel}
        end;
    #channel{} = Channel ->
        {1, Channel}
    end.

update_channel(#channel{q = Q} = Ch, Req) ->
    Ch#channel{q = queue:in(Req, Q)}.

enqueue_channel(Account, Req, #state{channels = Q} = State) ->
    NewState = case find_channel(Account, Q) of
    {new, Channel0} ->
        State#state{channels = queue:in(update_channel(Channel0, Req), Q)};
    {Elem, Channel0} ->
        Channel = update_channel(Channel0, Req),
        % the channel already exists in the queue - update it in place
        L = element(Elem, Q),
        NewQ = setelement(Elem, Q, lists:keyreplace(Account, 2, L, Channel)),
        State#state{channels = NewQ}
    end,
    maybe_submit_request(NewState).

maybe_submit_request(#state{global_max=C, reqs=R} = St) when length(R) < C ->
    make_next_request(St);
maybe_submit_request(#state{min = Min} = State) ->
    % look for a channel which hasn't reached the minimum yet
    make_next_request(State, Min).

make_next_request(#state{max = Max} = State) ->
    % default behavior, look for a channel not yet maxed out
    make_next_request(State, Max).

make_next_request(#state{channels = Channels, reqs = R} = State, Threshold) ->
    case next_unblocked_channel(Channels, R, Threshold, queue:new()) of
    {#channel{name = Name, q = Q} = Ch, OutChannels} ->
        {{value, Item}, NewQ} = queue:out(Q),
        case queue:is_empty(NewQ) of
        true ->
            NewCh = OutChannels;
        false ->
            NewCh = queue:in(Ch#channel{q = NewQ}, OutChannels)
        end,
        submit_request(Name, Item, State#state{channels = NewCh});
    {nil, OutQ} ->
        % everybody is using their allotted slots, try again later
        State#state{channels = OutQ}
    end.

next_unblocked_channel(InQ, Reqs, Max, OutQ)  ->
    case queue:out(InQ) of
    {empty, _} -> % all channels blocked
        {nil, OutQ};
    {{value, #channel{name=Name} = Channel}, NewQ} ->
        case length([1 || {N, _, _} <- Reqs, N =:= Name]) >= Max of
        true -> % channel is blocked, keep searching
            next_unblocked_channel(NewQ, Reqs, Max, queue:in(Channel, OutQ));
        false ->
            {Channel, queue:join(NewQ, OutQ)}
        end
    end.


submit_request(Channel, {{rlimit,_,Pri,T0}, From}, #state{reqs=Reqs} = State) ->
    % rlimit fd means that we'll get a response back
    % from the pid after it performs the call on its
    % own
    Ref = erlang:monitor(process, element(1, From)),
    gen_server:reply(From, {Ref, nil}),
    record_stats(Channel, Pri, T0),
    State#state{reqs = [{Channel, notify, Ref} | Reqs]};

submit_request(Channel, {{Fd,Call,Pri,T0}, From}, #state{reqs=Reqs} = State) ->
    % make the request
    Ref = erlang:monitor(process, Fd),
    Fd ! {'$gen_call', {self(), Ref}, Call},
    record_stats(Channel, Pri, T0),
    State#state{reqs = [{Channel, From, Ref} | Reqs]}.

record_stats(Channel, Pri, T0) ->
    IOClass = if is_tuple(Pri) -> element(1, Pri); true -> Pri end,
    Now = erlang:monotonic_time(),
    Latency = erlang:convert_time_unit(
        Now - T0, native, millisecond),
    catch couch_stats:increment_counter([couchdb, io_queue, IOClass]),
    catch couch_stats:increment_counter([couchdb, io_queue, osproc]),
    catch couch_stats:update_histogram([couchdb, io_queue, latency], Latency),
    update_counter(Channel, IOClass, osproc).

update_counter(Channel, IOClass, RW) ->
    try ets:update_counter(osq_counters, {Channel, IOClass, RW}, 1)
    catch error:badarg ->
        ets:insert(osq_counters, {{Channel, IOClass, RW}, 1})
    end.

threshold(Name, Default) ->
    try list_to_integer(config:get("osq", Name)) of
    C when C > 0->
        C;
    _ ->
        Default
    catch _:_ ->
        Default
    end.
