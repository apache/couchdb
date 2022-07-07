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

-module(ioq_server).
-behaviour(gen_server).
-vsn(1).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/0, call/3]).
-export([add_channel/3, rem_channel/2, list_custom_channels/0]).

-record(channel, {
    name,
    qI = queue:new(), % client readers
    qU = queue:new(), % db updater
    qV = queue:new()  % view index updates
}).

-record(state, {
    counters,
    histos,
    reqs = [],
    concurrency = 20,
    channels = queue:new(),
    qC = queue:new(), % compaction
    qR = queue:new(), % internal replication
    qL = queue:new(),
    dedupe,
    class_priorities,
    op_priorities
}).

-record(request, {
    fd,
    msg,
    class,
    channel, % the name of the channel, not the actual data structure
    from,
    ref,
    t0,
    tsub
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WARNING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This server relies on the internal structure of the channels queue as a   %%
%% {list(), list()} to do in-place modifications of some elements.  We are   %%
%% "running on thin ice", as it were.                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call(Fd, Msg, Priority) ->
    {Class, Channel} = analyze_priority(Priority),
    Request = #request{
        fd = Fd,
        msg = Msg,
        channel = Channel,
        class = Class,
        t0 = erlang:monotonic_time()
    },
    case config:get("ioq.bypass", atom_to_list(Class)) of
        "true" ->
            RW = rw(Msg),
            catch couch_stats:increment_counter([couchdb, io_queue_bypassed, Class]),
            catch couch_stats:increment_counter([couchdb, io_queue_bypassed, RW]),
            gen_server:call(ioq:fd_pid(Fd), Msg, infinity);
        _ ->
            gen_server:call(?MODULE, Request, infinity)
    end.

add_channel(Account, DbName, ChannelName) ->
    ok = ioq_kv:put({Account, DbName}, ChannelName).

rem_channel(Account, DbName) ->
    ok = ioq_kv:delete({Account, DbName}).

list_custom_channels() ->
    ioq_kv:all().

init([]) ->
    State = #state{
        counters = ets:new(ioq_counters, []),
        histos = ets:new(ioq_histos, [named_table, ordered_set])
    },
    erlang:send_after(get_interval(), self(), dump_table),
    {ok, update_config(State)}.

handle_call({set_priority, Pri}, _From, State) ->
    {reply, process_flag(priority, Pri), State, 0};

handle_call({set_concurrency, C}, _From, State) when is_integer(C), C > 0 ->
    {reply, State#state.concurrency, State#state{concurrency = C}, 0};

handle_call(get_concurrency, _From, State) ->
    {reply, State#state.concurrency, State, 0};

handle_call(get_counters, _From, #state{counters = Tab} = State) ->
    {reply, Tab, State, 0};

handle_call(get_queue_depths, _From, State) ->
    Channels = lists:map(fun(#channel{name=N, qI=I, qU=U, qV=V}) ->
        {N, [queue:len(I), queue:len(U), queue:len(V)]}
    end, queue:to_list(State#state.channels)),
    Response = [
        {compaction, queue:len(State#state.qC)},
        {replication, queue:len(State#state.qR)},
        {low, queue:len(State#state.qL)},
        {channels, {Channels}}
    ],
    {reply, Response, State, 0};

handle_call(reset_histos, _From, State) ->
    ets:delete_all_objects(State#state.histos),
    {reply, ok, State, 0};

handle_call(#request{} = Req, From, State) ->
    {noreply, enqueue_request(Req#request{from = From}, State), 0};

%% backwards-compatible mode for messages sent during hot upgrade
handle_call({Fd, Msg, Priority, T0}, From, State) ->
    {Class, Chan} = analyze_priority(Priority),
    R = #request{fd=Fd, msg=Msg, channel=Chan, class=Class, t0=T0, from=From},
    {noreply, enqueue_request(R, State), 0};

handle_call(_Msg, _From, State) ->
    {reply, ignored, State, 0}.

handle_cast(update_config, State) ->
    {noreply, update_config(State), 0};

handle_cast(_Msg, State) ->
    {noreply, State, 0}.

handle_info({Ref, Reply}, #state{reqs = Reqs} = State) ->
    case lists:keytake(Ref, #request.ref, Reqs) of
    {value, #request{from=From} = Req, Reqs2} ->
        TResponse = erlang:monotonic_time(),
        erlang:demonitor(Ref, [flush]),
        reply_to_all(From, Reply),
        update_histograms(ioq_histos, Req, TResponse),
        {noreply, State#state{reqs = Reqs2}, 0};
    false ->
        {noreply, State, 0}
    end;

handle_info({'DOWN', Ref, _, _, Reason}, #state{reqs = Reqs} = State) ->
    case lists:keytake(Ref, #request.ref, Reqs) of
    {value, #request{from=From}, Reqs2} ->
        reply_to_all(From, {'EXIT', Reason}),
        {noreply, State#state{reqs = Reqs2}, 0};
    false ->
        {noreply, State, 0}
    end;

handle_info(dump_table, State) ->
    erlang:send_after(get_interval(), self(), dump_table),
    {noreply, dump_table(State), 0};

handle_info(timeout, State) ->
    {noreply, maybe_submit_request(State)};

handle_info(_Info, State) ->
    {noreply, State, 0}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, #state{}=State, _Extra) ->
    {ok, State}.

update_config(State) ->
    Concurrency = try
        list_to_integer(config:get("ioq", "concurrency", "20"))
    catch _:_ ->
        20
    end,

    DeDupe = config:get("ioq", "dedupe", "true") == "true",

    P1 = to_float(catch config:get("ioq", "customer", "1.0")),
    P2 = to_float(catch config:get("ioq", "replication", "0.001")),
    P3 = to_float(catch config:get("ioq", "compaction", "0.0001")),
    P4 = to_float(catch config:get("ioq", "low", "0.0001")),

    P5 = to_float(catch config:get("ioq", "reads", "1.0")),
    P6 = to_float(catch config:get("ioq", "writes", "1.0")),
    P7 = to_float(catch config:get("ioq", "views", "1.0")),

    State#state{
        concurrency = Concurrency,
        dedupe = DeDupe,
        class_priorities = [P1, P2, P3, P4],
        op_priorities = [P5, P6, P7]
    }.

reply_to_all([], _Reply) ->
    ok;
reply_to_all([From|Rest], Reply) ->
    gen_server:reply(From, Reply),
    reply_to_all(Rest, Reply);
reply_to_all(From, Reply) ->
    gen_server:reply(From, Reply).

analyze_priority({interactive, Shard}) ->
    {interactive, channel_name(Shard)};
analyze_priority({db_update, Shard}) ->
    {db_update, channel_name(Shard)};
analyze_priority({view_update, Shard, _GroupId}) ->
    {view_update, channel_name(Shard)};
analyze_priority({db_compact, _Shard}) ->
    {db_compact, nil};
analyze_priority({view_compact, _Shard, _GroupId}) ->
    {view_compact, nil};
analyze_priority({internal_repl, _Shard}) ->
    {internal_repl, nil};
analyze_priority({low, _Shard}) ->
    {low, nil};
analyze_priority(_Else) ->
    {other, other}.

channel_name(Shard) ->
    try split(Shard) of
    [<<"shards">>, _, <<"heroku">>, AppId | _] ->
        <<AppId/binary, ".heroku">>;
    [<<"shards">>, _, DbName] ->
        ioq_kv:get({other, DbName}, other);
    [<<"shards">>, _, Account, DbName] ->
        ioq_kv:get({Account, DbName}, Account);
    [<<"shards">>, _, Account | DbParts] ->
        ioq_kv:get({Account, filename:join(DbParts)}, Account);
    _ ->
        other
    catch _:_ ->
        other
    end.

enqueue_request(#request{class = db_compact} = Req, State) ->
    State#state{qC = update_queue(Req, State#state.qC, State#state.dedupe)};
enqueue_request(#request{class = view_compact} = Req, State) ->
    State#state{qC = update_queue(Req, State#state.qC, State#state.dedupe)};
enqueue_request(#request{class = internal_repl} = Req, State) ->
    State#state{qR = update_queue(Req, State#state.qR, State#state.dedupe)};
enqueue_request(#request{class = low} = Req, State) ->
    State#state{qL = update_queue(Req, State#state.qL, State#state.dedupe)};
enqueue_request(Req, State) ->
    enqueue_channel(Req, State).

find_channel(Account, {A, B}) ->
    case lists:keyfind(Account, #channel.name, A) of
    false ->
        case lists:keyfind(Account, #channel.name, B) of
        false ->
            {new, #channel{name = Account}};
        #channel{} = Channel ->
            {2, Channel}
        end;
    #channel{} = Channel ->
        {1, Channel}
    end.

update_channel(Ch, #request{class = view_update} = Req, Dedupe) ->
    Ch#channel{qV = update_queue(Req, Ch#channel.qV, Dedupe)};
update_channel(Ch, #request{class = db_update} = Req, Dedupe) ->
    Ch#channel{qU = update_queue(Req, Ch#channel.qU, Dedupe)};
update_channel(Ch, Req, Dedupe) ->
    % everything else is interactive IO class
    Ch#channel{qI = update_queue(Req, Ch#channel.qI, Dedupe)}.

update_queue(#request{from=From, fd=Fd, msg={pread_iolist, Pos}}=Req, Q, DD) ->
    case maybe_dedupe(Fd, Pos, Q, DD) of
    false ->
        queue:in(Req, Q);
    {Elem, N, #request{from=From1}=Match} ->
        catch couch_stats:increment_counter([couchdb, io_queue, merged]),
        Match1 = Match#request{from=append(From, From1)},
        L = element(Elem, Q),
        {H, [Match|T]} = lists:split(N, L),
        setelement(Elem, Q, H ++ [Match1|T])
    end;
update_queue(Req, Q, _Dedupe) ->
    queue:in(Req, Q).

append(A, B) when is_list(B) ->
    [A|B];
append(A, B) ->
    [A, B].

maybe_dedupe(Fd, Pos, Q, Dedupe) ->
    case Dedupe of
        true -> matching_request(Fd, Pos, Q);
        false -> false
    end.

matching_request(Fd, Pos, {A, B}) ->
    case matching_request(Fd, Pos, A) of
    false ->
        case matching_request(Fd, Pos, B) of
        false ->
            false;
        {N, Request} ->
            {2, N, Request}
        end;
    {N, Request} ->
        {1, N, Request}
    end;
matching_request(Fd, Pos, List) ->
    matching_request(Fd, Pos, 0, List).

matching_request(_Fd, _Pos, _N, []) ->
    false;
matching_request(Fd, Pos, N, [#request{fd=Fd, msg={pread_iolist, Pos}}=Req|_]) ->
    {N, Req};
matching_request(Fd, Pos, N, [_|Rest]) ->
    matching_request(Fd, Pos, N + 1, Rest).

enqueue_channel(#request{channel=Account} = Req, #state{channels=Q} = State) ->
    DD = State#state.dedupe,
    case find_channel(Account, Q) of
    {new, Channel0} ->
        State#state{channels = queue:in(update_channel(Channel0, Req, DD), Q)};
    {Elem, Channel0} ->
        Channel = update_channel(Channel0, Req, DD),
        % the channel already exists in the queue - update it in place
        L = element(Elem, Q),
        NewL = lists:keyreplace(Account, #channel.name, L, Channel),
        NewQ = setelement(Elem, Q, NewL),
        State#state{channels = NewQ}
    end.

maybe_submit_request(#state{concurrency=C,reqs=R} = St) when length(R) < C ->
    case make_next_request(St) of
        St ->
            St;
        NewState when length(R) >= C-1 ->
            NewState;
        NewState ->
            maybe_submit_request(NewState)
    end;
maybe_submit_request(State) ->
    State.

sort_queues(QueuesAndPriorities, Normalization, Choice) ->
    sort_queues(QueuesAndPriorities, Normalization, Choice, 0, [], []).

sort_queues([{Q, _Pri}], _Norm, _Choice, _X, [], Acc) ->
    lists:reverse([Q | Acc]);
sort_queues([{Q, Pri}], Norm, Choice, _X, Skipped, Acc) ->
    sort_queues(lists:reverse(Skipped), Norm - Pri, Choice, 0, [], [Q | Acc]);
sort_queues([{Q, Pri} | Rest], Norm, Choice, X, Skipped, Acc) ->
    if Choice < ((X + Pri) / Norm) ->
        Remaining = lists:reverse(Skipped, Rest),
        sort_queues(Remaining, Norm - Pri, Choice, 0, [], [Q | Acc]);
    true ->
        sort_queues(Rest, Norm, Choice, X + Pri, [{Q, Pri} | Skipped], Acc)
    end.

make_next_request(State) ->
    #state{
        channels = Ch,
        qC = QC,
        qR = QR,
        qL = QL,
        class_priorities = ClassP,
        op_priorities = OpP
    } = State,


    {Item, [NewCh, NewQR, NewQC, NewQL]} =
        choose_next_request([Ch, QR, QC, QL], ClassP),

    case Item of nil ->
        State;
    #channel{qI = QI, qU = QU, qV = QV} = Channel ->
        % An IO channel has at least one interactive or view indexing request.
        % If the channel has more than one request, we'll toss it back into the
        % queue after we've extracted one here
        {Item2, [QI2, QU2, QV2]} =
            choose_next_request([QI, QU, QV], OpP),
        case queue:is_empty(QU2) andalso
             queue:is_empty(QI2) andalso
             queue:is_empty(QV2) of
        true ->
            NewCh2 = NewCh;
        false ->
            NewCh2 = queue:in(Channel#channel{qI=QI2, qU=QU2, qV=QV2}, NewCh)
        end,
        submit_request(Item2, State#state{channels=NewCh2, qC=NewQC,
            qR=NewQR, qL=NewQL});
    _ ->
        % Item is a background (compaction or internal replication) task
        submit_request(Item, State#state{channels=NewCh, qC=NewQC, qR=NewQR,
            qL=NewQL})
    end.

submit_request(Request, State) ->
    #request{
        channel = Channel,
        fd = IOF,
        msg = Call,
        t0 = T0,
        class = IOClass
    } = Request,
    #state{reqs = Reqs, counters = Counters} = State,
    Fd = ioq:fd_pid(IOF),

    % make the request
    Ref = erlang:monitor(process, Fd),
    Fd ! {'$gen_call', {self(), Ref}, Call},

    % record some stats
    RW = rw(Call),
    SubmitTime = erlang:monotonic_time(),
    Latency = erlang:convert_time_unit(SubmitTime - T0, native, millisecond),
    catch couch_stats:increment_counter([couchdb, io_queue, IOClass]),
    catch couch_stats:increment_counter([couchdb, io_queue, RW]),
    catch couch_stats:update_histogram([couchdb, io_queue, latency], Latency),
    update_counter(Counters, Channel, IOClass, RW),
    State#state{reqs = [Request#request{tsub=SubmitTime, ref=Ref} | Reqs]}.

update_counter(Tab, Channel, IOClass, RW) ->
    upsert(Tab, {Channel, IOClass, RW}, 1).

update_histograms(Tab, Req, TResponse) ->
    #request{t0=T0, tsub=TSubmit, class=Class, channel=Channel, msg=Msg} = Req,
    Delta1 = erlang:convert_time_unit(
        TSubmit - T0, native, microsecond),
    Delta2 = erlang:convert_time_unit(
        TResponse - TSubmit, native, microsecond),
    Bin1 = timebin(Delta1),
    Bin2 = timebin(Delta2),
    Bin3 = timebin(Delta1+Delta2),
    if Channel =/= nil ->
        upsert(Tab, {Channel, submit_delay, Bin1}, 1),
        upsert(Tab, {Channel, svctm, Bin2}, 1),
        upsert(Tab, {Channel, iowait, Bin3}, 1);
    true -> ok end,
    Key = make_key(Class, Msg),
    upsert(Tab, {Key, submit_delay, Bin1}, 1),
    upsert(Tab, {Key, svctm, Bin2}, 1),
    upsert(Tab, {Key, iowait, Bin3}, 1).

make_key(db_compact, _) ->
    <<"compaction">>;
make_key(view_compact, _) ->
    <<"compaction">>;
make_key(internal_repl, _) ->
    <<"replication">>;
make_key(low, _) ->
    <<"low">>;
make_key(view_update, _) ->
    <<"views">>;
make_key(db_update, _) ->
    <<"writes">>;
make_key(interactive, {pread_iolist, _}) ->
    <<"reads">>;
make_key(interactive, {append_bin, _}) ->
    <<"writes">>;
make_key(_, _) ->
    <<"other">>.

upsert(Tab, Key, Incr) ->
    try ets:update_counter(Tab, Key, Incr)
    catch error:badarg ->
        ets:insert(Tab, {Key, Incr})
    end.

timebin(V) when V =< 0 ->
    0;
timebin(V) ->
    trunc(10*math:log10(V)).

choose_next_request(Qs, Priorities) ->
    Norm = lists:sum(Priorities),
    QueuesAndPriorities = lists:zip(Qs, Priorities),
    SortedQueues = sort_queues(QueuesAndPriorities, Norm, rand:uniform()),
    {Item, NewQueues} = choose_prioritized_request(SortedQueues),
    Map0 = lists:zip(SortedQueues, NewQueues),
    {Item, [element(2, lists:keyfind(Q, 1, Map0)) || Q <- Qs]}.

choose_prioritized_request(Qs) ->
    choose_prioritized_request(Qs, []).

choose_prioritized_request([], Empties) ->
    {nil, lists:reverse(Empties)};
choose_prioritized_request([Q | Rest], Empties) ->
    case queue:out(Q) of
    {empty, _} ->
        choose_prioritized_request(Rest, [Q | Empties]);
    {{value, Item}, NewQ} ->
        {Item, lists:reverse([NewQ | Empties], Rest)}
    end.

to_float("0") ->
    0.00001;
to_float("1") ->
    1.0;
to_float(String) when is_list(String) ->
    try list_to_float(String) catch error:badarg -> 0.5 end;
to_float(_) ->
    0.5.

dump_table(#state{counters = Tab} = State) ->
    Pid = spawn(fun save_to_db/0),
    ets:give_away(Tab, Pid, nil),
    State#state{counters = ets:new(ioq_counters, [])}.

save_to_db() ->
    Timeout = get_interval(),
    receive {'ETS-TRANSFER', Tab, _, _} ->
        Dict = ets:foldl(fun table_fold/2, dict:new(), Tab),
        TS = list_to_binary(iso8601_timestamp()),
        Doc = {[
            {<<"_id">>, TS},
            {type, ioq},
            {node, node()},
            {accounts, {dict:to_list(Dict)}}
        ]},
        try
            fabric:update_doc(get_stats_dbname(), Doc, [])
        catch
            error:database_does_not_exist ->
                couch_log:debug("Missing IOQ stats db: ~s", [get_stats_dbname()]);
            error:conflict ->
                couch_log:info("~p:save_to_db conflict saving ~p", [?MODULE, Doc])
        end
    after Timeout ->
        error_logger:error_report({?MODULE, "ets transfer failed"})
    end.

table_fold({{other, _, _}, _}, D) ->
    D;
table_fold({{Channel, interactive, reads}, X}, D) ->
    dict:update(Channel, fun([A,B,C]) -> [A+X,B,C] end, [X,0,0], D);
table_fold({{Channel, interactive, writes}, X}, D) ->
    dict:update(Channel, fun([A,B,C]) -> [A,B+X,C] end, [0,X,0], D);
table_fold({{Channel, db_update, reads}, X}, D) ->
    dict:update(Channel, fun([A,B,C]) -> [A,B+X,C] end, [0,X,0], D);
table_fold({{Channel, db_update, writes}, X}, D) ->
    dict:update(Channel, fun([A,B,C]) -> [A,B+X,C] end, [0,X,0], D);
table_fold({{Channel, view_update, reads}, X}, D) ->
    dict:update(Channel, fun([A,B,C]) -> [A,B,C+X] end, [0,0,X], D);
table_fold({{Channel, view_update, writes}, X}, D) ->
    dict:update(Channel, fun([A,B,C]) -> [A,B,C+X] end, [0,0,X], D);
table_fold(_, D) ->
    D.

iso8601_timestamp() ->
    {_,_,Micro} = Now = os:timestamp(),
    {{Year,Month,Date},{Hour,Minute,Second}} = calendar:now_to_datetime(Now),
    Format = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
    io_lib:format(Format, [Year, Month, Date, Hour, Minute, Second, Micro]).

get_interval() ->
    config:get_integer("ioq", "stats_interval", 60*1000).

get_stats_dbname() ->
    config:get("ioq", "stats_db", "stats").

split(B) when is_binary(B) ->
    split(B, 0, 0, []);
split(B) -> B.

split(B, O, S, Acc) ->
    case B of
    <<_:O/binary>> ->
        Len = O - S,
        <<_:S/binary, Part:Len/binary>> = B,
        lists:reverse(Acc, [Part]);
    <<_:O/binary, $/, _/binary>> ->
        Len = O - S,
        <<_:S/binary, Part:Len/binary, _/binary>> = B,
        split(B, O+1, O+1, [Part | Acc]);
    _ ->
        split(B, O+1, S, Acc)
    end.

rw({pread_iolist, _}) ->
    reads;
rw({append_bin, _}) ->
    writes;
rw(_) ->
    unknown.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sort_queues_test() ->
    ?assertEqual([a, b, c], sort_queues([{a,0.5}, {b,0.2}, {c,0.3}], 1, 0.10)),
    ?assertEqual([a, c, b], sort_queues([{a,0.5}, {b,0.2}, {c,0.3}], 1, 0.45)),
    ?assertEqual([b, a, c], sort_queues([{a,0.5}, {b,0.2}, {c,0.3}], 1, 0.60)),
    ?assertEqual([b, c, a], sort_queues([{a,0.5}, {b,0.2}, {c,0.3}], 1, 0.65)),
    ?assertEqual([c, a, b], sort_queues([{a,0.5}, {b,0.2}, {c,0.3}], 1, 0.71)),
    ?assertEqual([c, b, a], sort_queues([{a,0.5}, {b,0.2}, {c,0.3}], 1, 0.90)).

-endif.
