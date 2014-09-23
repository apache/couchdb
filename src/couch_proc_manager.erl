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

-module(couch_proc_manager).
-behaviour(gen_server).
-behaviour(config_listener).
-vsn(1).

-export([
    start_link/0,
    get_proc_count/0,
    get_stale_proc_count/0,
    new_proc/1,
    reload/0,
    terminate_stale_procs/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
    handle_config_change/5
]).

-include_lib("couch/include/couch_db.hrl").

-define(PROCS, couch_proc_manager_procs).
-define(WAITERS, couch_proc_manager_waiters).
-define(OPENING, couch_proc_manager_opening).

-record(state, {
    config,
    counts,
    threshold_ts,
    hard_limit,
    soft_limit
}).

-record(client, {
    timestamp,
    from,
    lang,
    ddoc,
    ddoc_key
}).

-record(proc_int, {
    pid,
    lang,
    client,
    ddoc_keys = [],
    prompt_fun,
    set_timeout_fun,
    stop_fun,
    t0 = os:timestamp()
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_proc_count() ->
    gen_server:call(?MODULE, get_proc_count).


get_stale_proc_count() ->
    gen_server:call(?MODULE, get_stale_proc_count).


reload() ->
    gen_server:call(?MODULE, set_threshold_ts).


terminate_stale_procs() ->
    gen_server:call(?MODULE, terminate_stale_procs).


init([]) ->
    process_flag(trap_exit, true),
    ok = config:listen_for_changes(?MODULE, undefined),

    TableOpts = [public, named_table, ordered_set],
    ets:new(?PROCS, TableOpts ++ [{keypos, #proc_int.pid}]),
    ets:new(?WAITERS, TableOpts ++ [{keypos, #client.timestamp}]),
    ets:new(?OPENING, [public, named_table, set]),

    {ok, #state{
        config = get_proc_config(),
        counts = dict:new(),
        threshold_ts = os:timestamp(),
        hard_limit = get_hard_limit(),
        soft_limit = get_soft_limit()
    }}.


terminate(_Reason, _State) ->
    ets:foldl(fun(#proc_int{pid=P}, _) ->
        couch_util:shutdown_sync(P)
    end, 0, ?PROCS),
    ok.


handle_call(get_proc_count, _From, State) ->
    NumProcs = ets:info(?PROCS, size),
    NumOpening = ets:info(?OPENING, size),
    {reply, NumProcs + NumOpening, State};

handle_call(get_stale_proc_count, _From, State) ->
    #state{threshold_ts = T0} = State,
    MatchSpec = [{#proc_int{t0='$1', _='_'}, [{'<', '$1', {T0}}], [true]}],
    {reply, ets:select_count(?PROCS, MatchSpec), State};

handle_call({get_proc, #doc{body={Props}}=DDoc, DDocKey}, From, State) ->
    {ClientPid, _} = From,
    LangStr = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    Lang = couch_util:to_binary(LangStr),
    IterFun = fun(Proc, Acc) ->
        case lists:member(DDocKey, Proc#proc_int.ddoc_keys) of
            true ->
                {stop, assign_proc(ClientPid, Proc)};
            false ->
                {ok, Acc}
        end
    end,
    TeachFun = fun(Proc0, Acc) ->
        try
            {ok, Proc1} = teach_ddoc(DDoc, DDocKey, Proc0),
            {stop, assign_proc(ClientPid, Proc1)}
        catch _:_ ->
            {ok, Acc}
        end
    end,
    Client = #client{from=From, lang=Lang, ddoc=DDoc, ddoc_key=DDocKey},
    find_proc(State, Client, [IterFun, TeachFun]);

handle_call({get_proc, Lang}, From, State) ->
    {ClientPid, _} = From,
    IterFun = fun(Proc, _Acc) ->
        {stop, assign_proc(ClientPid, Proc)}
    end,
    Client = #client{from=From, lang=couch_util:to_binary(Lang)},
    find_proc(State, Client, [IterFun]);

handle_call({ret_proc, #proc{client=Ref} = Proc}, _From, State) ->
    erlang:demonitor(Ref, [flush]),
    NewState = case ets:lookup(?PROCS, Proc#proc.pid) of
        [#proc_int{}=ProcInt] ->
            return_proc(State, ProcInt);
        [] ->
            % Proc must've died and we already
            % cleared it out of the table in
            % the handle_info clause.
            State
    end,
    {reply, true, NewState};

handle_call(set_threshold_ts, _From, State) ->
    FoldFun = fun
        (#proc_int{client = undefined} = Proc, StateAcc) ->
            remove_proc(StateAcc, Proc);
        (_, StateAcc) ->
            StateAcc
    end,
    NewState = ets:foldl(FoldFun, State, ?PROCS),
    {reply, ok, NewState#state{threshold_ts = os:timestamp()}};

handle_call(terminate_stale_procs, _From, #state{threshold_ts = Ts1} = State) ->
    FoldFun = fun
        (#proc_int{client = undefined, t0 = Ts2} = Proc, StateAcc) ->
            case Ts1 > Ts2 of
                true ->
                    remove_proc(StateAcc, Proc);
                false ->
                    StateAcc
            end;
        (_, StateAcc) ->
            StateAcc
    end,
    NewState = ets:foldl(FoldFun, State, ?PROCS),
    {reply, ok, NewState};

handle_call(_Call, _From, State) ->
    {reply, ignored, State}.


handle_cast({os_proc_idle, Pid}, #state{counts=Counts}=State) ->
    NewState = case ets:lookup(?PROCS, Pid) of
        [#proc_int{client=undefined, lang=Lang}=Proc] ->
            case dict:find(Lang, Counts) of
                {ok, Count} when Count >= State#state.soft_limit ->
                    couch_log:info("Closing idle OS Process: ~p", [Pid]),
                    remove_proc(State, Proc);
                {ok, _} ->
                    State
            end;
        _ ->
            State
    end,
    {noreply, NewState};

handle_cast(reload_config, State) ->
    NewState = State#state{
        config = get_proc_config(),
        hard_limit = get_hard_limit(),
        soft_limit = get_soft_limit()
    },
    {noreply, flush_waiters(NewState)};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(shutdown, State) ->
    {stop, shutdown, State};

handle_info({'EXIT', Pid, {spawn_ok, Proc0, {ClientPid,_} = From}}, State) ->
    ets:delete(?OPENING, Pid),
    link(Proc0#proc_int.pid),
    Proc = assign_proc(ClientPid, Proc0),
    gen_server:reply(From, {ok, Proc, State#state.config}),
    {noreply, State};

handle_info({'EXIT', Pid, spawn_error}, State) ->
    [{Pid, #client{lang=Lang}}] = ets:lookup(?OPENING, Pid),
    ets:delete(?OPENING, Pid),
    NewState = State#state{
        counts = dict:update_counter(Lang, -1, State#state.counts)
    },
    {noreply, flush_waiters(NewState, Lang)};

handle_info({'EXIT', Pid, Reason}, State) ->
    couch_log:info("~p ~p died ~p", [?MODULE, Pid, Reason]),
    case ets:lookup(?PROCS, Pid) of
        [#proc_int{} = Proc] ->
            NewState = remove_proc(State, Proc),
            {noreply, flush_waiters(NewState, Proc#proc_int.lang)};
        [] ->
            {noreply, State}
    end;

handle_info({'DOWN', Ref, _, _, _Reason}, State0) ->
    case ets:match_object(?PROCS, #proc_int{client=Ref, _='_'}) of
        [#proc_int{} = Proc] ->
            {noreply, return_proc(State0, Proc)};
        [] ->
            {noreply, State0}
    end;

handle_info({gen_event_EXIT, {config_listener, ?MODULE}, _Reason}, State) ->
    erlang:send_after(5000, self(), restart_config_listener),
    {noreply, State};

handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, undefined),
    % Reload our config in case it changed in the last
    % five seconds.
    handle_cast(reload, State);

handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, #state{}=State, _Extra) ->
    {ok, State}.


handle_config_change("query_server_config", _, _, _, _) ->
    gen_server:cast(?MODULE, reload_config),
    {ok, undefined};
handle_config_change(_, _, _, _, _) ->
    {ok, undefined}.


find_proc(State, Client, [Fun | FindFuns]) ->
    try iter_procs(Client#client.lang, Fun, undefined) of
        {not_found, _} ->
            find_proc(State, Client, FindFuns);
        {ok, Proc} ->
            {reply, {ok, Proc, State#state.config}, State}
    catch error:Reason ->
        couch_log:error("~p ~p ~p", [?MODULE, Reason, erlang:get_stacktrace()]),
        {reply, {error, Reason}, State}
    end;
find_proc(State, Client, []) ->
    {noreply, maybe_spawn_proc(State, Client)}.


iter_procs(Lang, Fun, Acc) when is_binary(Lang) ->
    Pattern = #proc_int{lang=Lang, client=undefined, _='_'},
    MSpec = [{Pattern, [], ['$_']}],
    case ets:select_reverse(?PROCS, MSpec, 25) of
        '$end_of_table' ->
            {not_found, Acc};
        Continuation ->
            iter_procs_int(Continuation, Fun, Acc)
    end.


iter_procs_int({[], Continuation0}, Fun, Acc) ->
    case ets:select_reverse(Continuation0) of
        '$end_of_table' ->
            {not_found, Acc};
        Continuation1 ->
            iter_procs_int(Continuation1, Fun, Acc)
    end;
iter_procs_int({[Proc | Rest], Continuation}, Fun, Acc0) ->
    case Fun(Proc, Acc0) of
        {ok, Acc1} ->
            iter_procs_int({Rest, Continuation}, Fun, Acc1);
        {stop, Acc1} ->
            {ok, Acc1}
    end.


maybe_spawn_proc(State, Client) ->
    case dict:find(Client#client.lang, State#state.counts) of
    {ok, Count} when Count >= State#state.hard_limit ->
        add_waiting_client(Client),
        State;
    _ ->
        spawn_proc(State, Client)
    end.


spawn_proc(State, Client) ->
    Pid = spawn_link(?MODULE, new_proc, [Client]),
    ets:insert(?OPENING, {Pid, Client}),
    Counts = State#state.counts,
    Lang = Client#client.lang,
    State#state{
        counts = dict:update_counter(Lang, 1, Counts)
    }.


new_proc(#client{ddoc=undefined, ddoc_key=undefined}=Client) ->
    #client{from=From, lang=Lang} = Client,
    Resp = try
        case new_proc_int(From, Lang) of
            {ok, Proc} ->
                {spawn_ok, Proc, From};
            Error ->
                gen_server:reply(From, {error, Error}),
                spawn_error
        end
    catch _:_ ->
        spawn_error
    end,
    exit(Resp);

new_proc(Client) ->
    #client{from=From, lang=Lang, ddoc=DDoc, ddoc_key=DDocKey} = Client,
    Resp = try
        case new_proc_int(From, Lang) of
        {ok, NewProc} ->
            case teach_ddoc(DDoc, DDocKey, NewProc) of
            {ok, Proc} ->
                {spawn_ok, Proc, From};
            {error, Reason} ->
                gen_server:reply(From, {error, Reason}),
                spawn_error
            end;
        Error ->
            gen_server:reply(From, {error, Error}),
            spawn_error
        end
    catch _:_ ->
        spawn_error
    end,
    exit(Resp).


new_proc_int(From, Lang) when is_binary(Lang) ->
    LangStr = binary_to_list(Lang),
    case config:get("query_servers", LangStr) of
    undefined ->
        case config:get("native_query_servers", LangStr) of
        undefined ->
            gen_server:reply(From, {unknown_query_language, Lang});
        SpecStr ->
            {ok, {M,F,A}} = couch_util:parse_term(SpecStr),
            {ok, Pid} = apply(M, F, A),
            make_proc(Pid, Lang, M)
        end;
    Command ->
        {ok, Pid} = couch_os_process:start_link(Command),
        make_proc(Pid, Lang, couch_os_process)
    end.


teach_ddoc(DDoc, {DDocId, _Rev}=DDocKey, #proc_int{ddoc_keys=Keys}=Proc) ->
    % send ddoc over the wire
    % we only share the rev with the client we know to update code
    % but it only keeps the latest copy, per each ddoc, around.
    true = couch_query_servers:proc_prompt(
        export_proc(Proc),
        [<<"ddoc">>, <<"new">>, DDocId, couch_doc:to_json_obj(DDoc, [])]),
    % we should remove any other ddocs keys for this docid
    % because the query server overwrites without the rev
    Keys2 = [{D,R} || {D,R} <- Keys, D /= DDocId],
    % add ddoc to the proc
    {ok, Proc#proc_int{ddoc_keys=[DDocKey|Keys2]}}.


make_proc(Pid, Lang, Mod) when is_binary(Lang) ->
    Proc = #proc_int{
        lang = Lang,
        pid = Pid,
        prompt_fun = {Mod, prompt},
        set_timeout_fun = {Mod, set_timeout},
        stop_fun = {Mod, stop}
    },
    unlink(Pid),
    {ok, Proc}.


assign_proc(Pid, #proc_int{client=undefined}=Proc0) when is_pid(Pid) ->
    Proc = Proc0#proc_int{client = erlang:monitor(process, Pid)},
    ets:insert(?PROCS, Proc),
    export_proc(Proc);
assign_proc(#client{}=Client, #proc_int{client=undefined}=Proc) ->
    {Pid, _} = Client#client.from,
    assign_proc(Pid, Proc).


return_proc(#state{} = State, #proc_int{} = ProcInt) ->
    #proc_int{pid = Pid, lang = Lang} = ProcInt,
    NewState = case is_process_alive(Pid) of true ->
        case ProcInt#proc_int.t0 < State#state.threshold_ts of
            true ->
                remove_proc(State, Pid);
            false ->
                gen_server:cast(Pid, garbage_collect),
                true = ets:update_element(?PROCS, Pid, [
                    {#proc_int.client, undefined}
                ]),
                State
        end;
    false ->
        remove_proc(State, ProcInt)
    end,
    flush_waiters(NewState, Lang).


remove_proc(State, #proc_int{}=Proc) ->
    ets:delete(?PROCS, Proc#proc_int.pid),
    case is_process_alive(Proc#proc_int.pid) of true ->
        unlink(Proc#proc_int.pid),
        gen_server:cast(Proc#proc_int.pid, stop);
    false ->
        ok
    end,
    Counts = State#state.counts,
    Lang = Proc#proc_int.lang,
    State#state{
        counts = dict:update_counter(Lang, -1, Counts)
    }.


-spec export_proc(#proc_int{}) -> #proc{}.
export_proc(#proc_int{} = ProcInt) ->
    ProcIntList = tuple_to_list(ProcInt),
    ProcLen = record_info(size, proc),
    [_ | Data] = lists:sublist(ProcIntList, ProcLen),
    list_to_tuple([proc | Data]).


flush_waiters(State) ->
    dict:fold(fun(Lang, Count, StateAcc) ->
        case Count < State#state.hard_limit of
            true ->
                flush_waiters(StateAcc, Lang);
            false ->
                StateAcc
        end
    end, State, State#state.counts).


flush_waiters(State, Lang) ->
    case dict:fetch(Lang, State#state.counts) of
        Count when Count < State#state.hard_limit ->
            case get_waiting_client(Lang) of
                #client{} = Client ->
                    NewState = spawn_proc(State, Client),
                    flush_waiters(NewState, Lang);
                undefined ->
                    State
            end;
        _ ->
            State
    end.


add_waiting_client(Client) ->
    ets:insert(?WAITERS, Client#client{timestamp=os:timestamp()}).


get_waiting_client(Lang) ->
    case ets:match_object(?WAITERS, #client{lang=Lang, _='_'}, 1) of
        '$end_of_table' ->
            undefined;
        {[#client{}=Client], _} ->
            ets:delete(?WAITERS, Client#client.timestamp),
            Client
    end.


get_proc_config() ->
    Limit = config:get("query_server_config", "reduce_limit", "true"),
    Timeout = config:get("couchdb", "os_process_timeout", "5000"),
    {[
        {<<"reduce_limit">>, list_to_atom(Limit)},
        {<<"timeout">>, list_to_integer(Timeout)}
    ]}.


get_hard_limit() ->
    LimStr = config:get("query_server_config", "os_process_limit", "100"),
    list_to_integer(LimStr).


get_soft_limit() ->
    LimStr = config:get("query_server_config", "os_process_soft_limit", "100"),
    list_to_integer(LimStr).
