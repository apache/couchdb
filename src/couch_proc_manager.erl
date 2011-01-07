-module(couch_proc_manager).
-behaviour(gen_server).
-behaviour(config_listener).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
    code_change/3]).

-export([start_link/0, get_proc_count/0, new_proc/2, new_proc/4]).

% config_listener api
-export([handle_config_change/5]).

-include_lib("couch/include/couch_db.hrl").

-record(state, {
    tab,
    config
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_proc_count() ->
    gen_server:call(?MODULE, get_proc_count).

init([]) ->
    process_flag(trap_exit, true),
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, #state{
        tab = ets:new(procs, [ordered_set, {keypos, #proc.pid}]),
        config = get_proc_config()
    }}.

handle_call(get_table, _From, State) ->
    {reply, State#state.tab, State};

handle_call(get_proc_count, _From, State) ->
    {reply, ets:info(State#state.tab, size), State};

handle_call({get_proc, #doc{body={Props}}=DDoc, DDocKey}, From, State) ->
    {Client, _} = From,
    Lang = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    IterFun = fun(Proc, Acc) ->
        case lists:member(DDocKey, Proc#proc.ddoc_keys) of
            true ->
                {stop, assign_proc(State#state.tab, Client, Proc)};
            false ->
                {ok, Acc}
        end
    end,
    TeachFun = fun(Proc0, Acc) ->
        try
            {ok, Proc1} = teach_ddoc(DDoc, DDocKey, Proc0),
            {stop, assign_proc(State#state.tab, Client, Proc1)}
        catch _:_ ->
            {ok, Acc}
        end
    end,
    try iter_procs(State#state.tab, Lang, IterFun, nil) of
    {not_found, _} ->
        case iter_procs(State#state.tab, Lang, TeachFun, nil) of
        {not_found, _} ->
            spawn_link(?MODULE, new_proc, [From, Lang, DDoc, DDocKey]),
            {noreply, State};
        {ok, Proc} ->
            {reply, {ok, Proc, State#state.config}, State}
        end;
    {ok, Proc} ->
        {reply, {ok, Proc, State#state.config}, State}
    catch error:Reason ->
        ?LOG_ERROR("~p ~p ~p", [?MODULE, Reason, erlang:get_stacktrace()]),
        {reply, {error, Reason}, State}
    end;

handle_call({get_proc, Lang}, {Client, _} = From, State) ->
    IterFun = fun(Proc, _Acc) ->
        {stop, assign_proc(State#state.tab, Client, Proc)}
    end,
    try iter_procs(State#state.tab, Lang, IterFun, nil) of
    {not_found, _} ->
        spawn_link(?MODULE, new_proc, [From, Lang]),
        {noreply, State};
    {ok, Proc} ->
        {reply, {ok, Proc, State#state.config}, State}
    catch error:Reason ->
        ?LOG_ERROR("~p ~p ~p", [?MODULE, Reason, erlang:get_stacktrace()]),
        {reply, {error, Reason}, State}
    end;

handle_call({ret_proc, #proc{client=Ref} = Proc}, _From, State) ->
    erlang:demonitor(Ref, [flush]),
    % We need to check if the process is alive here, as the client could be
    % handing us a #proc{} with a dead one.  We would have already removed the
    % #proc{} from our own table, so the alternative is to do a lookup in the
    % table before the insert.  Don't know which approach is cheaper.
    return_proc(State#state.tab, Proc),
    {reply, true, State};

handle_call(_Call, _From, State) ->
    {reply, ignored, State}.

handle_cast({os_proc_idle, Pid}, #state{tab=Tab}=State) ->
    Limit = couch_config:get("query_server_config", "os_process_soft_limit", "100"),
    case ets:lookup(Tab, Pid) of
        [#proc{client=nil}] ->
            case ets:info(Tab, size) > list_to_integer(Limit) of
                true ->
                    ?LOG_INFO("Closing idle OS Process: ~p", [Pid]),
                    ets:delete(Tab, Pid),
                    case is_process_alive(Pid) of
                        true ->
                            unlink(Pid),
                            gen_server:cast(Pid, stop);
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    {noreply, State};
handle_cast(reload_config, State) ->
    {noreply, State#state{config = get_proc_config()}};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', _, {ok, Proc0, {Client,_} = From}}, State) ->
    link(Proc0#proc.pid),
    Proc = assign_proc(State#state.tab, Client, Proc0),
    gen_server:reply(From, {ok, Proc, State#state.config}),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    ?LOG_INFO("~p ~p died ~p", [?MODULE, Pid, Reason]),
    ets:delete(State#state.tab, Pid),
    {noreply, State};

handle_info({'DOWN', Ref, _, _, _Reason}, State) ->
    case ets:match_object(State#state.tab, #proc{client=Ref, _='_'}) of
    [] ->
        ok;
    [#proc{} = Proc] ->
        return_proc(State#state.tab, Proc)
    end,
    {noreply, State};

handle_info({gen_event_EXIT, {config_listener, ?MODULE}, _Reason}, State) ->
    erlang:send_after(5000, self(), restart_config_listener),
    {noreply, State};

handle_info(restart_config_lister, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{tab=Tab}) ->
    ets:foldl(fun(#proc{pid=P}, _) -> couch_util:shutdown_sync(P) end, 0, Tab),
    ok.

code_change(_OldVsn, #state{tab = Tab} = State, _Extra) ->
    NewTab = ets:new(procs, [ordered_set, {keypos, #proc.pid}]),
    true = ets:insert(NewTab, ets:tab2list(Tab)),
    true = ets:delete(Tab),
    {ok, State#state{tab = NewTab}}.

handle_config_change("query_server_config", _, _, _, _) ->
    gen_server:cast(?MODULE, reload_config),
    {ok, nil};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

iter_procs(Tab, Lang, Fun, Acc) when is_binary(Lang) ->
    iter_procs(Tab, binary_to_list(Lang), Fun, Acc);
iter_procs(Tab, Lang, Fun, Acc) ->
    Pattern = #proc{lang=Lang, client=nil, _='_'},
    MSpec = [{Pattern, [], ['$_']}],
    case ets:select_reverse(Tab, MSpec, 25) of
        '$end_of_table' ->
            {not_found, Acc};
        Continuation ->
            iter_procs(Continuation, Fun, Acc)
    end.

iter_procs({[], Continuation0}, Fun, Acc) ->
    case ets:select_reverse(Continuation0) of
        '$end_of_table' ->
            {not_found, Acc};
        Continuation1 ->
            iter_procs(Continuation1, Fun, Acc)
    end;
iter_procs({[Proc | Rest], Continuation}, Fun, Acc0) ->
    case Fun(Proc, Acc0) of
        {ok, Acc1} ->
            iter_procs({Rest, Continuation}, Fun, Acc1);
        {stop, Acc1} ->
            {ok, Acc1}
    end.

new_proc(From, Lang) ->
    case new_proc_int(From, Lang) of
    {ok, Proc} ->
        exit({ok, Proc, From});
    Error ->
        gen_server:reply(From, {error, Error})
    end.

new_proc(From, Lang, DDoc, DDocKey) ->
    case new_proc_int(From, Lang) of
    {ok, NewProc} ->
        case proc_with_ddoc(DDoc, DDocKey, [NewProc]) of
        {ok, Proc} ->
            exit({ok, Proc, From});
        {error, Reason} ->
            gen_server:reply(From, {error, Reason})
        end;
    Error ->
        gen_server:reply(From, {error, Error})
    end.

new_proc_int(From, Lang) when is_binary(Lang) ->
    new_proc_int(From, binary_to_list(Lang));
new_proc_int(From, Lang) when is_list(Lang) ->
    case couch_config:get("query_servers", Lang) of
    undefined ->
        case couch_config:get("native_query_servers", Lang) of
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

make_proc(Pid, Lang, Mod) ->
    Proc = #proc{
        lang = Lang,
        pid = Pid,
        prompt_fun = {Mod, prompt},
        prompt_many_fun = {Mod, prompt_many},
        set_timeout_fun = {Mod, set_timeout},
        stop_fun = {Mod, stop}
    },
    unlink(Pid),
    {ok, Proc}.

assign_proc(Tab, Client, #proc{client=nil}=Proc0) ->
    Proc = Proc0#proc{client = erlang:monitor(process, Client)},
    ets:insert(Tab, Proc),
    Proc.

return_proc(Tab, #proc{pid=Pid} = Proc) ->
    case is_process_alive(Pid) of true ->
        gen_server:cast(Pid, garbage_collect),
        ets:insert(Tab, Proc#proc{client=nil});
    false ->
        ets:delete(Tab, Pid)
    end.

get_proc_config() ->
    Limit = config:get("query_server_config", "reduce_limit", "true"),
    Timeout = config:get("couchdb", "os_process_timeout", "5000"),
    {[
        {<<"reduce_limit">>, list_to_atom(Limit)},
        {<<"timeout">>, list_to_integer(Timeout)}
    ]}.

proc_with_ddoc(DDoc, DDocKey, Procs) ->
    Filter = fun(#proc{ddoc_keys=Keys}) -> not lists:member(DDocKey, Keys) end,
    case lists:dropwhile(Filter, Procs) of
    [DDocProc|_] ->
        {ok, DDocProc};
    [] ->
        teach_any_proc(DDoc, DDocKey, Procs)
    end.

teach_any_proc(DDoc, DDocKey, [Proc|Rest]) ->
    try
        teach_ddoc(DDoc, DDocKey, Proc)
    catch _:_ ->
        teach_any_proc(DDoc, DDocKey, Rest)
    end;
teach_any_proc(_, _, []) ->
    {error, noproc}.

teach_ddoc(DDoc, {DDocId, _Rev}=DDocKey, #proc{ddoc_keys=Keys}=Proc) ->
    % send ddoc over the wire
    % we only share the rev with the client we know to update code
    % but it only keeps the latest copy, per each ddoc, around.
    true = couch_query_servers:proc_prompt(Proc, [<<"ddoc">>, <<"new">>,
        DDocId, couch_doc:to_json_obj(DDoc, [])]),
    % we should remove any other ddocs keys for this docid
    % because the query server overwrites without the rev
    Keys2 = [{D,R} || {D,R} <- Keys, D /= DDocId],
    % add ddoc to the proc
    {ok, Proc#proc{ddoc_keys=[DDocKey|Keys2]}}.
