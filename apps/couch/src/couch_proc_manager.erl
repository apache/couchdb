-module(couch_proc_manager).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
    code_change/3]).

-export([start_link/0, get_proc_count/0]).

-include("couch_db.hrl").

-record(state, {tab}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_proc_count() ->
    gen_server:call(?MODULE, get_proc_count).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{tab = ets:new(procs, [{keypos, #proc.pid}])}}.

handle_call(get_table, _From, State) ->
    {reply, State#state.tab, State};

handle_call(get_proc_count, _From, State) ->
    {reply, ets:info(State#state.tab, size), State};

handle_call({get_proc, #doc{body={Props}}=DDoc, DDocKey}, {Client, _}, State) ->
    Lang = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    try get_procs(State#state.tab, Lang) of
    Procs ->
        case proc_with_ddoc(DDoc, DDocKey, Procs) of
        {ok, Proc0} ->
            Proc = Proc0#proc{client = erlang:monitor(process, Client)},
            ets:insert(State#state.tab, Proc),
            {reply, {ok, Proc, get_query_server_config()}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
        end
    catch {unknown_query_language, _} ->
        {reply, {unknown_query_language, Lang}, State};
    error:Reason ->
        ?LOG_ERROR("~p ~p ~p", [?MODULE, Reason, erlang:get_stacktrace()]),
        {reply, {error, Reason}, State}
    end;

handle_call({get_proc, Lang}, {Client, _}, State) ->
    try get_procs(State#state.tab, Lang) of
    [Proc0|_] ->
        Proc = Proc0#proc{client = erlang:monitor(process, Client)},
        ets:insert(State#state.tab, Proc),
        {reply, {ok, Proc, get_query_server_config()}, State}
    catch {unknown_query_language, _} ->
        {reply, {unknown_query_language, Lang}, State};
    error:Reason ->
        ?LOG_ERROR("~p ~p ~p", [?MODULE, Reason, erlang:get_stacktrace()]),
        {reply, {error, Reason}, State}
    end;

handle_call({ret_proc, #proc{client=Ref, pid=Pid} = Proc}, _From, State) ->
    erlang:demonitor(Ref, [flush]),
    % We need to check if the process is alive here, as the client could be
    % handing us a #proc{} with a dead one.  We would have already removed the
    % #proc{} from our own table, so the alternative is to do a lookup in the
    % table before the insert.  Don't know which approach is cheaper.
    case is_process_alive(Pid) of true ->
        maybe_reuse_proc(State#state.tab, Proc);
    false -> ok end,
    {reply, true, State};

handle_call(_Call, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    ?LOG_INFO("~p ~p died ~p", [?MODULE, Pid, Reason]),
    ets:delete(State#state.tab, Pid),
    {noreply, State};

handle_info({'DOWN', Ref, _, _, _Reason}, State) ->
    case ets:match_object(State#state.tab, #proc{client=Ref, _='_'}) of
    [] ->
        ok;
    [#proc{pid = Pid} = Proc] ->
        case is_process_alive(Pid) of true ->
            maybe_reuse_proc(State#state.tab, Proc);
        false -> ok end
    end,
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{tab=Tab}) ->
    ets:foldl(fun(#proc{pid=P}, _) -> couch_util:shutdown_sync(P) end, 0, Tab),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

maybe_reuse_proc(Tab, #proc{pid = Pid} = Proc) ->
    Limit = couch_config:get("query_server_config", "os_process_soft_limit", "100"),
    case ets:info(Tab, size) > list_to_integer(Limit) of
    true ->
        ets:delete(Tab, Pid),
        unlink(Pid),
        exit(Pid, kill);
    false ->
        garbage_collect(Pid),
        ets:insert(Tab, Proc#proc{client=nil})
    end.

get_procs(Tab, Lang) when is_binary(Lang) ->
    get_procs(Tab, binary_to_list(Lang));
get_procs(Tab, Lang) when is_list(Lang) ->
    case ets:match_object(Tab, #proc{lang=Lang, client=nil, _='_'}) of
    [] ->
        {ok, NewProc} = new_proc(Lang), % check OS process limit
        [NewProc];
    Procs ->
        Procs
    end.

new_proc(Lang) when is_list(Lang) ->
    case couch_config:get("query_servers", Lang) of
    undefined ->
        case couch_config:get("native_query_servers", Lang) of
        undefined ->
            throw({unknown_query_language, Lang});
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
        set_timeout_fun = {Mod, set_timeout},
        stop_fun = {Mod, stop}
    },
    {ok, Proc}.

get_query_server_config() ->
    Limit = couch_config:get("query_server_config", "reduce_limit", "true"),
    {[{<<"reduce_limit">>, list_to_atom(Limit)}]}.

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
