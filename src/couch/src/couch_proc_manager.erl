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

-export([
    start_link/0,
    get_proc/3,
    get_proc/1,
    ret_proc/1,
    os_proc_idle/1,
    get_proc_count/0,
    get_stale_proc_count/0,
    new_proc/1,
    reload/0,
    terminate_stale_procs/0,
    get_servers_from_env/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

-include_lib("couch/include/couch_db.hrl").

-define(PROCS, couch_proc_manager_procs).
-define(WAITERS, couch_proc_manager_waiters).
-define(OPENING, couch_proc_manager_opening).
-define(SERVERS, couch_proc_manager_servers).
-define(COUNTERS, couch_proc_manager_counters).
-define(IDLE_BY_DB, couch_proc_manager_idle_by_db).
-define(IDLE_ACCESS, couch_proc_manager_idle_access).
-define(RELISTEN_DELAY, 5000).

-record(state, {
    config,
    threshold_ts,
    hard_limit,
    soft_limit
}).

-type docid() :: iodata().
-type revision() :: {integer(), binary()}.

-record(client, {
    wait_key :: {binary(), integer(), gen_server:reply_tag()} | '_',
    from :: undefined | {pid(), gen_server:reply_tag()} | '_',
    lang :: binary() | '_',
    ddoc :: undefined | #doc{} | '_',
    db_key :: undefined | binary(),
    ddoc_key :: undefined | {DDocId :: docid(), Rev :: revision()} | '_'
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_proc(#doc{body = {Props}} = DDoc, DbKey, {_DDocId, _Rev} = DDocKey) ->
    LangStr = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    Lang = couch_util:to_binary(LangStr),
    Client = #client{lang = Lang, ddoc = DDoc, db_key = DbKey, ddoc_key = DDocKey},
    Timeout = get_os_process_timeout(),
    Res = gen_server:call(?MODULE, {get_proc, Client}, Timeout),
    couch_stats:increment_counter([couchdb, query_server, acquired_processes]),
    Res.

get_proc(LangStr) ->
    Lang = couch_util:to_binary(LangStr),
    Client = #client{lang = Lang},
    Timeout = get_os_process_timeout(),
    Res = gen_server:call(?MODULE, {get_proc, Client}, Timeout),
    couch_stats:increment_counter([couchdb, query_server, acquired_processes]),
    Res.

ret_proc(#proc{} = Proc) ->
    gen_server:call(?MODULE, {ret_proc, Proc}, infinity).

os_proc_idle(Proc) when is_pid(Proc) ->
    gen_server:cast(?MODULE, {os_proc_idle, Proc}).

get_proc_count() ->
    try
        ets:info(?PROCS, size) + ets:info(?OPENING, size)
    catch
        error:badarg ->
            0
    end.

get_stale_proc_count() ->
    gen_server:call(?MODULE, get_stale_proc_count).

reload() ->
    gen_server:call(?MODULE, set_threshold_ts).

terminate_stale_procs() ->
    gen_server:call(?MODULE, terminate_stale_procs).

init([]) ->
    process_flag(trap_exit, true),
    ok = config:listen_for_changes(?MODULE, undefined),

    % Main process table. Pid -> #proc{}
    ets:new(?PROCS, [named_table, {read_concurrency, true}, {keypos, #proc.pid}]),

    % #client{} waiters ordered by {Lang, timestamp(), Ref}
    ets:new(?WAITERS, [named_table, ordered_set, {keypos, #client.wait_key}]),

    % Async process openers. Pid -> #client{}
    ets:new(?OPENING, [named_table]),

    % Configured language servers Lang -> Start MFA | Command
    ets:new(?SERVERS, [named_table]),

    % Idle Pids. Ordered to allow partial key lookups {Lang, DbKey, Pid} -> DDocs
    ets:new(?IDLE_BY_DB, [named_table, ordered_set]),

    % Idle Db tagged pids ordered by last use. {Lang, timestamp(), Pid} -> true
    ets:new(?IDLE_ACCESS, [named_table, ordered_set]),

    % Lang -> number of procs spawn for that lang
    ets:new(?COUNTERS, [named_table]),

    ok = configure_language_servers(),

    {ok, #state{
        config = get_proc_config(),
        threshold_ts = timestamp(),
        hard_limit = get_hard_limit(),
        soft_limit = get_soft_limit()
    }}.

terminate(_Reason, _State) ->
    foreach_proc(fun(#proc{pid = P}) -> couch_util:shutdown_sync(P) end).

handle_call(get_stale_proc_count, _From, State) ->
    #state{threshold_ts = T0} = State,
    MatchSpec = [{#proc{threshold_ts = '$1', _ = '_'}, [{'<', '$1', T0}], [true]}],
    {reply, ets:select_count(?PROCS, MatchSpec), State};
handle_call({get_proc, #client{} = Client}, From, State) ->
    add_waiting_client(Client#client{from = From}),
    ok = flush_waiters(State, Client#client.lang),
    {noreply, State};
handle_call({ret_proc, #proc{} = Proc}, From, State) ->
    #proc{client = Ref, pid = Pid} = Proc,
    erlang:demonitor(Ref, [flush]),
    gen_server:reply(From, true),
    case ets:lookup(?PROCS, Pid) of
        [#proc{} = ProcInt] ->
            ok = return_proc(State, ProcInt);
        [] ->
            % Proc must've died and we already
            % cleared it out of the table in
            % the handle_info clause.
            ok
    end,
    {noreply, State};
handle_call(set_threshold_ts, _From, State) ->
    Fun = fun
        (#proc{client = undefined} = Proc) -> ok = remove_proc(Proc);
        (_) -> ok
    end,
    ok = foreach_proc(Fun),
    {reply, ok, State#state{threshold_ts = timestamp()}};
handle_call(terminate_stale_procs, _From, #state{threshold_ts = Ts1} = State) ->
    Fun = fun
        (#proc{client = undefined, threshold_ts = Ts2} = Proc) ->
            case Ts1 > Ts2 of
                true -> ok = remove_proc(Proc);
                false -> ok
            end;
        (_) ->
            ok
    end,
    foreach_proc(Fun),
    {reply, ok, State};
handle_call(_Call, _From, State) ->
    {reply, ignored, State}.

handle_cast({os_proc_idle, Pid}, #state{soft_limit = SoftLimit} = State) ->
    case ets:lookup(?PROCS, Pid) of
        [#proc{client = undefined, db_key = DbKey, lang = Lang} = Proc] ->
            IsOverSoftLimit = get_count(Lang) >= SoftLimit,
            IsTagged = DbKey =/= undefined,
            case IsOverSoftLimit orelse IsTagged of
                true ->
                    couch_log:debug("Closing tagged or idle OS Process: ~p", [Pid]),
                    ok = remove_proc(Proc);
                false ->
                    ok
            end;
        _ ->
            State
    end,
    {noreply, State};
handle_cast(reload_config, State) ->
    NewState = State#state{
        config = get_proc_config(),
        hard_limit = get_hard_limit(),
        soft_limit = get_soft_limit()
    },
    ok = configure_language_servers(),
    lists:foreach(
        fun({Lang, _}) ->
            ok = flush_waiters(NewState, Lang)
        end,
        ets:tab2list(?COUNTERS)
    ),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(shutdown, State) ->
    {stop, shutdown, State};
handle_info({'EXIT', Pid, {spawn_ok, Proc0, undefined = _From}}, State) ->
    % Use ets:take/2 to assert that opener existed before removing. Also assert that
    % the pid matches and the client was a bogus client
    [{Pid, #client{from = undefined}}] = ets:take(?OPENING, Pid),
    Proc = Proc0#proc{client = undefined},
    link(Proc#proc.pid),
    ets:insert(?PROCS, Proc),
    insert_idle_by_db(Proc),
    {noreply, State};
handle_info({'EXIT', Pid, {spawn_ok, Proc0, {ClientPid, _} = From}}, State) ->
    % Use ets:take/2 to assert that opener existed before removing
    [{Pid, #client{}}] = ets:take(?OPENING, Pid),
    link(Proc0#proc.pid),
    Proc = assign_proc(ClientPid, Proc0),
    gen_server:reply(From, {ok, Proc, State#state.config}),
    {noreply, State};
handle_info({'EXIT', Pid, spawn_error}, State) ->
    % Assert when removing that we always expect the opener to have been there
    [{Pid, #client{lang = Lang}}] = ets:take(?OPENING, Pid),
    dec_count(Lang),
    ok = flush_waiters(State, Lang),
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    couch_log:info("~p ~p died ~p", [?MODULE, Pid, Reason]),
    case ets:lookup(?PROCS, Pid) of
        [#proc{} = Proc] ->
            ok = remove_proc(Proc),
            ok = flush_waiters(State, Proc#proc.lang);
        [] ->
            ok
    end,
    {noreply, State};
handle_info({'DOWN', Ref, _, _, _Reason}, #state{} = State) ->
    case ets:match_object(?PROCS, #proc{client = Ref, _ = '_'}) of
        [#proc{} = Proc] -> ok = return_proc(State, Proc);
        [] -> ok
    end,
    {noreply, State};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    gen_server:cast(?MODULE, reload_config),
    erlang:send_after(?RELISTEN_DELAY, whereis(?MODULE), restart_config_listener).

handle_config_change("native_query_servers", _, _, _, _) ->
    gen_server:cast(?MODULE, reload_config),
    {ok, undefined};
handle_config_change("query_server_config", _, _, _, _) ->
    gen_server:cast(?MODULE, reload_config),
    {ok, undefined};
handle_config_change("couchdb", "js_engine", _, _, _) ->
    gen_server:cast(?MODULE, reload_config),
    % Besides reloading the config to switch the engine for the new procs, run
    % reload to kill all free and then released procs in the pool. This way we
    % ensure all the procs will be recycled and using the new engine as soon as
    % possible
    spawn(fun() -> reload() end),
    {ok, undefined};
handle_config_change(_, _, _, _, _) ->
    {ok, undefined}.

find_proc(#client{ddoc_key = undefined} = Client, _CanSpawn) ->
    #client{lang = Lang} = Client,
    % Find an unowned process first, if that fails find an owned one
    case find_proc(Lang, undefined, '_') of
        {ok, Proc} ->
            {ok, Proc};
        not_found ->
            case find_proc(Lang, '_', '_') of
                {ok, Proc} ->
                    {ok, Proc};
                Else ->
                    Else
            end;
        Else ->
            Else
    end;
find_proc(#client{} = Client, CanSpawn) ->
    #client{
        lang = Lang,
        ddoc = DDoc,
        db_key = DbKey,
        ddoc_key = DDocKey
    } = Client,
    case find_proc(Lang, DbKey, DDocKey) of
        not_found ->
            % Find a ddoc process used by the same db at least
            case find_proc(Lang, DbKey, '_') of
                {ok, Proc} ->
                    teach_ddoc(DDoc, DbKey, DDocKey, Proc);
                not_found ->
                    % Pick a process not used by any ddoc
                    case find_proc(Lang, undefined, '_') of
                        {ok, Proc} ->
                            replenish_untagged_pool(Lang, CanSpawn),
                            teach_ddoc(DDoc, DbKey, DDocKey, Proc);
                        Else ->
                            Else
                    end;
                Else ->
                    Else
            end;
        {ok, Proc} ->
            {ok, Proc};
        Else ->
            Else
    end.

find_proc(Lang, DbPat, DDocKey) when
    DbPat =:= '_' orelse DbPat =:= undefined orelse is_binary(DbPat),
    DDocKey =:= '_' orelse is_tuple(DDocKey)
->
    Pattern = {{Lang, DbPat, '$1'}, '$2'},
    Guards =
        case DDocKey of
            '_' -> [];
            {_, _} -> [{map_get, {const, DDocKey}, '$2'}]
        end,
    MSpec = [{Pattern, Guards, ['$1']}],
    case ets:select_reverse(?IDLE_BY_DB, MSpec, 1) of
        '$end_of_table' ->
            not_found;
        {[Pid], _Continuation} when is_pid(Pid) ->
            [#proc{client = undefined} = Proc] = ets:lookup(?PROCS, Pid),
            % Once it's found it's not idle any longer and it might be
            % "tought" a new ddoc, so its db_key might change
            remove_idle_by_db(Proc),
            remove_idle_access(Proc),
            {ok, Proc}
    end.

spawn_proc(#client{} = Client) ->
    Pid = spawn_link(?MODULE, new_proc, [Client]),
    ets:insert(?OPENING, {Pid, Client}),
    inc_count(Client#client.lang).

% This instance was spawned without a client to replenish
% the untagged pool asynchronously
new_proc(#client{from = undefined} = Client) ->
    #client{lang = Lang} = Client,
    Resp =
        try
            case new_proc_int(undefined, Lang) of
                {ok, Proc} ->
                    {spawn_ok, Proc, undefined};
                _Error ->
                    spawn_error
            end
        catch
            _:_ ->
                spawn_error
        end,
    exit(Resp);
new_proc(#client{ddoc = undefined, ddoc_key = undefined} = Client) ->
    #client{from = From, lang = Lang} = Client,
    Resp =
        try
            case new_proc_int(From, Lang) of
                {ok, Proc} ->
                    {spawn_ok, Proc, From};
                Error ->
                    gen_server:reply(From, {error, Error}),
                    spawn_error
            end
        catch
            _:_ ->
                spawn_error
        end,
    exit(Resp);
new_proc(#client{} = Client) ->
    #client{
        from = From,
        lang = Lang,
        ddoc = DDoc,
        db_key = DbKey,
        ddoc_key = DDocKey
    } = Client,
    Resp =
        try
            case new_proc_int(From, Lang) of
                {ok, NewProc} ->
                    {ok, Proc} = teach_ddoc(DDoc, DbKey, DDocKey, NewProc),
                    {spawn_ok, Proc, From};
                Error ->
                    gen_server:reply(From, {error, Error}),
                    spawn_error
            end
        catch
            _:_ ->
                spawn_error
        end,
    exit(Resp).

replenish_untagged_pool(Lang, _CanSpawn = true) ->
    % After an untagged instance is tagged, we try to replenish
    % the untagged pool asynchronously. Here we are using a "bogus"
    % #client{} with an undefined from field.
    ok = spawn_proc(#client{lang = Lang, from = undefined});
replenish_untagged_pool(_Lang, _CanSpawn = false) ->
    ok.

reap_idle(Num, <<_/binary>> = Lang) when is_integer(Num), Num >= 1 ->
    case ets:match_object(?IDLE_ACCESS, {{Lang, '_', '_'}, '_'}, Num) of
        '$end_of_table' ->
            0;
        {Objects = [_ | _], _} ->
            ok = reap_idle(Objects),
            length(Objects)
    end.

reap_idle([]) ->
    ok;
reap_idle([{{_Lang, _Ts, Pid}, true} | Rest]) ->
    case ets:lookup(?PROCS, Pid) of
        % Do an extra assert that client is undefined
        [#proc{client = undefined} = Proc] ->
            ok = remove_proc(Proc);
        [] ->
            ok
    end,
    reap_idle(Rest).

insert_idle_access(#proc{db_key = undefined}, _Ts) ->
    % Only tagged proc are index in ?IDLE_ACCESS
    ok;
insert_idle_access(#proc{db_key = <<_/binary>>} = Proc, Ts) ->
    #proc{lang = Lang, pid = Pid} = Proc,
    % Lang is used for partially bound key access
    % Pid is for uniqueness as time is not strictly monotonic
    true = ets:insert_new(?IDLE_ACCESS, {{Lang, Ts, Pid}, true}),
    ok.

remove_idle_access(#proc{db_key = undefined}) ->
    % Only tagged procs are indexed in ?IDLE_ACCESS
    ok;
remove_idle_access(#proc{db_key = <<_/binary>>} = Proc) ->
    #proc{last_use_ts = Ts, lang = Lang, pid = Pid} = Proc,
    true = ets:delete(?IDLE_ACCESS, {Lang, Ts, Pid}),
    ok.

insert_idle_by_db(#proc{} = Proc) ->
    #proc{lang = Lang, pid = Pid, db_key = Db, ddoc_keys = #{} = DDocs} = Proc,
    % An extra assert that only expect to insert a new object
    true = ets:insert_new(?IDLE_BY_DB, {{Lang, Db, Pid}, DDocs}),
    ok.

remove_idle_by_db(#proc{} = Proc) ->
    #proc{lang = Lang, pid = Pid, db_key = Db} = Proc,
    true = ets:delete(?IDLE_BY_DB, {Lang, Db, Pid}),
    ok.

split_string_if_longer(String, Pos) ->
    case length(String) > Pos of
        true -> lists:split(Pos, String);
        false -> false
    end.

split_by_char(String, Char) ->
    %% 17.5 doesn't have string:split
    %% the function doesn't handle errors
    %% it is designed to be used only in specific context
    Pos = string:chr(String, Char),
    {Key, [_Eq | Value]} = lists:split(Pos - 1, String),
    {Key, Value}.

get_servers_from_env(Spec) ->
    SpecLen = length(Spec),
    % loop over os:getenv(), match SPEC_
    lists:filtermap(
        fun(EnvStr) ->
            case split_string_if_longer(EnvStr, SpecLen) of
                {Spec, Rest} ->
                    {true, split_by_char(Rest, $=)};
                _ ->
                    false
            end
        end,
        os:getenv()
    ).

configure_language_servers() ->
    ets:delete_all_objects(?SERVERS),
    % NOTE: process environment variables cannot be altered except by
    % debugging the process or via os:putenv/2 calls.
    ets:insert(?SERVERS, get_servers_from_env("COUCHDB_QUERY_SERVER_")),
    ets:insert(?SERVERS, get_servers_from_env("COUCHDB_NATIVE_QUERY_SERVER_")),
    ets:insert(?SERVERS, [{"QUERY", {mango_native_proc, start_link, []}}]),
    maybe_configure_erlang_native_servers(),
    configure_js_engine(couch_server:get_js_engine()),
    ok.

get_query_server(LangStr) ->
    case ets:lookup(?SERVERS, string:to_upper(LangStr)) of
        [{_, Command}] -> Command;
        _ -> undefined
    end.

native_query_server_enabled() ->
    % 1. [native_query_server] enable_erlang_query_server = true | false
    % 2. if [native_query_server] erlang == {couch_native_process, start_link, []} -> pretend true as well
    NativeEnabled = config:get_boolean("native_query_servers", "enable_erlang_query_server", false),
    NativeLegacyConfig = config:get("native_query_servers", "erlang", ""),
    NativeLegacyEnabled = NativeLegacyConfig =:= "{couch_native_process, start_link, []}",
    NativeEnabled orelse NativeLegacyEnabled.

maybe_configure_erlang_native_servers() ->
    case native_query_server_enabled() of
        true ->
            ets:insert(?SERVERS, [
                {"ERLANG", {couch_native_process, start_link, []}}
            ]);
        _Else ->
            ok
    end.

configure_js_engine(<<"quickjs">>) ->
    ets:insert(?SERVERS, [
        {"JAVASCRIPT", couch_quickjs:mainjs_cmd()},
        {"COFFEESCRIPT", couch_quickjs:coffee_cmd()},
        {"JAVASCRIPT_QUICKJS", couch_quickjs:mainjs_cmd()}
    ]),
    case couch_server:with_spidermonkey() of
        true ->
            SM_ENV = os:getenv("COUCHDB_QUERY_SERVER_JAVASCRIPT"),
            ets:insert(?SERVERS, {"JAVASCRIPT_SPIDERMONKEY", SM_ENV});
        false ->
            ok
    end;
configure_js_engine(<<"spidermonkey">>) ->
    ets:insert(?SERVERS, [
        {"JAVASCRIPT_QUICKJS", couch_quickjs:mainjs_cmd()},
        {"JAVASCRIPT_SPIDERMONKEY", os:getenv("COUCHDB_QUERY_SERVER_JAVASCRIPT")}
    ]).

new_proc_int(From, Lang) when is_binary(Lang) ->
    LangStr = binary_to_list(Lang),
    case get_query_server(LangStr) of
        undefined ->
            case From of
                undefined -> ok;
                {_, _} -> gen_server:reply(From, {unknown_query_language, Lang})
            end;
        {M, F, A} ->
            {ok, Pid} = apply(M, F, A),
            make_proc(Pid, Lang, M);
        Command ->
            {ok, Pid} = couch_os_process:start_link(Command),
            make_proc(Pid, Lang, couch_os_process)
    end.

teach_ddoc(DDoc, DbKey, {DDocId, _Rev} = DDocKey, #proc{ddoc_keys = #{} = Keys} = Proc) ->
    % send ddoc over the wire
    % we only share the rev with the client we know to update code
    % but it only keeps the latest copy, per each ddoc, around.
    JsonDoc = couch_doc:to_json_obj(DDoc, []),
    Prompt = [<<"ddoc">>, <<"new">>, DDocId, JsonDoc],
    true = couch_query_servers:proc_prompt(Proc, Prompt),
    % we should remove any other ddocs keys for this docid
    % because the query server overwrites without the rev
    Keys2 = maps:filter(fun({Id, _}, true) -> Id =/= DDocId end, Keys),

    % add ddoc to the proc
    {ok, Proc#proc{db_key = DbKey, ddoc_keys = Keys2#{DDocKey => true}}}.

make_proc(Pid, Lang, Mod) when is_binary(Lang) ->
    Proc = #proc{
        lang = Lang,
        pid = Pid,
        prompt_fun = {Mod, prompt},
        set_timeout_fun = {Mod, set_timeout},
        stop_fun = {Mod, stop},
        threshold_ts = timestamp(),
        last_use_ts = timestamp()
    },
    unlink(Pid),
    {ok, Proc}.

assign_proc(Pid, #proc{client = undefined} = Proc0) when is_pid(Pid) ->
    Proc = Proc0#proc{client = erlang:monitor(process, Pid)},
    % It's important to insert the proc here instead of doing an update_element
    % as we might have updated the db_key or ddoc_keys in teach_ddoc/4
    ets:insert(?PROCS, Proc),
    Proc;
assign_proc(#client{} = Client, #proc{client = undefined} = Proc) ->
    {Pid, _} = Client#client.from,
    assign_proc(Pid, Proc).

return_proc(#state{} = State, #proc{} = Proc) ->
    #proc{pid = Pid, lang = Lang} = Proc,
    case is_process_alive(Pid) of
        true ->
            case Proc#proc.threshold_ts < State#state.threshold_ts of
                true ->
                    ok = remove_proc(Proc);
                false ->
                    gen_server:cast(Pid, garbage_collect),
                    Ts = timestamp(),
                    true = ets:update_element(?PROCS, Pid, [
                        {#proc.client, undefined},
                        {#proc.last_use_ts, Ts}
                    ]),
                    Proc1 = Proc#proc{client = undefined, last_use_ts = Ts},
                    insert_idle_access(Proc1, Ts),
                    insert_idle_by_db(Proc1)
            end;
        false ->
            ok = remove_proc(Proc)
    end,
    ok = flush_waiters(State, Lang).

remove_proc(#proc{pid = Pid} = Proc) ->
    remove_idle_access(Proc),
    remove_idle_by_db(Proc),
    ets:delete(?PROCS, Pid),
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            gen_server:cast(Pid, stop);
        false ->
            ok
    end,
    dec_count(Proc#proc.lang).

flush_waiters(#state{} = State, Lang) ->
    #state{hard_limit = HardLimit, config = {[_ | _] = Cfg}} = State,
    TimeoutMSec = couch_util:get_value(<<"timeout">>, Cfg),
    Timeout = erlang:convert_time_unit(TimeoutMSec, millisecond, native),
    StaleLimit = timestamp() - Timeout,
    case get_waiting_client(Lang) of
        #client{wait_key = {_, T, _}} = Client when is_integer(T), T < StaleLimit ->
            % Client waited too long and the gen_server call timeout
            % likey fired already, don't bother allocating a process for it
            remove_waiting_client(Client),
            flush_waiters(State, Lang);
        #client{from = From} = Client ->
            CanSpawn = get_count(Lang) < HardLimit,
            case find_proc(Client, CanSpawn) of
                {ok, Proc0} ->
                    Proc = assign_proc(Client, Proc0),
                    gen_server:reply(From, {ok, Proc, State#state.config}),
                    remove_waiting_client(Client),
                    flush_waiters(State, Lang);
                {error, Error} ->
                    gen_server:reply(From, {error, Error}),
                    remove_waiting_client(Client),
                    flush_waiters(State, Lang);
                not_found when CanSpawn ->
                    ok = spawn_proc(Client),
                    remove_waiting_client(Client),
                    flush_waiters(State, Lang);
                not_found ->
                    % 10% of limit
                    ReapBatch = round(HardLimit * 0.1 + 1),
                    case reap_idle(ReapBatch, Lang) of
                        N when is_integer(N), N > 0 ->
                            % We may have room available to spawn
                            case get_count(Lang) < HardLimit of
                                true ->
                                    ok = spawn_proc(Client),
                                    remove_waiting_client(Client),
                                    flush_waiters(State, Lang);
                                false ->
                                    ok
                            end;
                        0 ->
                            ok
                    end
            end;
        undefined ->
            ok
    end.

add_waiting_client(#client{from = {_Pid, Tag}, lang = Lang} = Client) ->
    % Use Lang in the key first since we can look it up using a partially bound
    % in get_waiting_client/2. Use the reply tag to provide uniqueness.
    Key = {Lang, timestamp(), Tag},
    true = ets:insert_new(?WAITERS, Client#client{wait_key = Key}).

-spec get_waiting_client(Lang :: binary()) -> undefined | #client{}.
get_waiting_client(Lang) ->
    % Use a partially bound key (Lang) to avoid scanning unrelated procs
    Key = {Lang, '_', '_'},
    case ets:match_object(?WAITERS, #client{wait_key = Key, _ = '_'}, 1) of
        '$end_of_table' ->
            undefined;
        {[#client{} = Client], _} ->
            Client
    end.

remove_waiting_client(#client{wait_key = Key}) ->
    ets:delete(?WAITERS, Key).

get_proc_config() ->
    Limit = config:get_boolean("query_server_config", "reduce_limit", true),
    Timeout = get_os_process_timeout(),
    {[
        {<<"reduce_limit">>, Limit},
        {<<"timeout">>, Timeout}
    ]}.

get_hard_limit() ->
    config:get_integer("query_server_config", "os_process_limit", 100).

get_soft_limit() ->
    config:get_integer("query_server_config", "os_process_soft_limit", 100).

get_os_process_timeout() ->
    config:get_integer("couchdb", "os_process_timeout", 5000).

timestamp() ->
    erlang:monotonic_time().

foreach_proc(Fun) when is_function(Fun, 1) ->
    FoldFun = fun(#proc{} = Proc, ok) ->
        Fun(Proc),
        ok
    end,
    ok = ets:foldl(FoldFun, ok, ?PROCS).

inc_count(Lang) ->
    ets:update_counter(?COUNTERS, Lang, 1, {Lang, 0}),
    ok.

dec_count(Lang) ->
    ets:update_counter(?COUNTERS, Lang, -1, {Lang, 0}),
    ok.

get_count(Lang) ->
    case ets:lookup(?COUNTERS, Lang) of
        [{_, Count}] when is_integer(Count) ->
            Count;
        [] ->
            0
    end.
