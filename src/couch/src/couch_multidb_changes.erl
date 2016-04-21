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

-module(couch_multidb_changes).

-behaviour(gen_server).

-export([
   start_link/4
]).

-export([
   init/1,
   terminate/2,
   handle_call/3,
   handle_info/2,
   handle_cast/2,
   code_change/3
]).

-export([
   changes_reader/3,
   changes_reader_cb/3
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-define(CTX, {user_ctx, #user_ctx{roles=[<<"_admin">>, <<"_replicator">>]}}).

-define(AVG_DELAY_MSEC, 100).
-define(MAX_DELAY_MSEC, 60000).

-record(state, {
    tid :: ets:tid(),
    mod :: atom(),
    ctx :: term(),
    suffix :: binary(),
    event_server :: reference(),
    scanner :: nil | pid(),
    pids :: [{binary(), pid()}],
    skip_ddocs :: boolean()
}).

% Behavior API

% For each db shard with a matching suffix, report created,
% deleted, found (discovered) and change events.

-callback db_created(DbName :: binary(), Context :: term()) ->
    Context :: term().

-callback db_deleted(DbName :: binary(), Context :: term()) ->
    Context :: term().

-callback db_found(DbName :: binary(), Context :: term()) ->
    Context :: term().

-callback db_change(DbName :: binary(), Change :: term(), Context :: term()) ->
    Context :: term().


% External API


% Opts list can contain:
%  - `skip_ddocs` : Skip design docs

-spec start_link(binary(), module(), term(), list()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(DbSuffix, Module, Context, Opts) when
    is_binary(DbSuffix), is_atom(Module), is_list(Opts) ->
    gen_server:start_link(?MODULE, [DbSuffix, Module, Context, Opts], []).


% gen_server callbacks

init([DbSuffix, Module, Context, Opts]) ->
    process_flag(trap_exit, true),
    Server = self(),
    {ok, #state{
        tid = ets:new(?MODULE, [set, protected]),
        mod = Module,
        ctx = Context,
        suffix = DbSuffix,
        event_server = register_with_event_server(Server),
        scanner = spawn_link(fun() -> scan_all_dbs(Server, DbSuffix) end),
        pids = [],
        skip_ddocs = proplists:is_defined(skip_ddocs, Opts)
    }}.


terminate(_Reason, _State) ->
    ok.


handle_call({change, DbName, Change}, _From,
    #state{skip_ddocs=SkipDDocs, mod=Mod, ctx=Ctx} = State) ->
    case {SkipDDocs, is_design_doc(Change)} of
        {true, true} ->
            {reply, ok, State};
        {_, _} ->
            {reply, ok, State#state{ctx=Mod:db_change(DbName, Change, Ctx)}}
    end;

handle_call({checkpoint, DbName, EndSeq}, _From, #state{tid=Ets} = State) ->
    case ets:lookup(Ets, DbName) of
        [] ->
            true = ets:insert(Ets, {DbName, EndSeq, false});
        [{DbName, _OldSeq, Rescan}] ->
            true = ets:insert(Ets, {DbName, EndSeq, Rescan})
    end,
    {reply, ok, State}.


handle_cast({resume_scan, DbName}, State) ->
    {noreply, resume_scan(DbName, State)}.


handle_info({'$couch_event', DbName, Event}, #state{suffix = Suf} = State) ->
    case Suf =:= couch_db:dbname_suffix(DbName) of
        true ->
            {noreply, db_callback(Event, DbName, State)};
        _ ->
            {noreply, State}
    end;

handle_info({'DOWN', Ref, _, _, Info}, #state{event_server = Ref} = State) ->
    {stop, {couch_event_server_died, Info}, State};

handle_info({'EXIT', From, normal}, #state{scanner = From} = State) ->
    {noreply, State#state{scanner=nil}};

handle_info({'EXIT', From, Reason}, #state{scanner = From} = State) ->
    {stop, {scanner_died, Reason}, State};

handle_info({'EXIT', From, Reason}, #state{pids = Pids} = State) ->
    couch_log:debug("~p change feed exited ~p", [State#state.suffix, From]),
    case lists:keytake(From, 2, Pids) of
        {value, {DbName, From}, NewPids} ->
            if Reason == normal -> ok; true ->
                Fmt = "~s : Known change feed ~w died :: ~w",
                couch_log:error(Fmt, [?MODULE, From, Reason])
            end,
            NewState = State#state{pids = NewPids},
            case ets:lookup(State#state.tid, DbName) of
                [{DbName, _EndSeq, true}] ->
                    {noreply, resume_scan(DbName, NewState)};
                _ ->
                    {noreply, NewState}
            end;
        false when Reason == normal ->
            {noreply, State};
        false ->
            Fmt = "~s(~p) : Unknown pid ~w died :: ~w",
            couch_log:error(Fmt, [?MODULE, State#state.suffix, From, Reason]),
            {stop, {unexpected_exit, From, Reason}, State}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% Private functions

-spec register_with_event_server(pid()) -> reference().
register_with_event_server(Server) ->
    Ref = erlang:monitor(process, couch_event_server),
    couch_event:register_all(Server),
    Ref.


-spec db_callback(created | deleted | updated, binary(), #state{}) -> #state{}.
db_callback(created, DbName, #state{mod = Mod, ctx = Ctx} = State) ->
    State#state{ctx = Mod:db_created(DbName, Ctx)};
db_callback(deleted, DbName, #state{mod = Mod, ctx = Ctx} = State) ->
    State#state{ctx = Mod:db_deleted(DbName, Ctx)};
db_callback(updated, DbName, State) ->
    resume_scan(DbName, State);
db_callback(_Other, _DbName, State) ->
    State.


-spec resume_scan(binary(), #state{}) -> #state{}.
resume_scan(DbName, #state{pids=Pids, tid=Ets} = State) ->
    case {lists:keyfind(DbName, 1, Pids), ets:lookup(Ets, DbName)} of
        {{DbName, _}, []} ->
            % Found existing change feed, but not entry in ETS
            % Flag a need to rescan from begining
            true = ets:insert(Ets, {DbName, 0, true}),
            State;
        {{DbName, _}, [{DbName, EndSeq, _}]} ->
            % Found existing change feed and entry in ETS
            % Flag a need to rescan from last ETS checkpoint
            true = ets:insert(Ets, {DbName, EndSeq, true}),
            State;
        {false, []} ->
            % No existing change feed running. No entry in ETS.
            % This is first time seeing this db shard.
            % Notify user with a found callback. Insert checkpoint
            % entry in ETS to start from 0. And start a change feed.
            true = ets:insert(Ets, {DbName, 0, false}),
            Mod = State#state.mod,
            Ctx = Mod:db_found(DbName, State#state.ctx),
            Pid = start_changes_reader(DbName, 0),
            State#state{ctx=Ctx, pids=[{DbName, Pid} | Pids]};
        {false, [{DbName, EndSeq, _}]} ->
            % No existing change feed running. Found existing checkpoint.
            % Start a new change reader from last checkpoint.
            true = ets:insert(Ets, {DbName, EndSeq, false}),
            Pid = start_changes_reader(DbName, EndSeq),
            State#state{pids=[{DbName, Pid} | Pids]}
     end.


start_changes_reader(DbName, Since) ->
    spawn_link(?MODULE, changes_reader, [self(), DbName, Since]).


changes_reader(Server, DbName, Since) ->
    {ok, Db} = couch_db:open_int(DbName, [?CTX, sys_db]),
    ChangesArgs = #changes_args{
        include_docs = true,
        since = Since,
        feed = "normal",
        timeout = infinity
    },
    ChFun = couch_changes:handle_db_changes(ChangesArgs, {json_req, null}, Db),
    ChFun({fun ?MODULE:changes_reader_cb/3, {Server, DbName}}).


changes_reader_cb({change, Change, _}, _, {Server, DbName}) ->
    ok = gen_server:call(Server, {change, DbName, Change}, infinity),
    {Server, DbName};
changes_reader_cb({stop, EndSeq}, _, {Server, DbName}) ->
    ok = gen_server:call(Server, {checkpoint, DbName, EndSeq}, infinity),
    {Server, DbName};
changes_reader_cb(_, _, Acc) ->
    Acc.


scan_all_dbs(Server, DbSuffix) when is_pid(Server) ->
    ok = scan_local_db(Server, DbSuffix),
    {ok, Db} = mem3_util:ensure_exists(
        config:get("mem3", "shards_db", "_dbs")),
    ChangesFun = couch_changes:handle_changes(#changes_args{}, nil, Db, nil),
    ChangesFun(fun({change, {Change}, _}, _) ->
        DbName = couch_util:get_value(<<"id">>, Change),
        case DbName of
            <<"_design/", _/binary>> ->
                ok;
            _Else ->
                case couch_replicator_utils:is_deleted(Change) of
                    true ->
                        ok;
                    false ->
                        [gen_server:cast(Server, {resume_scan, ShardName})
                         || ShardName <- filter_shards(DbName, DbSuffix)],
                        ok
                end
        end;
        (_, _) -> ok
    end),
    couch_db:close(Db).


filter_shards(DbName, DbSuffix) ->
    case DbSuffix =:= couch_db:dbname_suffix(DbName) of
    false ->
        [];
    true ->
        try
            [ShardName || #shard{name = ShardName} <- mem3:local_shards(DbName)]
        catch
            error:database_does_not_exist ->
                []
        end
    end.


scan_local_db(Server, DbSuffix) when is_pid(Server) ->
    case couch_db:open_int(DbSuffix, [?CTX, sys_db, nologifmissing]) of
        {ok, Db} ->
            gen_server:cast(Server, {resume_scan, DbSuffix}),
            ok = couch_db:close(Db);
        _Error ->
            ok
    end.


is_design_doc({Change}) ->
    case lists:keyfind(<<"id">>, 1, Change) of
        false ->
            false;
        {_, Id} ->
            is_design_doc_id(Id)
    end.


is_design_doc_id(<<?DESIGN_DOC_PREFIX, _/binary>>) ->
    true;
is_design_doc_id(_) ->
    false.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_eunit.hrl").

-define(MOD, multidb_test_module).
-define(SUFFIX, <<"suff">>).
-define(DBNAME, <<"shards/40000000-5fffffff/acct/suff.0123456789">>).

couch_multidb_changes_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            t_handle_call_change(),
            t_handle_call_change_filter_design_docs(),
            t_handle_call_checkpoint_new(),
            t_handle_call_checkpoint_existing(),
            t_handle_info_created(),
            t_handle_info_deleted(),
            t_handle_info_updated(),
            t_handle_info_other_event(),
            t_handle_info_created_other_db(),
            t_handle_info_scanner_exit_normal(),
            t_handle_info_scanner_crashed(),
            t_handle_info_event_server_exited(),
            t_handle_info_unknown_pid_exited(),
            t_handle_info_change_feed_exited(),
            t_handle_info_change_feed_exited_and_need_rescan(),
            t_spawn_changes_reader(),
            t_changes_reader_cb_change(),
            t_changes_reader_cb_stop(),
            t_changes_reader_cb_other(),
            t_handle_call_resume_scan_no_chfeed_no_ets_entry(),
            t_handle_call_resume_scan_chfeed_no_ets_entry(),
            t_handle_call_resume_scan_chfeed_ets_entry(),
            t_handle_call_resume_scan_no_chfeed_ets_entry(),
            t_start_link(),
            t_start_link_no_ddocs(),
            t_misc_gen_server_callbacks()
        ]
    }.


setup() ->
    mock_logs(),
    mock_callback_mod(),
    meck:expect(couch_event, register_all, 1, ok),
    meck:expect(config, get, ["mem3", "shards_db", '_'], "_dbs"),
    meck:expect(mem3_util, ensure_exists, 1, {ok, dbs}),
    ChangesFun = meck:val(fun(_) -> ok end),
    meck:expect(couch_changes, handle_changes, 4, ChangesFun),
    meck:expect(couch_db, open_int,
        fun(?DBNAME, [?CTX, sys_db]) -> {ok, db};
            (_, _) -> {not_found, no_db_file}
        end),
    meck:expect(couch_db, close, 1, ok),
    mock_changes_reader(),
    % create process to stand in for couch_event_server
    % mocking erlang:monitor doesn't work, so give it real process to monitor
    EvtPid = spawn_link(fun() -> receive looper -> ok end end),
    true = register(couch_event_server, EvtPid),
    EvtPid.


teardown(EvtPid) ->
    unlink(EvtPid),
    exit(EvtPid, kill),
    meck:unload().


t_handle_call_change() ->
    ?_test(begin
        State = mock_state(),
        Change = change_row(<<"blah">>),
        handle_call_ok({change, ?DBNAME, Change}, State),
        ?assert(meck:validate(?MOD)),
        ?assert(meck:called(?MOD, db_change, [?DBNAME, Change, zig]))
    end).


t_handle_call_change_filter_design_docs() ->
    ?_test(begin
        State0 = mock_state(),
        State = State0#state{skip_ddocs = true},
        Change = change_row(<<"_design/blah">>),
        handle_call_ok({change, ?DBNAME, Change}, State),
        ?assert(meck:validate(?MOD)),
        ?assertNot(meck:called(?MOD, db_change, [?DBNAME, Change, zig]))
    end).


t_handle_call_checkpoint_new() ->
    ?_test(begin
        Tid = mock_ets(),
        State = mock_state(Tid),
        handle_call_ok({checkpoint, ?DBNAME, 1}, State),
        ?assertEqual([{?DBNAME, 1, false}], ets:tab2list(Tid)),
        ets:delete(Tid)
    end).


t_handle_call_checkpoint_existing() ->
    ?_test(begin
        Tid = mock_ets(),
        State = mock_state(Tid),
        true = ets:insert(Tid, {?DBNAME, 1, true}),
        handle_call_ok({checkpoint, ?DBNAME, 2}, State),
        ?assertEqual([{?DBNAME, 2, true}], ets:tab2list(Tid)),
        ets:delete(Tid)
    end).


t_handle_info_created() ->
    ?_test(begin
        State = mock_state(),
        handle_info_check({'$couch_event', ?DBNAME, created}, State),
        ?assert(meck:validate(?MOD)),
        ?assert(meck:called(?MOD, db_created, [?DBNAME, zig]))
    end).


t_handle_info_deleted() ->
     ?_test(begin
        State = mock_state(),
        handle_info_check({'$couch_event', ?DBNAME, deleted}, State),
        ?assert(meck:validate(?MOD)),
        ?assert(meck:called(?MOD, db_deleted, [?DBNAME, zig]))
    end).


t_handle_info_updated() ->
     ?_test(begin
        Tid = mock_ets(),
        State = mock_state(Tid),
        handle_info_check({'$couch_event', ?DBNAME, updated}, State),
        ?assert(meck:validate(?MOD)),
        ?assert(meck:called(?MOD, db_found, [?DBNAME, zig]))
    end).


t_handle_info_other_event() ->
     ?_test(begin
        State = mock_state(),
        handle_info_check({'$couch_event', ?DBNAME, somethingelse}, State),
        ?assertNot(meck:called(?MOD, db_created, [?DBNAME, somethingelse])),
        ?assertNot(meck:called(?MOD, db_deleted, [?DBNAME, somethingelse])),
        ?assertNot(meck:called(?MOD, db_found, [?DBNAME, somethingelse]))
    end).


t_handle_info_created_other_db() ->
     ?_test(begin
        State = mock_state(),
        handle_info_check({'$couch_event', <<"otherdb">>, created}, State),
        ?assertNot(meck:called(?MOD, db_created, [?DBNAME, zig]))
    end).


t_handle_info_scanner_exit_normal() ->
    ?_test(begin
        Res = handle_info({'EXIT', spid, normal}, mock_state()),
        ?assertMatch({noreply, _}, Res),
        {noreply, RState} = Res,
        ?assertEqual(nil, RState#state.scanner)
    end).


t_handle_info_scanner_crashed() ->
    ?_test(begin
        Res = handle_info({'EXIT', spid, oops}, mock_state()),
        ?assertMatch({stop, {scanner_died, oops}, _State}, Res)
    end).


t_handle_info_event_server_exited() ->
    ?_test(begin
        Res = handle_info({'DOWN', esref, type, espid, reason}, mock_state()),
        ?assertMatch({stop, {couch_event_server_died, reason}, _}, Res)
    end).


t_handle_info_unknown_pid_exited() ->
    ?_test(begin
        State0 = mock_state(),
        Res0 =  handle_info({'EXIT', somepid, normal}, State0),
        ?assertMatch({noreply, State0}, Res0),
        State1 = mock_state(),
        Res1 = handle_info({'EXIT', somepid, oops}, State1),
        ?assertMatch({stop, {unexpected_exit, somepid, oops}, State1}, Res1)
    end).


t_handle_info_change_feed_exited() ->
    ?_test(begin
        Tid0 = mock_ets(),
        State0 = mock_state(Tid0, cpid),
        Res0 = handle_info({'EXIT', cpid, normal}, State0),
        ?assertMatch({noreply, _}, Res0),
        {noreply, RState0} = Res0,
        ?assertEqual([], RState0#state.pids),
        ets:delete(Tid0),
        Tid1 = mock_ets(),
        State1 = mock_state(Tid1, cpid),
        Res1 = handle_info({'EXIT', cpid, oops}, State1),
        ?assertMatch({noreply, _}, Res1),
        {noreply, RState1} = Res1,
        ?assertEqual([], RState1#state.pids),
        ets:delete(Tid1)
    end).


t_handle_info_change_feed_exited_and_need_rescan() ->
    ?_test(begin
        Tid = mock_ets(),
        true = ets:insert(Tid, {?DBNAME, 1, true}),
        State = mock_state(Tid, cpid),
        Res = handle_info({'EXIT', cpid, normal}, State),
        ?assertMatch({noreply, _}, Res),
        {noreply, RState} = Res,
        % rescan flag should have been reset to false
        ?assertEqual([{?DBNAME, 1, false}], ets:tab2list(Tid)),
        % a mock change feed process should be running
        [{?DBNAME, Pid}] = RState#state.pids,
        ?assert(is_pid(Pid)),
        ChArgs = kill_mock_changes_reader_and_get_its_args(Pid),
        ?assertEqual({self(), ?DBNAME}, ChArgs),
        ets:delete(Tid)
    end).


t_spawn_changes_reader() ->
    ?_test(begin
        Pid = start_changes_reader(?DBNAME, 3),
        ?assert(erlang:is_process_alive(Pid)),
        ChArgs = kill_mock_changes_reader_and_get_its_args(Pid),
        ?assertEqual({self(), ?DBNAME}, ChArgs),
        ?assert(meck:validate(couch_db)),
        ?assert(meck:validate(couch_changes)),
        ?assert(meck:called(couch_db, open_int, [?DBNAME, [?CTX, sys_db]])),
        ?assert(meck:called(couch_changes, handle_db_changes, [
            #changes_args{
                include_docs = true,
                since = 3,
                feed = "normal",
                timeout = infinity
            }, {json_req, null}, db]))
    end).


t_changes_reader_cb_change() ->
    ?_test(begin
        {ok, Pid} = start_link(?SUFFIX, ?MOD, zig, []),
        Change = change_row(<<"blah">>),
        ChArg = {change, Change, ignore},
        {Pid, ?DBNAME} = changes_reader_cb(ChArg, chtype, {Pid, ?DBNAME}),
        ?assert(meck:called(?MOD, db_change, [?DBNAME, Change, zig])),
        unlink(Pid),
        exit(Pid, kill)
    end).


t_changes_reader_cb_stop() ->
    ?_test(begin
        {ok, Pid} = start_link(?SUFFIX, ?MOD, zig, []),
        ChArg = {stop, 11},
        {Pid, ?DBNAME} = changes_reader_cb(ChArg, chtype, {Pid, ?DBNAME}),
        % We checkpoint on stop, check if checkpointed at correct sequence
        #state{tid = Tid} = sys:get_state(Pid),
        ?assertEqual([{?DBNAME, 11, false}], ets:tab2list(Tid)),
        unlink(Pid),
        exit(Pid, kill)
    end).


t_changes_reader_cb_other() ->
    ?_assertEqual(acc, changes_reader_cb(other, chtype, acc)).


t_handle_call_resume_scan_no_chfeed_no_ets_entry() ->
    ?_test(begin
        Tid = mock_ets(),
        State = mock_state(Tid),
        RState = resume_scan(?DBNAME, State),
        % Check if inserted checkpoint entry in ets starting at 0
        ?assertEqual([{?DBNAME, 0, false}], ets:tab2list(Tid)),
        % Check if called db_found callback
        ?assert(meck:called(?MOD, db_found, [?DBNAME, zig])),
        % Check if started a change reader
        [{?DBNAME, Pid}] = RState#state.pids,
        ChArgs = kill_mock_changes_reader_and_get_its_args(Pid),
        ?assertEqual({self(), ?DBNAME}, ChArgs),
        ?assert(meck:called(couch_changes, handle_db_changes, [
              #changes_args{
                  include_docs = true,
                  since = 0,
                  feed = "normal",
                  timeout = infinity
              }, {json_req, null}, db])),
        ets:delete(Tid)
    end).


t_handle_call_resume_scan_chfeed_no_ets_entry() ->
    ?_test(begin
        Tid = mock_ets(),
        Pid = start_changes_reader(?DBNAME, 0),
        State = mock_state(Tid, Pid),
        resume_scan(?DBNAME, State),
        % Check ets checkpoint is set to 0 and rescan = true
        ?assertEqual([{?DBNAME, 0, true}], ets:tab2list(Tid)),
        ets:delete(Tid),
        kill_mock_changes_reader_and_get_its_args(Pid)
    end).


t_handle_call_resume_scan_chfeed_ets_entry() ->
    ?_test(begin
        Tid = mock_ets(),
        true = ets:insert(Tid, [{?DBNAME, 2, false}]),
        Pid = start_changes_reader(?DBNAME, 1),
        State = mock_state(Tid, Pid),
        resume_scan(?DBNAME, State),
        % Check ets checkpoint is set to same endseq but rescan = true
        ?assertEqual([{?DBNAME, 2, true}], ets:tab2list(Tid)),
        ets:delete(Tid),
        kill_mock_changes_reader_and_get_its_args(Pid)
    end).


t_handle_call_resume_scan_no_chfeed_ets_entry() ->
    ?_test(begin
        Tid = mock_ets(),
        true = ets:insert(Tid, [{?DBNAME, 1, true}]),
        State = mock_state(Tid),
        RState = resume_scan(?DBNAME, State),
        % Check if reset rescan to false but kept same endseq
        ?assertEqual([{?DBNAME, 1, false}], ets:tab2list(Tid)),
        % Check if started a change reader
        [{?DBNAME, Pid}] = RState#state.pids,
        ChArgs = kill_mock_changes_reader_and_get_its_args(Pid),
        ?assertEqual({self(), ?DBNAME}, ChArgs),
        ?assert(meck:called(couch_changes, handle_db_changes, [
            #changes_args{
                include_docs = true,
                since = 1,
                feed = "normal",
                timeout = infinity
            }, {json_req, null}, db])),
        ets:delete(Tid)
    end).


t_start_link() ->
    ?_test(begin
        {ok, Pid} = start_link(?SUFFIX, ?MOD, nil, []),
        ?assert(is_pid(Pid)),
        ?assertMatch(#state{
            mod = ?MOD,
            suffix = ?SUFFIX,
            ctx = nil,
            pids = [],
            skip_ddocs = false
        },  sys:get_state(Pid)),
        unlink(Pid),
        exit(Pid, kill),
        ?assert(meck:called(couch_event, register_all, [Pid]))
    end).


t_start_link_no_ddocs() ->
    ?_test(begin
        {ok, Pid} = start_link(?SUFFIX, ?MOD, nil, [skip_ddocs]),
        ?assert(is_pid(Pid)),
        ?assertMatch(#state{
            mod = ?MOD,
            suffix = ?SUFFIX,
            ctx = nil,
            pids = [],
            skip_ddocs = true
        },  sys:get_state(Pid)),
        unlink(Pid),
        exit(Pid, kill)
    end).


t_misc_gen_server_callbacks() ->
    ?_test(begin
        ?assertEqual(ok, terminate(reason, state)),
        ?assertEqual({ok, state}, code_change(old, state, extra))
    end).


scan_dbs_test_() ->
{
    foreach,
    fun() -> test_util:start_couch([mem3, fabric]) end,
    fun(Ctx) -> test_util:stop_couch(Ctx) end,
    [
        t_pass_shard(),
        t_fail_shard(),
        t_pass_local(),
        t_fail_local()
    ]
}.


t_pass_shard() ->
    ?_test(begin
        DbName0 = ?tempdb(),
        DbSuffix = <<"_replicator">>,
        DbName = <<DbName0/binary, "/", DbSuffix/binary>>,
        ok = fabric:create_db(DbName, [?CTX]),
        ?assertEqual(8, length(filter_shards(DbName, DbSuffix))),
        fabric:delete_db(DbName, [?CTX])
    end).


t_fail_shard() ->
    ?_test(begin
        DbName = ?tempdb(),
        ok = fabric:create_db(DbName, [?CTX]),
        ?assertEqual([], filter_shards(DbName, <<"_replicator">>)),
        fabric:delete_db(DbName, [?CTX])
    end).


t_pass_local() ->
    ?_test(begin
        LocalDb = ?tempdb(),
        {ok, Db} = couch_db:create(LocalDb, [?CTX]),
        ok = couch_db:close(Db),
        scan_local_db(self(), LocalDb),
        receive
            {'$gen_cast', Msg} ->
                ?assertEqual(Msg, {resume_scan, LocalDb})
        after 0 ->
                ?assert(false)
        end
    end).


t_fail_local() ->
    ?_test(begin
        LocalDb = ?tempdb(),
        {ok, Db} = couch_db:create(LocalDb, [?CTX]),
        ok = couch_db:close(Db),
        scan_local_db(self(), <<"some_other_db">>),
        receive
            {'$gen_cast', Msg} ->
                ?assertNotEqual(Msg, {resume_scan, LocalDb})
        after 0 ->
                ?assert(true)
        end
    end).


% Test helper functions

mock_logs() ->
    meck:expect(couch_log, error, 2, ok),
    meck:expect(couch_log, notice, 2, ok),
    meck:expect(couch_log, info, 2, ok),
    meck:expect(couch_log, debug, 2, ok).


mock_callback_mod() ->
    meck:new(?MOD, [non_strict]),
    meck:expect(?MOD, db_created, fun(_DbName, Ctx) -> Ctx end),
    meck:expect(?MOD, db_deleted, fun(_DbName, Ctx) -> Ctx end),
    meck:expect(?MOD, db_found, fun(_DbName, Ctx) -> Ctx end),
    meck:expect(?MOD, db_change, fun(_DbName, _Change, Ctx) -> Ctx end).


mock_changes_reader_loop({_CbFun, {Server, DbName}}) ->
    receive
        die ->
            exit({Server, DbName})
    end.

kill_mock_changes_reader_and_get_its_args(Pid) ->
    Ref = monitor(process, Pid),
    unlink(Pid),
    Pid ! die,
    receive
        {'DOWN', Ref, _, Pid, {Server, DbName}} ->
            {Server, DbName}
        after 1000 ->
            erlang:error(spawn_change_reader_timeout)
    end.


mock_changes_reader() ->
    meck:expect(couch_changes, handle_db_changes,
        fun(_ChArgs, _Req, db) ->
            fun mock_changes_reader_loop/1
        end).


mock_ets() ->
    ets:new(multidb_test_ets, [set, public]).


mock_state() ->
    #state{
        mod = ?MOD,
        ctx = zig,
        suffix = ?SUFFIX,
        event_server = esref,
        scanner = spid,
        pids = []}.


mock_state(Ets) ->
    State = mock_state(),
    State#state{tid = Ets}.


mock_state(Ets, Pid) ->
    State = mock_state(Ets),
    State#state{pids = [{?DBNAME, Pid}]}.


change_row(Id) when is_binary(Id) ->
    {[
        {<<"seq">>, 1},
        {<<"id">>, Id},
        {<<"changes">>, [{[{<<"rev">>, <<"1-f00">>}]}]},
        {doc, {[{<<"_id">>, Id}, {<<"_rev">>, <<"1-f00">>}]}}
    ]}.


handle_call_ok(Msg, State) ->
    ?assertMatch({reply, ok, _}, handle_call(Msg, from, State)).


handle_info_check(Msg, State) ->
    ?assertMatch({noreply, _}, handle_info(Msg, State)).


-endif.
