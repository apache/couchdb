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

-module(couch_index).
-behaviour(gen_server).

-compile(tuple_calls).

-vsn(3).

%% API
-export([start_link/1, stop/1, get_state/2, get_info/1]).
-export([trigger_update/2]).
-export([compact/1, compact/2, get_compactor_pid/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("couch/include/couch_db.hrl").

% 10 minutes
-define(CHECK_INTERVAL, 600000).

-record(st, {
    mod,
    idx_state,
    updater,
    compactor,
    waiters = [],
    committed = true,
    shutdown = false
}).

start_link({Module0, IdxState0}) ->
    [Module, IdxState] = couch_index_plugin:before_open(Module0, IdxState0),
    proc_lib:start_link(?MODULE, init, [{Module, IdxState}]).

stop(Pid) ->
    gen_server:cast(Pid, stop).

get_state(Pid, RequestSeq) ->
    gen_server:call(Pid, {get_state, RequestSeq}, infinity).

get_info(Pid) ->
    gen_server:call(Pid, get_info, group_info_timeout_msec()).

trigger_update(Pid, UpdateSeq) ->
    gen_server:cast(Pid, {trigger_update, UpdateSeq}).

compact(Pid) ->
    compact(Pid, []).

compact(Pid, Options) ->
    {ok, CPid} = gen_server:call(Pid, compact),
    case lists:member(monitor, Options) of
        true -> {ok, erlang:monitor(process, CPid)};
        false -> ok
    end.

get_compactor_pid(Pid) ->
    gen_server:call(Pid, get_compactor_pid).

init({Mod, IdxState}) ->
    DbName = Mod:get(db_name, IdxState),
    erlang:put(io_priority, {view_update, DbName}),
    erlang:send_after(?CHECK_INTERVAL, self(), maybe_close),
    Resp = couch_util:with_db(DbName, fun(Db) ->
        case Mod:open(Db, IdxState) of
            {ok, IdxSt} ->
                couch_db:monitor(Db),
                {ok, IdxSt};
            Error ->
                Error
        end
    end),
    case Resp of
        {ok, NewIdxState} ->
            {ok, UPid} = couch_index_updater:start_link(self(), Mod),
            {ok, CPid} = couch_index_compactor:start_link(self(), Mod),
            State = #st{
                mod = Mod,
                idx_state = NewIdxState,
                updater = UPid,
                compactor = CPid
            },
            Args = [
                Mod:get(db_name, IdxState),
                Mod:get(idx_name, IdxState),
                couch_index_util:hexsig(Mod:get(signature, IdxState))
            ],
            couch_log:debug("Opening index for db: ~s idx: ~s sig: ~p", Args),
            proc_lib:init_ack({ok, self()}),
            gen_server:enter_loop(?MODULE, [], State);
        Other ->
            proc_lib:init_ack(Other)
    end.

terminate(Reason0, State) ->
    #st{mod = Mod, idx_state = IdxState} = State,
    case Reason0 of
        {shutdown, ddoc_updated} ->
            Mod:shutdown(IdxState),
            Reason = ddoc_updated;
        _ ->
            Mod:close(IdxState),
            Reason = Reason0
    end,
    send_all(State#st.waiters, Reason),
    couch_util:shutdown_sync(State#st.updater),
    couch_util:shutdown_sync(State#st.compactor),
    Args = [
        Mod:get(db_name, IdxState),
        Mod:get(idx_name, IdxState),
        couch_index_util:hexsig(Mod:get(signature, IdxState)),
        Reason
    ],
    couch_log:debug("Closing index for db: ~s idx: ~s sig: ~p because ~r", Args),
    ok.

handle_call({get_state, ReqSeq}, From, State) ->
    #st{
        mod = Mod,
        idx_state = IdxState,
        waiters = Waiters
    } = State,
    IdxSeq = Mod:get(update_seq, IdxState),
    case ReqSeq =< IdxSeq of
        true ->
            {reply, {ok, IdxState}, State};
        % View update required
        _ ->
            couch_index_updater:run(State#st.updater, IdxState),
            Waiters2 = [{From, ReqSeq} | Waiters],
            {noreply, State#st{waiters = Waiters2}, infinity}
    end;
handle_call(get_info, _From, State) ->
    #st{mod = Mod} = State,
    IdxState = State#st.idx_state,
    {ok, Info0} = Mod:get(info, IdxState),
    IsUpdating = couch_index_updater:is_running(State#st.updater),
    IsCompacting = couch_index_compactor:is_running(State#st.compactor),
    IdxSeq = Mod:get(update_seq, IdxState),
    GetCommSeq = fun(Db) -> couch_db:get_committed_update_seq(Db) end,
    DbName = Mod:get(db_name, IdxState),
    CommittedSeq = couch_util:with_db(DbName, GetCommSeq),
    Info =
        Info0 ++
            [
                {updater_running, IsUpdating},
                {compact_running, IsCompacting},
                {waiting_commit, State#st.committed == false},
                {waiting_clients, length(State#st.waiters)},
                {pending_updates, max(CommittedSeq - IdxSeq, 0)}
            ],
    {reply, {ok, Info}, State};
handle_call(reset, _From, State) ->
    #st{
        mod = Mod,
        idx_state = IdxState
    } = State,
    {ok, NewIdxState} = Mod:reset(IdxState),
    {reply, {ok, NewIdxState}, State#st{idx_state = NewIdxState}};
handle_call(compact, _From, State) ->
    Resp = couch_index_compactor:run(State#st.compactor, State#st.idx_state),
    {reply, Resp, State};
handle_call(get_compactor_pid, _From, State) ->
    {reply, {ok, State#st.compactor}, State};
handle_call({compacted, NewIdxState}, _From, State) ->
    #st{
        mod = Mod,
        idx_state = OldIdxState
    } = State,
    assert_signature_match(Mod, OldIdxState, NewIdxState),
    NewSeq = Mod:get(update_seq, NewIdxState),
    OldSeq = Mod:get(update_seq, OldIdxState),
    % For indices that require swapping files, we have to make sure we're
    % up to date with the current index. Otherwise indexes could roll back
    % (perhaps considerably) to previous points in history.
    case is_recompaction_enabled(NewIdxState, State) of
        true ->
            case NewSeq >= OldSeq of
                true -> {reply, ok, commit_compacted(NewIdxState, State)};
                false -> {reply, recompact, State}
            end;
        false ->
            {reply, ok, commit_compacted(NewIdxState, State)}
    end;
handle_call({compaction_failed, Reason}, _From, State) ->
    #st{
        mod = Mod,
        idx_state = OldIdxState,
        waiters = Waiters
    } = State,
    send_all(Waiters, Reason),
    {ok, NewIdxState} = Mod:remove_compacted(OldIdxState),
    NewState = State#st{idx_state = NewIdxState, waiters = []},
    {reply, {ok, NewIdxState}, NewState}.

handle_cast({trigger_update, UpdateSeq}, State) ->
    #st{
        mod = Mod,
        idx_state = IdxState
    } = State,
    case UpdateSeq =< Mod:get(update_seq, IdxState) of
        true ->
            {noreply, State};
        false ->
            couch_index_updater:run(State#st.updater, IdxState),
            {noreply, State}
    end;
handle_cast({updated, NewIdxState}, State) ->
    {noreply, NewState} = handle_cast({new_state, NewIdxState}, State),
    case NewState#st.shutdown andalso (NewState#st.waiters =:= []) of
        true ->
            {stop, normal, NewState};
        false ->
            maybe_restart_updater(NewState),
            {noreply, NewState}
    end;
handle_cast({new_state, NewIdxState}, State) ->
    #st{
        mod = Mod,
        idx_state = OldIdxState
    } = State,
    OldFd = Mod:get(fd, OldIdxState),
    NewFd = Mod:get(fd, NewIdxState),
    case NewFd == OldFd of
        true ->
            assert_signature_match(Mod, OldIdxState, NewIdxState),
            CurrSeq = Mod:get(update_seq, NewIdxState),
            Args = [
                Mod:get(db_name, NewIdxState),
                Mod:get(idx_name, NewIdxState),
                CurrSeq
            ],
            couch_log:debug("Updated index for db: ~s idx: ~s seq: ~B", Args),
            Rest = send_replies(State#st.waiters, CurrSeq, NewIdxState),
            case State#st.committed of
                true -> erlang:send_after(commit_delay(), self(), commit);
                false -> ok
            end,
            {noreply, State#st{
                idx_state = NewIdxState,
                waiters = Rest,
                committed = false
            }};
        false ->
            Fmt = "Ignoring update from old indexer for db: ~s idx: ~s",
            Args = [
                Mod:get(db_name, NewIdxState),
                Mod:get(idx_name, NewIdxState)
            ],
            couch_log:warning(Fmt, Args),
            {noreply, State}
    end;
handle_cast({update_error, Error}, State) ->
    send_all(State#st.waiters, Error),
    {noreply, State#st{waiters = []}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(delete, State) ->
    #st{mod = Mod, idx_state = IdxState} = State,
    ok = Mod:delete(IdxState),
    {stop, normal, State};
handle_cast({ddoc_updated, DDocResult}, State) ->
    #st{mod = Mod, idx_state = IdxState} = State,
    Shutdown =
        case DDocResult of
            {not_found, deleted} ->
                true;
            {ok, DDoc} ->
                DbName = Mod:get(db_name, IdxState),
                couch_util:with_db(DbName, fun(Db) ->
                    {ok, NewIdxState} = Mod:init(Db, DDoc),
                    Mod:get(signature, NewIdxState) =/= Mod:get(signature, IdxState)
                end)
        end,
    case Shutdown of
        true ->
            {stop, {shutdown, ddoc_updated}, State#st{shutdown = true}};
        false ->
            {noreply, State#st{shutdown = false}}
    end;
handle_cast(ddoc_updated, State) ->
    #st{mod = Mod, idx_state = IdxState} = State,
    DbName = Mod:get(db_name, IdxState),
    DDocId = Mod:get(idx_name, IdxState),
    Shutdown = couch_util:with_db(DbName, fun(Db) ->
        case couch_db:open_doc(Db, DDocId, [ejson_body, ?ADMIN_CTX]) of
            {not_found, deleted} ->
                true;
            {ok, DDoc} ->
                {ok, NewIdxState} = Mod:init(Db, DDoc),
                Mod:get(signature, NewIdxState) =/= Mod:get(signature, IdxState)
        end
    end),
    case Shutdown of
        true ->
            {stop, {shutdown, ddoc_updated}, State#st{shutdown = true}};
        false ->
            {noreply, State#st{shutdown = false}}
    end;
handle_cast(_Mesg, State) ->
    {stop, unhandled_cast, State}.

handle_info(commit, #st{committed = true} = State) ->
    {noreply, State};
handle_info(commit, State) ->
    #st{mod = Mod, idx_state = IdxState} = State,
    DbName = Mod:get(db_name, IdxState),
    IdxName = Mod:get(idx_name, IdxState),
    GetCommSeq = fun(Db) -> couch_db:get_committed_update_seq(Db) end,
    CommittedSeq = couch_util:with_db(DbName, GetCommSeq),
    case CommittedSeq >= Mod:get(update_seq, IdxState) of
        true ->
            % Commit the updates
            ok = Mod:commit(IdxState),
            couch_event:notify(DbName, {index_commit, IdxName}),
            {noreply, State#st{committed = true}};
        _ ->
            % We can't commit the header because the database seq that's
            % fully committed to disk is still behind us. If we committed
            % now and the database lost those changes our view could be
            % forever out of sync with the database. But a crash before we
            % commit these changes, no big deal, we only lose incremental
            % changes since last committal.
            erlang:send_after(commit_delay(), self(), commit),
            {noreply, State}
    end;
handle_info(maybe_close, State) ->
    % We need to periodically check if our index file still
    % exists on disk because index cleanups don't notify
    % the couch_index process when a file has been deleted. If
    % we don't check for this condition then the index can
    % remain open indefinitely wasting disk space.
    %
    % We make sure that we're idle before closing by looking
    % to see if we have any clients waiting for an update.
    Mod = State#st.mod,
    case State#st.waiters of
        [] ->
            case Mod:index_file_exists(State#st.idx_state) of
                true ->
                    erlang:send_after(?CHECK_INTERVAL, self(), maybe_close),
                    {noreply, State};
                false ->
                    {stop, normal, State}
            end;
        _ ->
            erlang:send_after(?CHECK_INTERVAL, self(), maybe_close),
            {noreply, State}
    end;
handle_info({'DOWN', _, _, _Pid, _}, #st{mod = Mod, idx_state = IdxState} = State) ->
    Args = [Mod:get(db_name, IdxState), Mod:get(idx_name, IdxState)],
    couch_log:debug("Index shutdown by monitor notice for db: ~s idx: ~s", Args),
    catch send_all(State#st.waiters, shutdown),
    {stop, normal, State#st{waiters = []}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

maybe_restart_updater(#st{waiters = []}) ->
    ok;
maybe_restart_updater(#st{idx_state = IdxState} = State) ->
    couch_index_updater:run(State#st.updater, IdxState).

send_all(Waiters, Reply) ->
    [gen_server:reply(From, Reply) || {From, _} <- Waiters].

send_replies(Waiters, UpdateSeq, IdxState) ->
    Pred = fun({_, S}) -> S =< UpdateSeq end,
    {ToSend, Remaining} = lists:partition(Pred, Waiters),
    [gen_server:reply(From, {ok, IdxState}) || {From, _} <- ToSend],
    Remaining.

assert_signature_match(Mod, OldIdxState, NewIdxState) ->
    case {Mod:get(signature, OldIdxState), Mod:get(signature, NewIdxState)} of
        {Sig, Sig} -> ok;
        _ -> erlang:error(signature_mismatch)
    end.

commit_compacted(NewIdxState, State) ->
    #st{
        mod = Mod,
        idx_state = OldIdxState,
        updater = Updater
    } = State,
    {ok, NewIdxState1} = Mod:swap_compacted(OldIdxState, NewIdxState),
    % Restart the indexer if it's running.
    case couch_index_updater:is_running(Updater) of
        true -> ok = couch_index_updater:restart(Updater, NewIdxState1);
        false -> ok
    end,
    case State#st.committed of
        true -> erlang:send_after(commit_delay(), self(), commit);
        false -> ok
    end,
    State#st{
        idx_state = NewIdxState1,
        committed = false
    }.

is_recompaction_enabled(IdxState, #st{mod = Mod}) ->
    DbName = binary_to_list(Mod:get(db_name, IdxState)),
    IdxName = binary_to_list(Mod:get(idx_name, IdxState)),
    IdxKey = DbName ++ ":" ++ IdxName,

    IdxSignature = couch_index_util:hexsig((Mod:get(signature, IdxState))),

    Global = get_value("view_compaction", "enabled_recompaction"),
    PerSignature = get_value("view_compaction.recompaction", IdxSignature),
    PerIdx = get_value("view_compaction.recompaction", IdxKey),
    PerDb = get_value("view_compaction.recompaction", DbName),

    find_most_specific([Global, PerDb, PerIdx, PerSignature], true).

find_most_specific(Settings, Default) ->
    Reversed = lists:reverse([Default | Settings]),
    [Value | _] = lists:dropwhile(fun(A) -> A =:= undefined end, Reversed),
    Value.

get_value(Section, Key) ->
    case config:get(Section, Key) of
        "enabled" -> true;
        "disabled" -> false;
        "true" -> true;
        "false" -> false;
        undefined -> undefined
    end.

commit_delay() ->
    config:get_integer("query_server_config", "commit_freq", 5) * 1000.

group_info_timeout_msec() ->
    Timeout = config:get("query_server_config", "group_info_timeout", "5000"),
    case Timeout of
        "infinity" ->
            infinity;
        Milliseconds ->
            list_to_integer(Milliseconds)
    end.

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

get(db_name, _, _) ->
    <<"db_name">>;
get(idx_name, _, _) ->
    <<"idx_name">>;
get(signature, _, _) ->
    <<61, 237, 157, 230, 136, 93, 96, 201, 204, 17, 137, 186, 50, 249, 44, 135>>.

setup_all() ->
    Ctx = test_util:start_couch(),
    ok = meck:new([config], [passthrough]),
    ok = meck:new([test_index], [non_strict]),
    ok = meck:expect(test_index, get, fun get/3),
    Ctx.

teardown_all(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).

setup(Settings) ->
    meck:reset([config, test_index]),
    ok = meck:expect(config, get, fun(Section, Key) ->
        configure(Section, Key, Settings)
    end),
    {undefined, #st{mod = {test_index}}}.

teardown(_, _) ->
    ok.

configure("view_compaction", "enabled_recompaction", [Global, _Db, _Index]) ->
    Global;
configure("view_compaction.recompaction", "db_name", [_Global, Db, _Index]) ->
    Db;
configure("view_compaction.recompaction", "db_name:" ++ _, [_, _, Index]) ->
    Index;
configure(Section, Key, _) ->
    meck:passthrough([Section, Key]).

recompaction_configuration_test_() ->
    {
        "Compaction tests",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreachx,
                fun setup/1,
                fun teardown/2,
                recompaction_configuration_tests()
            }
        }
    }.

recompaction_configuration_tests() ->
    AllCases = couch_tests_combinatorics:product([
        [undefined, "true", "false"],
        [undefined, "enabled", "disabled"],
        [undefined, "enabled", "disabled"]
    ]),

    EnabledCases = [
        [undefined, undefined, undefined],

        [undefined, undefined, "enabled"],
        [undefined, "enabled", undefined],
        [undefined, "disabled", "enabled"],
        [undefined, "enabled", "enabled"],

        ["true", undefined, undefined],
        ["true", undefined, "enabled"],
        ["true", "disabled", "enabled"],
        ["true", "enabled", undefined],
        ["true", "enabled", "enabled"],

        ["false", undefined, "enabled"],
        ["false", "enabled", undefined],
        ["false", "disabled", "enabled"],
        ["false", "enabled", "enabled"]
    ],

    DisabledCases = [
        [undefined, undefined, "disabled"],
        [undefined, "disabled", undefined],
        [undefined, "disabled", "disabled"],
        [undefined, "enabled", "disabled"],

        ["true", undefined, "disabled"],
        ["true", "disabled", undefined],
        ["true", "disabled", "disabled"],
        ["true", "enabled", "disabled"],

        ["false", undefined, undefined],
        ["false", undefined, "disabled"],
        ["false", "disabled", undefined],
        ["false", "disabled", "disabled"],
        ["false", "enabled", "disabled"]
    ],

    ?assertEqual([], AllCases -- (EnabledCases ++ DisabledCases)),

    [{Settings, fun should_not_call_recompact/2} || Settings <- DisabledCases] ++
        [{Settings, fun should_call_recompact/2} || Settings <- EnabledCases].

should_call_recompact(Settings, {IdxState, State}) ->
    {
        test_id(Settings),
        ?_test(begin
            ?assert(is_recompaction_enabled(IdxState, State)),
            ok
        end)
    }.

should_not_call_recompact(Settings, {IdxState, State}) ->
    {
        test_id(Settings),
        ?_test(begin
            ?assertNot(is_recompaction_enabled(IdxState, State)),
            ok
        end)
    }.

to_string(undefined) -> "undefined";
to_string(Value) -> Value.

test_id(Settings0) ->
    Settings1 = [to_string(Value) || Value <- Settings0],
    "[ " ++ lists:flatten(string:join(Settings1, " , ")) ++ " ]".

get_group_timeout_info_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> meck:unload() end,
        [
            t_group_timeout_info_integer(),
            t_group_timeout_info_infinity()
        ]
    }.

t_group_timeout_info_integer() ->
    ?_test(begin
        meck:expect(
            config,
            get,
            fun("query_server_config", "group_info_timeout", _) ->
                "5001"
            end
        ),
        ?assertEqual(5001, group_info_timeout_msec())
    end).

t_group_timeout_info_infinity() ->
    ?_test(begin
        meck:expect(
            config,
            get,
            fun("query_server_config", "group_info_timeout", _) ->
                "infinity"
            end
        ),
        ?assertEqual(infinity, group_info_timeout_msec())
    end).

-endif.
