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


%% API
-export([start_link/1, stop/1, get_state/2, get_info/1]).
-export([compact/1, compact/2, get_compactor_pid/1]).
-export([config_change/3]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


-include("couch_db.hrl").


-record(st, {
    mod,
    idx_state,
    updater,
    compactor,
    waiters=[],
    commit_delay,
    committed=true,
    shutdown=false
}).


start_link({Module, IdxState}) ->
    proc_lib:start_link(?MODULE, init, [{Module, IdxState}]).


stop(Pid) ->
    gen_server:cast(Pid, stop).


get_state(Pid, RequestSeq) ->
    gen_server:call(Pid, {get_state, RequestSeq}, infinity).


get_info(Pid) ->
    gen_server:call(Pid, get_info).


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

config_change("query_server_config", "commit_freq", NewValue) ->
    ok = gen_server:cast(?MODULE, {config_update, NewValue}).


init({Mod, IdxState}) ->
    ok = couch_config:register(fun ?MODULE:config_change/3),
    DbName = Mod:get(db_name, IdxState),
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
            Delay = couch_config:get("query_server_config", "commit_freq", "5"),
            MsDelay = 1000 * list_to_integer(Delay),
            State = #st{
                mod=Mod,
                idx_state=NewIdxState,
                updater=UPid,
                compactor=CPid,
                commit_delay=MsDelay
            },
            Args = [
                Mod:get(db_name, IdxState),
                Mod:get(idx_name, IdxState),
                couch_index_util:hexsig(Mod:get(signature, IdxState))
            ],
            ?LOG_INFO("Opening index for db: ~s idx: ~s sig: ~p", Args),
            proc_lib:init_ack({ok, self()}),
            gen_server:enter_loop(?MODULE, [], State);
        Other ->
            proc_lib:init_ack(Other)
    end.


terminate(Reason, State) ->
    #st{mod=Mod, idx_state=IdxState}=State,
    Mod:close(IdxState),
    send_all(State#st.waiters, Reason),
    couch_util:shutdown_sync(State#st.updater),
    couch_util:shutdown_sync(State#st.compactor),
    Args = [
        Mod:get(db_name, IdxState),
        Mod:get(idx_name, IdxState),
        couch_index_util:hexsig(Mod:get(signature, IdxState)),
        Reason
    ],
    ?LOG_INFO("Closing index for db: ~s idx: ~s sig: ~p~nreason: ~p", Args),
    ok.


handle_call({get_state, ReqSeq}, From, State) ->
    #st{
        mod=Mod,
        idx_state=IdxState,
        waiters=Waiters
    } = State,
    IdxSeq = Mod:get(update_seq, IdxState),
    case ReqSeq =< IdxSeq of
        true ->
            {reply, {ok, IdxState}, State};
        _ -> % View update required
            couch_index_updater:run(State#st.updater, IdxState),
            Waiters2 = [{From, ReqSeq} | Waiters],
            {noreply, State#st{waiters=Waiters2}, infinity}
    end;
handle_call(get_info, _From, State) ->
    #st{mod=Mod} = State,
    {ok, Info0} = Mod:get(info, State#st.idx_state),
    IsUpdating = couch_index_updater:is_running(State#st.updater),
    IsCompacting = couch_index_compactor:is_running(State#st.compactor),
    Info = Info0 ++ [
        {updater_running, IsUpdating},
        {compact_running, IsCompacting},
        {waiting_commit, State#st.committed == false},
        {waiting_clients, length(State#st.waiters)}
    ],
    {reply, {ok, Info}, State};
handle_call(reset, _From, State) ->
    #st{
        mod=Mod,
        idx_state=IdxState
    } = State,
    {ok, NewIdxState} = Mod:reset(IdxState),
    {reply, {ok, NewIdxState}, State#st{idx_state=NewIdxState}};
handle_call(compact, _From, State) ->
    Resp = couch_index_compactor:run(State#st.compactor, State#st.idx_state),
    {reply, Resp, State};
handle_call(get_compactor_pid, _From, State) ->
    {reply, {ok, State#st.compactor}, State};
handle_call({compacted, NewIdxState}, _From, State) ->
    #st{
        mod=Mod,
        idx_state=OldIdxState,
        updater=Updater,
        commit_delay=Delay
    } = State,
    assert_signature_match(Mod, OldIdxState, NewIdxState),
    NewSeq = Mod:get(update_seq, NewIdxState),
    OldSeq = Mod:get(update_seq, OldIdxState),
    % For indices that require swapping files, we have to make sure we're
    % up to date with the current index. Otherwise indexes could roll back
    % (perhaps considerably) to previous points in history.
    case NewSeq >= OldSeq of
        true ->
            {ok, NewIdxState1} = Mod:swap_compacted(OldIdxState, NewIdxState),
            % Restart the indexer if it's running.
            case couch_index_updater:is_running(Updater) of
                true -> ok = couch_index_updater:restart(Updater, NewIdxState1);
                false -> ok
            end,
            case State#st.committed of
                true -> erlang:send_after(Delay, self(), commit);
                false -> ok
            end,
            {reply, ok, State#st{
                idx_state=NewIdxState1,
                committed=false
            }};
        _ ->
            {reply, recompact, State}
    end.


handle_cast({config_change, NewDelay}, State) ->
    MsDelay = 1000 * list_to_integer(NewDelay),
    {noreply, State#st{commit_delay=MsDelay}};
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
        mod=Mod,
        idx_state=OldIdxState,
        commit_delay=Delay
    } = State,
    assert_signature_match(Mod, OldIdxState, NewIdxState),
    CurrSeq = Mod:get(update_seq, NewIdxState),
    Args = [
        Mod:get(db_name, NewIdxState),
        Mod:get(idx_name, NewIdxState),
        CurrSeq
    ],
    ?LOG_DEBUG("Updated index for db: ~s idx: ~s seq: ~B", Args),
    Rest = send_replies(State#st.waiters, CurrSeq, NewIdxState),
    case State#st.committed of
        true -> erlang:send_after(Delay, self(), commit);
        false -> ok
    end,
    {noreply, State#st{
        idx_state=NewIdxState,
        waiters=Rest,
        committed=false
    }};
handle_cast({update_error, Error}, State) ->
    send_all(State#st.waiters, Error),
    {noreply, State#st{waiters=[]}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(delete, State) ->
    #st{mod=Mod, idx_state=IdxState} = State,
    ok = Mod:delete(IdxState),
    {stop, normal, State};
handle_cast(ddoc_updated, State) ->
    #st{mod = Mod, idx_state = IdxState, waiters = Waiters} = State,
    DbName = Mod:get(db_name, IdxState),
    DDocId = Mod:get(idx_name, IdxState),
    Shutdown = couch_util:with_db(DbName, fun(Db) ->
        case couch_db:open_doc(Db, DDocId, [ejson_body]) of
            {not_found, deleted} ->
                true;
            {ok, DDoc} ->
                {ok, NewIdxState} = Mod:init(Db, DDoc),
                Mod:get(signature, NewIdxState) =/= Mod:get(signature, IdxState)
        end
    end),
    case Shutdown of
        true ->
            case Waiters of
                [] ->
                    {stop, normal, State};
                _ ->
                    {noreply, State#st{shutdown = true}}
            end;
        false ->
            {noreply, State#st{shutdown = false}}
    end;
handle_cast(_Mesg, State) ->
    {stop, unhandled_cast, State}.


handle_info(commit, #st{committed=true}=State) ->
    {noreply, State};
handle_info(commit, State) ->
    #st{mod=Mod, idx_state=IdxState, commit_delay=Delay} = State,
    DbName = Mod:get(db_name, IdxState),
    GetCommSeq = fun(Db) -> couch_db:get_committed_update_seq(Db) end,
    CommittedSeq = couch_util:with_db(DbName, GetCommSeq),
    case CommittedSeq >= Mod:get(update_seq, IdxState) of
        true ->
            % Commit the updates
            ok = Mod:commit(IdxState),
            {noreply, State#st{committed=true}};
        _ ->
            % We can't commit the header because the database seq that's
            % fully committed to disk is still behind us. If we committed
            % now and the database lost those changes our view could be
            % forever out of sync with the database. But a crash before we
            % commit these changes, no big deal, we only lose incremental
            % changes since last committal.
            erlang:send_after(Delay, self(), commit),
            {noreply, State}
    end;
handle_info({'DOWN', _, _, _Pid, _}, #st{mod=Mod, idx_state=IdxState}=State) ->
    Args = [Mod:get(db_name, IdxState), Mod:get(idx_name, IdxState)],
    ?LOG_INFO("Index shutdown by monitor notice for db: ~s idx: ~s", Args),
    catch send_all(State#st.waiters, shutdown),
    {stop, normal, State#st{waiters=[]}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


maybe_restart_updater(#st{waiters=[]}) ->
    ok;
maybe_restart_updater(#st{mod=Mod, idx_state=IdxState}=State) ->
    couch_util:with_db(Mod:get(db_name, IdxState), fun(Db) ->
        UpdateSeq = couch_db:get_update_seq(Db),
        CommittedSeq = couch_db:get_committed_update_seq(Db),
        CanUpdate = UpdateSeq > CommittedSeq,
        UOpts = Mod:get(update_options, IdxState),
        case CanUpdate and lists:member(committed_only, UOpts) of
            true -> couch_db:ensure_full_commit(Db);
            false -> ok
        end
    end),
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
