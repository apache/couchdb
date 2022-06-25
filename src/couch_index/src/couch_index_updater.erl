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

-module(couch_index_updater).
-behaviour(gen_server).

%% API
-export([start_link/2, run/2, is_running/1, update/2, restart/2]).

%% for upgrades
-export([update/3]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("couch/include/couch_db.hrl").

-record(st, {
    idx,
    mod,
    pid = nil
}).

start_link(Index, Module) ->
    gen_server:start_link(?MODULE, {Index, Module}, []).

run(Pid, IdxState) ->
    gen_server:call(Pid, {update, IdxState}).

is_running(Pid) ->
    gen_server:call(Pid, is_running).

update(Mod, State) ->
    update(nil, Mod, State).

restart(Pid, IdxState) ->
    gen_server:call(Pid, {restart, IdxState}).

init({Index, Module}) ->
    process_flag(trap_exit, true),
    {ok, #st{idx = Index, mod = Module}}.

terminate(_Reason, State) ->
    couch_util:shutdown_sync(State#st.pid),
    ok.

handle_call({update, _IdxState}, _From, #st{pid = Pid} = State) when is_pid(Pid) ->
    {reply, ok, State};
handle_call({update, IdxState}, _From, #st{idx = Idx, mod = Mod} = State) ->
    Args = [Mod:get(db_name, IdxState), Mod:get(idx_name, IdxState)],
    couch_log:info("Starting index update for db: ~s idx: ~s", Args),
    Pid = spawn_link(?MODULE, update, [Idx, Mod, IdxState]),
    {reply, ok, State#st{pid = Pid}};
handle_call({restart, IdxState}, _From, #st{idx = Idx, mod = Mod} = State) ->
    Args = [Mod:get(db_name, IdxState), Mod:get(idx_name, IdxState)],
    couch_log:info("Restarting index update for db: ~s idx: ~s", Args),
    Pid = State#st.pid,
    case is_pid(Pid) of
        true -> couch_util:shutdown_sync(State#st.pid);
        _ -> ok
    end,
    % Make sure and flush a possible 'EXIT' message
    % that's already in our mailbox
    receive
        {'EXIT', Pid, _} -> ok
    after 0 ->
        ok
    end,
    NewPid = spawn_link(?MODULE, update, [Idx, State#st.mod, IdxState]),
    {reply, ok, State#st{pid = NewPid}};
handle_call(is_running, _From, #st{pid = Pid} = State) when is_pid(Pid) ->
    {reply, true, State};
handle_call(is_running, _From, State) ->
    {reply, false, State}.

handle_cast(_Mesg, State) ->
    {stop, unknown_cast, State}.

handle_info({'EXIT', _, {updated, Pid, IdxState}}, #st{pid = Pid} = State) ->
    Mod = State#st.mod,
    Args = [Mod:get(db_name, IdxState), Mod:get(idx_name, IdxState)],
    couch_log:info("Index update finished for db: ~s idx: ~s", Args),
    ok = gen_server:cast(State#st.idx, {updated, IdxState}),
    {noreply, State#st{pid = undefined}};
handle_info({'EXIT', _, {reset, Pid}}, #st{idx = Idx, pid = Pid} = State) ->
    {ok, NewIdxState} = gen_server:call(State#st.idx, reset),
    Pid2 = spawn_link(?MODULE, update, [Idx, State#st.mod, NewIdxState]),
    {noreply, State#st{pid = Pid2}};
handle_info({'EXIT', Pid, normal}, #st{pid = Pid} = State) ->
    {noreply, State#st{pid = undefined}};
handle_info({'EXIT', Pid, {{nocatch, Error}, _Trace}}, State) ->
    handle_info({'EXIT', Pid, Error}, State);
handle_info({'EXIT', Pid, Error}, #st{pid = Pid} = State) ->
    ok = gen_server:cast(State#st.idx, {update_error, Error}),
    {noreply, State#st{pid = undefined}};
handle_info({'EXIT', Pid, _Reason}, #st{idx = Pid} = State) ->
    {stop, normal, State};
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info(_Mesg, State) ->
    {stop, unknown_info, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update(Idx, Mod, IdxState) ->
    DbName = Mod:get(db_name, IdxState),
    IndexName = Mod:get(idx_name, IdxState),
    erlang:put(io_priority, {view_update, DbName, IndexName}),
    CurrSeq = Mod:get(update_seq, IdxState),
    UpdateOpts = Mod:get(update_options, IdxState),
    CommittedOnly = lists:member(committed_only, UpdateOpts),
    IncludeDesign = lists:member(include_design, UpdateOpts),
    DocOpts =
        case lists:member(local_seq, UpdateOpts) of
            true -> [conflicts, deleted_conflicts, local_seq, deleted];
            _ -> [conflicts, deleted_conflicts,local_seq, deleted]
        end,

    couch_util:with_db(DbName, fun(Db) ->
        DbUpdateSeq = couch_db:get_update_seq(Db),
        DbCommittedSeq = couch_db:get_committed_update_seq(Db),

        NumUpdateChanges = couch_db:count_changes_since(Db, CurrSeq),
        NumPurgeChanges = count_pending_purged_docs_since(Db, Mod, IdxState),
        TotalChanges = NumUpdateChanges + NumPurgeChanges,
        {ok, PurgedIdxState} = purge_index(Db, Mod, IdxState),

        GetSeq = fun
            (#full_doc_info{update_seq = Seq}) -> Seq;
            (#doc_info{high_seq = Seq}) -> Seq
        end,

        GetInfo = fun
            (#full_doc_info{id=Id, update_seq=Seq, deleted=Del,access=Access}=FDI) ->
                {Id, Seq, Del, couch_doc:to_doc_info(FDI), Access};
            (#doc_info{id=Id, high_seq=Seq, revs=[RI|_],access=Access}=DI) ->
                {Id, Seq, RI#rev_info.deleted, DI, Access}
        end,

        LoadDoc = fun(DI) ->
            {DocId, Seq, Deleted, DocInfo, Access} = GetInfo(DI),

            case {IncludeDesign, DocId} of
                {false, <<"_design/", _/binary>>} ->
                    {nil, Seq};
                _ ->
                    case IndexName of % TODO: move into outer case statement
                        <<"_design/_access">> ->
                            {ok, Doc} = couch_db:open_doc_int(Db, DocInfo, DocOpts),
                            % TODO: hande conflicted docs in _access index
                            % probably remove
                            [RevInfo|_] = DocInfo#doc_info.revs,
                            Doc1 = Doc#doc{
                                meta = [{body_sp, RevInfo#rev_info.body_sp}],
                                access = Access
                            },
                            {Doc1, Seq};
                        _ when Deleted ->
                            {#doc{id=DocId, deleted=true}, Seq};
                        _ ->
                            {ok, Doc} = couch_db:open_doc_int(Db, DocInfo, DocOpts),
                            {Doc, Seq}
                    end
            end
        end,

        Proc = fun(DocInfo, {IdxStateAcc, _}) ->
            case CommittedOnly and (GetSeq(DocInfo) > DbCommittedSeq) of
                true ->
                    {stop, {IdxStateAcc, false}};
                false ->
                    {Doc, Seq} = LoadDoc(DocInfo),
                    {ok, NewSt} = Mod:process_doc(Doc, Seq, IdxStateAcc),
                    garbage_collect(),
                    {ok, {NewSt, true}}
            end
        end,
        {ok, InitIdxState} = Mod:start_update(
            Idx,
            PurgedIdxState,
            TotalChanges,
            NumPurgeChanges
        ),

        Acc0 = {InitIdxState, true},
        {ok, Acc} = couch_db:fold_changes(Db, CurrSeq, Proc, Acc0, []),
        {ProcIdxSt, SendLast} = Acc,

        % If we didn't bail due to hitting the last committed seq we need
        % to send our last update_seq through.
        {ok, LastIdxSt} =
            case SendLast of
                true ->
                    Mod:process_doc(nil, DbUpdateSeq, ProcIdxSt);
                _ ->
                    {ok, ProcIdxSt}
            end,

        {ok, FinalIdxState} = Mod:finish_update(LastIdxSt),
        exit({updated, self(), FinalIdxState})
    end).

purge_index(Db, Mod, IdxState) ->
    DbPurgeSeq = couch_db:get_purge_seq(Db),
    IdxPurgeSeq = Mod:get(purge_seq, IdxState),
    if
        IdxPurgeSeq == DbPurgeSeq ->
            {ok, IdxState};
        true ->
            FoldFun = fun({PurgeSeq, _UUId, Id, Revs}, Acc) ->
                Mod:purge(Db, PurgeSeq, [{Id, Revs}], Acc)
            end,
            {ok, NewStateAcc} =
                try
                    couch_db:fold_purge_infos(
                        Db,
                        IdxPurgeSeq,
                        FoldFun,
                        IdxState,
                        []
                    )
                catch
                    error:{invalid_start_purge_seq, _} ->
                        exit({reset, self()})
                end,
            Mod:update_local_purge_doc(Db, NewStateAcc),
            {ok, NewStateAcc}
    end.

count_pending_purged_docs_since(Db, Mod, IdxState) ->
    DbPurgeSeq = couch_db:get_purge_seq(Db),
    IdxPurgeSeq = Mod:get(purge_seq, IdxState),
    DbPurgeSeq - IdxPurgeSeq.
