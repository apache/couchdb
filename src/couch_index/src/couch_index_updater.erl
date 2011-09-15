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
-export([start_link/2, run/2, is_running/1, update/3, restart/2]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include("couch_db.hrl").

-record(st, {
    idx,
    mod,
    pid=nil
}).


start_link(Index, Module) ->
    gen_server:start_link(?MODULE, {Index, Module}, []).


run(Pid, IdxState) ->
    gen_server:call(Pid, {update, IdxState}).


is_running(Pid) ->
    gen_server:call(Pid, is_running).


restart(Pid, IdxState) ->
    gen_server:call(Pid, {restart, IdxState}).


init({Index, Module}) ->
    process_flag(trap_exit, true),
    {ok, #st{idx=Index, mod=Module}}.


terminate(_Reason, State) ->
    couch_util:shutdown_sync(State#st.pid),
    ok.


handle_call({update, _IdxState}, _From, #st{pid=Pid}=State) when is_pid(Pid) ->
    {reply, ok, State};
handle_call({update, IdxState}, _From, #st{idx=Idx, mod=Mod}=State) ->
    Pid = spawn_link(fun() -> update(Idx, Mod, IdxState) end),
    {reply, ok, State#st{pid=Pid}};
handle_call({restart, IdxState}, _From, #st{idx=Idx}=State) ->
    case is_pid(State#st.pid) of
        true -> couch_util:shutdown_sync(State#st.pid);
        _ -> ok
    end,
    Pid = spawn_link(fun() -> update(Idx, State#st.mod, IdxState) end),
    {reply, ok, State#st{pid=Pid}};
handle_call(is_running, _From, #st{pid=Pid}=State) when is_pid(Pid) ->
    {reply, true, State};
handle_call(is_running, _From, State) ->
    {reply, false, State}.


handle_cast(_Mesg, State) ->
    {stop, unknown_cast, State}.


handle_info({'EXIT', Pid, {updated, IdxState}}, #st{pid=Pid}=State) ->
    ok = gen_server:cast(State#st.idx, {updated, IdxState}),
    {noreply, State#st{pid=undefined}};
handle_info({'EXIT', Pid, reset}, #st{idx=Idx, pid=Pid}=State) ->
    {ok, NewIdxState} = gen_server:call(State#st.idx, reset),
    Pid2 = spawn_link(fun() -> update(Idx, State#st.mod, NewIdxState) end),
    {noreply, State#st{pid=Pid2}};
handle_info({'EXIT', Pid, normal}, #st{pid=Pid}=State) ->
    {noreply, State#st{pid=undefined}};
handle_info({'EXIT', Pid, {{nocatch, Error}, _Trace}}, State) ->
    handle_info({'EXIT', Pid, Error}, State);
handle_info({'EXIT', Pid, Error}, #st{pid=Pid}=State) ->
    ok = gen_server:cast(State#st.idx, {update_error, Error}),
    {noreply, State#st{pid=undefined}};
handle_info({'EXIT', Pid, _Reason}, #st{idx=Pid}=State) ->
    {stop, normal, State};
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info(_Mesg, State) ->
    {stop, unknown_info, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


update(Idx, Mod, IdxState) ->
    DbName = Mod:get(db_name, IdxState),
    CurrSeq = Mod:get(update_seq, IdxState),
    UpdateOpts = Mod:get(update_options, IdxState),
    CommittedOnly = lists:member(committed_only, UpdateOpts),
    IncludeDesign = lists:member(include_design, UpdateOpts),
    DocOpts = case lists:member(local_seq, UpdateOpts) of
        true -> [conflicts, deleted_conflicts, local_seq];
        _ -> [conflicts, deleted_conflicts]
    end,

    TaskType = <<"Indexer">>,
    Starting = <<"Starting index update.">>,
    couch_task_status:add_task(TaskType, Mod:get(idx_name, IdxState), Starting),

    couch_util:with_db(DbName, fun(Db) ->
        DbUpdateSeq = couch_db:get_update_seq(Db),
        DbCommittedSeq = couch_db:get_committed_update_seq(Db),

        PurgedIdxState = case purge_index(Db, Mod, IdxState) of
            {ok, IdxState0} -> IdxState0;
            reset -> exit(reset)
        end,

        couch_task_status:set_update_frequency(500),
        NumChanges = couch_db:count_changes_since(Db, CurrSeq),

        LoadDoc = fun(DocInfo) ->
            #doc_info{
                id=DocId,
                high_seq=Seq,
                revs=[#rev_info{deleted=Deleted} | _]
            } = DocInfo,

            case {IncludeDesign, DocId} of
                {false, <<"_design/", _/binary>>} ->
                    {nil, Seq};
                _ when Deleted ->
                    {#doc{id=DocId, deleted=true}, Seq};
                _ ->
                    {ok, Doc} = couch_db:open_doc_int(Db, DocInfo, DocOpts),
                    {Doc, Seq}
            end
        end,

        Proc = fun(DocInfo, _, {IdxStateAcc, Count, _}) ->
            HighSeq = DocInfo#doc_info.high_seq,
            case CommittedOnly and (HighSeq > DbCommittedSeq) of
                true ->
                    {stop, {IdxStateAcc, Count, false}};
                false ->
                    update_task_status(NumChanges, Count),
                    {Doc, Seq} = LoadDoc(DocInfo),
                    {ok, NewSt} = Mod:process_doc(Doc, Seq, IdxStateAcc),
                    {ok, {NewSt, Count+1, true}}
            end
        end,

        {ok, InitIdxState} = Mod:start_update(Idx, PurgedIdxState),
        Acc0 = {InitIdxState, 0, true},
        {ok, _, Acc} = couch_db:enum_docs_since(Db, CurrSeq, Proc, Acc0, []),
        {ProcIdxSt, _, SendLast} = Acc,

        % If we didn't bail due to hitting the last committed seq we need
        % to send our last update_seq through.
        {ok, LastIdxSt} = case SendLast of
            true ->
                Mod:process_doc(nil, DbUpdateSeq, ProcIdxSt);
            _ ->
                {ok, ProcIdxSt}
        end,

        couch_task_status:set_update_frequency(0),
        couch_task_status:update("Waiting for index writer to finish."),

        {ok, FinalIdxState} = Mod:finish_update(LastIdxSt),
        exit({updated, FinalIdxState})
    end).


purge_index(Db, Mod, IdxState) ->
    DbPurgeSeq = couch_db:get_purge_seq(Db),
    IdxPurgeSeq = Mod:get(purge_seq, IdxState),
    if
        DbPurgeSeq == IdxPurgeSeq ->
            {ok, IdxState};
        DbPurgeSeq == IdxPurgeSeq + 1 ->
            couch_task_status:update(<<"Purging index entries.">>),
            {ok, PurgedIdRevs} = couch_db:get_last_purged(Db),
            Mod:purge(Db, DbPurgeSeq, PurgedIdRevs, IdxState);
        true ->
            couch_task_status:update(<<"Resetting index due to purge state.">>),
            reset
    end.


update_task_status(Total, Count) ->
    PercDone = (Count * 100) div Total,
    Mesg = "Processed ~p of ~p changes (~p%)",
    couch_task_status:update(Mesg, [Count, Total, PercDone]).
