% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(dreyfus_index_updater).
-include_lib("couch/include/couch_db.hrl").
-include("dreyfus.hrl").

-export([update/2, load_docs/3]).

-import(couch_query_servers, [get_os_process/1, ret_os_process/1, proc_prompt/2]).

update(IndexPid, Index) ->
    #index{
        current_seq = CurSeq,
        dbname = DbName,
        ddoc_id = DDocId,
        name = IndexName
    } = Index,
    erlang:put(io_priority, {view_update, DbName, IndexName}),
    {ok, Db} = couch_db:open_int(DbName, []),
    try
        %% compute on all docs modified since we last computed.
        TotalChanges = couch_db:count_changes_since(Db, CurSeq),

        couch_task_status:add_task([
            {type, search_indexer},
            {database, DbName},
            {design_document, DDocId},
            {index, IndexName},
            {progress, 0},
            {changes_done, 0},
            {total_changes, TotalChanges}
        ]),

        %% update status every half second
        couch_task_status:set_update_frequency(500),

        NewCurSeq = couch_db:get_update_seq(Db),
        Proc = get_os_process(Index#index.def_lang),
        try
            true = proc_prompt(Proc, [<<"add_fun">>, Index#index.def]),
            EnumFun = fun ?MODULE:load_docs/3,
            Acc0 = {0, IndexPid, Db, Proc, TotalChanges, now()},

            {ok, _, _} = couch_db:enum_docs_since(Db, CurSeq, EnumFun, Acc0, []),
            ok = clouseau_rpc:commit(IndexPid, NewCurSeq)
        after
            ret_os_process(Proc)
        end,
        exit({updated, NewCurSeq})
    after
        couch_db:close(Db)
    end.

load_docs(FDI, _, {I, IndexPid, Db, Proc, Total, LastCommitTime}=Acc) ->
    couch_task_status:update([{changes_done, I}, {progress, (I * 100) div Total}]),
    DI = couch_doc:to_doc_info(FDI),
    #doc_info{id=Id, high_seq=Seq, revs=[#rev_info{deleted=Del}|_]} = DI,
    case Del of
        true ->
            ok = clouseau_rpc:delete(IndexPid, Id);
        false ->
            {ok, Doc} = couch_db:open_doc(Db, DI, []),
            Json = couch_doc:to_json_obj(Doc, []),
            [Fields|_] = proc_prompt(Proc, [<<"index_doc">>, Json]),
            Fields1 = [list_to_tuple(Field) || Field <- Fields],
            case Fields1 of
                [] -> ok = clouseau_rpc:delete(IndexPid, Id);
                _  -> ok = clouseau_rpc:update(IndexPid, Id, Fields1)
            end
    end,
    %% Force a commit every minute
    case timer:now_diff(Now = now(), LastCommitTime) >= 60000000 of
        true ->
            ok = clouseau_rpc:commit(IndexPid, Seq),
            {ok, {I+1, IndexPid, Db, Proc, Total, Now}};
        false ->
            {ok, setelement(1, Acc, I+1)}
    end.
