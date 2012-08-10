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

-module(couch_view_compactor).

-include ("couch_db.hrl").

-export([start_compact/2, cancel_compact/2]).

-record(acc, {
   btree = nil,
   last_id = nil,
   kvs = [],
   kvs_size = 0,
   changes = 0,
   total_changes
}).

%% @spec start_compact(DbName::binary(), GroupId:binary()) -> ok
%% @doc Compacts the views.  GroupId must not include the _design/ prefix
start_compact(DbName, GroupId) ->
    Pid = couch_view:get_group_server(DbName, <<"_design/",GroupId/binary>>),
    gen_server:call(Pid, {start_compact, fun compact_group/3}).

cancel_compact(DbName, GroupId) ->
    Pid = couch_view:get_group_server(DbName, <<"_design/", GroupId/binary>>),
    gen_server:call(Pid, cancel_compact).

%%=============================================================================
%% internal functions
%%=============================================================================

%% @spec compact_group(Group, NewGroup) -> ok
compact_group(Group, EmptyGroup, DbName) ->
    #group{
        current_seq = Seq,
        id_btree = IdBtree,
        name = GroupId,
        views = Views
    } = Group,

    #group{
        id_btree = EmptyIdBtree,
        views = EmptyViews
    } = EmptyGroup,

    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, DbReduce} = couch_btree:full_reduce(Db#db.fulldocinfo_by_id_btree),
    Count = element(1, DbReduce),

    TotalChanges = lists:foldl(
        fun(View, Acc) ->
            {ok, Kvs} = couch_view:get_row_count(View),
            Acc + Kvs
        end,
        Count, Views),
    Acc0 = #acc{total_changes = TotalChanges, btree = EmptyIdBtree},

    couch_task_status:add_task([
        {type, view_compaction},
        {database, DbName},
        {design_document, GroupId},
        {progress, 0}
    ]),
    BufferSize = list_to_integer(
        couch_config:get("view_compaction", "keyvalue_buffer_size", "2097152")),

    Fun = fun({DocId, _ViewIdKeys} = KV, Acc) ->
        #acc{btree = Bt, kvs = Kvs, kvs_size = KvsSize, last_id = LastId} = Acc,
        if DocId =:= LastId -> % COUCHDB-999
            ?LOG_ERROR("Duplicates of document `~s` detected in view group `~s`"
                ", database `~s` - view rebuild, from scratch, is required",
                [DocId, GroupId, DbName]),
            exit({view_duplicated_id, DocId});
        true -> ok end,
        KvsSize2 = KvsSize + ?term_size(KV),
        if KvsSize2 >= BufferSize ->
            {ok, Bt2} = couch_btree:add(Bt, lists:reverse([KV | Kvs])),
            Acc2 = update_task(Acc, 1 + length(Kvs)),
            {ok, Acc2#acc{btree = Bt2, kvs = [], kvs_size = 0, last_id = DocId}};
        true ->
            {ok, Acc#acc{kvs = [KV | Kvs], kvs_size = KvsSize2, last_id = DocId}}
        end
    end,
    {ok, _, #acc{btree = Bt3, kvs = Uncopied} = Acc1} = couch_btree:foldl(
        IdBtree, Fun, Acc0),
    {ok, NewIdBtree} = couch_btree:add(Bt3, lists:reverse(Uncopied)),
    Acc2 = update_task(Acc1, length(Uncopied)),

    {NewViews, _} = lists:mapfoldl(fun({View, EmptyView}, Acc) ->
        compact_view(View, EmptyView, BufferSize, Acc)
    end, Acc2, lists:zip(Views, EmptyViews)),

    NewGroup = EmptyGroup#group{
        id_btree=NewIdBtree,
        views=NewViews,
        current_seq=Seq
    },
    maybe_retry_compact(Db, GroupId, NewGroup).

maybe_retry_compact(#db{name = DbName} = Db, GroupId, NewGroup) ->
    #group{sig = Sig, fd = NewFd} = NewGroup,
    Header = {Sig, couch_view_group:get_index_header_data(NewGroup)},
    ok = couch_file:write_header(NewFd, Header),
    Pid = couch_view:get_group_server(DbName, GroupId),
    case gen_server:call(Pid, {compact_done, NewGroup}) of
    ok ->
        couch_db:close(Db);
    update ->
        {ok, Db2} = couch_db:reopen(Db),
        Self = self(),
        {MPid, MRef} = erlang:spawn_monitor(fun() ->
            couch_view_updater:update(Self, NewGroup, Db2)
        end),
        NewGroup1 = get_new_group(MPid, MRef),
        erlang:demonitor(MRef, [flush]),
        maybe_retry_compact(Db2, GroupId, NewGroup1)
    end.

get_new_group(Pid, Ref) ->
    receive
        {'DOWN', Ref, _, _, {new_group, NewGroup}} ->
            NewGroup;
        {'DOWN', Ref, _, _, Reason} ->
            erlang:error({view_compaction_error, Reason});
        {'$gen_cast', {Pid, new_group, NewGroup}} ->
            NewGroup;
        {'$gen_cast', {partial_update, _, _}} ->
            get_new_group(Pid, Ref);
        Else ->
            erlang:error({view_compaction_error, Else})
    end.

%% @spec compact_view(View, EmptyView, Retry, Acc) -> {CompactView, NewAcc}
compact_view(View, #view{btree = Bt0} = EmptyView, BufferSize, Acc0) ->
    %% Key is {Key,DocId}
    Fun = fun(KV, #acc{btree = Bt, kvs = Kvs, kvs_size = KvsSize} = Acc) ->
        KvsSize2 = KvsSize + ?term_size(KV),
        if KvsSize2 >= BufferSize ->
            {ok, Bt2} = couch_btree:add(Bt, lists:reverse([KV | Kvs])),
            Acc2 = update_task(Acc, 1 + length(Kvs)),
            {ok, Acc2#acc{btree = Bt2, kvs = [], kvs_size = 0}};
        true ->
            {ok, Acc#acc{kvs = [KV | Kvs], kvs_size = KvsSize2}}
        end
    end,

    {ok, _, #acc{btree = Bt, kvs = Uncopied} = Acc1} = couch_btree:foldl(
        View#view.btree, Fun, Acc0#acc{kvs = [], kvs_size = 0, btree = Bt0}),
    {ok, NewBt} = couch_btree:add(Bt, lists:reverse(Uncopied)),
    Acc2 = update_task(Acc1, length(Uncopied)),
    {EmptyView#view{btree = NewBt}, Acc2}.

update_task(#acc{changes = Changes, total_changes = Total} = Acc, ChangesInc) ->
    Changes2 = Changes + ChangesInc,
    couch_task_status:update([{progress, (Changes2 * 100) div Total}]),
    Acc#acc{changes = Changes2}.
