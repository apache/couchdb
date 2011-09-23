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

-export([start_compact/2]).

%% @spec start_compact(DbName::binary(), GroupId:binary()) -> ok
%% @doc Compacts the views.  GroupId must not include the _design/ prefix
start_compact(DbName, GroupId) ->
    Pid = couch_view:get_group_server(DbName, <<"_design/",GroupId/binary>>),
    gen_server:call(Pid, {start_compact, fun compact_group/3}).

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
    {ok, {Count, _}} = couch_btree:full_reduce(Db#db.fulldocinfo_by_id_btree),

    <<"_design", ShortName/binary>> = GroupId,
    TaskName = <<DbName/binary, ShortName/binary>>,
    couch_task_status:add_task(<<"View Group Compaction">>, TaskName, <<"">>),

    Fun = fun({DocId, _ViewIdKeys} = KV, {Bt, Acc, TotalCopied, LastId}) ->
        if DocId =:= LastId -> % COUCHDB-999
            ?LOG_ERROR("Duplicates of document `~s` detected in view group `~s`"
                ", database `~s` - view rebuild, from scratch, is required",
                [DocId, GroupId, DbName]),
            exit({view_duplicated_id, DocId});
        true -> ok end,
        if TotalCopied rem 10000 =:= 0 ->
            couch_task_status:update("Copied ~p of ~p Ids (~p%)",
                [TotalCopied, Count, (TotalCopied*100) div Count]),
            {ok, Bt2} = couch_btree:add(Bt, lists:reverse([KV|Acc])),
            {ok, {Bt2, [], TotalCopied+1, DocId}};
        true ->
            {ok, {Bt, [KV|Acc], TotalCopied+1, DocId}}
        end
    end,
    {ok, _, {Bt3, Uncopied, _Total, _LastId}} = couch_btree:foldl(IdBtree, Fun,
        {EmptyIdBtree, [], 0, nil}),
    {ok, NewIdBtree} = couch_btree:add(Bt3, lists:reverse(Uncopied)),

    NewViews = lists:map(fun({View, EmptyView}) ->
        compact_view(View, EmptyView)
    end, lists:zip(Views, EmptyViews)),

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
        {_, Ref} = erlang:spawn_monitor(fun() ->
            couch_view_updater:update(nil, NewGroup, Db2)
        end),
        receive
        {'DOWN', Ref, _, _, {new_group, NewGroup2}} ->
            maybe_retry_compact(Db2, GroupId, NewGroup2)
        end
    end.

%% @spec compact_view(View, EmptyView, Retry) -> CompactView
compact_view(View, EmptyView) ->
    {ok, Count} = couch_view:get_row_count(View),

    %% Key is {Key,DocId}
    Fun = fun(KV, {Bt, Acc, TotalCopied}) ->
        if TotalCopied rem 10000 =:= 0 ->
            couch_task_status:update("View #~p: copied ~p of ~p KVs (~p%)",
                [View#view.id_num, TotalCopied, Count, (TotalCopied*100) div Count]),
            {ok, Bt2} = couch_btree:add(Bt, lists:reverse([KV|Acc])),
            {ok, {Bt2, [], TotalCopied + 1}};
        true ->
            {ok, {Bt, [KV|Acc], TotalCopied + 1}}
        end
    end,

    {ok, _, {Bt3, Uncopied, _Total}} = couch_btree:foldl(View#view.btree, Fun,
        {EmptyView#view.btree, [], 0}),
    {ok, NewBt} = couch_btree:add(Bt3, lists:reverse(Uncopied)),
    EmptyView#view{btree = NewBt}.

