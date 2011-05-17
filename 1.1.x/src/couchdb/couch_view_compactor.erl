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
    gen_server:cast(Pid, {start_compact, fun compact_group/2}).

%%=============================================================================
%% internal functions
%%=============================================================================

%% @spec compact_group(Group, NewGroup) -> ok
compact_group(Group, EmptyGroup) ->
    #group{
        current_seq = Seq,
        id_btree = IdBtree,
        name = GroupId,
        views = Views
    } = Group,

    #group{
        db = Db,
        id_btree = EmptyIdBtree,
        views = EmptyViews
    } = EmptyGroup,

    {ok, {Count, _}} = couch_btree:full_reduce(Db#db.fulldocinfo_by_id_btree),

    <<"_design", ShortName/binary>> = GroupId,
    DbName = couch_db:name(Db),
    TaskName = <<DbName/binary, ShortName/binary>>,
    couch_task_status:add_task(<<"View Group Compaction">>, TaskName, <<"">>),

    Fun = fun({DocId, _ViewIdKeys} = KV, {Bt, Acc, TotalCopied, LastId}) ->
        if DocId =:= LastId -> % COUCHDB-999
            Msg = "Duplicates of ~s detected in ~s ~s - rebuild required",
            exit(io_lib:format(Msg, [DocId, DbName, GroupId]));
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

    Pid = couch_view:get_group_server(DbName, GroupId),
    gen_server:cast(Pid, {compact_done, NewGroup}).

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

