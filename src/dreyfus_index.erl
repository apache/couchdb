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

%% A dreyfus_index gen_server is linked to its clouseau twin.

-module(dreyfus_index).
-behaviour(gen_server).
-vsn(1).
-include_lib("couch/include/couch_db.hrl").
-include("dreyfus.hrl").


% public api.
-export([start_link/2, design_doc_to_index/2, await/2, search/2, info/1,
         group1/2, group2/2,
         design_doc_to_indexes/1]).

% gen_server api.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

% private definitions.
-record(state, {
    dbname,
    index,
    updater_pid=nil,
    index_pid=nil,
    waiting_list=[]
}).

% exported for callback.
-export([search_int/2, group1_int/2, group2_int/2, info_int/1]).

% public functions.
start_link(DbName, Index) ->
    proc_lib:start_link(?MODULE, init, [{DbName, Index}]).

await(Pid, MinSeq) ->
    MFA = {gen_server, call, [Pid, {await, MinSeq}, infinity]},
    dreyfus_util:time([index, await], MFA).

search(Pid0, QueryArgs) ->
    Pid = to_index_pid(Pid0),
    MFA = {?MODULE, search_int, [Pid, QueryArgs]},
    dreyfus_util:time([index, search], MFA).

group1(Pid0, QueryArgs) ->
    Pid = to_index_pid(Pid0),
    MFA = {?MODULE, group1_int, [Pid, QueryArgs]},
    dreyfus_util:time([index, group1], MFA).

group2(Pid0, QueryArgs) ->
    Pid = to_index_pid(Pid0),
    MFA = {?MODULE, group2_int, [Pid, QueryArgs]},
    dreyfus_util:time([index, group2], MFA).

info(Pid0) ->
    Pid = to_index_pid(Pid0),
    MFA = {?MODULE, info_int, [Pid]},
    dreyfus_util:time([index, info], MFA).

%% We either have a dreyfus_index gen_server pid or the remote
%% clouseau pid.
to_index_pid(Pid) ->
    case node(Pid) == node() of
        true  -> gen_server:call(Pid, get_index_pid, infinity);
        false -> Pid
    end.

design_doc_to_indexes(#doc{body={Fields}}=Doc) ->
    RawIndexes = couch_util:get_value(<<"indexes">>, Fields, {[]}),
    case RawIndexes of
        {IndexList} when is_list(IndexList) ->
            {IndexNames, _} = lists:unzip(IndexList),
            lists:flatmap(
                fun(IndexName) ->
                    case (catch design_doc_to_index(Doc, IndexName)) of
                        {ok, #index{}=Index} -> [Index];
                        _ -> []
                    end
                end,
                IndexNames);
        _ -> []
    end.

% gen_server functions.

init({DbName, Index}) ->
    process_flag(trap_exit, true),
    case open_index(DbName, Index) of
        {ok, Pid, Seq} ->
            State=#state{
              dbname=DbName,
              index=Index#index{current_seq=Seq, dbname=DbName},
              index_pid=Pid
             },
            case couch_db:open_int(DbName, []) of
                {ok, Db} ->
                    try couch_db:monitor(Db) after couch_db:close(Db) end,
                    proc_lib:init_ack({ok, self()}),
                    gen_server:enter_loop(?MODULE, [], State);
                Error ->
                    proc_lib:init_ack(Error)
            end;
        Error ->
            proc_lib:init_ack(Error)
    end.

handle_call({await, RequestSeq}, From,
            #state{
                index=#index{current_seq=Seq}=Index,
                index_pid=IndexPid,
                updater_pid=nil,
                waiting_list=WaitList
            }=State) when RequestSeq > Seq ->
    UpPid = spawn_link(fun() -> dreyfus_index_updater:update(IndexPid, Index) end),
    {noreply, State#state{
        updater_pid=UpPid,
        waiting_list=[{From,RequestSeq}|WaitList]
    }};
handle_call({await, RequestSeq}, _From,
            #state{index=#index{current_seq=Seq}}=State) when RequestSeq =< Seq ->
    {reply, {ok, State#state.index_pid, Seq}, State};
handle_call({await, RequestSeq}, From, #state{waiting_list=WaitList}=State) ->
    {noreply, State#state{
        waiting_list=[{From,RequestSeq}|WaitList]
    }};

handle_call(get_index_pid, _From, State) -> % upgrade
    {reply, State#state.index_pid, State};

handle_call({search, QueryArgs0}, _From, State) -> % obsolete
    Reply = search_int(State#state.index_pid, QueryArgs0),
    {reply, Reply, State};

handle_call({group1, QueryArgs0}, _From, State) -> % obsolete
    Reply = group1_int(State#state.index_pid, QueryArgs0),
    {reply, Reply, State};

handle_call({group2, QueryArgs0}, _From, State) -> % obsolete
    Reply = group2_int(State#state.index_pid, QueryArgs0),
    {reply, Reply, State};

handle_call(info, _From, State) -> % obsolete
    Reply = info_int(State#state.index_pid),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', FromPid, {updated, NewSeq}},
            #state{
              index=Index0,
              index_pid=IndexPid,
              updater_pid=UpPid,
              waiting_list=WaitList
             }=State) when UpPid == FromPid ->
    Index = Index0#index{current_seq=NewSeq},
    case reply_with_index(IndexPid, Index, WaitList) of
    [] ->
        {noreply, State#state{index=Index,
                              updater_pid=nil,
                              waiting_list=[]
                             }};
    StillWaiting ->
        Pid = spawn_link(fun() -> dreyfus_index_updater:update(IndexPid, Index) end),
        {noreply, State#state{index=Index,
                              updater_pid=Pid,
                              waiting_list=StillWaiting
                             }}
    end;
handle_info({'EXIT', _, {updated, _}}, State) ->
    {noreply, State};
handle_info({'EXIT', FromPid, Reason}, #state{
              index=Index,
              index_pid=IndexPid,
              waiting_list=WaitList
             }=State) when FromPid == IndexPid ->
    couch_log:notice(
        "index for ~p closed with reason ~p", [index_name(Index), Reason]),
    [gen_server:reply(Pid, {error, Reason}) || {Pid, _} <- WaitList],
    {stop, normal, State};
handle_info({'EXIT', FromPid, Reason}, #state{
              index=Index,
              updater_pid=UpPid,
              waiting_list=WaitList
             }=State) when FromPid == UpPid ->
    couch_log:info("Shutting down index server ~p, updater ~p closing w/ reason ~w",
        [index_name(Index), UpPid, Reason]),
    [gen_server:reply(Pid, {error, Reason}) || {Pid, _} <- WaitList],
    {stop, normal, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    % probably dreyfus_index_manager.
    couch_log:notice("Unknown pid ~p closed with reason ~p", [Pid, Reason]),
    {stop, normal, State};
handle_info({'DOWN',_,_,Pid,Reason}, #state{
              index=Index,
              waiting_list=WaitList
             }=State) ->
    couch_log:info("Shutting down index server ~p, db ~p closing w/ reason ~w",
        [index_name(Index), Pid, Reason]),
    [gen_server:reply(P, {error, Reason}) || {P, _} <- WaitList],
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions.

open_index(DbName, #index{analyzer=Analyzer, sig=Sig}) ->
    Path = <<DbName/binary,"/",Sig/binary>>,
    case clouseau_rpc:open_index(self(), Path, Analyzer) of
        {ok, Pid} ->
            case clouseau_rpc:get_update_seq(Pid) of
                {ok, Seq} ->
                    {ok, Pid, Seq};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

design_doc_to_index(#doc{id=Id,body={Fields}}, IndexName) ->
    Language = couch_util:get_value(<<"language">>, Fields, <<"javascript">>),
    {RawIndexes} = couch_util:get_value(<<"indexes">>, Fields, {[]}),
    InvalidDDocError = {invalid_design_doc,
        <<"index `", IndexName/binary, "` must have parameter `index`">>},
    case lists:keyfind(IndexName, 1, RawIndexes) of
        false ->
            {error, {not_found, <<IndexName/binary, " not found.">>}};
        {IndexName, {Index}} ->
            Analyzer = couch_util:get_value(<<"analyzer">>, Index, <<"standard">>),
            case couch_util:get_value(<<"index">>, Index) of
                undefined ->
                    {error, InvalidDDocError};
                Def ->
                    Sig = ?l2b(couch_util:to_hex(crypto:hash(md5,
                        term_to_binary({Analyzer, Def})))),
                    {ok, #index{
                        analyzer=Analyzer,
                        ddoc_id=Id,
                        def=Def,
                        def_lang=Language,
                        name=IndexName,
                        sig=Sig}}
            end;
        _ ->
            {error, InvalidDDocError}
    end.

reply_with_index(IndexPid, Index, WaitList) ->
    reply_with_index(IndexPid, Index, WaitList, []).

reply_with_index(_IndexPid, _Index, [], Acc) ->
    Acc;
reply_with_index(IndexPid, #index{current_seq=IndexSeq}=Index, [{Pid, Seq}|Rest], Acc) when Seq =< IndexSeq ->
    gen_server:reply(Pid, {ok, IndexPid, IndexSeq}),
    reply_with_index(IndexPid, Index, Rest, Acc);
reply_with_index(IndexPid, Index, [{Pid, Seq}|Rest], Acc) ->
    reply_with_index(IndexPid, Index, Rest, [{Pid, Seq}|Acc]).

index_name(#index{dbname=DbName,ddoc_id=DDocId,name=IndexName}) ->
    <<DbName/binary, " ", DDocId/binary, " ", IndexName/binary>>.

args_to_proplist(#index_query_args{} = Args) ->
    [
     {'query', Args#index_query_args.q},
     {limit, Args#index_query_args.limit},
     {refresh, Args#index_query_args.stale =:= false},
     {'after', Args#index_query_args.bookmark},
     {sort, Args#index_query_args.sort},
     {include_fields, Args#index_query_args.include_fields},
     {counts, Args#index_query_args.counts},
     {ranges, Args#index_query_args.ranges},
     {drilldown, Args#index_query_args.drilldown},
     {highlight_fields, Args#index_query_args.highlight_fields},
     {highlight_pre_tag, Args#index_query_args.highlight_pre_tag},
     {highlight_post_tag, Args#index_query_args.highlight_post_tag},
     {highlight_number, Args#index_query_args.highlight_number},
     {highlight_size, Args#index_query_args.highlight_size}
    ].

args_to_proplist2(#index_query_args{} = Args) ->
    [
     {'query', Args#index_query_args.q},
     {field, Args#index_query_args.grouping#grouping.by},
     {refresh, Args#index_query_args.stale =:= false},
     {groups, Args#index_query_args.grouping#grouping.groups},
     {group_sort, Args#index_query_args.grouping#grouping.sort},
     {sort, Args#index_query_args.sort},
     {limit, Args#index_query_args.limit},
     {include_fields, Args#index_query_args.include_fields},
     {highlight_fields, Args#index_query_args.highlight_fields},
     {highlight_pre_tag, Args#index_query_args.highlight_pre_tag},
     {highlight_post_tag, Args#index_query_args.highlight_post_tag},
     {highlight_number, Args#index_query_args.highlight_number},
     {highlight_size, Args#index_query_args.highlight_size}
    ].

search_int(Pid, QueryArgs0) ->
    QueryArgs = dreyfus_util:upgrade(QueryArgs0),
    case QueryArgs of
        #index_query_args{counts=nil,ranges=nil,drilldown=[],include_fields=nil,
                         highlight_fields=nil} ->
            clouseau_rpc:search(
                Pid,
                QueryArgs#index_query_args.q,
                QueryArgs#index_query_args.limit,
                QueryArgs#index_query_args.stale =:= false,
                QueryArgs#index_query_args.bookmark,
                QueryArgs#index_query_args.sort);
        _ ->
            Props = args_to_proplist(QueryArgs),
            clouseau_rpc:search(Pid, Props)
    end.

group1_int(Pid, QueryArgs0) ->
    QueryArgs = dreyfus_util:upgrade(QueryArgs0),
    #index_query_args{
        q = Query,
        stale = Stale,
        grouping = #grouping{
            by = GroupBy,
            offset = Offset,
            limit = Limit,
            sort = Sort
        }
    } = QueryArgs,
    clouseau_rpc:group1(Pid, Query, GroupBy, Stale =:= false, Sort,
                        Offset, Limit).

group2_int(Pid, QueryArgs0) ->
    QueryArgs = dreyfus_util:upgrade(QueryArgs0),
    case QueryArgs of
        #index_query_args{include_fields=nil, highlight_fields=nil} -> %remove after upgrade
            #index_query_args{
                q = Query,
                stale = Stale,
                sort = DocSort,
                limit = DocLimit,
                grouping = #grouping{
                    by = GroupBy,
                    groups = Groups,
                    sort = GroupSort
                }
            } = QueryArgs,
            clouseau_rpc:group2(Pid, Query, GroupBy, Stale =:= false, Groups,
                                GroupSort, DocSort, DocLimit);
        _ ->
            Props = args_to_proplist2(QueryArgs),
            clouseau_rpc:group2(Pid, Props)
    end.

info_int(Pid) ->
    clouseau_rpc:info(Pid).
