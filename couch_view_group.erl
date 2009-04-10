% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_view_group).
-behaviour(gen_server).

%% API
-export([start_link/1, request_group/2]).
-export([design_doc_to_view_group/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("couch_db.hrl").
	 
-record(group_state, {
    type,
    db_name,
    init_args,
    group,
    updater_pid=nil,
    compactor_pid=nil,
    waiting_commit=false,
    waiting_list=[],
    ref_counter=nil
}).

% api methods
request_group(Pid, Seq) ->
    ?LOG_DEBUG("request_group {Pid, Seq} ~p", [{Pid, Seq}]),
    case gen_server:call(Pid, {request_group, Seq}, infinity) of
    {ok, Group, RefCounter} ->
        couch_ref_counter:add(RefCounter),
        {ok, Group};
    Else ->
        ?LOG_DEBUG("get_updated_group replied with _Else ~p", [Else]),
        Else
    end.


% from template
start_link(InitArgs) ->
    case gen_server:start_link(couch_view_group,
            {InitArgs, self(), Ref = make_ref()}, []) of
    {ok, Pid} ->
        {ok, Pid};
    ignore -> 
        receive
        {Ref, Pid, Error} ->
            case process_info(self(), trap_exit) of
            {trap_exit, true} -> receive {'EXIT', Pid, _} -> ok end;
            {trap_exit, false} -> ok
            end,
            Error
        end;
    Error ->
        Error
    end.

% init differentiates between temp and design_doc views. It creates a closure
% which spawns the appropriate view_updater. (It might also spawn the first
% view_updater run.)
init({InitArgs, ReturnPid, Ref}) ->
    process_flag(trap_exit, true),
    case prepare_group(InitArgs, false) of
    {ok, #group{db=Db, fd=Fd}=Group} ->
        couch_db:monitor(Db),
        Pid = spawn_link(fun()-> couch_view_updater:update(Group) end),
        {ok, RefCounter} = couch_ref_counter:start([Fd]),
        {ok, #group_state{
                db_name=couch_db:name(Db),
                init_args=InitArgs,
                updater_pid = Pid,
                group=Group,
                ref_counter=RefCounter}};
    Error ->
        ReturnPid ! {Ref, self(), Error},
        ignore
    end.




% There are two sources of messages: couch_view, which requests an up to date
% view group, and the couch_view_updater, which when spawned, updates the
% group and sends it back here. We employ a caching mechanism, so that between
% database writes, we don't have to spawn a couch_view_updater with every view
% request. This should give us more control, and the ability to request view
% statuses eventually.

% The caching mechanism: each request is submitted with a seq_id for the
% database at the time it was read. We guarantee to return a view from that
% sequence or newer.

% If the request sequence is higher than our current high_target seq, we set
% that as the highest seqence. If the updater is not running, we launch it.

handle_call({request_group, RequestSeq}, From, 
        #group_state{
            db_name=DbName,
            group=#group{current_seq=Seq}=Group,
            updater_pid=nil,
            waiting_list=WaitList
            }=State) when RequestSeq > Seq ->
    {ok, Db} = couch_db:open(DbName, []),
    Group2 = Group#group{db=Db},
    Pid = spawn_link(fun()-> couch_view_updater:update(Group2) end),
    
    {noreply, State#group_state{
        updater_pid=Pid,
        group=Group2,
        waiting_list=[{From,RequestSeq}|WaitList]
        }, infinity};
        

% If the request seqence is less than or equal to the seq_id of a known Group,
% we respond with that Group.
handle_call({request_group, RequestSeq}, _From, #group_state{
            group = #group{current_seq=GroupSeq} = Group,
            ref_counter = RefCounter
        } = State) when RequestSeq =< GroupSeq  ->
    {reply, {ok, Group, RefCounter}, State};

% Otherwise: TargetSeq => RequestSeq > GroupSeq
% We've already initiated the appropriate action, so just hold the response until the group is up to the RequestSeq
handle_call({request_group, RequestSeq}, From,
        #group_state{waiting_list=WaitList}=State) ->
    {noreply, State#group_state{
        waiting_list=[{From, RequestSeq}|WaitList]
        }, infinity}.


handle_cast({start_compact, CompactFun}, #group_state{ compactor_pid=nil, 
        group=Group, init_args={view, RootDir, DbName, GroupId} } = State) ->
    ?LOG_INFO("Starting view group compaction", []),
    {ok, Db} = couch_db:open(DbName, []),
    {ok, Fd} = open_index_file(RootDir, DbName, <<GroupId/binary,".compact">>),
    NewGroup = reset_file(Db, Fd, DbName, Group),
    Pid = spawn_link(fun() -> CompactFun(Group, NewGroup) end),
    {noreply, State#group_state{compactor_pid = Pid}};
handle_cast({start_compact, _}, State) ->
    %% compact already running, this is a no-op
    {noreply, State};

handle_cast({compact_done, #group{fd=NewFd, current_seq=NewSeq} = NewGroup}, 
        #group_state{ 
            group = #group{current_seq=OldSeq} = Group,
            init_args = {view, RootDir, DbName, GroupId}, 
            updater_pid = nil,
            ref_counter = RefCounter
        } = State) when NewSeq >= OldSeq ->
    ?LOG_INFO("View Group compaction complete", []),
    BaseName = RootDir ++ "/." ++ ?b2l(DbName) ++ ?b2l(GroupId),
    FileName = BaseName ++ ".view",
    CompactName = BaseName ++".compact.view",
    file:delete(FileName),
    ok = file:rename(CompactName, FileName),
    
    %% cleanup old group
    couch_ref_counter:drop(RefCounter),
    {ok, NewRefCounter} = couch_ref_counter:start([NewFd]),
    case Group#group.db of
        nil -> ok;
        Else -> couch_db:close(Else)
    end,
    
    erlang:send_after(1000, self(), delayed_commit),
    {noreply, State#group_state{
        group=NewGroup, 
        ref_counter=NewRefCounter,
        compactor_pid=nil
    }};
handle_cast({compact_done, NewGroup}, #group_state{ 
        init_args={view, _RootDir, DbName, GroupId} } = State) ->
    ?LOG_INFO("View index compaction still behind main file", []),
    couch_db:close(NewGroup#group.db),
    {ok, Db} = couch_db:open(DbName, []),
    Pid = spawn_link(fun() -> 
        {_,Ref} = erlang:spawn_monitor(fun() -> 
            couch_view_updater:update(NewGroup#group{db = Db})
        end),
        receive
            {'DOWN', Ref, _, _, {new_group, NewGroup2}} ->
                #group{name=GroupId} = NewGroup2,
                Pid2 = couch_view:get_group_server(DbName, GroupId),
                gen_server:cast(Pid2, {compact_done, NewGroup2})
        end
    end),
    {noreply, State#group_state{compactor_pid = Pid}}.

handle_info(delayed_commit, #group_state{db_name=DbName,group=Group}=State) ->
    {ok, Db} = couch_db:open(DbName, []),
    CommittedSeq = couch_db:get_committed_update_seq(Db),
    couch_db:close(Db),
    if CommittedSeq >= Group#group.current_seq ->
        % save the header
        Header = {Group#group.sig, get_index_header_data(Group)},
        ok = couch_file:write_header(Group#group.fd, <<$r, $c, $k, 0>>, Header),
        {noreply, State#group_state{waiting_commit=false}};
    true ->
        % We can't commit the header because the database seq that's fully
        % committed to disk is still behind us. If we committed now and the
        % database lost those changes our view could be forever out of sync
        % with the database. But a crash before we commit these changes, no big
        % deal, we only lose incremental changes since last committal.
        erlang:send_after(1000, self(), delayed_commit),
        {noreply, State#group_state{waiting_commit=true}}
    end;

handle_info({'EXIT', FromPid, {new_group, #group{db=Db}=Group}},
        #group_state{db_name=DbName,
            updater_pid=UpPid,
            ref_counter=RefCounter,
            waiting_list=WaitList,
            waiting_commit=WaitingCommit}=State) when UpPid == FromPid ->
    ok = couch_db:close(Db),

    if Group#group.type == view andalso not WaitingCommit ->
        erlang:send_after(1000, self(), delayed_commit);
    true -> ok
    end,
    case reply_with_group(Group, WaitList, [], RefCounter) of
    [] ->
        {noreply, State#group_state{waiting_commit=true, waiting_list=[],
                group=Group#group{db=nil}, updater_pid=nil}};
    StillWaiting ->
        % we still have some waiters, reopen the database and reupdate the index
        {ok, Db2} = couch_db:open(DbName, []),
        Group2 = Group#group{db=Db2},
        Pid = spawn_link(fun() -> couch_view_updater:update(Group2) end),
        {noreply, State#group_state{waiting_commit=true,
                waiting_list=StillWaiting, group=Group2, updater_pid=Pid}}
    end;
    
handle_info({'EXIT', FromPid, reset}, 
        #group_state{
            init_args=InitArgs,
            updater_pid=UpPid,
            group=Group}=State) when UpPid == FromPid ->
    ok = couch_db:close(Group#group.db),
    case prepare_group(InitArgs, true) of
    {ok, ResetGroup} ->
        Pid = spawn_link(fun()-> couch_view_updater:update(ResetGroup) end),
        {noreply, State#group_state{
                updater_pid=Pid,
                group=ResetGroup}};
    Error ->
        {stop, normal, reply_all(State, Error)}
    end;
    
handle_info({'EXIT', _FromPid, normal}, State) ->
    {noreply, State};
    
handle_info({'EXIT', FromPid, Reason}, State) ->
    ?LOG_DEBUG("Exit from linked pid: ~p", [{FromPid, Reason}]),
    {stop, Reason, State};
    
handle_info({'DOWN',_,_,_,_}, State) ->
    ?LOG_INFO("Shutting down view group server, monitored db is closing.", []),
    {stop, normal, reply_all(State, shutdown)}.


terminate(Reason, State) ->
    reply_all(State, Reason),
    couch_util:terminate_linked(Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local Functions

% reply_with_group/3
% for each item in the WaitingList {Pid, Seq}
% if the Seq is =< GroupSeq, reply
reply_with_group(Group=#group{current_seq=GroupSeq}, [{Pid, Seq}|WaitList], 
        StillWaiting, RefCounter) when Seq =< GroupSeq ->
    gen_server:reply(Pid, {ok, Group, RefCounter}),
    reply_with_group(Group, WaitList, StillWaiting, RefCounter);

% else
% put it in the continuing waiting list    
reply_with_group(Group, [{Pid, Seq}|WaitList], StillWaiting, RefCounter) ->
    reply_with_group(Group, WaitList, [{Pid, Seq}|StillWaiting], RefCounter);

% return the still waiting list
reply_with_group(_Group, [], StillWaiting, _RefCounter) ->
    StillWaiting.

reply_all(#group_state{waiting_list=WaitList}=State, Reply) ->
    [catch gen_server:reply(Pid, Reply) || {Pid, _} <- WaitList],
    State#group_state{waiting_list=[]}.

prepare_group({view, RootDir, DbName, GroupId}, ForceReset)->
    case open_db_group(DbName, GroupId) of
    {ok, Db, #group{sig=Sig}=Group} ->
        case open_index_file(RootDir, DbName, GroupId) of
        {ok, Fd} ->
            if ForceReset ->
                {ok, reset_file(Db, Fd, DbName, Group)};
            true ->
                case (catch couch_file:read_header(Fd, <<$r, $c, $k, 0>>)) of
                {ok, {Sig, HeaderInfo}} ->
                    % sigs match!
                    {ok, init_group(Db, Fd, Group, HeaderInfo)};
                _ ->
                    {ok, reset_file(Db, Fd, DbName, Group)}
                end
            end;
        Error ->
            catch delete_index_file(RootDir, DbName, GroupId),
            Error
        end;
    Error ->
        catch delete_index_file(RootDir, DbName, GroupId),
        Error
    end;
prepare_group({slow_view, DbName, Fd, Lang, DesignOptions, MapSrc, RedSrc}, _ForceReset) ->
    case couch_db:open(DbName, []) of
    {ok, Db} ->
        View = #view{map_names=[<<"_temp">>],
            id_num=0,
            btree=nil,
            def=MapSrc,
            reduce_funs= if RedSrc==[] -> []; true -> [{<<"_temp">>, RedSrc}] end},
        {ok, init_group(Db, Fd, #group{type=slow_view, name= <<"_temp">>, db=Db,
            views=[View], def_lang=Lang, design_options=DesignOptions}, nil)};
    Error ->
        Error
    end.


get_index_header_data(#group{current_seq=Seq, purge_seq=PurgeSeq, 
            id_btree=IdBtree,views=Views}) ->
    ViewStates = [couch_btree:get_state(Btree) || #view{btree=Btree} <- Views],
    #index_header{seq=Seq,
            purge_seq=PurgeSeq,
            id_btree_state=couch_btree:get_state(IdBtree),
            view_states=ViewStates}.


open_index_file(RootDir, DbName, GroupId) ->
    FileName = RootDir ++ "/." ++ ?b2l(DbName) ++ ?b2l(GroupId) ++".view",
    case couch_file:open(FileName) of
    {ok, Fd}        -> {ok, Fd};
    {error, enoent} -> couch_file:open(FileName, [create]);
    Error           -> Error
    end.
    
open_db_group(DbName, GroupId) ->
    case couch_db:open(DbName, []) of
    {ok, Db} ->
        case couch_db:open_doc(Db, GroupId) of
        {ok, Doc} ->
            {ok, Db, design_doc_to_view_group(Doc)};
        Else ->
            couch_db:close(Db),
            Else
        end;
    Else ->
        Else
    end.

% maybe move to another module
design_doc_to_view_group(#doc{id=Id,body={Fields}}) ->
    Language = proplists:get_value(<<"language">>, Fields, <<"javascript">>),
    {DesignOptions} = proplists:get_value(<<"options">>, Fields, {[]}),
    {RawViews} = proplists:get_value(<<"views">>, Fields, {[]}),

    % add the views to a dictionary object, with the map source as the key
    DictBySrc =
    lists:foldl(
        fun({Name, {MRFuns}}, DictBySrcAcc) ->
            MapSrc = proplists:get_value(<<"map">>, MRFuns),
            RedSrc = proplists:get_value(<<"reduce">>, MRFuns, null),
            View =
            case dict:find(MapSrc, DictBySrcAcc) of
                {ok, View0} -> View0;
                error -> #view{def=MapSrc} % create new view object
            end,
            View2 =
            if RedSrc == null ->
                View#view{map_names=[Name|View#view.map_names]};
            true ->
                View#view{reduce_funs=[{Name,RedSrc}|View#view.reduce_funs]}
            end,
            dict:store(MapSrc, View2, DictBySrcAcc)
        end, dict:new(), RawViews),
    % number the views
    {Views, _N} = lists:mapfoldl(
        fun({_Src, View}, N) ->
            {View#view{id_num=N},N+1}
        end, 0, dict:to_list(DictBySrc)),

    Group = #group{name=Id, views=Views, def_lang=Language, design_options=DesignOptions},
    Group#group{sig=erlang:md5(term_to_binary(Group))}.

reset_group(#group{views=Views}=Group) ->
    Views2 = [View#view{btree=nil} || View <- Views],
    Group#group{db=nil,fd=nil,query_server=nil,current_seq=0,
            id_btree=nil,views=Views2}.

reset_file(Db, Fd, DbName, #group{sig=Sig,name=Name} = Group) ->
    ?LOG_DEBUG("Reseting group index \"~s\" in db ~s", [Name, DbName]),
    ok = couch_file:truncate(Fd, 0),
    ok = couch_file:write_header(Fd, <<$r, $c, $k, 0>>, {Sig, nil}),
    init_group(Db, Fd, reset_group(Group), nil).

delete_index_file(RootDir, DbName, GroupId) ->
    file:delete(RootDir ++ "/." ++ binary_to_list(DbName)
            ++ binary_to_list(GroupId) ++ ".view").

init_group(Db, Fd, #group{views=Views}=Group, nil) ->
    init_group(Db, Fd, Group,
        #index_header{seq=0, purge_seq=couch_db:get_purge_seq(Db),
            id_btree_state=nil, view_states=[nil || _ <- Views]});
init_group(Db, Fd, #group{def_lang=Lang,views=Views}=Group, IndexHeader) ->
     #index_header{seq=Seq, purge_seq=PurgeSeq,
            id_btree_state=IdBtreeState, view_states=ViewStates} = IndexHeader,
    {ok, IdBtree} = couch_btree:open(IdBtreeState, Fd),
    Views2 = lists:zipwith(
        fun(BtreeState, #view{reduce_funs=RedFuns}=View) ->
            FunSrcs = [FunSrc || {_Name, FunSrc} <- RedFuns],
            ReduceFun = 
                fun(reduce, KVs) ->
                    KVs2 = couch_view:expand_dups(KVs,[]),
                    KVs3 = couch_view:detuple_kvs(KVs2,[]),
                    {ok, Reduced} = couch_query_servers:reduce(Lang, FunSrcs, 
                        KVs3),
                    {length(KVs3), Reduced};
                (rereduce, Reds) ->
                    Count = lists:sum([Count0 || {Count0, _} <- Reds]),
                    UserReds = [UserRedsList || {_, UserRedsList} <- Reds],
                    {ok, Reduced} = couch_query_servers:rereduce(Lang, FunSrcs,
                        UserReds),
                    {Count, Reduced}
                end,
            {ok, Btree} = couch_btree:open(BtreeState, Fd,
                        [{less, fun couch_view:less_json_keys/2},
                            {reduce, ReduceFun}]),
            View#view{btree=Btree}
        end,
        ViewStates, Views),
    Group#group{db=Db, fd=Fd, current_seq=Seq, purge_seq=PurgeSeq,
        id_btree=IdBtree, views=Views2}.


