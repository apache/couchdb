%   Copyright 2007, 2008 Damien Katz <damien_katz@yahoo.com>
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

-module(couch_view).
-behaviour(gen_server).

-export([start_link/1,fold/4,fold/5,less_json/2, start_update_loop/3, start_temp_update_loop/5]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-export([get_reduce_view/1, get_map_view/1,get_row_count/1,reduce_to_count/1, fold_reduce/7]).

-include("couch_db.hrl").

-record(group,
    {sig=nil,
    db=nil,
    fd=nil,
    name,
    def_lang,
    views,
    id_btree=nil,
    current_seq=0,
    query_server=nil
    }).

-record(view,
    {id_num,
    map_names=[],
    def,
    btree=nil,
    reduce_funs=[]
    }).

-record(server,
    {root_dir
    }).

start_link(RootDir) ->
    gen_server:start_link({local, couch_view}, couch_view, RootDir, []).



get_temp_updater(DbName, Type, MapSrc, RedSrc) ->
    {ok, Pid} = gen_server:call(couch_view, {start_temp_updater, DbName, Type, MapSrc, RedSrc}),
    Pid.

get_updater(DbName, GroupId) ->
    {ok, Pid} = gen_server:call(couch_view, {start_updater, DbName, GroupId}),
    Pid.
    
get_updated_group(Pid) ->
	Mref = erlang:monitor(process, Pid),
    receive
	{'DOWN', Mref, _, _, Reason} ->
	    throw(Reason)
    after 0 ->
	    Pid ! {self(), get_updated},
	    receive
    	{Pid, Response} ->
    	    erlang:demonitor(Mref),
    	    receive
        		{'DOWN', Mref, _, _, _} -> ok
        	    after 0 -> ok
    	    end,
    	    Response;
    	{'DOWN', Mref, _, _, Reason} ->
    	    throw(Reason)
        end
    end.

get_row_count(#view{btree=Bt}) ->
    {ok, {Count, _Reds}} = couch_btree:full_reduce(Bt),
    {ok, Count}.

get_reduce_view({temp, DbName, Type, MapSrc, RedSrc}) ->
    {ok, #group{views=[View]}} = get_updated_group(get_temp_updater(DbName, Type, MapSrc, RedSrc)),
    {ok, {temp_reduce, View}};
get_reduce_view({DbName, GroupId, Name}) ->
    {ok, #group{views=Views,def_lang=Lang}} =
            get_updated_group(get_updater(DbName, GroupId)),
    get_reduce_view0(Name, Lang, Views).

get_reduce_view0(_Name, _Lang, []) ->
    {not_found, missing_named_view};
get_reduce_view0(Name, Lang, [#view{reduce_funs=RedFuns}=View|Rest]) ->
    case get_key_pos(Name, RedFuns, 0) of
        0 -> get_reduce_view0(Name, Lang, Rest);
        N -> {ok, {reduce, N, Lang, View}}
    end.

fold_reduce({temp_reduce, #view{btree=Bt}}, Dir, StartKey, EndKey, GroupFun, Fun, Acc) ->

    WrapperFun = fun({GroupedKey, _}, PartialReds, Acc0) ->
            {_, [Red]} = couch_btree:final_reduce(Bt, PartialReds),
            Fun(GroupedKey, Red, Acc0)
        end,
    couch_btree:fold_reduce(Bt, Dir, StartKey, EndKey, GroupFun,
            WrapperFun, Acc);

fold_reduce({reduce, NthRed, Lang, #view{btree=Bt, reduce_funs=RedFuns}}, Dir, StartKey, EndKey, GroupFun, Fun, Acc) ->
    PreResultPadding = lists:duplicate(NthRed - 1, []),
    PostResultPadding = lists:duplicate(length(RedFuns) - NthRed, []),
    {_Name, FunSrc} = lists:nth(NthRed,RedFuns),
    ReduceFun =
        fun(reduce, KVs) ->
            {ok, Reduced} = couch_query_servers:reduce(Lang, [FunSrc], KVs),
            {0, PreResultPadding ++ Reduced ++ PostResultPadding};
        (rereduce, Reds) ->
            UserReds = [[lists:nth(NthRed, UserRedsList)] || {_, UserRedsList} <- Reds],
            {ok, Reduced} = couch_query_servers:rereduce(Lang, [FunSrc], UserReds),
            {0, PreResultPadding ++ Reduced ++ PostResultPadding}
        end,
    WrapperFun = fun({GroupedKey, _}, PartialReds, Acc0) ->
            {_, Reds} = couch_btree:final_reduce(ReduceFun, PartialReds),
            Fun(GroupedKey, lists:nth(NthRed, Reds), Acc0)
        end,
    couch_btree:fold_reduce(Bt, Dir, StartKey, EndKey, GroupFun,
            WrapperFun, Acc).
        
get_key_pos(_Key, [], _N) ->
    0;
get_key_pos(Key, [{Key1,_Value}|_], N) when Key == Key1 ->
    N + 1;
get_key_pos(Key, [_|Rest], N) ->
    get_key_pos(Key, Rest, N+1).
        
get_map_view({temp, DbName, Type, Src}) ->
    {ok, #group{views=[View]}} = get_updated_group(get_temp_updater(DbName, Type, Src, [])),
    {ok, View};
get_map_view({DbName, GroupId, Name}) ->
    {ok, #group{views=Views}} = get_updated_group(get_updater(DbName, GroupId)),
    get_map_view0(Name, Views).

get_map_view0(_Name, []) ->
    {not_found, missing_named_view};
get_map_view0(Name, [#view{map_names=MapNames}=View|Rest]) ->
    case lists:member(Name, MapNames) of
        true -> {ok, View};
        false -> get_map_view0(Name, Rest)
    end.

reduce_to_count(Reductions) ->
    {Count, _} = 
    couch_btree:final_reduce(
        fun(reduce, KVs) ->
            {length(KVs), []};
        (rereduce, Reds) ->
            {lists:sum([Count0 || {Count0, _} <- Reds]), []}
        end, Reductions),
    Count.
                

design_doc_to_view_group(#doc{id=Id,body={obj, Fields}}) ->
    Language = proplists:get_value("language", Fields, "javascript"),
    {obj, RawViews} = proplists:get_value("views", Fields, {obj, []}),
            
    % add the views to a dictionary object, with the map source as the key
    DictBySrc =
    lists:foldl(
        fun({Name, {obj, MRFuns}}, DictBySrcAcc) ->
            MapSrc = proplists:get_value("map", MRFuns),
            RedSrc = proplists:get_value("reduce", MRFuns, null),
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
    
    Group = #group{name=Id, views=Views, def_lang=Language},
    Group#group{sig=erlang:md5(term_to_binary(Group))}.
    


fold(#view{btree=Btree}, Dir, Fun, Acc) ->
    {ok, _AccResult} = couch_btree:fold(Btree, Dir, Fun, Acc).

fold(#view{btree=Btree}, StartKey, Dir, Fun, Acc) ->
    {ok, _AccResult} = couch_btree:fold(Btree, StartKey, Dir, Fun, Acc).


init(RootDir) ->
    couch_db_update_notifier:start_link(
        fun({deleted, DbName}) ->
            gen_server:cast(couch_view, {reset_indexes, DbName});
        ({created, DbName}) ->
            gen_server:cast(couch_view, {reset_indexes, DbName});
        (_Else) ->
            ok
        end),
    ets:new(couch_views_by_db, [bag, private, named_table]),
    ets:new(couch_views_by_name, [set, protected, named_table]),
    ets:new(couch_views_by_updater, [set, private, named_table]),
    ets:new(couch_views_temp_fd_by_db, [set, protected, named_table]),
    process_flag(trap_exit, true),
    {ok, #server{root_dir=RootDir}}.

terminate(_Reason, _) ->
    catch ets:delete(couch_views_by_name),
    catch ets:delete(couch_views_by_updater),
    catch ets:delete(couch_views_by_db),
    catch ets:delete(couch_views_temp_fd_by_db).


handle_call({start_temp_updater, DbName, Lang, MapSrc, RedSrc}, _From, #server{root_dir=Root}=Server) ->
    <<SigInt:128/integer>> = erlang:md5(Lang ++ [0] ++ MapSrc ++ [0] ++ RedSrc),
    Name = lists:flatten(io_lib:format("_temp_~.36B",[SigInt])),
    Pid = 
    case ets:lookup(couch_views_by_name, {DbName, Name}) of
    [] ->
        case ets:lookup(couch_views_temp_fd_by_db, DbName) of
        [] ->
            FileName = Root ++ "/." ++ DbName ++ "_temp",
            {ok, Fd} = couch_file:open(FileName, [create, overwrite]),
            Count = 0;
        [{_, Fd, Count}] ->
            ok
        end,
        ?LOG_DEBUG("Spawning new temp update process for db ~s.", [DbName]),
        NewPid = spawn_link(couch_view, start_temp_update_loop,
                    [DbName, Fd, Lang, MapSrc, RedSrc]),
        true = ets:insert(couch_views_temp_fd_by_db, {DbName, Fd, Count + 1}),
        add_to_ets(NewPid, DbName, Name),
        NewPid;
    [{_, ExistingPid0}] ->
        ExistingPid0
    end,
    {reply, {ok, Pid}, Server};
handle_call({start_updater, DbName, GroupId}, _From, #server{root_dir=Root}=Server) ->
    Pid = 
    case ets:lookup(couch_views_by_name, {DbName, GroupId}) of
    [] ->
        ?LOG_DEBUG("Spawning new update process for view group ~s in database ~s.", [GroupId, DbName]),
        NewPid = spawn_link(couch_view, start_update_loop, [Root, DbName, GroupId]),
        add_to_ets(NewPid, DbName, GroupId),
        NewPid;
    [{_, ExistingPid0}] ->
        ExistingPid0
    end,
    {reply, {ok, Pid}, Server}.

handle_cast({reset_indexes, DbName}, #server{root_dir=Root}=Server) ->
    % shutdown all the updaters
    Names = ets:lookup(couch_views_by_db, DbName),
    lists:foreach(
        fun({_DbName, GroupId}) ->
            ?LOG_DEBUG("Killing update process for view group ~s. in database ~s.", [GroupId, DbName]),
            [{_, Pid}] = ets:lookup(couch_views_by_name, {DbName, GroupId}),
            exit(Pid, kill),
            receive {'EXIT', Pid, _} ->
                delete_from_ets(Pid, DbName, GroupId)
            end
        end, Names),
    delete_index_dir(Root, DbName),
    file:delete(Root ++ "/." ++ DbName ++ "_temp"),
    {noreply, Server}.

handle_info({'EXIT', _FromPid, normal}, Server) ->
    {noreply, Server};
handle_info({'EXIT', FromPid, Reason}, #server{root_dir=RootDir}=Server) ->
    case ets:lookup(couch_views_by_updater, FromPid) of
    [] -> % non-updater linked process must have died, we propagate the error
        ?LOG_ERROR("Exit on non-updater process: ~p", [Reason]),
        exit(Reason);
    [{_, {DbName, "_temp_" ++ _ = GroupId}}] ->
        delete_from_ets(FromPid, DbName, GroupId),
        [{_, Fd, Count}] = ets:lookup(couch_views_temp_fd_by_db, DbName),
        case Count of
        1 -> % Last ref
            couch_file:close(Fd),
            file:delete(RootDir ++ "/." ++ DbName ++ "_temp"),
            true = ets:delete(couch_views_temp_fd_by_db, DbName);
        _ ->
            true = ets:insert(couch_views_temp_fd_by_db, {DbName, Fd, Count - 1})
        end;
    [{_, {DbName, GroupId}}] ->
        delete_from_ets(FromPid, DbName, GroupId)
    end,
    {noreply, Server};
handle_info(Msg, _Server) ->
    ?LOG_ERROR("Bad message received for view module: ~p", [Msg]),
    exit({error, Msg}).
    
add_to_ets(Pid, DbName, GroupId) ->
    true = ets:insert(couch_views_by_updater, {Pid, {DbName, GroupId}}),
    true = ets:insert(couch_views_by_name, {{DbName, GroupId}, Pid}),
    true = ets:insert(couch_views_by_db, {DbName, GroupId}).
    
delete_from_ets(Pid, DbName, GroupId) ->
    true = ets:delete(couch_views_by_updater, Pid),
    true = ets:delete(couch_views_by_name, {DbName, GroupId}),
    true = ets:delete_object(couch_views_by_db, {DbName, GroupId}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


start_temp_update_loop(DbName, Fd, Lang, MapSrc, RedSrc) ->
    NotifyPids = get_notify_pids(1000),
    case couch_server:open(DbName) of
    {ok, Db} ->
        View = #view{map_names=["_temp"],
            id_num=0,
            btree=nil,
            def=MapSrc,
            reduce_funs= if RedSrc==[] -> []; true -> [{"_temp", RedSrc}] end},
        Group = #group{name="_temp",
            db=Db,
            views=[View],
            current_seq=0,
            def_lang=Lang,
            id_btree=nil},
        Group2 = init_group(Db, Fd, Group,nil),
        temp_update_loop(Group2, NotifyPids);
    Else ->
        exit(Else)
    end.

temp_update_loop(Group, NotifyPids) ->
    {ok, Group2} = update_group(Group),
    [Pid ! {self(), {ok, Group2}} || Pid <- NotifyPids],
    garbage_collect(),
    temp_update_loop(Group2, get_notify_pids(10000)).


reset_group(#group{views=Views}=Group) ->
    Views2 = [View#view{btree=nil} || View <- Views],
    Group#group{db=nil,fd=nil,query_server=nil,current_seq=0,
            id_btree=nil,views=Views2}.

start_update_loop(RootDir, DbName, GroupId) ->
    % wait for a notify request before doing anything. This way, we can just
    % exit and any exits will be noticed by the callers.
    start_update_loop(RootDir, DbName, GroupId, get_notify_pids(1000)).
    
start_update_loop(RootDir, DbName, GroupId, NotifyPids) ->
    {Db, Group} =
    case (catch couch_server:open(DbName)) of
    {ok, Db0} ->
        case (catch couch_db:open_doc(Db0, GroupId)) of
        {ok, Doc} ->
            {Db0, design_doc_to_view_group(Doc)};
 		Else ->
 		    delete_index_file(RootDir, DbName, GroupId),
 		    exit(Else)
 		end;
 	Else ->
 	    delete_index_file(RootDir, DbName, GroupId),
 	    exit(Else)
 	end,
 	FileName = RootDir ++ "/." ++ DbName ++ GroupId ++".view",
 	Group2 =
    case couch_file:open(FileName) of
    {ok, Fd} ->
        Sig = Group#group.sig,
        case (catch couch_file:read_header(Fd, <<$r, $c, $k, 0>>)) of
        {ok, {Sig, HeaderInfo}} ->
            % sigs match!
            init_group(Db, Fd, Group, HeaderInfo);
        _ ->
            reset_file(Db, Fd, DbName, Group)
        end;
    {error, enoent} ->
        case couch_file:open(FileName, [create]) of
        {ok, Fd} -> reset_file(Db, Fd, DbName, Group);
        Error    -> throw(Error)
        end
    end,
    
    update_loop(RootDir, DbName, GroupId, Group2, NotifyPids).

reset_file(Db, Fd, DbName, #group{sig=Sig,name=Name} = Group) ->
    ?LOG_DEBUG("Reseting group index \"~s\" in db ~s", [Name, DbName]),
    ok = couch_file:truncate(Fd, 0),
    ok = couch_file:write_header(Fd, <<$r, $c, $k, 0>>, {Sig, nil}),
    init_group(Db, Fd, reset_group(Group), nil).

update_loop(RootDir, DbName, GroupId, #group{sig=Sig,fd=Fd}=Group, NotifyPids) ->
    try update_group(Group) of
    {ok, Group2} ->    
        HeaderData = {Sig, get_index_header_data(Group2)},
        ok = couch_file:write_header(Fd, <<$r, $c, $k, 0>>, HeaderData),
        [Pid ! {self(), {ok, Group2}} || Pid <- NotifyPids],
        garbage_collect(),
        update_loop(RootDir, DbName, GroupId, Group2, get_notify_pids(100000))
    catch
    restart ->
        couch_file:close(Group#group.fd),
        start_update_loop(RootDir, DbName, GroupId, NotifyPids ++ get_notify_pids())
    end.

% wait for the first request to come in.
get_notify_pids(Wait) ->
    receive
    {Pid, get_updated} ->
        [Pid | get_notify_pids()];
    Else ->
        ?LOG_ERROR("Unexpected message in view updater: ~p", [Else]),
        exit({error, Else})
    after Wait ->
        exit(wait_timeout)
	end.
% then keep getting all available and return.
get_notify_pids() ->
    receive
    {Pid, get_updated} ->
        [Pid | get_notify_pids()]
	after 0 ->
	    []
	end.
    
update_group(#group{db=Db,current_seq=CurrentSeq, views=Views}=Group) ->
    ViewEmptyKVs = [{View, []} || View <- Views],
    % compute on all docs modified since we last computed.
    {ok, {UncomputedDocs, Group2, ViewKVsToAdd, DocIdViewIdKeys, NewSeq}}
        = couch_db:enum_docs_since(
            Db,
            CurrentSeq,
            fun(DocInfo, _, Acc) -> process_doc(Db, DocInfo, Acc) end,
            {[], Group, ViewEmptyKVs, [], CurrentSeq}
            ),

    {Group3, Results} = view_compute(Group2, UncomputedDocs),
    {ViewKVsToAdd2, DocIdViewIdKeys2} = view_insert_query_results(UncomputedDocs, Results, ViewKVsToAdd, DocIdViewIdKeys),
    couch_query_servers:stop_doc_map(Group3#group.query_server),
    if CurrentSeq /= NewSeq ->
        {ok, Group4} = write_changes(Group3, ViewKVsToAdd2, DocIdViewIdKeys2, NewSeq),
        {ok, Group4#group{query_server=nil}};
    true ->
        {ok, Group3#group{query_server=nil}}
    end.
    
delete_index_dir(RootDir, DbName) ->
    nuke_dir(RootDir ++ "/." ++ DbName ++ "_design").

nuke_dir(Dir) ->
    case file:list_dir(Dir) of
    {error, enoent} -> ok; % doesn't exist
    {ok, Files} ->
        lists:foreach(
            fun(File)->
                Full = Dir ++ "/" ++ File,
                case file:delete(Full) of
                ok -> ok;
                {error, eperm} ->
                    ok = nuke_dir(Full)
                end
            end,
            Files),    
        ok = file:del_dir(Dir)
    end.

delete_index_file(RootDir, DbName, GroupId) ->
    file:delete(RootDir ++ "/." ++ DbName ++ GroupId ++ ".view").

init_group(Db, Fd, #group{views=Views}=Group, nil = _IndexHeaderData) ->
    init_group(Db, Fd, Group, {0, nil, [nil || _ <- Views]});
init_group(Db, Fd, #group{def_lang=Lang,views=Views}=Group,
        {Seq, IdBtreeState, ViewStates} = _IndexHeaderData) ->
    {ok, IdBtree} = couch_btree:open(IdBtreeState, Fd),
    Views2 = lists:zipwith(
        fun(BtreeState, #view{reduce_funs=RedFuns}=View) ->
            FunSrcs = [FunSrc || {_Name, FunSrc} <- RedFuns],
            ReduceFun = 
                fun(reduce, KVs) ->
                    {ok, Reduced} = couch_query_servers:reduce(Lang, FunSrcs, KVs),
                    {length(KVs), Reduced};
                (rereduce, Reds) ->
                    Count = lists:sum([Count0 || {Count0, _} <- Reds]),
                    UserReds = [UserRedsList || {_, UserRedsList} <- Reds],
                    {ok, Reduced} = couch_query_servers:rereduce(Lang, FunSrcs, UserReds),
                    {Count, Reduced}
                end,
            {ok, Btree} = couch_btree:open(BtreeState, Fd,
                        [{less, fun less_json/2},{reduce, ReduceFun}]),
            View#view{btree=Btree}
        end,
        ViewStates, Views),
    Group#group{db=Db, fd=Fd, current_seq=Seq, id_btree=IdBtree, views=Views2}.


get_index_header_data(#group{current_seq=Seq,id_btree=IdBtree,views=Views}) ->
    ViewStates = [couch_btree:get_state(Btree) || #view{btree=Btree} <- Views],
    {Seq, couch_btree:get_state(IdBtree), ViewStates}.



less_json(A, B) ->
    TypeA = type_sort(A),
    TypeB = type_sort(B),
    if
    TypeA == TypeB ->
        less_same_type(A,B);
    true ->
        TypeA < TypeB
    end.

type_sort(V) when is_atom(V) -> 0;
type_sort(V) when is_integer(V) -> 1;
type_sort(V) when is_float(V) -> 1;
type_sort(V) when is_list(V) -> 2;
type_sort({obj, _}) -> 4; % must come before tuple test below
type_sort(V) when is_tuple(V) -> 3;
type_sort(V) when is_binary(V) -> 5.

atom_sort(nil) -> 0;
atom_sort(null) -> 1;
atom_sort(false) -> 2;
atom_sort(true) -> 3.

less_same_type(A,B) when is_atom(A) ->
    atom_sort(A) < atom_sort(B);
less_same_type(A,B) when is_list(A) ->
    couch_util:collate(A, B) < 0;
less_same_type({obj, AProps}, {obj, BProps}) ->
    less_props(AProps, BProps);
less_same_type(A, B) when is_tuple(A) ->
    less_list(tuple_to_list(A),tuple_to_list(B));
less_same_type(A, B) ->
    A < B.

ensure_list(V) when is_list(V) -> V;
ensure_list(V) when is_atom(V) -> atom_to_list(V).

less_props([], [_|_]) ->
    true;
less_props(_, []) ->
    false;
less_props([{AKey, AValue}|RestA], [{BKey, BValue}|RestB]) ->
    case couch_util:collate(ensure_list(AKey), ensure_list(BKey)) of
    -1 -> true;
    1 -> false;
    0 ->
        case less_json(AValue, BValue) of
        true -> true;
        false ->
            case less_json(BValue, AValue) of
            true -> false;
            false ->
                less_props(RestA, RestB)
            end
        end
    end.

less_list([], [_|_]) ->
    true;
less_list(_, []) ->
    false;
less_list([A|RestA], [B|RestB]) ->
    case less_json(A,B) of
    true -> true;
    false ->
        case less_json(B,A) of
        true -> false;
        false ->
            less_list(RestA, RestB)
        end
    end.

process_doc(Db, DocInfo, {Docs, #group{sig=Sig,name=GroupId}=Group, ViewKVs, DocIdViewIdKeys, _LastSeq}) ->
    % This fun computes once for each document
    #doc_info{id=DocId, update_seq=Seq, deleted=Deleted} = DocInfo,
    case DocId of
    GroupId ->
        % uh oh. this is the design doc with our definitions. See if
        % anything in the definition changed.
        case couch_db:open_doc(Db, DocInfo) of
        {ok, Doc} ->
            case design_doc_to_view_group(Doc) of
            #group{sig=Sig} ->
                % The same md5 signature, keep on computing
                {ok, {Docs, Group, ViewKVs, DocIdViewIdKeys, Seq}};
            _ ->
                throw(restart)
            end;
        {not_found, deleted} ->
            throw(restart)
        end;
    ?DESIGN_DOC_PREFIX ++ _ -> % we skip design docs
        {ok, {Docs, Group, ViewKVs, DocIdViewIdKeys, Seq}};
    _ ->
        {Docs2, DocIdViewIdKeys2} =
        if Deleted ->
            {Docs, [{DocId, []} | DocIdViewIdKeys]};
        true ->
            {ok, Doc} = couch_db:open_doc(Db, DocInfo, [conflicts, deleted_conflicts]),
            {[Doc | Docs], DocIdViewIdKeys}
        end,
        case couch_util:should_flush() of
        true ->
            {Group1, Results} = view_compute(Group, Docs2),
            {ViewKVs3, DocIdViewIdKeys3} = view_insert_query_results(Docs2, Results, ViewKVs, DocIdViewIdKeys2),
            {ok, Group2} = write_changes(Group1, ViewKVs3, DocIdViewIdKeys3, Seq),
            garbage_collect(),
            ViewEmptyKeyValues = [{View, []} || View <- Group2#group.views],
            {ok, {[], Group2, ViewEmptyKeyValues, [], Seq}};
        false ->
            {ok, {Docs2, Group, ViewKVs, DocIdViewIdKeys2, Seq}}
        end
    end.

view_insert_query_results([], [], ViewKVs, DocIdViewIdKeysAcc) ->
    {ViewKVs, DocIdViewIdKeysAcc};
view_insert_query_results([Doc|RestDocs], [QueryResults | RestResults], ViewKVs, DocIdViewIdKeysAcc) ->
    {NewViewKVs, NewViewIdKeys} = view_insert_doc_query_results(Doc, QueryResults, ViewKVs, [], []),
    NewDocIdViewIdKeys = [{Doc#doc.id, NewViewIdKeys} | DocIdViewIdKeysAcc],
    view_insert_query_results(RestDocs, RestResults, NewViewKVs, NewDocIdViewIdKeys).


view_insert_doc_query_results(_Doc, [], [], ViewKVsAcc, ViewIdKeysAcc) ->
    {lists:reverse(ViewKVsAcc), lists:reverse(ViewIdKeysAcc)};
view_insert_doc_query_results(#doc{id=DocId}=Doc, [ResultKVs|RestResults], [{View, KVs}|RestViewKVs], ViewKVsAcc, ViewIdKeysAcc) ->
    NewKVs = [{{Key, DocId}, Value} || {Key, Value} <- ResultKVs],
    NewViewIdKeys = [{View#view.id_num, Key} || {Key, _Value} <- ResultKVs],
    NewViewKVsAcc = [{View, NewKVs ++ KVs} | ViewKVsAcc],
    NewViewIdKeysAcc = NewViewIdKeys ++ ViewIdKeysAcc,
    view_insert_doc_query_results(Doc, RestResults, RestViewKVs, NewViewKVsAcc, NewViewIdKeysAcc).

view_compute(Group, []) ->
    {Group, []};
view_compute(#group{def_lang=DefLang, query_server=QueryServerIn}=Group, Docs) ->
    {ok, QueryServer} =
    case QueryServerIn of
    nil -> % doc map not started
        Definitions = [View#view.def || View <- Group#group.views],
        couch_query_servers:start_doc_map(DefLang, Definitions);
    _ ->
        {ok, QueryServerIn}
    end,
    {ok, Results} = couch_query_servers:map_docs(QueryServer, Docs),
    {Group#group{query_server=QueryServer}, Results}.


dict_find(Key, DefaultValue, Dict) ->
    case dict:find(Key, Dict) of
    {ok, Value} ->
        Value;
    error ->
        DefaultValue
    end.

write_changes(Group, ViewKeyValuesToAdd, DocIdViewIdKeys, NewSeq) ->
    #group{id_btree=IdBtree} = Group,

    AddDocIdViewIdKeys = [{DocId, ViewIdKeys} || {DocId, ViewIdKeys} <- DocIdViewIdKeys, ViewIdKeys /= []],
    RemoveDocIds = [DocId || {DocId, ViewIdKeys} <- DocIdViewIdKeys, ViewIdKeys == []],
    LookupDocIds = [DocId || {DocId, _ViewIdKeys} <- DocIdViewIdKeys],

    {ok, LookupResults, IdBtree2}
        = couch_btree:query_modify(IdBtree, LookupDocIds, AddDocIdViewIdKeys, RemoveDocIds),
    KeysToRemoveByView = lists:foldl(
        fun(LookupResult, KeysToRemoveByViewAcc) ->
            case LookupResult of
            {ok, {DocId, ViewIdKeys}} ->
                lists:foldl(
                    fun({ViewId, Key}, KeysToRemoveByViewAcc2) ->
                        dict:append(ViewId, {Key, DocId}, KeysToRemoveByViewAcc2)
                    end,
                    KeysToRemoveByViewAcc, ViewIdKeys);
            {not_found, _} ->
                KeysToRemoveByViewAcc
            end
        end,
        dict:new(), LookupResults),

    Views2 = [
        begin
            KeysToRemove = dict_find(View#view.id_num, [], KeysToRemoveByView),
            {ok, ViewBtree2} = couch_btree:add_remove(View#view.btree, AddKeyValues, KeysToRemove),
            View#view{btree = ViewBtree2}
        end
    ||
        {View, AddKeyValues} <- ViewKeyValuesToAdd
    ],
    Group2 = Group#group{views=Views2, current_seq=NewSeq, id_btree=IdBtree2},
    {ok, Group2}.
