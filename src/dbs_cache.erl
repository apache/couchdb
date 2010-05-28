-module(dbs_cache).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/0]).

-include("membership.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(partitions, [bag, protected, named_table, {keypos,#shard.dbname}]),
    ets:new(memnodes, [bag, protected, named_table]),
    cache_dbs(),
    Self = self(),
    couch_db_update_notifier:start_link(fun({updated, <<"dbs">>}) ->
        Self ! rebuild_dbs_cache;
    (_) -> ok end),
    {ok, nil}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(rebuild_dbs_cache, State) ->
    receive rebuild_dbs_cache ->
        handle_info(rebuild_dbs_cache, State)
    after 0 -> ok end,
    T0 = now(),
    ?LOG_INFO("rebuilding dbs DB cache", []),
    ets:delete_all_objects(partitions),
    ets:delete_all_objects(memnodes),
    cache_dbs(),
    ?LOG_INFO("rebuild of dbs DB cache complete in ~p ms",
        [round(timer:now_diff(now(),T0)/1000)]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cache_dbs() ->
    try couch_db:open(<<"dbs">>, []) of
    {ok, Db} ->
        Bt = Db#db.id_tree,
        FoldFun = fun(#full_doc_info{id=Id, deleted=false} = FullDocInfo, _, _) ->
            {ok, Doc} = couch_db:open_doc_int(Db, FullDocInfo, []),
            {Props} = couch_doc:to_json_obj(Doc, []),
            cache_map(Id, Props),
            cache_nodes(Id, Props),
            {ok, true};
        (_, _, _) ->
            {ok, nil}
        end,
        couch_btree:foldl(Bt, FoldFun, nil),
        couch_db:close(Db)
    catch exit:{noproc,{gen_server,call,[couch_server|_]}} ->
        timer:sleep(1000),
        exit(couch_server_is_dead)
    end.

cache_map(Id, Props) ->
    Map = couch_util:get_value(map, Props),
    lists:foreach(fun({[{node,Node},{b,Beg},{e,End}]}) ->
        Part = #shard{
            name = partitions:shard_name(Beg, Id),
            dbname = Id,
            node = Node,
            range = [Beg,End]
        },
        ets:insert(partitions, Part)
    end, Map).

cache_nodes(Id, Props) ->
    Nodes = couch_util:get_value(nodes, Props),
    lists:foreach(fun({[{order,Order},{node, Node},{options,Opts}]}) ->
        ets:insert(memnodes, {Id, {Order, Node, Opts}})
    end, Nodes).

%{ok, ets:insert(dbs_cache, {Id, Props})};
