% Copyright 2013 Cloudant. All rights reserved.

-module(global_changes_server).
-behaviour(gen_server).
-vsn(1).


-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
    update_docs/2
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").


-record(state, {
    update_db,
    pending_update_count,
    last_update_time,
    pending_updates,
    max_write_delay,
    dbname,
    handler_ref
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, Handler} = global_changes_listener:start(),
    % get configs as strings
    UpdateDb0 = config:get("global_changes", "update_db", "true"),
    MaxWriteDelay0 = config:get("global_changes", "max_write_delay", "25"),

    % make config strings into other data types
    UpdateDb = case UpdateDb0 of "false" -> false; _ -> true end,
    MaxWriteDelay = list_to_integer(MaxWriteDelay0),

    State = #state{
        update_db=UpdateDb,
        pending_update_count=0,
        pending_updates=sets:new(),
        max_write_delay=MaxWriteDelay,
        dbname=global_changes_util:get_dbname(),
        handler_ref=erlang:monitor(process, Handler)
    },
    {ok, State}.


terminate(_Reason, _Srv) ->
    ok.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, #state{update_db=false}=State) ->
    {noreply, State};
handle_cast({update_docs, DocIds}, State) ->
    Pending = sets:union(sets:from_list(DocIds), State#state.pending_updates),
    NewState = State#state{
        pending_updates=Pending,
        pending_update_count=sets:size(Pending)
    },
    maybe_update_docs(NewState);

handle_cast({set_max_write_delay, MaxWriteDelay}, State) ->
    NewState = State#state{max_write_delay=MaxWriteDelay},
    maybe_update_docs(NewState);
handle_cast({set_update_db, Boolean}, State0) ->
    % If turning update_db off, clear out server state
    State = case {Boolean, State0#state.update_db} of
        {false, true} ->
            State0#state{
                update_db=Boolean,
                pending_updates=sets:new(),
                pending_update_count=0,
                last_update_time=undefined
            };
        _ ->
            State0#state{update_db=Boolean}
    end,
    maybe_update_docs(State);
handle_cast(_Msg, State) ->
    maybe_update_docs(State).


handle_info(start_listener, State) ->
    {ok, Handler} = global_changes_listener:start(),
    NewState = State#state{
        handler_ref=erlang:monitor(process, Handler)
    },
    maybe_update_docs(NewState);
handle_info({'DOWN', Ref, _, _, Reason}, #state{handler_ref=Ref}=State) ->
    couch_log:error("global_changes_listener terminated: ~w", [Reason]),
    erlang:send_after(5000, self(), start_listener),
    maybe_update_docs(State);
handle_info(_, State) ->
    maybe_update_docs(State).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


maybe_update_docs(#state{pending_update_count=0}=State) ->
    {noreply, State};
maybe_update_docs(#state{update_db=true}=State) ->
    #state{max_write_delay=MaxWriteDelay, last_update_time=LastUpdateTime} = State,
    Now = os:timestamp(),
    case LastUpdateTime of
    undefined ->
        {noreply, State#state{last_update_time=Now}, MaxWriteDelay};
    _ ->
        Delta = round(timer:now_diff(Now, LastUpdateTime)/1000),
        if Delta >= MaxWriteDelay ->
            DocIds = sets:to_list(State#state.pending_updates),
            try group_ids_by_shard(State#state.dbname, DocIds) of
            GroupedIds ->
                Docs = dict:fold(fun(ShardName, Ids, DocInfoAcc) ->
                    {ok, Shard} = couch_db:open(ShardName, []),
                    try
                        GroupedDocs = get_docs_locally(Shard, Ids),
                        GroupedDocs ++ DocInfoAcc
                    after
                        couch_db:close(Shard)
                    end
                end, [], GroupedIds),

                spawn(fun() ->
                    fabric:update_docs(State#state.dbname, Docs, [])
                end)
            catch error:database_does_not_exist ->
                {noreply, State}
            end,
            {noreply, State#state{
                pending_updates=sets:new(),
                pending_update_count=0,
                last_update_time=undefined
            }};
        true ->
            {noreply, State, MaxWriteDelay-Delta}
        end
    end;
maybe_update_docs(State) ->
    {noreply, State}.


update_docs(Node, Updates) ->
    gen_server:cast({?MODULE, Node}, {update_docs, Updates}).


group_ids_by_shard(DbName, DocIds) ->
    LocalNode = node(),
    lists:foldl(fun(DocId, Acc) ->
        Shards = mem3:shards(DbName, DocId),
        lists:foldl(fun
            (#shard{node=Node, name=Name}, Acc1) when Node == LocalNode ->
                dict:append(Name, DocId, Acc1);
            (_, Acc1) ->
                Acc1
        end, Acc, Shards)
    end, dict:new(), DocIds).


get_docs_locally(Shard, Ids) ->
    lists:map(fun(Id) ->
        DocInfo = couch_db:get_doc_info(Shard, Id),
        #doc{id=Id, revs=get_rev(DocInfo)}
    end, Ids).


get_rev(not_found) ->
    {0, []};
get_rev({ok, #doc_info{revs=[RevInfo]}}) ->
    {Pos, Rev} = RevInfo#rev_info.rev,
    {Pos, [Rev]};
get_rev({ok, #doc_info{revs=[RevInfo|_]}}) ->
    % couch_doc:to_doc_info/1 sorts things so that the first
    % #rev_info in the list is the "winning" revision which is
    % the one we'd want to base our edit off of. In theory
    % global_changes should never encounter a conflict by design
    % but we should record if it happens in case our design isn't
    % quite right.
    {Pos, Rev} = RevInfo#rev_info.rev,
    {Pos, [Rev]}.
