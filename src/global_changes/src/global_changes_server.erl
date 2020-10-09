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
    code_change/3,
    format_status/2
]).

-export([
    update_docs/2
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-record(state, {
    update_db,
    pending_update_count,
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
    MaxWriteDelay0 = config:get("global_changes", "max_write_delay", "500"),

    % make config strings into other data types
    UpdateDb = case UpdateDb0 of "false" -> false; _ -> true end,
    MaxWriteDelay = list_to_integer(MaxWriteDelay0),

    % Start our write triggers
    erlang:send_after(MaxWriteDelay, self(), flush_updates),

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
    PendingCount = sets:size(Pending),
    couch_stats:update_gauge(
        [global_changes, server_pending_updates],
        PendingCount
    ),
    NewState = State#state{
        pending_updates=Pending,
        pending_update_count=PendingCount
    },
    {noreply, NewState};

handle_cast({set_max_write_delay, MaxWriteDelay}, State) ->
    NewState = State#state{max_write_delay=MaxWriteDelay},
    {noreply, NewState};
handle_cast({set_update_db, Boolean}, State0) ->
    % If turning update_db off, clear out server state
    State = case {Boolean, State0#state.update_db} of
        {false, true} ->
            State0#state{
                update_db=Boolean,
                pending_updates=sets:new(),
                pending_update_count=0
            };
        _ ->
            State0#state{update_db=Boolean}
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(flush_updates, #state{pending_update_count=0}=State) ->
    erlang:send_after(State#state.max_write_delay, self(), flush_updates),
    {noreply, State};
handle_info(flush_updates, #state{update_db=false}=State) ->
    erlang:send_after(State#state.max_write_delay, self(), flush_updates),
    {noreply, State};
handle_info(flush_updates, State) ->
    erlang:send_after(State#state.max_write_delay, self(), flush_updates),
    flush_updates(State);
handle_info(start_listener, State) ->
    {ok, Handler} = global_changes_listener:start(),
    NewState = State#state{
        handler_ref=erlang:monitor(process, Handler)
    },
    {noreply, NewState};
handle_info({'DOWN', Ref, _, _, Reason}, #state{handler_ref=Ref}=State) ->
    couch_log:error("global_changes_listener terminated: ~w", [Reason]),
    erlang:send_after(5000, self(), start_listener),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, [_PDict, State]) ->
    Scrubbed = State#state{
        pending_updates=nil
    },
    [{data, [{"State",
        ?record_to_keyval(state, Scrubbed)
    }]}].

flush_updates(State) ->
    DocIds = sets:to_list(State#state.pending_updates),
    try group_ids_by_shard(State#state.dbname, DocIds) of
    GroupedIds ->
        Docs = dict:fold(fun(ShardName, Ids, DocInfoAcc) ->
            {ok, Shard} = couch_db:open(ShardName, [?ADMIN_CTX]),
            try
                GroupedDocs = get_docs_locally(Shard, Ids),
                GroupedDocs ++ DocInfoAcc
            after
                couch_db:close(Shard)
            end
        end, [], GroupedIds),

        spawn(fun() ->
            fabric:update_docs(State#state.dbname, Docs, [])
        end),

        Count = State#state.pending_update_count,
        couch_stats:increment_counter(
            [global_changes, db_writes],
            Count
        )
    catch error:database_does_not_exist ->
        {noreply, State}
    end,
    couch_stats:update_gauge(
        [global_changes, server_pending_updates],
        0
    ),
    {noreply, State#state{
        pending_updates=sets:new(),
        pending_update_count=0
    }}.


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
    couch_stats:increment_counter([global_changes, event_doc_conflict]),
    {Pos, Rev} = RevInfo#rev_info.rev,
    {Pos, [Rev]}.
