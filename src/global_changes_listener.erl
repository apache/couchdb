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

-module(global_changes_listener).
-behavior(couch_event_listener).


-export([
    start/0
]).

-export([
    init/1,
    terminate/2,
    handle_event/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    update_db,
    pending_update_count,
    pending_updates,
    last_update_time,
    max_event_delay,
    dbname
}).


-include_lib("mem3/include/mem3.hrl").


start() ->
    couch_event_listener:start(?MODULE, nil, [all_dbs]).


init(_) ->
    % get configs as strings
    UpdateDb0 = config:get("global_changes", "update_db", "true"),
    MaxEventDelay0 = config:get("global_changes", "max_event_delay", "25"),

    % make config strings into other data types
    UpdateDb = case UpdateDb0 of "false" -> false; _ -> true end,
    MaxEventDelay = list_to_integer(MaxEventDelay0),

    State = #state{
        update_db=UpdateDb,
        pending_update_count=0,
        pending_updates=sets:new(),
        max_event_delay=MaxEventDelay,
        dbname=global_changes_util:get_dbname()
    },
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


handle_event(_ShardName, _Event, #state{update_db=false}=State) ->
    {ok, State};
handle_event(ShardName, Event, State0)
        when Event =:= updated orelse Event =:= deleted
        orelse Event =:= created ->
    #state{dbname=ChangesDbName} = State0,
    State = case mem3:dbname(ShardName) of
        ChangesDbName ->
            State0;
        DbName ->
            #state{pending_update_count=Count} = State0,
            EventBin = erlang:atom_to_binary(Event, latin1),
            Key = <<EventBin/binary, <<":">>/binary, DbName/binary>>,
            Pending = sets:add_element(Key, State0#state.pending_updates),
            couch_stats:update_gauge(
                [global_changes, listener_pending_updates],
                Count + 1
            ),
            State0#state{pending_updates=Pending, pending_update_count=Count+1}
    end,
    maybe_send_updates(State);
handle_event(_DbName, _Event, State) ->
    maybe_send_updates(State).


handle_cast({set_max_event_delay, MaxEventDelay}, State) ->
    maybe_send_updates(State#state{max_event_delay=MaxEventDelay});
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
    maybe_send_updates(State);
handle_cast(_Msg, State) ->
    maybe_send_updates(State).


maybe_send_updates(#state{pending_update_count=0}=State) ->
    {ok, State};
maybe_send_updates(#state{update_db=true}=State) ->
    #state{max_event_delay=MaxEventDelay, last_update_time=LastUpdateTime} = State,
    Now = os:timestamp(),
    case LastUpdateTime of
    undefined ->
        {ok, State#state{last_update_time=Now}, MaxEventDelay};
    _ ->
        Delta = timer:now_diff(Now, LastUpdateTime) div 1000,
        if Delta >= MaxEventDelay ->
            Updates = sets:to_list(State#state.pending_updates),
            try group_updates_by_node(State#state.dbname, Updates) of
                Grouped ->
                    dict:map(fun(Node, Docs) ->
                        couch_stats:increment_counter([global_changes, rpcs]),
                        global_changes_server:update_docs(Node, Docs)
                    end, Grouped)
            catch error:database_does_not_exist ->
                ok
            end,
            couch_stats:update_gauge(
                [global_changes, listener_pending_updates],
                0
            ),
            State1 = State#state{
                pending_updates=sets:new(),
                pending_update_count=0,
                last_update_time=undefined
            },
            {ok, State1};
        true ->
            {ok, State, MaxEventDelay-Delta}
        end
    end;
maybe_send_updates(State) ->
    {ok, State}.


handle_info(_Msg, State) ->
    maybe_send_updates(State).


-spec group_updates_by_node(binary(), [binary()]) -> dict().
group_updates_by_node(DbName, Updates) ->
    lists:foldl(fun(Key, OuterAcc) ->
        Shards = mem3:shards(DbName, Key),
        lists:foldl(fun(#shard{node=Node}, InnerAcc) ->
            dict:append(Node, Key, InnerAcc)
        end, OuterAcc, Shards)
    end, dict:new(), Updates).
