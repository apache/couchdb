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

% todo
% - remove existance check on increment(), decrement() and record(). have
%   modules initialize counters on startup.

-module(couch_stats_collector).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).


-export([start/0, stop/0, get/1, 
        increment/1, decrement/1,
        record/2, clear/1,
        all/0, all/1]).

-record(state, {}).

-define(ABSOLUTE_VALUE_COUNTER_TABLE, abs_table).
-define(HIT_COUNTER_TABLE, hit_table).


% PUBLIC API

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:call(?MODULE, stop).

get(Key) ->
    case ets:lookup(?HIT_COUNTER_TABLE, Key) of
        [] -> 
            case ets:lookup(?ABSOLUTE_VALUE_COUNTER_TABLE, Key) of
                [] -> 
                    0;
                Result2 -> extract_value_from_ets_result(Key, Result2)
            end;
        [{_,Result1}] -> Result1
    end.

increment({Module, Key}) when is_integer(Key) ->
    increment({Module, list_to_atom(integer_to_list(Key))});
increment(Key) ->
    case catch ets:update_counter(?HIT_COUNTER_TABLE, Key, 1) of
        {'EXIT', {badarg, _}} -> ets:insert(?HIT_COUNTER_TABLE, {Key, 1});
        _ -> ok
    end.
    
decrement(Key) ->
    case catch ets:update_counter(?HIT_COUNTER_TABLE, Key, -1) of
        {'EXIT', {badarg, _}} -> ets:insert(?HIT_COUNTER_TABLE, {Key, -1});
        _ -> ok
    end.
    
record(Key, Value) ->
    ets:insert(?ABSOLUTE_VALUE_COUNTER_TABLE, {Key, Value}).

clear(Key) ->
    true = ets:delete(?ABSOLUTE_VALUE_COUNTER_TABLE, Key).

all() ->
    lists:append(ets:tab2list(?HIT_COUNTER_TABLE), 
        ets:tab2list(?ABSOLUTE_VALUE_COUNTER_TABLE)).

all(Type) ->
    case Type of
        incremental -> ets:tab2list(?HIT_COUNTER_TABLE);
        absolute -> ets:tab2list(?ABSOLUTE_VALUE_COUNTER_TABLE)
    end.


% GEN_SERVER


init(_) ->
    ets:new(?HIT_COUNTER_TABLE, [named_table, set, public]),
    ets:new(?ABSOLUTE_VALUE_COUNTER_TABLE, [named_table, duplicate_bag, public]),
    {ok, #state{}}.


handle_call(stop, _, State) ->
    {stop, normal, stopped, State}.


% PRIVATE API

extract_value_from_ets_result(_Key, Result) ->
    lists:map(fun({_, Value}) -> Value end, Result).


% Unused gen_server behaviour API functions that we need to declare.
  
%% @doc Unused
handle_cast(foo, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Unused
terminate(_Reason, _State) -> ok.

%% @doc Unused
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%% Tests

-ifdef(TEST).
% Internal API unit tests go here


-endif.