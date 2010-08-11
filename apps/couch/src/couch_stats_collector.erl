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

% todo
% - remove existance check on increment(), decrement() and record(). have
%   modules initialize counters on startup.

-module(couch_stats_collector).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([all/0, all/1, get/1, increment/1, decrement/1, record/2, clear/1]).
-export([track_process_count/1, track_process_count/2]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(HIT_TABLE, stats_hit_table).
-define(ABS_TABLE, stats_abs_table).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

all() ->
    ets:tab2list(?HIT_TABLE) ++ abs_to_list().

all(Type) ->
    case Type of
        incremental -> ets:tab2list(?HIT_TABLE);
        absolute -> abs_to_list()
    end.

get(Key) ->
    case ets:lookup(?HIT_TABLE, Key) of
        [] ->
            case ets:lookup(?ABS_TABLE, Key) of
                [] ->
                    nil;
                AbsVals ->
                    lists:map(fun({_, Value}) -> Value end, AbsVals)
            end;
        [{_, Counter}] ->
            Counter
    end.

increment(Key) ->
    Key2 = make_key(Key),
    case catch ets:update_counter(?HIT_TABLE, Key2, 1) of
        {'EXIT', {badarg, _}} ->
            catch ets:insert(?HIT_TABLE, {Key2, 1}),
            ok;
        _ ->
            ok
    end.

decrement(Key) ->
    Key2 = make_key(Key),
    case catch ets:update_counter(?HIT_TABLE, Key2, -1) of
        {'EXIT', {badarg, _}} ->
            catch ets:insert(?HIT_TABLE, {Key2, -1}),
            ok;
        _ -> ok
    end.

record(Key, Value) ->
    catch ets:insert(?ABS_TABLE, {make_key(Key), Value}).

clear(Key) ->
    catch ets:delete(?ABS_TABLE, make_key(Key)).

track_process_count(Stat) ->
    track_process_count(self(), Stat).

track_process_count(Pid, Stat) ->
    MonitorFun = fun() ->
        Ref = erlang:monitor(process, Pid),
        receive {'DOWN', Ref, _, _, _} -> ok end,
        couch_stats_collector:decrement(Stat)
    end,
    case (catch couch_stats_collector:increment(Stat)) of
        ok -> spawn(MonitorFun);
        _ -> ok
    end.


init(_) ->
    ets:new(?HIT_TABLE, [named_table, set, public]),
    ets:new(?ABS_TABLE, [named_table, duplicate_bag, public]),
    {ok, nil}.

terminate(_Reason, _State) ->
    ok.

handle_call(stop, _, State) ->
    {stop, normal, stopped, State}.

handle_cast(foo, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


make_key({Module, Key}) when is_integer(Key) ->
    {Module, list_to_atom(integer_to_list(Key))};
make_key(Key) ->
    Key.

abs_to_list() ->
    SortedKVs = lists:sort(ets:tab2list(?ABS_TABLE)),
    lists:foldl(fun({Key, Val}, Acc) ->
        case Acc of
            [] ->
                [{Key, [Val]}];
            [{Key, Prev} | Rest] ->
                [{Key, [Val | Prev]} | Rest];
            Others ->
                [{Key, [Val]} | Others]
        end
    end, [], SortedKVs).