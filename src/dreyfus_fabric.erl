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

-module(dreyfus_fabric).
-export([get_json_docs/2, handle_error_message/6]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").
-include("dreyfus.hrl").

get_json_docs(DbName, DocIds) ->
    fabric:all_docs(DbName, fun callback/2, [], [{keys, DocIds}, {include_docs, true}]).

callback({meta,_}, Acc) ->
    {ok, Acc};
callback({error, Reason}, _Acc) ->
    {error, Reason};
callback({row, Row}, Acc) ->
    {id, Id} = lists:keyfind(id, 1, Row),
    {ok, [{Id, lists:keyfind(doc, 1, Row)}|Acc]};
callback(complete, Acc) ->
    {ok, lists:reverse(Acc)};
callback(timeout, _Acc) ->
    {error, timeout}.

handle_error_message({rexi_DOWN, _, {_, NodeRef}, _}, _Worker,
                     Counters, _Replacements, _StartFun, _StartArgs) ->
    case fabric_util:remove_down_workers(Counters, NodeRef) of
    {ok, NewCounters} ->
        {ok, NewCounters};
    error ->
        {error, {nodedown, <<"progress not possible">>}}
    end;
handle_error_message({rexi_EXIT, {maintenance_mode, _}}, Worker,
                     Counters, Replacements, StartFun, StartArgs) ->
    handle_replacement(Worker, Counters, Replacements, StartFun, StartArgs);
handle_error_message({rexi_EXIT, Reason}, Worker,
                     Counters, _Replacements, _StartFun, _StartArgs) ->
    handle_error(Reason, Worker, Counters);
handle_error_message({error, Reason}, Worker,
                     Counters, _Replacements, _StartFun, _StartArgs) ->
    handle_error(Reason, Worker, Counters);
handle_error_message({'EXIT', Reason}, Worker,
                     Counters, _Replacements, _StartFun, _StartArgs) ->
    handle_error({exit, Reason}, Worker, Counters);
handle_error_message(Reason, Worker, Counters,
                     _Replacements, _StartFun, _StartArgs) ->
    couch_log:error("Unexpected error during request: ~p", [Reason]),
    handle_error(Reason, Worker, Counters).

handle_error(Reason, Worker, Counters0) ->
    Counters = fabric_dict:erase(Worker, Counters0),
    case fabric_view:is_progress_possible(Counters) of
    true ->
        {ok, Counters};
    false ->
        {error, Reason}
    end.

handle_replacement(Worker, OldCntrs0, OldReplacements, StartFun, StartArgs) ->
    OldCounters = lists:filter(fun({#shard{ref=R}, _}) ->
        R /= Worker#shard.ref
    end, OldCntrs0),
    case lists:keytake(Worker#shard.range, 1, OldReplacements) of
        {value, {_Range, Replacements}, NewReplacements} ->
            NewCounters = lists:foldl(fun(Repl, CounterAcc) ->
                NewCounter = start_replacement(StartFun, StartArgs, Repl),
                fabric_dict:store(NewCounter, nil, CounterAcc)
            end, OldCounters, Replacements),
            true = fabric_view:is_progress_possible(NewCounters),
            NewRefs = fabric_dict:fetch_keys(NewCounters),
            {new_refs, NewRefs, NewCounters, NewReplacements};
        false ->
            handle_error({nodedown, <<"progress not possible">>},
                         Worker, OldCounters)
    end.

start_replacement(StartFun, StartArgs, Shard) ->
    [DDoc, IndexName, QueryArgs] = StartArgs,
    After = case QueryArgs#index_query_args.bookmark of
        Bookmark when is_list(Bookmark) ->
            lists:foldl(fun({#shard{range=R0}, After0}, Acc) ->
                case R0 == Shard#shard.range of
                    true -> After0;
                    false -> Acc
                end
            end, nil, Bookmark);
        _ ->
            nil
    end,
    QueryArgs1 = QueryArgs#index_query_args{bookmark=After},
    StartArgs1 = [DDoc, IndexName, QueryArgs1],
    Ref = rexi:cast(Shard#shard.node,
                    {dreyfus_rpc, StartFun,
                     [Shard#shard.name|StartArgs1]}),
    Shard#shard{ref = Ref}.
