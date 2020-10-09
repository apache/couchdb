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
-export([get_json_docs/2, handle_error_message/7]).

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
        Counters, _Replacements, _StartFun, _StartArgs, RingOpts) ->
    case fabric_util:remove_down_workers(Counters, NodeRef, RingOpts) of
    {ok, NewCounters} ->
        {ok, NewCounters};
    error ->
        {error, {nodedown, <<"progress not possible">>}}
    end;
handle_error_message({rexi_EXIT, {maintenance_mode, _}}, Worker,
        Counters, Replacements, StartFun, StartArgs, RingOpts) ->
    handle_replacement(Worker, Counters, Replacements, StartFun, StartArgs,
        RingOpts);
handle_error_message({rexi_EXIT, Reason}, Worker,
        Counters, _Replacements, _StartFun, _StartArgs, RingOpts) ->
    handle_error(Reason, Worker, Counters, RingOpts);
handle_error_message({error, Reason}, Worker,
        Counters, _Replacements, _StartFun, _StartArgs, RingOpts) ->
    handle_error(Reason, Worker, Counters, RingOpts);
handle_error_message({'EXIT', Reason}, Worker,
        Counters, _Replacements, _StartFun, _StartArgs, RingOpts) ->
    handle_error({exit, Reason}, Worker, Counters, RingOpts);
handle_error_message(Reason, Worker, Counters,
        _Replacements, _StartFun, _StartArgs, RingOpts) ->
    couch_log:error("Unexpected error during request: ~p", [Reason]),
    handle_error(Reason, Worker, Counters, RingOpts).

handle_error(Reason, Worker, Counters0, RingOpts) ->
    Counters = fabric_dict:erase(Worker, Counters0),
    case fabric_ring:is_progress_possible(Counters, RingOpts) of
    true ->
        {ok, Counters};
    false ->
        {error, Reason}
    end.

handle_replacement(Worker, OldCntrs0, OldReplacements, StartFun, StartArgs,
        RingOpts) ->
    OldCounters = lists:filter(fun({#shard{ref=R}, _}) ->
        R /= Worker#shard.ref
    end, OldCntrs0),
    case lists:keytake(Worker#shard.range, 1, OldReplacements) of
        {value, {_Range, Replacements}, NewReplacements} ->
            NewCounters = lists:foldl(fun(Repl, CounterAcc) ->
                NewCounter = start_replacement(StartFun, StartArgs, Repl),
                fabric_dict:store(NewCounter, nil, CounterAcc)
            end, OldCounters, Replacements),
            true = fabric_ring:is_progress_possible(NewCounters, RingOpts),
            NewRefs = fabric_dict:fetch_keys(NewCounters),
            {new_refs, NewRefs, NewCounters, NewReplacements};
        false ->
            handle_error({nodedown, <<"progress not possible">>},
                         Worker, OldCounters, RingOpts)
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


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


node_down_test() ->
    [S1, S2, S3] = [
        mk_shard("n1", [0, 4]),
        mk_shard("n1", [5, ?RING_END]),
        mk_shard("n2", [0, ?RING_END])
    ],
    [W1, W2, W3] = [
         S1#shard{ref = make_ref()},
         S2#shard{ref = make_ref()},
         S3#shard{ref = make_ref()}
    ],
    Counters1 = fabric_dict:init([W1, W2, W3], nil),

    N1 = S1#shard.node,
    Msg1 = {rexi_DOWN, nil, {nil, N1}, nil},
    Res1 =  handle_error_message(Msg1, nil, Counters1, nil, nil, nil, []),
    ?assertEqual({ok, [{W3, nil}]}, Res1),

    {ok, Counters2} = Res1,
    N2 = S3#shard.node,
    Msg2 = {rexi_DOWN, nil, {nil, N2}, nil},
    Res2 =  handle_error_message(Msg2, nil, Counters2, nil, nil, nil, []),
    ?assertEqual({error, {nodedown, <<"progress not possible">>}}, Res2).


worker_error_test() ->
    [S1, S2] = [
        mk_shard("n1", [0, ?RING_END]),
        mk_shard("n2", [0, ?RING_END])
    ],
    [W1, W2] = [S1#shard{ref = make_ref()}, S2#shard{ref = make_ref()}],
    Counters1 = fabric_dict:init([W1, W2], nil),

    Res1 = handle_error(bam, W1, Counters1, []),
    ?assertEqual({ok, [{W2, nil}]}, Res1),

    {ok, Counters2} = Res1,
    ?assertEqual({error, boom}, handle_error(boom, W2, Counters2, [])).


node_down_with_partitions_test() ->
    [S1, S2] = [
        mk_shard("n1", [0, 4]),
        mk_shard("n2", [0, 8])
    ],
    [W1, W2] = [
        S1#shard{ref = make_ref()},
        S2#shard{ref = make_ref()}
    ],
    Counters1 = fabric_dict:init([W1, W2], nil),
    RingOpts = [{any, [S1, S2]}],

    N1 = S1#shard.node,
    Msg1 = {rexi_DOWN, nil, {nil, N1}, nil},
    Res1 =  handle_error_message(Msg1, nil, Counters1, nil, nil, nil, RingOpts),
    ?assertEqual({ok, [{W2, nil}]}, Res1),

    {ok, Counters2} = Res1,
    N2 = S2#shard.node,
    Msg2 = {rexi_DOWN, nil, {nil, N2}, nil},
    Res2 =  handle_error_message(Msg2, nil, Counters2, nil, nil, nil, RingOpts),
    ?assertEqual({error, {nodedown, <<"progress not possible">>}}, Res2).


worker_error_with_partitions_test() ->
    [S1, S2] = [
        mk_shard("n1", [0, 4]),
        mk_shard("n2", [0, 8])],
    [W1, W2] = [
        S1#shard{ref = make_ref()},
        S2#shard{ref = make_ref()}
    ],
    Counters1 = fabric_dict:init([W1, W2], nil),
    RingOpts = [{any, [S1, S2]}],

    Res1 = handle_error(bam, W1, Counters1, RingOpts),
    ?assertEqual({ok, [{W2, nil}]}, Res1),

    {ok, Counters2} = Res1,
    ?assertEqual({error, boom}, handle_error(boom, W2, Counters2, RingOpts)).


mk_shard(Name, Range) ->
    Node = list_to_atom(Name),
    BName = list_to_binary(Name),
    #shard{name = BName, node = Node, range = Range}.

-endif.
