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

-module(mem3_reshard_api).

-export([
    create_jobs/5,
    get_jobs/0,
    get_job/1,
    get_summary/0,
    resume_job/1,
    stop_job/2,
    start_shard_splitting/0,
    stop_shard_splitting/1,
    get_shard_splitting_state/0
]).

create_jobs(Node, Shard, Db, Range, split) ->
    lists:map(
        fun(S) ->
            N = mem3:node(S),
            Name = mem3:name(S),
            case rpc:call(N, mem3_reshard, start_split_job, [Name]) of
                {badrpc, Error} ->
                    {error, Error, N, Name};
                {ok, JobId} ->
                    {ok, JobId, N, Name};
                {error, Error} ->
                    {error, Error, N, Name}
            end
        end,
        pick_shards(Node, Shard, Db, Range)
    ).

get_jobs() ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_reshard, jobs, []),
    lists:flatten(Replies).

get_job(JobId) ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_reshard, job, [JobId]),
    case [JobInfo || {ok, JobInfo} <- Replies] of
        [JobInfo | _] ->
            {ok, JobInfo};
        [] ->
            {error, not_found}
    end.

get_summary() ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_reshard, get_state, []),
    Stats0 = #{
        running => 0,
        total => 0,
        completed => 0,
        failed => 0,
        stopped => 0
    },
    StatsF = lists:foldl(
        fun({Res}, Stats) ->
            maps:map(
                fun(Stat, OldVal) ->
                    OldVal + couch_util:get_value(Stat, Res, 0)
                end,
                Stats
            )
        end,
        Stats0,
        Replies
    ),
    {State, Reason} = state_and_reason(Replies),
    StateReasonProps = [{state, State}, {state_reason, Reason}],
    {StateReasonProps ++ lists:sort(maps:to_list(StatsF))}.

resume_job(JobId) ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(
        Nodes,
        mem3_reshard,
        resume_job,
        [JobId]
    ),
    WithoutNotFound = [R || R <- Replies, R =/= {error, not_found}],
    case lists:usort(WithoutNotFound) of
        [ok] ->
            ok;
        [{error, Error} | _] ->
            {error, {[{error, couch_util:to_binary(Error)}]}};
        [] ->
            {error, not_found}
    end.

stop_job(JobId, Reason) ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(
        Nodes,
        mem3_reshard,
        stop_job,
        [JobId, Reason]
    ),
    WithoutNotFound = [R || R <- Replies, R =/= {error, not_found}],
    case lists:usort(WithoutNotFound) of
        [ok] ->
            ok;
        [{error, Error} | _] ->
            {error, {[{error, couch_util:to_binary(Error)}]}};
        [] ->
            {error, not_found}
    end.

start_shard_splitting() ->
    {Replies, _Bad} = rpc:multicall(mem3_reshard, start, []),
    case lists:usort(lists:flatten(Replies)) of
        [ok] ->
            {ok, {[{ok, true}]}};
        [Error | _] ->
            {error, {[{error, couch_util:to_binary(Error)}]}}
    end.

stop_shard_splitting(Reason) ->
    {Replies, _Bad} = rpc:multicall(mem3_reshard, stop, [Reason]),
    case lists:usort(lists:flatten(Replies)) of
        [ok] ->
            {ok, {[{ok, true}]}};
        [Error | _] ->
            {error, {[{error, couch_util:to_binary(Error)}]}}
    end.

get_shard_splitting_state() ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_reshard, get_state, []),
    state_and_reason(Replies).

state_and_reason(StateReplies) ->
    AccF = lists:foldl(
        fun({ResProps}, Acc) ->
            Reason = get_reason(ResProps),
            case couch_util:get_value(state, ResProps) of
                <<"running">> -> orddict:append(running, Reason, Acc);
                <<"stopped">> -> orddict:append(stopped, Reason, Acc);
                undefined -> Acc
            end
        end,
        orddict:from_list([{running, []}, {stopped, []}]),
        StateReplies
    ),
    Running = orddict:fetch(running, AccF),
    case length(Running) > 0 of
        true ->
            Reason = pick_reason(Running),
            {running, Reason};
        false ->
            Reason = pick_reason(orddict:fetch(stopped, AccF)),
            {stopped, Reason}
    end.

pick_reason(Reasons) ->
    Reasons1 = lists:usort(Reasons),
    Reasons2 = [R || R <- Reasons1, R =/= undefined],
    case Reasons2 of
        [] -> null;
        [R1 | _] -> R1
    end.

get_reason(StateProps) when is_list(StateProps) ->
    case couch_util:get_value(state_info, StateProps) of
        [] -> undefined;
        undefined -> undefined;
        {SInfoProps} -> couch_util:get_value(reason, SInfoProps)
    end.

pick_shards(undefined, undefined, Db, undefined) when is_binary(Db) ->
    check_node_required(),
    check_range_required(),
    mem3:shards(Db);
pick_shards(Node, undefined, Db, undefined) when
    is_atom(Node),
    is_binary(Db)
->
    check_range_required(),
    [S || S <- mem3:shards(Db), mem3:node(S) == Node];
pick_shards(undefined, undefined, Db, [_B, _E] = Range) when is_binary(Db) ->
    check_node_required(),
    [S || S <- mem3:shards(Db), mem3:range(S) == Range];
pick_shards(Node, undefined, Db, [_B, _E] = Range) when
    is_atom(Node),
    is_binary(Db)
->
    [S || S <- mem3:shards(Db), mem3:node(S) == Node, mem3:range(S) == Range];
pick_shards(undefined, Shard, undefined, undefined) when is_binary(Shard) ->
    check_node_required(),
    Db = mem3:dbname(Shard),
    [S || S <- mem3:shards(Db), mem3:name(S) == Shard];
pick_shards(Node, Shard, undefined, undefined) when
    is_atom(Node),
    is_binary(Shard)
->
    Db = mem3:dbname(Shard),
    [S || S <- mem3:shards(Db), mem3:name(S) == Shard, mem3:node(S) == Node];
pick_shards(_, undefined, undefined, _) ->
    throw({bad_request, <<"Must specify at least `db` or `shard`">>});
pick_shards(_, Db, Shard, _) when is_binary(Db), is_binary(Shard) ->
    throw({bad_request, <<"`db` and `shard` are mutually exclusive">>}).

check_node_required() ->
    case config:get_boolean("reshard", "require_node_param", false) of
        true ->
            throw({bad_request, <<"`node` prameter is required">>});
        false ->
            ok
    end.

check_range_required() ->
    case config:get_boolean("reshard", "require_range_param", false) of
        true ->
            throw({bad_request, <<"`range` prameter is required">>});
        false ->
            ok
    end.
