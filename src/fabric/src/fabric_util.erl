% Copyright 2010 Cloudant
%
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

-module(fabric_util).

-export([submit_jobs/3, submit_jobs/4, cleanup/1, recv/4, get_db/1, get_db/2, error_info/1,
        update_counter/3, remove_ancestors/2, create_monitors/1, kv/2,
        remove_down_workers/2]).
-export([request_timeout/0]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

remove_down_workers(Workers, BadNode) ->
    Filter = fun(#shard{node = Node}, _) -> Node =/= BadNode end,
    NewWorkers = fabric_dict:filter(Filter, Workers),
    case fabric_view:is_progress_possible(NewWorkers) of
    true ->
        {ok, NewWorkers};
    false ->
        error
    end.

submit_jobs(Shards, EndPoint, ExtraArgs) ->
    submit_jobs(Shards, fabric_rpc, EndPoint, ExtraArgs).

submit_jobs(Shards, Module, EndPoint, ExtraArgs) ->
    lists:map(fun(#shard{node=Node, name=ShardName} = Shard) ->
        Ref = rexi:cast(Node, {Module, EndPoint, [ShardName | ExtraArgs]}),
        Shard#shard{ref = Ref}
    end, Shards).

cleanup(Workers) ->
    [rexi:kill(Node, Ref) || #shard{node=Node, ref=Ref} <- Workers].

recv(Workers, Keypos, Fun, Acc0) ->
    rexi_utils:recv(Workers, Keypos, Fun, Acc0, request_timeout(), infinity).

request_timeout() ->
    case config:get("fabric", "request_timeout", "60000") of
        "infinity" -> infinity;
        N -> list_to_integer(N)
    end.

get_db(DbName) ->
    get_db(DbName, []).

get_db(DbName, Options) ->
    {Local, SameZone, DifferentZone} = mem3:group_by_proximity(mem3:shards(DbName)),
    % Prefer shards on the same node over other nodes, prefer shards in the same zone over
    % over zones and sort each remote list by name so that we don't repeatedly try the same node.
    Shards = Local ++ lists:keysort(#shard.name, SameZone) ++ lists:keysort(#shard.name, DifferentZone),
    % suppress shards from down nodes
    Nodes = [node()|erlang:nodes()],
    Live = [S || #shard{node = N} = S <- Shards, lists:member(N, Nodes)],
    get_shard(Live, Options, 100).

get_shard([], _Opts, _Timeout) ->
    erlang:error({internal_server_error, "No DB shards could be opened."});
get_shard([#shard{node = Node, name = Name} | Rest], Opts, Timeout) ->
    case rpc:call(Node, couch_db, open, [Name, [{timeout, Timeout} | Opts]]) of
    {ok, Db} ->
        {ok, Db};
    {unauthorized, _} = Error ->
        throw(Error);
    {badrpc, {'EXIT', {timeout, _}}} ->
        get_shard(Rest, Opts, 2*Timeout);
    _Else ->
        get_shard(Rest, Opts, Timeout)
    end.

error_info({{<<"reduce_overflow_error">>, _} = Error, _Stack}) ->
    Error;
error_info({{timeout, _} = Error, _Stack}) ->
    Error;
error_info({{Error, Reason}, Stack}) ->
    {Error, Reason, Stack};
error_info({Error, Stack}) ->
    {Error, nil, Stack}.

update_counter(Item, Incr, D) ->
    UpdateFun = fun ({Old, Count}) -> {Old, Count + Incr} end,
    orddict:update(make_key(Item), UpdateFun, {Item, Incr}, D).

make_key({ok, L}) when is_list(L) ->
    make_key(L);
make_key([]) ->
    [];
make_key([{ok, #doc{revs= {Pos,[RevId | _]}}} | Rest]) ->
    [{ok, {Pos, RevId}} | make_key(Rest)];
make_key([{{not_found, missing}, Rev} | Rest]) ->
    [{not_found, Rev} | make_key(Rest)];
make_key({ok, #doc{id=Id,revs=Revs}}) ->
    {Id, Revs};
make_key(Else) ->
    Else.

% this presumes the incoming list is sorted, i.e. shorter revlists come first
remove_ancestors([], Acc) ->
    lists:reverse(Acc);
remove_ancestors([{_, {{not_found, _}, Count}} = Head | Tail], Acc) ->
    % any document is a descendant
    case lists:filter(fun({_,{{ok, #doc{}}, _}}) -> true; (_) -> false end, Tail) of
    [{_,{{ok, #doc{}} = Descendant, _}} | _] ->
        remove_ancestors(update_counter(Descendant, Count, Tail), Acc);
    [] ->
        remove_ancestors(Tail, [Head | Acc])
    end;
remove_ancestors([{_,{{ok, #doc{revs = {Pos, Revs}}}, Count}} = Head | Tail], Acc) ->
    Descendants = lists:dropwhile(fun
    ({_,{{ok, #doc{revs = {Pos2, Revs2}}}, _}}) ->
        case lists:nthtail(erlang:min(Pos2 - Pos, length(Revs2)), Revs2) of
        [] ->
            % impossible to tell if Revs2 is a descendant - assume no
            true;
        History ->
            % if Revs2 is a descendant, History is a prefix of Revs
            not lists:prefix(History, Revs)
        end
    end, Tail),
    case Descendants of [] ->
        remove_ancestors(Tail, [Head | Acc]);
    [{Descendant, _} | _] ->
        remove_ancestors(update_counter(Descendant, Count, Tail), Acc)
    end;
remove_ancestors([Error | Tail], Acc) ->
    remove_ancestors(Tail, [Error | Acc]).

create_monitors(Shards) ->
    MonRefs = lists:usort([{rexi_server, N} || #shard{node=N} <- Shards]),
    rexi_monitor:start(MonRefs).

%% verify only id and rev are used in key.
update_counter_test() ->
    Reply = {ok, #doc{id = <<"id">>, revs = <<"rev">>,
                    body = <<"body">>, atts = <<"atts">>}},
    ?assertEqual([{{<<"id">>,<<"rev">>}, {Reply, 1}}],
        update_counter(Reply, 1, [])).

remove_ancestors_test() ->
    Foo1 = {ok, #doc{revs = {1, [<<"foo">>]}}},
    Foo2 = {ok, #doc{revs = {2, [<<"foo2">>, <<"foo">>]}}},
    Bar1 = {ok, #doc{revs = {1, [<<"bar">>]}}},
    Bar2 = {not_found, {1,<<"bar">>}},
    ?assertEqual(
        [kv(Bar1,1), kv(Foo1,1)],
        remove_ancestors([kv(Bar1,1), kv(Foo1,1)], [])
    ),
    ?assertEqual(
        [kv(Bar1,1), kv(Foo2,2)],
        remove_ancestors([kv(Bar1,1), kv(Foo1,1), kv(Foo2,1)], [])
    ),
    ?assertEqual(
        [kv(Bar1,2)],
        remove_ancestors([kv(Bar2,1), kv(Bar1,1)], [])
    ).

%% test function
kv(Item, Count) ->
    {make_key(Item), {Item,Count}}.
