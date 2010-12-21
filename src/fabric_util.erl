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

-export([submit_jobs/3, cleanup/1, recv/4, get_db/1, remove_ancestors/2]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

submit_jobs(Shards, EndPoint, ExtraArgs) ->
    lists:map(fun(#shard{node=Node, name=ShardName} = Shard) ->
        Ref = rexi:cast(Node, {fabric_rpc, EndPoint, [ShardName | ExtraArgs]}),
        Shard#shard{ref = Ref}
    end, Shards).

cleanup(Workers) ->
    [rexi:kill(Node, Ref) || #shard{node=Node, ref=Ref} <- Workers].

recv(Workers, Keypos, Fun, Acc0) ->
    Timeout = case couch_config:get("fabric", "request_timeout", "60000") of
    "infinity" -> infinity;
    N -> list_to_integer(N)
    end,
    rexi_utils:recv(Workers, Keypos, Fun, Acc0, Timeout, infinity).


get_db(DbName) ->
    Shards = mem3:shards(DbName),
    case lists:partition(fun(#shard{node = N}) -> N =:= node() end, Shards) of
    {[#shard{name = ShardName}|_], _} ->
        % prefer node-local DBs
        couch_db:open(ShardName, []);
    {[], [#shard{node = Node, name = ShardName}|_]} ->
        % but don't require them
        rpc:call(Node, couch_db, open, [ShardName, []])
    end.

% this presumes the incoming list is sorted, i.e. shorter revlists come first
remove_ancestors([], Acc) ->
    lists:reverse(Acc);
remove_ancestors([{{not_found, _}, Count} = Head | Tail], Acc) ->
    % any document is a descendant
    case lists:filter(fun({{ok, #doc{}}, _}) -> true; (_) -> false end, Tail) of
    [{{ok, #doc{}} = Descendant, _} | _] ->
        remove_ancestors(orddict:update_counter(Descendant, Count, Tail), Acc);
    [] ->
        remove_ancestors(Tail, [Head | Acc])
    end;
remove_ancestors([{{ok, #doc{revs = {Pos, Revs}}}, Count} = Head | Tail], Acc) ->
    Descendants = lists:dropwhile(fun
    ({{ok, #doc{revs = {Pos2, Revs2}}}, _}) ->
        case lists:nthtail(Pos2 - Pos, Revs2) of
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
        remove_ancestors(orddict:update_counter(Descendant, Count, Tail), Acc)
    end.

remove_ancestors_test() ->
    Foo1 = {ok, #doc{revs = {1, [<<"foo">>]}}},
    Foo2 = {ok, #doc{revs = {2, [<<"foo2">>, <<"foo">>]}}},
    Bar1 = {ok, #doc{revs = {1, [<<"bar">>]}}},
    Bar2 = {not_found, {1,<<"bar">>}},
    ?assertEqual(
        [{Bar1,1}, {Foo1,1}],
        remove_ancestors([{Bar1,1}, {Foo1,1}], [])
    ),
    ?assertEqual(
        [{Bar1,1}, {Foo2,2}],
        remove_ancestors([{Bar1,1}, {Foo1,1}, {Foo2,1}], [])
    ),
    ?assertEqual(
        [{Bar1,2}],
        remove_ancestors([{Bar2,1}, {Bar1,1}], [])
    ).
