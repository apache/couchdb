%% -------------------------------------------------------------------
%%
%% weatherreport - automated diagnostic tools for CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Diagnostic that checks whether the current node can be
%% safely rebuilt (i.e. taken out of service).
-module(weatherreport_check_safe_to_rebuild).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-spec description() -> string().
description() ->
    "Check whether the node can safely be taken out of service".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

%% @doc Check if rebuilding a node is safe. Safe in this context means
%% that no shard would end up with N<Threshold when the node is offline
-spec safe_to_rebuild(atom(), integer()) -> [list()].
safe_to_rebuild(Node, RawThreshold) ->
    Threshold =
        case config:get("couchdb", "maintenance_mode") of
            "true" ->
                RawThreshold - 1;
            _ ->
                RawThreshold
        end,
    BelowThreshold = fun
        ({_, _, {_, C}}) when C =< Threshold -> true;
        (_) -> false
    end,
    ToKV = fun({Db, Range, Status}) -> {[Db, Range], Status} end,

    ShardsInDanger = dict:from_list(
        lists:map(
            ToKV,
            lists:filter(BelowThreshold, custodian:report())
        )
    ),

    mem3_shards:fold(
        fun(Shard, Acc) ->
            case Shard of
                {shard, _, Node, Db, [Start, End], _} ->
                    case dict:find([Db, [Start, End]], ShardsInDanger) of
                        {_, _} ->
                            PrettyRange = [
                                couch_util:to_hex(<<Start:32/integer>>),
                                couch_util:to_hex(<<End:32/integer>>)
                            ],
                            PrettyShard = lists:flatten(
                                io_lib:format("~s ~s-~s", [Db | PrettyRange])
                            ),
                            [PrettyShard | Acc];
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end,
        []
    ).

-spec shards_to_message(atom(), list()) -> {atom(), {atom(), list()}}.
shards_to_message(n1, []) ->
    {info, {n1, []}};
shards_to_message(n1, Shards) ->
    {error, {n1, Shards}};
shards_to_message(n0, []) ->
    {info, {n0, []}};
shards_to_message(n0, Shards) ->
    {crit, {n0, Shards}}.

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    N0Shards = safe_to_rebuild(node(), 1),
    N1Shards = lists:subtract(safe_to_rebuild(node(), 2), N0Shards),
    [shards_to_message(n0, N0Shards), shards_to_message(n1, N1Shards)].

-spec format(term()) -> {io:format(), [term()]}.
format({n1, []}) ->
    {"This node can be rebuilt without causing any shards to become N=1", []};
format({n1, Shards}) ->
    {
        "Rebuilding this node will leave the following shards with only one live copy: ~s",
        [string:join(Shards, ", ")]
    };
format({n0, []}) ->
    {"This node can be rebuilt without causing any shards to become N=0", []};
format({n0, Shards}) ->
    {
        "Rebuilding this node will leave the following shard with NO live copies: ~s",
        [string:join(Shards, ", ")]
    }.
