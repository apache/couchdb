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

-module(mem3_util).

-export([name_shard/2, create_partition_map/5, build_shards/2,
    n_val/2, q_val/1, to_atom/1, to_integer/1, write_db_doc/1, delete_db_doc/1,
    shard_info/1, ensure_exists/1, open_db_doc/1, get_or_create_db/2]).
-export([is_deleted/1, rotate_list/2]).
-export([get_shard_opts/1, get_engine_opt/1, get_props_opt/1]).
-export([get_shard_props/1, find_dirty_shards/0]).
-export([
    iso8601_timestamp/0,
    live_nodes/0,
    replicate_dbs_to_all_nodes/1,
    replicate_dbs_from_all_nodes/1,
    range_overlap/2,
    get_ring/1,
    get_ring/2,
    get_ring/3,
    get_ring/4,
    non_overlapping_shards/1,
    non_overlapping_shards/3,
    calculate_max_n/1
]).

%% do not use outside mem3.
-export([build_ordered_shards/2, downcast/1]).

-export([create_partition_map/4, name_shard/1]).
-deprecated({create_partition_map, 4, eventually}).
-deprecated({name_shard, 1, eventually}).

-define(RINGTOP, 2 bsl 31).  % CRC32 space

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").


name_shard(Shard) ->
    name_shard(Shard, "").

name_shard(#shard{dbname = DbName, range=Range} = Shard, Suffix) ->
    Name = make_name(DbName, Range, Suffix),
    Shard#shard{name = ?l2b(Name)};

name_shard(#ordered_shard{dbname = DbName, range=Range} = Shard, Suffix) ->
    Name = make_name(DbName, Range, Suffix),
    Shard#ordered_shard{name = ?l2b(Name)}.

make_name(DbName, [B,E], Suffix) ->
    ["shards/", couch_util:to_hex(<<B:32/integer>>), "-",
     couch_util:to_hex(<<E:32/integer>>), "/", DbName, Suffix].

create_partition_map(DbName, N, Q, Nodes) ->
    create_partition_map(DbName, N, Q, Nodes, "").

create_partition_map(DbName, N, Q, Nodes, Suffix) when Q > 0 ->
    UniqueShards = make_key_ranges((?RINGTOP) div Q, 0, []),
    Shards0 = lists:flatten([lists:duplicate(N, S) || S <- UniqueShards]),
    Shards1 = attach_nodes(Shards0, [], Nodes, []),
    [name_shard(S#shard{dbname=DbName}, Suffix) || S <- Shards1].

make_key_ranges(I, CurrentPos, Acc) when I > 0, CurrentPos >= ?RINGTOP ->
    Acc;
make_key_ranges(Increment, Start, Acc) when Increment > 0 ->
    case Start + 2*Increment of
    X when X > ?RINGTOP ->
        End = ?RINGTOP - 1;
    _ ->
        End = Start + Increment - 1
    end,
    make_key_ranges(Increment, End+1, [#shard{range=[Start, End]} | Acc]).

attach_nodes([], Acc, _, _) ->
    lists:reverse(Acc);
attach_nodes(Shards, Acc, [], UsedNodes) ->
    attach_nodes(Shards, Acc, lists:reverse(UsedNodes), []);
attach_nodes([S | Rest], Acc, [Node | Nodes], UsedNodes) ->
    attach_nodes(Rest, [S#shard{node=Node} | Acc], Nodes, [Node | UsedNodes]).

open_db_doc(DocId) ->
    {ok, Db} = couch_db:open(mem3_sync:shards_db(), [?ADMIN_CTX]),
    try couch_db:open_doc(Db, DocId, [ejson_body]) after couch_db:close(Db) end.

write_db_doc(Doc) ->
    write_db_doc(mem3_sync:shards_db(), Doc, true).

write_db_doc(DbName, #doc{id=Id, body=Body} = Doc, ShouldMutate) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    try couch_db:open_doc(Db, Id, [ejson_body]) of
    {ok, #doc{body = Body}} ->
        % the doc is already in the desired state, we're done here
        ok;
    {not_found, _} when ShouldMutate ->
        try couch_db:update_doc(Db, Doc, []) of
        {ok, _} ->
            ok
        catch conflict ->
            % check to see if this was a replication race or a different edit
            write_db_doc(DbName, Doc, false)
        end;
    _ ->
        % the doc already exists in a different state
        conflict
    after
        couch_db:close(Db)
    end.

delete_db_doc(DocId) ->
    gen_server:cast(mem3_shards, {cache_remove, DocId}),
    delete_db_doc(mem3_sync:shards_db(), DocId, true).

delete_db_doc(DbName, DocId, ShouldMutate) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    {ok, Revs} = couch_db:open_doc_revs(Db, DocId, all, []),
    try [Doc#doc{deleted=true} || {ok, #doc{deleted=false}=Doc} <- Revs] of
    [] ->
        not_found;
    Docs when ShouldMutate ->
        try couch_db:update_docs(Db, Docs, []) of
        {ok, _} ->
            ok
        catch conflict ->
            % check to see if this was a replication race or if leafs survived
            delete_db_doc(DbName, DocId, false)
        end;
    _ ->
        % we have live leafs that we aren't allowed to delete. let's bail
        conflict
    after
        couch_db:close(Db)
    end.

%% Always returns original #shard records.
-spec build_shards(binary(), list()) -> [#shard{}].
build_shards(DbName, DocProps) ->
    build_shards_by_node(DbName, DocProps).

%% Will return #ordered_shard records if by_node and by_range
%% are symmetrical, #shard records otherwise.
-spec build_ordered_shards(binary(), list()) ->
    [#shard{}] | [#ordered_shard{}].
build_ordered_shards(DbName, DocProps) ->
    ByNode = build_shards_by_node(DbName, DocProps),
    ByRange = build_shards_by_range(DbName, DocProps),
    Symmetrical = lists:sort(ByNode) =:= lists:sort(downcast(ByRange)),
    case Symmetrical of
        true  -> ByRange;
        false -> ByNode
    end.

build_shards_by_node(DbName, DocProps) ->
    {ByNode} = couch_util:get_value(<<"by_node">>, DocProps, {[]}),
    Suffix = couch_util:get_value(<<"shard_suffix">>, DocProps, ""),
    lists:flatmap(fun({Node, Ranges}) ->
        lists:map(fun(Range) ->
            [B,E] = string:tokens(?b2l(Range), "-"),
            Beg = httpd_util:hexlist_to_integer(B),
            End = httpd_util:hexlist_to_integer(E),
            name_shard(#shard{
                dbname = DbName,
                node = to_atom(Node),
                range = [Beg, End],
                opts = get_shard_opts(DocProps)
            }, Suffix)
        end, Ranges)
    end, ByNode).

build_shards_by_range(DbName, DocProps) ->
    {ByRange} = couch_util:get_value(<<"by_range">>, DocProps, {[]}),
    Suffix = couch_util:get_value(<<"shard_suffix">>, DocProps, ""),
    lists:flatmap(fun({Range, Nodes}) ->
        lists:map(fun({Node, Order}) ->
            [B,E] = string:tokens(?b2l(Range), "-"),
            Beg = httpd_util:hexlist_to_integer(B),
            End = httpd_util:hexlist_to_integer(E),
            name_shard(#ordered_shard{
                dbname = DbName,
                node = to_atom(Node),
                range = [Beg, End],
                order = Order,
                opts = get_shard_opts(DocProps)
            }, Suffix)
        end, lists:zip(Nodes, lists:seq(1, length(Nodes))))
    end, ByRange).

to_atom(Node) when is_binary(Node) ->
    list_to_atom(binary_to_list(Node));
to_atom(Node) when is_atom(Node) ->
    Node.

to_integer(N) when is_integer(N) ->
    N;
to_integer(N) when is_binary(N) ->
    list_to_integer(binary_to_list(N));
to_integer(N) when is_list(N) ->
    list_to_integer(N).

get_shard_opts(DocProps) ->
    get_engine_opt(DocProps) ++ get_props_opt(DocProps).

get_engine_opt(DocProps) ->
    case couch_util:get_value(<<"engine">>, DocProps) of
        Engine when is_binary(Engine) ->
            [{engine, Engine}];
        _ ->
            []
    end.

get_props_opt(DocProps) ->
    case couch_util:get_value(<<"props">>, DocProps) of
        {Props} when is_list(Props) ->
            [{props, db_props_from_json(Props)}];
        _ ->
            []
    end.

db_props_from_json([]) ->
    [];

db_props_from_json([{<<"partitioned">>, Value} | Rest]) ->
    [{partitioned, Value} | db_props_from_json(Rest)];

db_props_from_json([{<<"hash">>, [MBin, FBin, A]} | Rest]) ->
    M = binary_to_existing_atom(MBin, utf8),
    F = binary_to_existing_atom(FBin, utf8),
    [{hash, [M, F, A]} | db_props_from_json(Rest)];

db_props_from_json([{K, V} | Rest]) ->
    [{K, V} | db_props_from_json(Rest)].

n_val(undefined, NodeCount) ->
    n_val(config:get("cluster", "n", "3"), NodeCount);
n_val(N, NodeCount) when is_list(N) ->
    n_val(list_to_integer(N), NodeCount);
n_val(N, NodeCount) when is_integer(NodeCount), N > NodeCount ->
    couch_log:error("Request to create N=~p DB but only ~p node(s)", [N, NodeCount]),
    NodeCount;
n_val(N, _) when N < 1 ->
    1;
n_val(N, _) ->
    N.

q_val(Q) when is_list(Q) ->
    q_val(list_to_integer(Q));
q_val(Q) when Q > 0 ->
    Q;
q_val(_) ->
    throw({error, invalid_q_value}).

shard_info(DbName) ->
    [{n, mem3:n(DbName)},
     {q, length(mem3:shards(DbName)) div mem3:n(DbName)}].

ensure_exists(DbName) when is_list(DbName) ->
    ensure_exists(list_to_binary(DbName));
ensure_exists(DbName) ->
    Options = [nologifmissing, sys_db, {create_if_missing, true}, ?ADMIN_CTX],
    case couch_db:open(DbName, Options) of
    {ok, Db} ->
        {ok, Db};
    file_exists ->
        couch_db:open(DbName, [sys_db, ?ADMIN_CTX])
    end.


is_deleted(Change) ->
    case couch_util:get_value(<<"deleted">>, Change) of
    undefined ->
        % keep backwards compatibility for a while
        couch_util:get_value(deleted, Change, false);
    Else ->
        Else
    end.

rotate_list(_Key, []) ->
    [];
rotate_list(Key, List) when not is_binary(Key) ->
    rotate_list(term_to_binary(Key), List);
rotate_list(Key, List) ->
    {H, T} = lists:split(erlang:crc32(Key) rem length(List), List),
    T ++ H.

downcast(#shard{}=S) ->
    S;
downcast(#ordered_shard{}=S) ->
    #shard{
       name = S#ordered_shard.name,
       node = S#ordered_shard.node,
       dbname = S#ordered_shard.dbname,
       range = S#ordered_shard.range,
       ref = S#ordered_shard.ref,
       opts = S#ordered_shard.opts
      };
downcast(Shards) when is_list(Shards) ->
    [downcast(Shard) || Shard <- Shards].

iso8601_timestamp() ->
    {_,_,Micro} = Now = os:timestamp(),
    {{Year,Month,Date},{Hour,Minute,Second}} = calendar:now_to_datetime(Now),
    Format = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
    io_lib:format(Format, [Year, Month, Date, Hour, Minute, Second, Micro]).


live_nodes() ->
    LiveNodes = [node() | nodes()],
    Mem3Nodes = lists:sort(mem3:nodes()),
    [N || N <- Mem3Nodes, lists:member(N, LiveNodes)].


% Replicate "dbs" db to all nodes. Basically push the changes to all the live
% mem3:nodes(). Returns only after all current changes have been replicated,
% which could be a while.
%
replicate_dbs_to_all_nodes(Timeout) ->
    DbName = mem3_sync:shards_db(),
    Targets= mem3_util:live_nodes() -- [node()],
    Res =  [start_replication(node(), T, DbName, Timeout) || T <- Targets],
    collect_replication_results(Res, Timeout).


% Replicate "dbs" db from all nodes to this node. Basically make an rpc call
% to all the nodes an have them push their changes to this node. Then monitor
% them until they are all done.
%
replicate_dbs_from_all_nodes(Timeout) ->
    DbName = mem3_sync:shards_db(),
    Sources = mem3_util:live_nodes() -- [node()],
    Res = [start_replication(S, node(), DbName, Timeout) || S <- Sources],
    collect_replication_results(Res, Timeout).


% Spawn and monitor a single replication of a database to a target node.
% Returns {ok, PidRef}. This function could be called locally or remotely from
% mem3_rpc, for instance when replicating other nodes' data to this node.
%
start_replication(Source, Target, DbName, Timeout) ->
    spawn_monitor(fun() ->
        case mem3_rpc:replicate(Source, Target, DbName, Timeout) of
            {ok, 0} ->
                exit(ok);
            Other ->
                exit(Other)
        end
    end).


collect_replication_results(Replications, Timeout) ->
    Res = [collect_replication_result(R, Timeout) || R <- Replications],
    case [R || R <- Res, R =/= ok] of
        [] ->
            ok;
        Errors ->
            {error, Errors}
    end.


collect_replication_result({Pid, Ref}, Timeout) when is_pid(Pid) ->
    receive
        {'DOWN', Ref, _, _, Res} ->
            Res
    after Timeout ->
        demonitor(Pid, [flush]),
        exit(Pid, kill),
        {error, {timeout, Timeout, node(Pid)}}
    end;

collect_replication_result(Error, _) ->
    {error, Error}.


% Consider these cases:
%
%       A-------B
%
% overlap:
%   X--------Y
%         X-Y
%          X-------Y
% X-------------------Y
%
% no overlap:
% X-Y                     because A !=< Y
%                   X-Y   because X !=< B
%
range_overlap([A, B], [X, Y]) when
        is_integer(A), is_integer(B),
        is_integer(X), is_integer(Y),
        A =< B, X =< Y ->
    A =< Y andalso X =< B.


non_overlapping_shards(Shards) ->
    {Start, End} = lists:foldl(fun(Shard, {Min, Max}) ->
        [B, E] = mem3:range(Shard),
        {min(B, Min), max(E, Max)}
    end, {0, ?RING_END}, Shards),
    non_overlapping_shards(Shards, Start, End).


non_overlapping_shards([], _, _) ->
    [];

non_overlapping_shards(Shards, Start, End) ->
    Ranges = lists:map(fun(Shard) ->
        [B, E] = mem3:range(Shard),
        {B, E}
    end, Shards),
    Ring = get_ring(Ranges, fun sort_ranges_fun/2, Start, End),
    lists:filter(fun(Shard) ->
        [B, E] = mem3:range(Shard),
        lists:member({B, E}, Ring)
    end, Shards).


% Given a list of shards, return the maximum number of copies
% across all the ranges. If the ring is incomplete it will return 0.
% If there it is an n = 1 database, it should return 1, etc.
calculate_max_n(Shards) ->
    Ranges = lists:map(fun(Shard) ->
        [B, E] = mem3:range(Shard),
        {B, E}
    end, Shards),
    calculate_max_n(Ranges, get_ring(Ranges), 0).


calculate_max_n(_Ranges, [], N) ->
    N;

calculate_max_n(Ranges, Ring, N) ->
    NewRanges = Ranges -- Ring,
    calculate_max_n(NewRanges, get_ring(NewRanges), N + 1).


get_ring(Ranges) ->
    get_ring(Ranges, fun sort_ranges_fun/2, 0, ?RING_END).


get_ring(Ranges, SortFun) when is_function(SortFun, 2) ->
    get_ring(Ranges, SortFun, 0, ?RING_END).


get_ring(Ranges, Start, End) when is_integer(Start), is_integer(End),
        Start >= 0, End >= 0, Start =< End ->
    get_ring(Ranges, fun sort_ranges_fun/2, Start, End).

% Build a ring out of a list of possibly overlapping ranges. If a ring cannot
% be built then [] is returned. Start and End supply a custom range such that
% only intervals in that range will be considered. SortFun is a custom sorting
% function to sort intervals before the ring is built. The custom sort function
% can be used to prioritize how the ring is built, for example, whether to use
% shortest ranges first (and thus have more total shards) or longer or any
% other scheme.
%
get_ring([], _SortFun, _Start, _End) ->
    [];
get_ring(Ranges, SortFun, Start, End) when is_function(SortFun, 2),
        is_integer(Start), is_integer(End),
        Start >= 0, End >= 0, Start =< End  ->
    Sorted = lists:usort(SortFun, Ranges),
    case get_subring_int(Start, End, Sorted) of
        fail -> [];
        Ring -> Ring
    end.


get_subring_int(_, _, []) ->
    fail;

get_subring_int(Start, EndMax, [{Start, End} = Range | Tail]) ->
    case End =:= EndMax of
        true ->
            [Range];
        false ->
            case get_subring_int(End + 1, EndMax, Tail) of
                fail ->
                    get_subring_int(Start, EndMax, Tail);
                Acc ->
                    [Range | Acc]
            end
    end;

get_subring_int(Start1, _, [{Start2, _} | _]) when Start2 > Start1 ->
    % Found a gap, this attempt is done
    fail;

get_subring_int(Start1, EndMax, [{Start2, _} | Rest]) when Start2 < Start1 ->
    % We've overlapped the head, skip the shard
    get_subring_int(Start1, EndMax, Rest).


% Sort ranges by starting point, then sort so that
% the longest range comes first
sort_ranges_fun({B, E1}, {B, E2}) ->
    E2 =< E1;

sort_ranges_fun({B1, _}, {B2, _}) ->
    B1 =< B2.


get_or_create_db(DbName, Options) ->
    case couch_db:open_int(DbName, Options) of
        {ok, _} = OkDb ->
            OkDb;
        {not_found, no_db_file} ->
            try
                DbOpts = case mem3:dbname(DbName) of
                    DbName  -> [];
                    MDbName -> mem3_shards:opts_for_db(MDbName)
                end,
                Options1 = [{create_if_missing, true} | Options],
                Options2 = merge_opts(DbOpts, Options1),
                couch_db:open_int(DbName, Options2)
            catch error:database_does_not_exist ->
                throw({error, missing_target})
            end;
        Else ->
            Else
    end.


%% merge two proplists, atom options only valid in Old
merge_opts(New, Old) ->
    lists:foldl(fun({Key, Val}, Acc) ->
        lists:keystore(Key, 1, Acc, {Key, Val})
    end, Old, New).


get_shard_props(ShardName) ->
    case couch_db:open_int(ShardName, []) of
        {ok, Db} ->
            Props = case couch_db_engine:get_props(Db) of
                undefined -> [];
                Else -> Else
            end,
            %% We don't normally store the default engine name
            EngineProps = case couch_db_engine:get_engine(Db) of
                couch_bt_engine ->
                    [];
                EngineName ->
                    [{engine, EngineName}]
            end,
            [{props, Props} | EngineProps];
        {not_found, _} ->
            not_found;
        Else ->
            Else
    end.


find_dirty_shards() ->
    mem3_shards:fold(fun(#shard{node=Node, name=Name, opts=Opts}=Shard, Acc) ->
        case Opts of
            [] ->
                Acc;
            [{props, []}] ->
                Acc;
            _ ->
                Props = rpc:call(Node, ?MODULE, get_shard_props, [Name]),
                case Props =:= Opts of
                    true ->
                        Acc;
                    false ->
                        [{Shard, Props} | Acc]
                end
        end
    end, []).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

range_overlap_test_() ->
    [?_assertEqual(Res, range_overlap(R1, R2)) || {R1, R2, Res} <- [
        {[2, 6], [1, 3], true},
        {[2, 6], [3, 4], true},
        {[2, 6], [4, 8], true},
        {[2, 6], [1, 9], true},
        {[2, 6], [1, 2], true},
        {[2, 6], [6, 7], true},
        {[2, 6], [0, 1], false},
        {[2, 6], [7, 9], false}
    ]].


non_overlapping_shards_test() ->
    [?_assertEqual(Res, non_overlapping_shards(Shards)) || {Shards, Res} <- [
        {
            [shard(0, ?RING_END)],
            [shard(0, ?RING_END)]
        },
        {
            [shard(0, 1)],
            [shard(0, 1)]
        },
        {
            [shard(0, 1), shard(0, 1)],
            [shard(0, 1)]
        },
        {
            [shard(0, 1), shard(3, 4)],
            []
        },
        {
            [shard(0, 1), shard(1, 2), shard(2, 3)],
            [shard(0, 1), shard(2, 3)]
        },
        {
            [shard(1, 2), shard(0, 1)],
            [shard(0, 1), shard(1, 2)]
        },
        {
            [shard(0, 1), shard(0, 2), shard(2, 5), shard(3, 5)],
            [shard(0, 2), shard(2, 5)]
        },
        {
            [shard(0, 2), shard(4, 5), shard(1, 3)],
            []
        }

    ]].


calculate_max_n_test_() ->
     [?_assertEqual(Res, calculate_max_n(Shards)) || {Res, Shards} <- [
        {0, []},
        {0, [shard(1, ?RING_END)]},
        {1, [shard(0, ?RING_END)]},
        {1, [shard(0, ?RING_END), shard(1, ?RING_END)]},
        {2, [shard(0, ?RING_END), shard(0, ?RING_END)]},
        {2, [shard(0, 1), shard(2, ?RING_END), shard(0, ?RING_END)]},
        {0, [shard(0, 3), shard(5, ?RING_END), shard(1, ?RING_END)]}
     ]].


shard(Begin, End) ->
    #shard{range = [Begin, End]}.

-endif.
