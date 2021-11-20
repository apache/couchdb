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

-export([
    submit_jobs/3, submit_jobs/4,
    cleanup/1,
    recv/4,
    get_db/1, get_db/2,
    error_info/1,
    update_counter/3,
    remove_ancestors/2,
    create_monitors/1,
    kv/2,
    remove_down_workers/2, remove_down_workers/3,
    doc_id_and_rev/1
]).
-export([request_timeout/0, attachments_timeout/0, all_docs_timeout/0, view_timeout/1]).
-export([log_timeout/2, remove_done_workers/2]).
-export([is_users_db/1, is_replicator_db/1]).
-export([open_cluster_db/1, open_cluster_db/2]).
-export([is_partitioned/1]).
-export([validate_all_docs_args/2, validate_args/3]).
-export([upgrade_mrargs/1]).
-export([worker_ranges/1]).
-export([get_uuid_prefix_len/0]).
-export([isolate/1, isolate/2]).

-compile({inline, [{doc_id_and_rev, 1}]}).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("eunit/include/eunit.hrl").

remove_down_workers(Workers, BadNode) ->
    remove_down_workers(Workers, BadNode, []).

remove_down_workers(Workers, BadNode, RingOpts) ->
    Filter = fun(#shard{node = Node}, _) -> Node =/= BadNode end,
    NewWorkers = fabric_dict:filter(Filter, Workers),
    case fabric_ring:is_progress_possible(NewWorkers, RingOpts) of
        true ->
            {ok, NewWorkers};
        false ->
            error
    end.

submit_jobs(Shards, EndPoint, ExtraArgs) ->
    submit_jobs(Shards, fabric_rpc, EndPoint, ExtraArgs).

submit_jobs(Shards, Module, EndPoint, ExtraArgs) ->
    lists:map(
        fun(#shard{node = Node, name = ShardName} = Shard) ->
            Ref = rexi:cast(Node, {Module, EndPoint, [ShardName | ExtraArgs]}),
            Shard#shard{ref = Ref}
        end,
        Shards
    ).

cleanup(Workers) ->
    rexi:kill_all([{Node, Ref} || #shard{node = Node, ref = Ref} <- Workers]).

recv(Workers, Keypos, Fun, Acc0) ->
    rexi_utils:recv(Workers, Keypos, Fun, Acc0, request_timeout(), infinity).

request_timeout() ->
    timeout("request", "60000").

all_docs_timeout() ->
    timeout("all_docs", "10000").

attachments_timeout() ->
    timeout("attachments", "600000").

view_timeout(Args) ->
    PartitionQuery = couch_mrview_util:get_extra(Args, partition, false),
    case PartitionQuery of
        false -> timeout("view", "infinity");
        _ -> timeout("partition_view", "infinity")
    end.

timeout(Type, Default) ->
    case config:get("fabric", Type ++ "_timeout", Default) of
        "infinity" -> infinity;
        N -> list_to_integer(N)
    end.

log_timeout(Workers, EndPoint) ->
    CounterKey = [fabric, worker, timeouts],
    couch_stats:increment_counter(CounterKey),
    lists:map(
        fun(#shard{node = Dest, name = Name}) ->
            Fmt = "fabric_worker_timeout ~s,~p,~p",
            couch_log:error(Fmt, [EndPoint, Dest, Name])
        end,
        Workers
    ).

remove_done_workers(Workers, WaitingIndicator) ->
    [W || {W, WI} <- fabric_dict:to_list(Workers), WI == WaitingIndicator].

get_db(DbName) ->
    get_db(DbName, []).

get_db(DbName, Options) ->
    {Local, SameZone, DifferentZone} = mem3:group_by_proximity(mem3:shards(DbName)),
    % Prefer shards on the same node over other nodes, prefer shards in the same zone over
    % over zones and sort each remote list by name so that we don't repeatedly try the same node.
    Shards =
        Local ++ lists:keysort(#shard.name, SameZone) ++ lists:keysort(#shard.name, DifferentZone),
    % suppress shards from down nodes
    Nodes = [node() | erlang:nodes()],
    Live = [S || #shard{node = N} = S <- Shards, lists:member(N, Nodes)],
    % Only accept factors > 1, otherwise our math breaks further down
    Factor = max(2, config:get_integer("fabric", "shard_timeout_factor", 2)),
    MinTimeout = config:get_integer("fabric", "shard_timeout_min_msec", 100),
    MaxTimeout = request_timeout(),
    Timeout = get_db_timeout(length(Live), Factor, MinTimeout, MaxTimeout),
    get_shard(Live, Options, Timeout, Factor).

get_shard([], _Opts, _Timeout, _Factor) ->
    erlang:error({internal_server_error, "No DB shards could be opened."});
get_shard([#shard{node = Node, name = Name} | Rest], Opts, Timeout, Factor) ->
    Mon = rexi_monitor:start([rexi_utils:server_pid(Node)]),
    MFA = {fabric_rpc, open_shard, [Name, [{timeout, Timeout} | Opts]]},
    Ref = rexi:cast(Node, self(), MFA, [sync]),
    try
        receive
            {Ref, {ok, Db}} ->
                {ok, Db};
            {Ref, {'rexi_EXIT', {{unauthorized, _} = Error, _}}} ->
                throw(Error);
            {Ref, {'rexi_EXIT', {{forbidden, _} = Error, _}}} ->
                throw(Error);
            {Ref, Reason} ->
                couch_log:debug("Failed to open shard ~p because: ~p", [Name, Reason]),
                get_shard(Rest, Opts, Timeout, Factor)
        after Timeout ->
            couch_log:debug("Failed to open shard ~p after: ~p", [Name, Timeout]),
            get_shard(Rest, Opts, Factor * Timeout, Factor)
        end
    after
        rexi_monitor:stop(Mon)
    end.

get_db_timeout(N, Factor, MinTimeout, infinity) ->
    % MaxTimeout may be infinity so we just use the largest Erlang small int to
    % avoid blowing up the arithmetic
    get_db_timeout(N, Factor, MinTimeout, 1 bsl 59);
get_db_timeout(N, Factor, MinTimeout, MaxTimeout) ->
    %
    % The progression of timeouts forms a geometric series:
    %
    %     MaxTimeout = T + T*F + T*F^2 + T*F^3 ...
    %
    % Where T is the initial timeout and F is the factor. The formula for
    % the sum is:
    %
    %     Sum[T * F^I, I <- 0..N] = T * (1 - F^(N + 1)) / (1 - F)
    %
    % Then, for a given sum and factor we can calculate the initial timeout T:
    %
    %     T = Sum / ((1 - F^(N+1)) / (1 - F))
    %
    Timeout = MaxTimeout / ((1 - math:pow(Factor, N + 1)) / (1 - Factor)),
    % Apply a minimum timeout value
    max(MinTimeout, trunc(Timeout)).

error_info({{timeout, _} = Error, _Stack}) ->
    Error;
error_info({{Error, Reason}, Stack}) ->
    {Error, Reason, Stack};
error_info({Error, Stack}) ->
    {Error, nil, Stack}.

update_counter(Item, Incr, D) ->
    UpdateFun = fun({Old, Count}) -> {Old, Count + Incr} end,
    orddict:update(make_key(Item), UpdateFun, {Item, Incr}, D).

make_key({ok, L}) when is_list(L) ->
    make_key(L);
make_key([]) ->
    [];
make_key([{ok, #doc{revs = {Pos, [RevId | _]}}} | Rest]) ->
    [{ok, {Pos, RevId}} | make_key(Rest)];
make_key([{{not_found, missing}, Rev} | Rest]) ->
    [{not_found, Rev} | make_key(Rest)];
make_key({ok, #doc{id = Id, revs = Revs}}) ->
    {Id, Revs};
make_key(Else) ->
    Else.

% this presumes the incoming list is sorted, i.e. shorter revlists come first
remove_ancestors([], Acc) ->
    lists:reverse(Acc);
remove_ancestors([{_, {{not_found, _}, Count}} = Head | Tail], Acc) ->
    % any document is a descendant
    case
        lists:filter(
            fun
                ({_, {{ok, #doc{}}, _}}) -> true;
                (_) -> false
            end,
            Tail
        )
    of
        [{_, {{ok, #doc{}} = Descendant, _}} | _] ->
            remove_ancestors(update_counter(Descendant, Count, Tail), Acc);
        [] ->
            remove_ancestors(Tail, [Head | Acc])
    end;
remove_ancestors([{_, {{ok, #doc{revs = {Pos, Revs}}}, Count}} = Head | Tail], Acc) ->
    Descendants = lists:dropwhile(
        fun({_, {{ok, #doc{revs = {Pos2, Revs2}}}, _}}) ->
            case lists:nthtail(erlang:min(Pos2 - Pos, length(Revs2)), Revs2) of
                [] ->
                    % impossible to tell if Revs2 is a descendant - assume no
                    true;
                History ->
                    % if Revs2 is a descendant, History is a prefix of Revs
                    not lists:prefix(History, Revs)
            end
        end,
        Tail
    ),
    case Descendants of
        [] ->
            remove_ancestors(Tail, [Head | Acc]);
        [{Descendant, _} | _] ->
            remove_ancestors(update_counter(Descendant, Count, Tail), Acc)
    end;
remove_ancestors([Error | Tail], Acc) ->
    remove_ancestors(Tail, [Error | Acc]).

create_monitors(Shards) ->
    MonRefs = lists:usort([rexi_utils:server_pid(N) || #shard{node = N} <- Shards]),
    rexi_monitor:start(MonRefs).

%% verify only id and rev are used in key.
update_counter_test() ->
    Reply =
        {ok, #doc{
            id = <<"id">>,
            revs = <<"rev">>,
            body = <<"body">>,
            atts = <<"atts">>
        }},
    ?assertEqual(
        [{{<<"id">>, <<"rev">>}, {Reply, 1}}],
        update_counter(Reply, 1, [])
    ).

remove_ancestors_test() ->
    Foo1 = {ok, #doc{revs = {1, [<<"foo">>]}}},
    Foo2 = {ok, #doc{revs = {2, [<<"foo2">>, <<"foo">>]}}},
    Bar1 = {ok, #doc{revs = {1, [<<"bar">>]}}},
    Bar2 = {not_found, {1, <<"bar">>}},
    ?assertEqual(
        [kv(Bar1, 1), kv(Foo1, 1)],
        remove_ancestors([kv(Bar1, 1), kv(Foo1, 1)], [])
    ),
    ?assertEqual(
        [kv(Bar1, 1), kv(Foo2, 2)],
        remove_ancestors([kv(Bar1, 1), kv(Foo1, 1), kv(Foo2, 1)], [])
    ),
    ?assertEqual(
        [kv(Bar1, 2)],
        remove_ancestors([kv(Bar2, 1), kv(Bar1, 1)], [])
    ).

is_replicator_db(DbName) ->
    path_ends_with(DbName, <<"_replicator">>).

is_users_db(DbName) ->
    ConfigName = list_to_binary(
        config:get(
            "chttpd_auth", "authentication_db", "_users"
        )
    ),
    DbName == ConfigName orelse path_ends_with(DbName, <<"_users">>).

path_ends_with(Path, Suffix) ->
    Suffix =:= couch_db:dbname_suffix(Path).

open_cluster_db(#shard{dbname = DbName, opts = Options}) ->
    case couch_util:get_value(props, Options) of
        Props when is_list(Props) ->
            {ok, Db} = couch_db:clustered_db(DbName, [{props, Props}]),
            Db;
        _ ->
            {ok, Db} = couch_db:clustered_db(DbName, []),
            Db
    end.

open_cluster_db(DbName, Opts) ->
    % as admin
    {SecProps} = fabric:get_security(DbName),
    UserCtx = couch_util:get_value(user_ctx, Opts, #user_ctx{}),
    {ok, Db} = couch_db:clustered_db(DbName, UserCtx, SecProps),
    Db.

%% test function
kv(Item, Count) ->
    {make_key(Item), {Item, Count}}.

doc_id_and_rev(#doc{id = DocId, revs = {RevNum, [RevHash | _]}}) ->
    {DocId, {RevNum, RevHash}}.

is_partitioned(DbName0) when is_binary(DbName0) ->
    Shards = mem3:shards(fabric:dbname(DbName0)),
    is_partitioned(open_cluster_db(hd(Shards)));
is_partitioned(Db) ->
    couch_db:is_partitioned(Db).

validate_all_docs_args(DbName, Args) when is_binary(DbName) ->
    Shards = mem3:shards(fabric:dbname(DbName)),
    Db = open_cluster_db(hd(Shards)),
    validate_all_docs_args(Db, Args);
validate_all_docs_args(Db, Args) ->
    true = couch_db:is_clustered(Db),
    couch_mrview_util:validate_all_docs_args(Db, Args).

validate_args(DbName, DDoc, Args) when is_binary(DbName) ->
    Shards = mem3:shards(fabric:dbname(DbName)),
    Db = open_cluster_db(hd(Shards)),
    validate_args(Db, DDoc, Args);
validate_args(Db, DDoc, Args) ->
    true = couch_db:is_clustered(Db),
    couch_mrview_util:validate_args(Db, DDoc, Args).

upgrade_mrargs(#mrargs{} = Args) ->
    Args;
upgrade_mrargs(
    {mrargs, ViewType, Reduce, PreflightFun, StartKey, StartKeyDocId, EndKey, EndKeyDocId, Keys,
        Direction, Limit, Skip, GroupLevel, Group, Stale, MultiGet, InclusiveEnd, IncludeDocs,
        DocOptions, UpdateSeq, Conflicts, Callback, Sorted, Extra}
) ->
    {Stable, Update} =
        case Stale of
            ok -> {true, false};
            update_after -> {true, lazy};
            _ -> {false, true}
        end,
    #mrargs{
        view_type = ViewType,
        reduce = Reduce,
        preflight_fun = PreflightFun,
        start_key = StartKey,
        start_key_docid = StartKeyDocId,
        end_key = EndKey,
        end_key_docid = EndKeyDocId,
        keys = Keys,
        direction = Direction,
        limit = Limit,
        skip = Skip,
        group_level = GroupLevel,
        group = Group,
        stable = Stable,
        update = Update,
        multi_get = MultiGet,
        inclusive_end = InclusiveEnd,
        include_docs = IncludeDocs,
        doc_options = DocOptions,
        update_seq = UpdateSeq,
        conflicts = Conflicts,
        callback = Callback,
        sorted = Sorted,
        extra = Extra
    }.

worker_ranges(Workers) ->
    Ranges = fabric_dict:fold(
        fun(#shard{range = [X, Y]}, _, Acc) ->
            [{X, Y} | Acc]
        end,
        [],
        Workers
    ),
    lists:usort(Ranges).

get_uuid_prefix_len() ->
    config:get_integer("fabric", "uuid_prefix_len", 7).

% If we issue multiple fabric calls from the same process we have to isolate
% them so in case of error they don't pollute the processes dictionary or the
% mailbox

isolate(Fun) ->
    isolate(Fun, infinity).

isolate(Fun, Timeout) ->
    {Pid, Ref} = erlang:spawn_monitor(fun() -> exit(do_isolate(Fun)) end),
    receive
        {'DOWN', Ref, _, _, {'$isolres', Res}} ->
            Res;
        {'DOWN', Ref, _, _, {'$isolerr', Tag, Reason, Stack}} ->
            erlang:raise(Tag, Reason, Stack)
    after Timeout ->
        erlang:demonitor(Ref, [flush]),
        exit(Pid, kill),
        erlang:error(timeout)
    end.

% OTP_RELEASE is defined in OTP 21+ only
-ifdef(OTP_RELEASE).

do_isolate(Fun) ->
    try
        {'$isolres', Fun()}
    catch
        Tag:Reason:Stack ->
            {'$isolerr', Tag, Reason, Stack}
    end.

-else.

do_isolate(Fun) ->
    try
        {'$isolres', Fun()}
    catch ?STACKTRACE(Tag, Reason, Stack)
        {'$isolerr', Tag, Reason, Stack}
    end.

-endif.

get_db_timeout_test() ->
    % Q=1, N=1
    ?assertEqual(20000, get_db_timeout(1, 2, 100, 60000)),

    % Q=2, N=1
    ?assertEqual(8571, get_db_timeout(2, 2, 100, 60000)),

    % Q=2, N=3 (default)
    ?assertEqual(472, get_db_timeout(2 * 3, 2, 100, 60000)),

    % Q=3, N=3
    ?assertEqual(100, get_db_timeout(3 * 3, 2, 100, 60000)),

    % Q=4, N=1
    ?assertEqual(1935, get_db_timeout(4, 2, 100, 60000)),

    % Q=8, N=1
    ?assertEqual(117, get_db_timeout(8, 2, 100, 60000)),

    % Q=8, N=3 (default in 2.x)
    ?assertEqual(100, get_db_timeout(8 * 3, 2, 100, 60000)),

    % Q=256, N=3
    ?assertEqual(100, get_db_timeout(256 * 3, 2, 100, 60000)),

    % Large factor = 100
    ?assertEqual(100, get_db_timeout(2 * 3, 100, 100, 60000)),

    % Small total request timeout = 1 sec
    ?assertEqual(100, get_db_timeout(2 * 3, 2, 100, 1000)),

    % Large total request timeout
    ?assertEqual(28346, get_db_timeout(2 * 3, 2, 100, 3600000)),

    % No shards at all
    ?assertEqual(60000, get_db_timeout(0, 2, 100, 60000)),

    % request_timeout was set to infinity, with enough shards it still gets to
    % 100 min timeout at the start from the exponential logic
    ?assertEqual(100, get_db_timeout(64, 2, 100, infinity)).
