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
        remove_down_workers/2, doc_id_and_rev/1]).
-export([request_timeout/0, attachments_timeout/0, all_docs_timeout/0]).
-export([stream_start/2, stream_start/4]).

-compile({inline, [{doc_id_and_rev,1}]}).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(stream_acc, {
    workers,
    start_fun,
    replacements
}).

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

stream_start(Workers, Keypos) ->
    stream_start(Workers, Keypos, undefined, undefined).

stream_start(Workers0, Keypos, StartFun, Replacements) ->
    Fun = fun handle_stream_start/3,
    Acc = #stream_acc{
        workers = fabric_dict:init(Workers0, waiting),
        start_fun = StartFun,
        replacements = Replacements
    },
    Timeout = request_timeout(),
    case rexi_utils:recv(Workers0, Keypos, Fun, Acc, Timeout, infinity) of
        {ok, #stream_acc{workers=Workers}} ->
            true = fabric_view:is_progress_possible(Workers),
            AckedWorkers = fabric_dict:fold(fun(Worker, From, WorkerAcc) ->
                rexi:stream_start(From),
                [Worker | WorkerAcc]
            end, [], Workers),
            {ok, AckedWorkers};
        Else ->
            Else
    end.

handle_stream_start({rexi_DOWN, _, {_, NodeRef}, _}, _, St) ->
    case fabric_util:remove_down_workers(St#stream_acc.workers, NodeRef) of
    {ok, Workers} ->
        {ok, St#stream_acc{workers=Workers}};
    error ->
        Reason = {nodedown, <<"progress not possible">>},
        {error, Reason}
    end;
handle_stream_start({rexi_EXIT, Reason}, Worker, St) ->
    Workers = fabric_dict:erase(Worker, St#stream_acc.workers),
    case {fabric_view:is_progress_possible(Workers), Reason} of
    {true, _} ->
        {ok, St#stream_acc{workers=Workers}};
    {false, {maintenance_mode, _Node}} ->
        % Check if we have replacements for this range
        % and start the new workers if so.
        case lists:keytake(Worker#shard.range, 1, St#stream_acc.replacements) of
            {value, {_Range, WorkerReplacements}, NewReplacements} ->
                FinalWorkers = lists:foldl(fun(Repl, NewWorkers) ->
                    NewWorker = (St#stream_acc.start_fun)(Repl),
                    fabric_dict:store(NewWorker, waiting, NewWorkers)
                end, Workers, WorkerReplacements),
                % Assert that our replaced worker provides us
                % the oppurtunity to make progress.
                true = fabric_view:is_progress_possible(FinalWorkers),
                NewRefs = fabric_dict:fetch_keys(FinalWorkers),
                {new_refs, NewRefs, St#stream_acc{
                    workers=FinalWorkers,
                    replacements=NewReplacements
                }};
            false ->
                % If we progress isn't possible and we don't have any
                % replacements then we're dead in the water.
                Error = {nodedown, <<"progress not possible">>},
                {error, Error}
        end;
    {false, _} ->
        {error, fabric_util:error_info(Reason)}
    end;
handle_stream_start(rexi_STREAM_INIT, {Worker, From}, St) ->
    case fabric_dict:lookup_element(Worker, St#stream_acc.workers) of
    undefined ->
        % This worker lost the race with other partition copies, terminate
        rexi:stream_cancel(From),
        {ok, St};
    waiting ->
        % Don't ack the worker yet so they don't start sending us
        % rows until we're ready
        Workers0 = fabric_dict:store(Worker, From, St#stream_acc.workers),
        Workers1 = fabric_view:remove_overlapping_shards(Worker, Workers0),
        case fabric_dict:any(waiting, Workers1) of
            true ->
                {ok, St#stream_acc{workers=Workers1}};
            false ->
                {stop, St#stream_acc{workers=Workers1}}
        end
    end;
handle_stream_start(Else, _, _) ->
    exit({invalid_stream_start, Else}).

recv(Workers, Keypos, Fun, Acc0) ->
    rexi_utils:recv(Workers, Keypos, Fun, Acc0, request_timeout(), infinity).

request_timeout() ->
    timeout("request", "60000").

all_docs_timeout() ->
    timeout("all_docs", "10000").

attachments_timeout() ->
    timeout("attachments", "600000").

timeout(Type, Default) ->
    case config:get("fabric", Type ++ "_timeout", Default) of
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
    Factor = list_to_integer(config:get("fabric", "shard_timeout_factor", "2")),
    get_shard(Live, Options, 100, Factor).

get_shard([], _Opts, _Timeout, _Factor) ->
    erlang:error({internal_server_error, "No DB shards could be opened."});
get_shard([#shard{node = Node, name = Name} | Rest], Opts, Timeout, Factor) ->
    case rpc:call(Node, couch_db, open, [Name, [{timeout, Timeout} | Opts]]) of
    {ok, Db} ->
        {ok, Db};
    {unauthorized, _} = Error ->
        throw(Error);
    {badrpc, {'EXIT', {timeout, _}}} ->
        get_shard(Rest, Opts, Factor * Timeout, Factor);
    _Else ->
        get_shard(Rest, Opts, Timeout, Factor)
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
    MonRefs = lists:usort([
        rexi_utils:server_pid(N) || #shard{node=N} <- Shards
    ]),
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

doc_id_and_rev(#doc{id=DocId, revs={RevNum, [RevHash|_]}}) ->
    {DocId, {RevNum, RevHash}}.
