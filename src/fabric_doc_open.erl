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

-module(fabric_doc_open).

-export([go/3]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").


-record(acc, {
    dbname,
    workers,
    r,
    state,
    replies,
    q_reply
}).


go(DbName, Id, Options) ->
    Workers = fabric_util:submit_jobs(mem3:shards(DbName,Id), open_doc,
        [Id, [deleted|Options]]),
    SuppressDeletedDoc = not lists:member(deleted, Options),
    N = mem3:n(DbName),
    R = couch_util:get_value(r, Options, integer_to_list(mem3:quorum(DbName))),
    Acc0 = #acc{
        dbname = DbName,
        workers = Workers,
        r = erlang:min(N, list_to_integer(R)),
        state = r_not_met,
        replies = []
    },
    RexiMon = fabric_util:create_monitors(Workers),
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, #acc{}=Acc} ->
        Reply = handle_response(Acc),
        format_reply(Reply, SuppressDeletedDoc);
    Error ->
        Error
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_, Node}, _}, _Worker, Acc) ->
    NewWorkers = [W || #shard{node=N}=W <- Acc#acc.workers, N /= Node],
    case NewWorkers of
    [] ->
        {stop, Acc#acc{workers=[]}};
    _ ->
        {ok, Acc#acc{workers=NewWorkers}}
    end;
handle_message({rexi_EXIT, _Reason}, Worker, Acc) ->
    NewWorkers = lists:delete(Worker, Acc#acc.workers),
    case NewWorkers of
    [] ->
        {stop, Acc#acc{workers=[]}};
    _ ->
        {ok, Acc#acc{workers=NewWorkers}}
    end;
handle_message(Reply, Worker, Acc) ->
    NewReplies = fabric_util:update_counter(Reply, 1, Acc#acc.replies),
    NewAcc = Acc#acc{replies = NewReplies},
    case is_r_met(Acc#acc.workers, NewReplies, Acc#acc.r) of
    {true, QuorumReply} ->
        fabric_util:cleanup(lists:delete(Worker, Acc#acc.workers)),
        {stop, NewAcc#acc{workers=[], state=r_met, q_reply=QuorumReply}};
    wait_for_more ->
        NewWorkers = lists:delete(Worker, Acc#acc.workers),
        {ok, NewAcc#acc{workers=NewWorkers}};
    no_more_workers ->
        {stop, NewAcc#acc{workers=[]}}
    end.

handle_response(#acc{state=r_met, replies=Replies, q_reply=QuorumReply}=Acc) ->
    case {Replies, fabric_util:remove_ancestors(Replies, [])} of
        {[_], [_]} ->
            % Complete agreement amongst all copies
            QuorumReply;
        {[_|_], [{_, {QuorumReply, _}}]} ->
            % Any divergent replies are ancestors of the QuorumReply,
            % repair the document asynchronously
            spawn(fun() -> read_repair(Acc) end),
            QuorumReply;
        _Else ->
            % real disagreement amongst the workers, block for the repair
            read_repair(Acc)
    end;
handle_response(Acc) ->
    read_repair(Acc).

is_r_met(Workers, Replies, R) ->
    case lists:dropwhile(fun({_,{_, Count}}) -> Count < R end, Replies) of
    [{_,{QuorumReply, _}} | _] ->
        {true, QuorumReply};
    [] when length(Workers) > 1 ->
        wait_for_more;
    [] ->
        no_more_workers
    end.

read_repair(#acc{dbname=DbName, replies=Replies}) ->
    Docs = [Doc || {_, {{ok, #doc{}=Doc}, _}} <- Replies],
    case Docs of
    % omit local docs from read repair
    [#doc{id = <<?LOCAL_DOC_PREFIX, _/binary>>} | _] ->
        choose_reply(Docs);
    [#doc{id=Id} | _] ->
        Ctx = #user_ctx{roles=[<<"_admin">>]},
        Opts = [replicated_changes, {user_ctx, Ctx}],
        Res = fabric:update_docs(DbName, Docs, Opts),
        twig:log(notice, "read_repair ~s ~s ~p", [DbName, Id, Res]),
        choose_reply(Docs);
    [] ->
        % Try hard to return some sort of information
        % to the client.
        Values = [V || {_, {V, _}} <- Replies],
        case lists:member({not_found, missing}, Values) of
            true ->
                {not_found, missing};
            false when length(Values) > 0 ->
                % Sort for stability in responses in
                % case we have some weird condition
                hd(lists:sort(Values));
            false ->
                {error, read_failure}
        end
    end.

choose_reply(Docs) ->
    % Sort descending by {not deleted, rev}. This should match
    % the logic of couch_doc:to_doc_info/1.
    [Winner | _] = lists:sort(fun(DocA, DocB) ->
        InfoA = {not DocA#doc.deleted, DocA#doc.revs},
        InfoB = {not DocB#doc.deleted, DocB#doc.revs},
        InfoA > InfoB
    end, Docs),
    {ok, Winner}.

format_reply({ok, #doc{deleted=true}}, true) ->
    {not_found, deleted};
format_reply(Else, _) ->
    Else.


is_r_met_test() ->
    Workers0 = [],
    Workers1 = [nil],
    Workers2 = [nil,nil],

    % Successful cases

    ?assertEqual(
        {true, foo},
        is_r_met([], [fabric_util:kv(foo,2)], 2)
    ),

    ?assertEqual(
        {true, foo},
        is_r_met([], [fabric_util:kv(foo,3)], 2)
    ),

    ?assertEqual(
        {true, foo},
        is_r_met([], [fabric_util:kv(foo,1)], 1)
    ),

    ?assertEqual(
        {true, foo},
        is_r_met([], [fabric_util:kv(foo,2), fabric_util:kv(bar,1)], 2)
    ),

    ?assertEqual(
        {true, bar},
        is_r_met([], [fabric_util:kv(bar,1), fabric_util:kv(bar,2)], 2)
    ),

    ?assertEqual(
        {true, bar},
        is_r_met([], [fabric_util:kv(bar,2), fabric_util:kv(foo,1)], 2)
    ),

    % Not met, but wait for more messages

    ?assertEqual(
        wait_for_more,
        is_r_met(Workers2, [fabric_util:kv(foo,1)], 2)
    ),

    ?assertEqual(
        wait_for_more,
        is_r_met(Workers2, [fabric_util:kv(foo,2)], 3)
    ),

    ?assertEqual(
        wait_for_more,
        is_r_met(Workers2, [fabric_util:kv(foo,1), fabric_util:kv(bar,1)], 2)
    ),

    % Not met, bail out

    ?assertEqual(
        no_more_workers,
        is_r_met(Workers0, [fabric_util:kv(foo,1)], 2)
    ),

    ?assertEqual(
        no_more_workers,
        is_r_met(Workers1, [fabric_util:kv(foo,1)], 2)
    ),

    ?assertEqual(
        no_more_workers,
        is_r_met(Workers1, [fabric_util:kv(foo,1), fabric_util:kv(bar,1)], 2)
    ),

    ?assertEqual(
        no_more_workers,
        is_r_met(Workers1, [fabric_util:kv(foo,2)], 3)
    ),

    ok.

handle_message_down_test() ->
    Node0 = 'foo@localhost',
    Node1 = 'bar@localhost',
    Down0 = {rexi_DOWN, nil, {nil, Node0}, nil},
    Down1 = {rexi_DOWN, nil, {nil, Node1}, nil},
    Workers0 = [#shard{node=Node0} || _ <- [a, b]],
    Worker1 = #shard{node=Node1},
    Workers1 = Workers0 ++ [Worker1],

    % Stop when no more workers are left
    ?assertEqual(
        {stop, #acc{workers=[]}},
        handle_message(Down0, nil, #acc{workers=Workers0})
    ),

    % Continue when we have more workers
    ?assertEqual(
        {ok, #acc{workers=[Worker1]}},
        handle_message(Down0, nil, #acc{workers=Workers1})
    ),

    % A second DOWN removes the remaining workers
    ?assertEqual(
        {stop, #acc{workers=[]}},
        handle_message(Down1, nil, #acc{workers=[Worker1]})
    ),

    ok.

handle_message_exit_test() ->
    Exit = {rexi_EXIT, nil},
    Worker0 = #shard{ref=erlang:make_ref()},
    Worker1 = #shard{ref=erlang:make_ref()},

    % Only removes the specified worker
    ?assertEqual(
        {ok, #acc{workers=[Worker1]}},
        handle_message(Exit, Worker0, #acc{workers=[Worker0, Worker1]})
    ),

    ?assertEqual(
        {ok, #acc{workers=[Worker0]}},
        handle_message(Exit, Worker1, #acc{workers=[Worker0, Worker1]})
    ),

    % We bail if it was the last worker
    ?assertEqual(
        {stop, #acc{workers=[]}},
        handle_message(Exit, Worker0, #acc{workers=[Worker0]})
    ),

    ok.

handle_message_reply_test() ->
    start_meck_(),
    meck:expect(rexi, kill, fun(_, _) -> ok end),

    Worker0 = #shard{ref=erlang:make_ref()},
    Worker1 = #shard{ref=erlang:make_ref()},
    Worker2 = #shard{ref=erlang:make_ref()},
    Workers = [Worker0, Worker1, Worker2],
    Acc0 = #acc{workers=Workers, r=2, replies=[]},

    % Test that we continue when we haven't met R yet
    ?assertEqual(
        {ok, Acc0#acc{
            workers=[Worker0, Worker1],
            replies=[fabric_util:kv(foo,1)]
        }},
        handle_message(foo, Worker2, Acc0)
    ),

    ?assertEqual(
        {ok, Acc0#acc{
            workers=[Worker0, Worker1],
            replies=[fabric_util:kv(bar,1), fabric_util:kv(foo,1)]
        }},
        handle_message(bar, Worker2, Acc0#acc{
            replies=[fabric_util:kv(foo,1)]
        })
    ),

    % Test that we don't get a quorum when R isn't met. q_reply
    % isn't set and state remains unchanged and {stop, NewAcc}
    % is returned. Bit subtle on the assertions here.

    ?assertEqual(
        {stop, Acc0#acc{workers=[],replies=[fabric_util:kv(foo,1)]}},
        handle_message(foo, Worker0, Acc0#acc{workers=[Worker0]})
    ),

    ?assertEqual(
        {stop, Acc0#acc{
            workers=[],
            replies=[fabric_util:kv(bar,1), fabric_util:kv(foo,1)]
        }},
        handle_message(bar, Worker0, Acc0#acc{
            workers=[Worker0],
            replies=[fabric_util:kv(foo,1)]
        })
    ),

    % Check that when R is met we stop with a new state and
    % a q_reply.

    ?assertEqual(
        {stop, Acc0#acc{
            workers=[],
            replies=[fabric_util:kv(foo,2)],
            state=r_met,
            q_reply=foo
        }},
        handle_message(foo, Worker1, Acc0#acc{
            workers=[Worker0, Worker1],
            replies=[fabric_util:kv(foo,1)]
        })
    ),

    ?assertEqual(
        {stop, Acc0#acc{
            workers=[],
            r=1,
            replies=[fabric_util:kv(foo,1)],
            state=r_met,
            q_reply=foo
        }},
        handle_message(foo, Worker0, Acc0#acc{r=1})
    ),

    ?assertEqual(
        {stop, Acc0#acc{
            workers=[],
            replies=[fabric_util:kv(bar,1), fabric_util:kv(foo,2)],
            state=r_met,
            q_reply=foo
        }},
        handle_message(foo, Worker0, Acc0#acc{
            workers=[Worker0],
            replies=[fabric_util:kv(bar,1), fabric_util:kv(foo,1)]
        })
    ),

    stop_meck_(),
    ok.

read_repair_test() ->
    start_meck_(),
    meck:expect(twig, log, fun(_, _, _) -> ok end),

    Foo1 = {ok, #doc{revs = {1,[<<"foo">>]}}},
    Foo2 = {ok, #doc{revs = {2,[<<"foo2">>,<<"foo">>]}}},
    NFM = {not_found, missing},

    % Test when we have actual doc data to repair

    meck:expect(fabric, update_docs, fun(_, [_], _) -> {ok, []} end),
    Acc0 = #acc{
        dbname = <<"name">>,
        replies = [fabric_util:kv(Foo1,1)]
    },
    ?assertEqual(Foo1, read_repair(Acc0)),

    meck:expect(fabric, update_docs, fun(_, [_, _], _) -> {ok, []} end),
    Acc1 = #acc{
        dbname = <<"name">>,
        replies = [fabric_util:kv(Foo1,1), fabric_util:kv(Foo2,1)]
    },
    ?assertEqual(Foo2, read_repair(Acc1)),

    % Test when we have nothing but errors

    Acc2 = #acc{replies=[fabric_util:kv(NFM, 1)]},
    ?assertEqual(NFM, read_repair(Acc2)),

    Acc3 = #acc{replies=[fabric_util:kv(NFM,1), fabric_util:kv(foo,2)]},
    ?assertEqual(NFM, read_repair(Acc3)),

    Acc4 = #acc{replies=[fabric_util:kv(foo,1), fabric_util:kv(bar,1)]},
    ?assertEqual(bar, read_repair(Acc4)),

    stop_meck_(),
    ok.

handle_response_quorum_met_test() ->
    start_meck_(),
    meck:expect(twig, log, fun(_, _, _) -> ok end),
    meck:expect(fabric, update_docs, fun(_, _, _) -> {ok, []} end),

    Foo1 = {ok, #doc{revs = {1,[<<"foo">>]}}},
    Foo2 = {ok, #doc{revs = {2,[<<"foo2">>,<<"foo">>]}}},
    Bar1 = {ok, #doc{revs = {1,[<<"bar">>]}}},

    BasicOkAcc = #acc{
        state=r_met,
        replies=[fabric_util:kv(Foo1,2)],
        q_reply=Foo1
    },
    ?assertEqual(Foo1, handle_response(BasicOkAcc)),

    WithAncestorsAcc = #acc{
        state=r_met,
        replies=[fabric_util:kv(Foo1,1), fabric_util:kv(Foo2,2)],
        q_reply=Foo2
    },
    ?assertEqual(Foo2, handle_response(WithAncestorsAcc)),

    % This also checks when the quorum isn't the most recent
    % revision.
    DeeperWinsAcc = #acc{
        state=r_met,
        replies=[fabric_util:kv(Foo1,2), fabric_util:kv(Foo2,1)],
        q_reply=Foo1
    },
    ?assertEqual(Foo2, handle_response(DeeperWinsAcc)),

    % Check that we return the proper doc based on rev
    % (ie, pos is equal)
    BiggerRevWinsAcc = #acc{
        state=r_met,
        replies=[fabric_util:kv(Foo1,1), fabric_util:kv(Bar1,2)],
        q_reply=Bar1
    },
    ?assertEqual(Foo1, handle_response(BiggerRevWinsAcc)),

    % r_not_met is a proxy to read_repair so we rely on
    % read_repair_test for those conditions.

    stop_meck_(),
    ok.


start_meck_() ->
    meck:new([twig, rexi, fabric]).

stop_meck_() ->
    meck:unload([twig, rexi, fabric]).
