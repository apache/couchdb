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

go(DbName, Id, Options) ->
    Workers = fabric_util:submit_jobs(mem3:shards(DbName,Id), open_doc,
        [Id, [deleted|Options]]),
    SuppressDeletedDoc = not lists:member(deleted, Options),
    N = mem3:n(DbName),
    R = couch_util:get_value(r, Options, integer_to_list(mem3:quorum(DbName))),
    RepairOpts = [{r, integer_to_list(N)} | Options],
    Acc0 = {Workers, erlang:min(N, list_to_integer(R)), []},
    RexiMon = fabric_util:create_monitors(Workers),
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, Reply} ->
        format_reply(Reply, SuppressDeletedDoc);
    {error, needs_repair, Reply} ->
        spawn(fabric, open_revs, [DbName, Id, all, RepairOpts]),
        format_reply(Reply, SuppressDeletedDoc);
    {error, needs_repair} ->
        % we couldn't determine the correct reply, so we'll run a sync repair
        {ok, Results} = fabric:open_revs(DbName, Id, all, RepairOpts),
        case lists:partition(fun({ok, #doc{deleted=Del}}) -> Del end, Results) of
        {[], []} ->
            {not_found, missing};
        {_DeletedDocs, []} when SuppressDeletedDoc ->
            {not_found, deleted};
        {DeletedDocs, []} ->
            lists:last(lists:sort(DeletedDocs));
        {_, LiveDocs} ->
            lists:last(lists:sort(LiveDocs))
        end;
    Error ->
        Error
    after
        rexi_monitor:stop(RexiMon)
    end.

format_reply({ok, #doc{deleted=true}}, true) ->
    {not_found, deleted};
format_reply(Else, _) ->
    Else.

handle_message({rexi_DOWN, _, {_,NodeRef},_}, _Worker, {Workers, R, Replies}) ->
    NewWorkers = lists:keydelete(NodeRef, #shard.node, Workers),
    case NewWorkers of
    [] ->
        {error, needs_repair};
    _ ->
        {ok, {NewWorkers, R, Replies}}
    end;
handle_message({rexi_EXIT, _Reason}, Worker, Acc0) ->
    skip_message(Worker, Acc0);
handle_message(Reply, Worker, {Workers, R, Replies}) ->
    NewReplies = fabric_util:update_counter(Reply, 1, Replies),
    case lists:dropwhile(fun({_,{_, Count}}) -> Count < R end, NewReplies) of
    [{_,{QuorumReply, _}} | _] ->
        fabric_util:cleanup(lists:delete(Worker,Workers)),
        case {NewReplies, fabric_util:remove_ancestors(NewReplies, [])} of
        {[_], [_]} ->
            % complete agreement amongst all copies
            {stop, QuorumReply};
        {[_|_], [_]} ->
            % any divergent replies are ancestors of the QuorumReply
            {error, needs_repair, QuorumReply};
        _Else ->
            % real disagreement amongst the workers, block for the repair
            {error, needs_repair}
        end;
    [] ->
        if length(Workers) =:= 1 ->
            {error, needs_repair};
        true ->
            {ok, {lists:delete(Worker,Workers), R, NewReplies}}
        end
    end.

skip_message(_Worker, {Workers, _R, _Replies}) when length(Workers) =:= 1 ->
    {error, needs_repair};
skip_message(Worker, {Workers, R, Replies}) ->
    {ok, {lists:delete(Worker,Workers), R, Replies}}.


open_doc_test() ->
    Foo1 = {ok, #doc{revs = {1,[<<"foo">>]}}},
    Foo2 = {ok, #doc{revs = {2,[<<"foo2">>,<<"foo">>]}}},
    Bar1 = {ok, #doc{revs = {1,[<<"bar">>]}}},
    Baz1 = {ok, #doc{revs = {1,[<<"baz">>]}}},
    NF = {not_found, missing},
    State0 = {[nil, nil, nil], 2, []},
    State1 = {[nil, nil], 2, [fabric_util:kv(Foo1,1)]},
    State2 = {[nil], 2, [fabric_util:kv(Bar1,1), fabric_util:kv(Foo1,1)]},
    State3 = {[nil], 2, [fabric_util:kv(Foo1,1), fabric_util:kv(Foo2,1)]},
    ?assertEqual({ok, State1}, handle_message(Foo1, nil, State0)),

    % normal case - quorum reached, no disagreement
    ?assertEqual({stop, Foo1}, handle_message(Foo1, nil, State1)),

    % 2nd worker disagrees, voting continues
    ?assertEqual({ok, State2}, handle_message(Bar1, nil, State1)),

    % 3rd worker resolves voting, but repair is needed
    ?assertEqual({error, needs_repair}, handle_message(Foo1, nil, State2)),

    % 2nd worker comes up with descendant of Foo1, voting continues
    ?assertEqual({ok, State3}, handle_message(Foo2, nil, State1)),

    % 3rd worker is also a descendant so run repair async
    ?assertEqual({error, needs_repair, Foo2}, handle_message(Foo2, nil,
        State3)),

    % We only run async repair when every revision is part of the same branch
    ?assertEqual({error, needs_repair}, handle_message(Bar1, nil, State3)),

    % not_found is considered to be an ancestor of everybody
    {ok, State4} = handle_message(NF, nil, State1),
    ?assertEqual({error, needs_repair, Foo1}, handle_message(Foo1, nil,
        State4)),

    % 3 distinct edit branches result in quorum failure
    ?assertEqual({error, needs_repair}, handle_message(Baz1, nil, State2)).
