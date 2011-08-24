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

-module(fabric_view_changes).

-export([go/5, start_update_notifier/1, pack_seqs/1]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

go(DbName, Feed, Options, Callback, Acc0) when Feed == "continuous" orelse
        Feed == "longpoll" ->
    Args = make_changes_args(Options),
    Since = get_start_seq(DbName, Args),
    case validate_start_seq(DbName, Since) of
    ok ->
        {ok, Acc} = Callback(start, Acc0),
        Notifiers = start_update_notifiers(DbName),
        {Timeout, _} = couch_changes:get_changes_timeout(Args, Callback),
        try
            keep_sending_changes(
                DbName,
                Args,
                Callback,
                Since,
                Acc,
                Timeout
            )
        after
            stop_update_notifiers(Notifiers),
            couch_changes:get_rest_db_updated()
        end;
    Error ->
        Callback(Error, Acc0)
    end;

go(DbName, "normal", Options, Callback, Acc0) ->
    Args = make_changes_args(Options),
    Since = get_start_seq(DbName, Args),
    case validate_start_seq(DbName, Since) of
    ok ->
        {ok, Acc} = Callback(start, Acc0),
        {ok, #collector{counters=Seqs, user_acc=AccOut}} = send_changes(
            DbName,
            Args,
            Callback,
            Since,
            Acc,
            5000
        ),
        Callback({stop, pack_seqs(Seqs)}, AccOut);
    Error ->
        Callback(Error, Acc0)
    end.

keep_sending_changes(DbName, Args, Callback, Seqs, AccIn, Timeout) ->
    #changes_args{limit=Limit, feed=Feed, heartbeat=Heartbeat} = Args,
    {ok, Collector} = send_changes(DbName, Args, Callback, Seqs, AccIn, Timeout),
    #collector{limit=Limit2, counters=NewSeqs, user_acc=AccOut} = Collector,
    LastSeq = pack_seqs(NewSeqs),
    if Limit > Limit2, Feed == "longpoll" ->
        Callback({stop, LastSeq}, AccOut);
    true ->
        case wait_db_updated(Timeout) of
        updated ->
            keep_sending_changes(
                DbName,
                Args#changes_args{limit=Limit2},
                Callback,
                LastSeq,
                AccOut,
                Timeout
            );
        timeout ->
            case Heartbeat of undefined ->
                Callback({stop, LastSeq}, AccOut);
            _ ->
                {ok, AccTimeout} = Callback(timeout, AccOut),
                keep_sending_changes(DbName, Args#changes_args{limit=Limit2},
                    Callback, LastSeq, AccTimeout, Timeout)
            end
        end
    end.

send_changes(DbName, ChangesArgs, Callback, PackedSeqs, AccIn, Timeout) ->
    AllShards = mem3:shards(DbName),
    Seqs = lists:flatmap(fun({#shard{name=Name, node=N} = Shard, Seq}) ->
        case lists:member(Shard, AllShards) of
        true ->
            Ref = rexi:cast(N, {fabric_rpc, changes, [Name,ChangesArgs,Seq]}),
            [{Shard#shard{ref = Ref}, Seq}];
        false ->
            % Find some replacement shards to cover the missing range
            % TODO It's possible in rare cases of shard merging to end up
            % with overlapping shard ranges from this technique
            lists:map(fun(#shard{name=Name2, node=N2} = NewShard) ->
                Ref = rexi:cast(N2, {fabric_rpc, changes, [Name2,ChangesArgs,0]}),
                {NewShard#shard{ref = Ref}, 0}
            end, find_replacement_shards(Shard, AllShards))
        end
    end, unpack_seqs(PackedSeqs, DbName)),
    {Workers, _} = lists:unzip(Seqs),
    State = #collector{
        query_args = ChangesArgs,
        callback = Callback,
        counters = orddict:from_list(Seqs),
        user_acc = AccIn,
        limit = ChangesArgs#changes_args.limit,
        rows = Seqs % store sequence positions instead
    },
    %% TODO: errors need to be handled here
    try
        receive_results(Workers, State, Timeout, Callback, AccIn)
    after
        fabric_util:cleanup(Workers)
    end.

receive_results(Workers, State, Timeout, Callback, AccIn) ->
    case rexi_utils:recv(Workers, #shard.ref, fun handle_message/3, State,
            infinity, Timeout) of
    {timeout, _NewState} ->
        {ok, AccOut} = Callback(timeout, AccIn),
        NewState = State#collector{user_acc = AccOut},
        receive_results(Workers, NewState, Timeout, Callback, AccOut);
    {_, NewState} ->
        {ok, NewState}
    end.

handle_message({rexi_DOWN, _, _, _}, nil, State) ->
    % TODO see if progress can be made here, possibly by removing all shards
    % from that node and checking is_progress_possible
    {ok, State};

handle_message({rexi_EXIT, Reason}, Worker, State) ->
    #collector{
        callback=Callback,
        counters=Counters0,
        rows = Seqs0,
        user_acc=Acc
    } = State,
    Counters = fabric_dict:erase(Worker, Counters0),
    Seqs = fabric_dict:erase(Worker, Seqs0),
    case fabric_view:is_progress_possible(Counters) of
    true ->
        {ok, State#collector{counters = Counters, rows=Seqs}};
    false ->
        {ok, Resp} = Callback({error, fabric_util:error_info(Reason)}, Acc),
        {error, Resp}
    end;

handle_message(_, _, #collector{limit=0} = State) ->
    {stop, State};

handle_message(#change{key=Seq} = Row0, {Worker, From}, St) ->
    #collector{
        query_args = #changes_args{include_docs=IncludeDocs},
        callback = Callback,
        counters = S0,
        limit = Limit,
        user_acc = AccIn
    } = St,
    case fabric_dict:lookup_element(Worker, S0) of
    undefined ->
        % this worker lost the race with other partition copies, terminate it
        gen_server:reply(From, stop),
        {ok, St};
    _ ->
        S1 = fabric_dict:store(Worker, Seq, S0),
        S2 = fabric_view:remove_overlapping_shards(Worker, S1),
        Row = Row0#change{key = pack_seqs(S2)},
        {Go, Acc} = Callback(changes_row(Row, IncludeDocs), AccIn),
        gen_server:reply(From, Go),
        {Go, St#collector{counters=S2, limit=Limit-1, user_acc=Acc}}
    end;

handle_message({complete, EndSeq}, Worker, State) ->
    #collector{
        counters = S0,
        total_rows = Completed % override
    } = State,
    case fabric_dict:lookup_element(Worker, S0) of
    undefined ->
        {ok, State};
    _ ->
        S1 = fabric_dict:store(Worker, EndSeq, S0),
        % unlikely to have overlaps here, but possible w/ filters
        S2 = fabric_view:remove_overlapping_shards(Worker, S1),
        NewState = State#collector{counters=S2, total_rows=Completed+1},
        case fabric_dict:size(S2) =:= (Completed+1) of
        true ->
            {stop, NewState};
        false ->
            {ok, NewState}
        end
    end.

make_changes_args(#changes_args{style=Style, filter=undefined}=Args) ->
    Args#changes_args{filter = Style};
make_changes_args(Args) ->
    Args.

get_start_seq(_DbName, #changes_args{dir=fwd, since=Since}) ->
    Since;
get_start_seq(DbName, #changes_args{dir=rev}) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_update_seq, []),
    {ok, Since} = fabric_util:recv(Workers, #shard.ref,
        fun collect_update_seqs/3, fabric_dict:init(Workers, -1)),
    Since.

collect_update_seqs(Seq, Shard, Counters) when is_integer(Seq) ->
    case fabric_dict:lookup_element(Shard, Counters) of
    undefined ->
        % already heard from someone else in this range
        {ok, Counters};
    -1 ->
        C1 = fabric_dict:store(Shard, Seq, Counters),
        C2 = fabric_view:remove_overlapping_shards(Shard, C1),
        case fabric_dict:any(-1, C2) of
        true ->
            {ok, C2};
        false ->
            {stop, pack_seqs(C2)}
        end
    end.

pack_seqs(Workers) ->
    SeqList = [{N,R,S} || {#shard{node=N, range=R}, S} <- Workers],
    SeqSum = lists:sum(element(2, lists:unzip(Workers))),
    Opaque = couch_util:encodeBase64Url(term_to_binary(SeqList, [compressed])),
    [SeqSum, Opaque].

unpack_seqs(0, DbName) ->
    fabric_dict:init(mem3:shards(DbName), 0);

unpack_seqs("0", DbName) ->
    fabric_dict:init(mem3:shards(DbName), 0);

unpack_seqs(Packed, DbName) ->
    {match, [Opaque]} = re:run(Packed, "(?<opaque>[a-zA-Z0-9-_]+)([\"\\]])*$",
        [{capture, [opaque], binary}]),
    % TODO relies on internal structure of fabric_dict as keylist
    lists:map(fun({Node, [A,B], Seq}) ->
        Match = #shard{node=Node, range=[A,B], dbname=DbName, _ = '_'},
        case ets:match_object(partitions, Match) of
        [Shard] ->
            {Shard, Seq};
        [] ->
            {Match, Seq} % will be replaced in find_replacement_shards
        end
    end, binary_to_term(couch_util:decodeBase64Url(Opaque))).

start_update_notifiers(DbName) ->
    lists:map(fun(#shard{node=Node, name=Name}) ->
        {Node, rexi:cast(Node, {?MODULE, start_update_notifier, [Name]})}
    end, mem3:shards(DbName)).

% rexi endpoint
start_update_notifier(DbName) ->
    {Caller, _} = get(rexi_from),
    Fun = fun({_, X}) when X == DbName -> Caller ! db_updated; (_) -> ok end,
    Id = {couch_db_update_notifier, make_ref()},
    ok = gen_event:add_sup_handler(couch_db_update, Id, Fun),
    receive {gen_event_EXIT, Id, Reason} ->
        rexi:reply({gen_event_EXIT, DbName, Reason})
    end.

stop_update_notifiers(Notifiers) ->
    [rexi:kill(Node, Ref) || {Node, Ref} <- Notifiers].

changes_row(#change{key=Seq, id=Id, value=Value, deleted=true, doc=Doc}, true) ->
    {change, {[{seq,Seq}, {id,Id}, {changes,Value}, {deleted, true}, {doc, Doc}]}};
changes_row(#change{key=Seq, id=Id, value=Value, deleted=true}, false) ->
    {change, {[{seq,Seq}, {id,Id}, {changes,Value}, {deleted, true}]}};
changes_row(#change{key=Seq, id=Id, value=Value, doc={error,Reason}}, true) ->
    {change, {[{seq,Seq}, {id,Id}, {changes,Value}, {error,Reason}]}};
changes_row(#change{key=Seq, id=Id, value=Value, doc=Doc}, true) ->
    {change, {[{seq,Seq}, {id,Id}, {changes,Value}, {doc,Doc}]}};
changes_row(#change{key=Seq, id=Id, value=Value}, false) ->
    {change, {[{seq,Seq}, {id,Id}, {changes,Value}]}}.

find_replacement_shards(#shard{range=Range}, AllShards) ->
    % TODO make this moar betta -- we might have split or merged the partition
    [Shard || Shard <- AllShards, Shard#shard.range =:= Range].

wait_db_updated(Timeout) ->
    receive db_updated -> couch_changes:get_rest_db_updated()
    after Timeout -> timeout end.

validate_start_seq(DbName, Seq) ->
    try unpack_seqs(Seq, DbName) of _Any ->
        ok
    catch _:_ ->
        Reason = <<"Malformed sequence supplied in 'since' parameter.">>,
        {error, {bad_request, Reason}}
    end.
