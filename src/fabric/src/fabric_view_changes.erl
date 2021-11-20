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

-export([go/5, pack_seqs/1, unpack_seqs/2]).
-export([increment_changes_epoch/0]).

%% exported for upgrade purposes.
-export([keep_sending_changes/8]).

%% exported for testing and remsh debugging
-export([decode_seq/1]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(fabric_db_update_listener, [wait_db_updated/1]).

go(DbName, Feed, Options, Callback, Acc0) when
    Feed == "continuous" orelse
        Feed == "longpoll" orelse Feed == "eventsource"
->
    Args = make_changes_args(Options),
    Since = get_start_seq(DbName, Args),
    case validate_start_seq(DbName, Since) of
        ok ->
            {ok, Acc} = Callback(start, Acc0),
            {Timeout, _} = couch_changes:get_changes_timeout(Args, Callback),
            Ref = make_ref(),
            Parent = self(),
            UpdateListener = {
                spawn_link(
                    fabric_db_update_listener,
                    go,
                    [Parent, Ref, DbName, Timeout]
                ),
                Ref
            },
            put(changes_epoch, get_changes_epoch()),
            try
                keep_sending_changes(
                    DbName,
                    Args,
                    Callback,
                    Since,
                    Acc,
                    Timeout,
                    UpdateListener,
                    os:timestamp()
                )
            after
                fabric_db_update_listener:stop(UpdateListener)
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
            {ok, Collector} = send_changes(
                DbName,
                Args,
                Callback,
                Since,
                Acc,
                5000
            ),
            #collector{counters = Seqs, user_acc = AccOut, offset = Offset} = Collector,
            Callback({stop, pack_seqs(Seqs), pending_count(Offset)}, AccOut);
        Error ->
            Callback(Error, Acc0)
    end.

keep_sending_changes(DbName, Args, Callback, Seqs, AccIn, Timeout, UpListen, T0) ->
    #changes_args{limit = Limit, feed = Feed, heartbeat = Heartbeat} = Args,
    {ok, Collector} = send_changes(DbName, Args, Callback, Seqs, AccIn, Timeout),
    #collector{
        limit = Limit2,
        counters = NewSeqs,
        offset = Offset,
        user_acc = AccOut0
    } = Collector,
    LastSeq = pack_seqs(NewSeqs),
    MaintenanceMode = config:get("couchdb", "maintenance_mode"),
    NewEpoch = get_changes_epoch() > erlang:get(changes_epoch),
    if
        Limit > Limit2, Feed == "longpoll";
        MaintenanceMode == "true";
        MaintenanceMode == "nolb";
        NewEpoch ->
            Callback({stop, LastSeq, pending_count(Offset)}, AccOut0);
        true ->
            {ok, AccOut} = Callback(waiting_for_updates, AccOut0),
            WaitForUpdate = wait_db_updated(UpListen),
            AccumulatedTime = timer:now_diff(os:timestamp(), T0) div 1000,
            Max =
                case config:get("fabric", "changes_duration") of
                    undefined ->
                        infinity;
                    MaxStr ->
                        list_to_integer(MaxStr)
                end,
            case {Heartbeat, AccumulatedTime > Max, WaitForUpdate} of
                {_, _, changes_feed_died} ->
                    Callback({stop, LastSeq, pending_count(Offset)}, AccOut);
                {undefined, _, timeout} ->
                    Callback({stop, LastSeq, pending_count(Offset)}, AccOut);
                {_, true, timeout} ->
                    Callback({stop, LastSeq, pending_count(Offset)}, AccOut);
                _ ->
                    {ok, AccTimeout} = Callback(timeout, AccOut),
                    ?MODULE:keep_sending_changes(
                        DbName,
                        Args#changes_args{limit = Limit2},
                        Callback,
                        LastSeq,
                        AccTimeout,
                        Timeout,
                        UpListen,
                        T0
                    )
            end
    end.

send_changes(DbName, ChangesArgs, Callback, PackedSeqs, AccIn, Timeout) ->
    LiveNodes = [node() | nodes()],
    AllLiveShards = mem3:live_shards(DbName, LiveNodes),
    Seqs0 = unpack_seqs(PackedSeqs, DbName),
    {WSeqs0, Dead, Reps} = find_replacements(Seqs0, AllLiveShards),
    % Start workers which didn't need replacements
    WSeqs = lists:map(
        fun({#shard{name = Name, node = N} = S, Seq}) ->
            Ref = rexi:cast(N, {fabric_rpc, changes, [Name, ChangesArgs, Seq]}),
            {S#shard{ref = Ref}, Seq}
        end,
        WSeqs0
    ),
    % For some dead workers see if they are a result of split shards. In that
    % case make a replacement argument so that local rexi workers can calculate
    % (hopefully) a > 0 update sequence.
    {WSplitSeqs0, Reps1} = find_split_shard_replacements(Dead, Reps),
    WSplitSeqs = lists:map(
        fun({#shard{name = Name, node = N} = S, Seq}) ->
            Arg = make_replacement_arg(N, Seq),
            Ref = rexi:cast(N, {fabric_rpc, changes, [Name, ChangesArgs, Arg]}),
            {S#shard{ref = Ref}, Seq}
        end,
        WSplitSeqs0
    ),
    % For ranges that were not split, look for a replacement on a different node
    WReps = lists:map(
        fun(#shard{name = Name, node = NewNode, range = R} = S) ->
            Arg = find_replacement_sequence(Dead, R),
            case Arg =/= 0 of
                true -> ok;
                false -> couch_log:warning("~p reset seq for ~p", [?MODULE, S])
            end,
            Ref = rexi:cast(NewNode, {fabric_rpc, changes, [Name, ChangesArgs, Arg]}),
            {S#shard{ref = Ref}, 0}
        end,
        Reps1
    ),
    Seqs = WSeqs ++ WSplitSeqs ++ WReps,
    {Workers0, _} = lists:unzip(Seqs),
    Repls = fabric_ring:get_shard_replacements(DbName, Workers0),
    StartFun = fun(#shard{name = Name, node = N, range = R0} = Shard) ->
        SeqArg = find_replacement_sequence(Seqs, R0),
        case SeqArg =/= 0 of
            true -> ok;
            false -> couch_log:warning("~p StartFun reset seq for ~p", [?MODULE, Shard])
        end,
        Ref = rexi:cast(N, {fabric_rpc, changes, [Name, ChangesArgs, SeqArg]}),
        Shard#shard{ref = Ref}
    end,
    RexiMon = fabric_util:create_monitors(Workers0),
    try
        case fabric_streams:start(Workers0, #shard.ref, StartFun, Repls) of
            {ok, Workers} ->
                try
                    LiveSeqs = lists:map(
                        fun(W) ->
                            case lists:keyfind(W, 1, Seqs) of
                                {W, Seq} -> {W, Seq};
                                _ -> {W, 0}
                            end
                        end,
                        Workers
                    ),
                    send_changes(
                        DbName,
                        Workers,
                        LiveSeqs,
                        ChangesArgs,
                        Callback,
                        AccIn,
                        Timeout
                    )
                after
                    fabric_streams:cleanup(Workers)
                end;
            {timeout, NewState} ->
                DefunctWorkers = fabric_util:remove_done_workers(
                    NewState#stream_acc.workers,
                    waiting
                ),
                fabric_util:log_timeout(
                    DefunctWorkers,
                    "changes"
                ),
                throw({error, timeout});
            {error, Reason} ->
                throw({error, Reason});
            Else ->
                throw({error, Else})
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

send_changes(DbName, Workers, Seqs, ChangesArgs, Callback, AccIn, Timeout) ->
    State = #collector{
        db_name = DbName,
        query_args = ChangesArgs,
        callback = Callback,
        counters = orddict:from_list(Seqs),
        user_acc = AccIn,
        limit = ChangesArgs#changes_args.limit,
        offset = fabric_dict:init(Workers, null),
        % store sequence positions instead
        rows = Seqs
    },
    %% TODO: errors need to be handled here
    receive_results(Workers, State, Timeout, Callback).

receive_results(Workers, State, Timeout, Callback) ->
    case
        rexi_utils:recv(
            Workers,
            #shard.ref,
            fun handle_message/3,
            State,
            Timeout,
            infinity
        )
    of
        {timeout, NewState0} ->
            {ok, AccOut} = Callback(timeout, NewState0#collector.user_acc),
            NewState = NewState0#collector{user_acc = AccOut},
            receive_results(Workers, NewState, Timeout, Callback);
        {_, NewState} ->
            {ok, NewState}
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _, State) ->
    fabric_view:check_down_shards(State, NodeRef);
handle_message({rexi_EXIT, Reason}, Worker, State) ->
    fabric_view:handle_worker_exit(State, Worker, Reason);
% Temporary upgrade clause - Case 24236
handle_message({complete, Key}, Worker, State) when is_tuple(Key) ->
    handle_message({complete, [{seq, Key}, {pending, 0}]}, Worker, State);
handle_message({change, Props}, {Worker, _}, #collector{limit = 0} = State) ->
    O0 = State#collector.offset,
    O1 =
        case fabric_dict:lookup_element(Worker, O0) of
            null ->
                % Use Pending+1 because we're ignoring this row in the response
                Pending = couch_util:get_value(pending, Props, 0),
                fabric_dict:store(Worker, Pending + 1, O0);
            _ ->
                O0
        end,
    maybe_stop(State#collector{offset = O1});
handle_message({complete, Props}, Worker, #collector{limit = 0} = State) ->
    O0 = State#collector.offset,
    O1 =
        case fabric_dict:lookup_element(Worker, O0) of
            null ->
                fabric_dict:store(Worker, couch_util:get_value(pending, Props), O0);
            _ ->
                O0
        end,
    maybe_stop(State#collector{offset = O1});
handle_message({no_pass, Props}, {Worker, From}, #collector{limit = 0} = State) when
    is_list(Props)
->
    #collector{counters = S0, offset = O0} = State,
    O1 =
        case fabric_dict:lookup_element(Worker, O0) of
            null ->
                fabric_dict:store(Worker, couch_util:get_value(pending, Props), O0);
            _ ->
                O0
        end,
    S1 = fabric_dict:store(Worker, couch_util:get_value(seq, Props), S0),
    rexi:stream_ack(From),
    maybe_stop(State#collector{counters = S1, offset = O1});
handle_message(#change{} = Row, {Worker, From}, St) ->
    Change =
        {change, [
            {seq, Row#change.key},
            {id, Row#change.id},
            {changes, Row#change.value},
            {deleted, Row#change.deleted},
            {doc, Row#change.doc}
        ]},
    handle_message(Change, {Worker, From}, St);
handle_message({change, Props}, {Worker, From}, St) ->
    #collector{
        callback = Callback,
        counters = S0,
        offset = O0,
        limit = Limit,
        user_acc = AccIn
    } = St,
    true = fabric_dict:is_key(Worker, S0),
    S1 = fabric_dict:store(Worker, couch_util:get_value(seq, Props), S0),
    O1 = fabric_dict:store(Worker, couch_util:get_value(pending, Props), O0),
    % Temporary hack for FB 23637
    Interval = erlang:get(changes_seq_interval),
    if
        (Interval == undefined) orelse (Limit rem Interval == 0) ->
            Props2 = lists:keyreplace(seq, 1, Props, {seq, pack_seqs(S1)});
        true ->
            Props2 = lists:keyreplace(seq, 1, Props, {seq, null})
    end,
    {Go, Acc} = Callback(changes_row(Props2), AccIn),
    rexi:stream_ack(From),
    {Go, St#collector{counters = S1, offset = O1, limit = Limit - 1, user_acc = Acc}};
%% upgrade clause
handle_message({no_pass, Seq}, From, St) when is_integer(Seq) ->
    handle_message({no_pass, [{seq, Seq}]}, From, St);
handle_message({no_pass, Props}, {Worker, From}, St) ->
    Seq = couch_util:get_value(seq, Props),
    #collector{counters = S0} = St,
    true = fabric_dict:is_key(Worker, S0),
    S1 = fabric_dict:store(Worker, Seq, S0),
    rexi:stream_ack(From),
    {ok, St#collector{counters = S1}};
handle_message({complete, Props}, Worker, State) ->
    Key = couch_util:get_value(seq, Props),
    #collector{
        counters = S0,
        offset = O0,
        % override
        total_rows = Completed
    } = State,
    true = fabric_dict:is_key(Worker, S0),
    S1 = fabric_dict:store(Worker, Key, S0),
    O1 = fabric_dict:store(Worker, couch_util:get_value(pending, Props), O0),
    NewState = State#collector{counters = S1, offset = O1, total_rows = Completed + 1},
    % We're relying on S1 having exactly the numnber of workers that
    % are participtaing in this response. With the new stream_start
    % that's a bit more obvious but historically it wasn't quite
    % so clear. The Completed variable is just a hacky override
    % of the total_rows field in the #collector{} record.
    NumWorkers = fabric_dict:size(S1),
    Go =
        case NumWorkers =:= (Completed + 1) of
            true -> stop;
            false -> ok
        end,
    {Go, NewState}.

make_replacement_arg(Node, {Seq, Uuid}) ->
    {replace, Node, Uuid, Seq};
make_replacement_arg(_Node, {Seq, Uuid, EpochNode}) ->
    % The replacement should properly be computed aginst the node that owned
    % the sequence when it was written to disk (the EpochNode) rather than the
    % node we're trying to replace.
    {replace, EpochNode, Uuid, Seq};
make_replacement_arg(_, _) ->
    0.

maybe_stop(#collector{offset = Offset} = State) ->
    case fabric_dict:any(null, Offset) of
        false ->
            {stop, State};
        true ->
            % Wait till we've heard from everyone to compute pending count
            {ok, State}
    end.

make_changes_args(#changes_args{style = Style, filter_fun = undefined} = Args) ->
    Args#changes_args{filter_fun = {default, Style}};
make_changes_args(Args) ->
    Args.

get_start_seq(DbName, #changes_args{dir = Dir, since = Since}) when
    Dir == rev; Since == "now"
->
    {ok, Info} = fabric:get_db_info(DbName),
    couch_util:get_value(update_seq, Info);
get_start_seq(_DbName, #changes_args{dir = fwd, since = Since}) ->
    Since.

pending_count(Dict) ->
    fabric_dict:fold(
        fun
            (_Worker, Count, Acc) when is_integer(Count), is_integer(Acc) ->
                Count + Acc;
            (_Worker, _Count, _Acc) ->
                null
        end,
        0,
        Dict
    ).

pack_seqs(Workers) ->
    SeqList = [{N, R, S} || {#shard{node = N, range = R}, S} <- Workers],
    SeqSum = lists:sum([seq(S) || {_, _, S} <- SeqList]),
    Opaque = couch_util:encodeBase64Url(term_to_binary(SeqList, [compressed])),
    ?l2b([integer_to_list(SeqSum), $-, Opaque]).

seq({Seq, _Uuid, _Node}) -> Seq;
seq({Seq, _Uuid}) -> Seq;
seq(Seq) -> Seq.

unpack_seq_regex_match(Packed) ->
    NewPattern = "^\\[[0-9]+\s*,\s*\"(?<opaque>.*)\"\\]$",
    OldPattern = "^\"?([0-9]+-)?(?<opaque>.*?)\"?$",
    Options = [{capture, [opaque], binary}],
    case re:run(Packed, NewPattern, Options) of
        {match, Match} ->
            Match;
        nomatch ->
            {match, Match} = re:run(Packed, OldPattern, Options),
            Match
    end.

unpack_seq_decode_term(Opaque) ->
    binary_to_term(couch_util:decodeBase64Url(Opaque)).

% This is used for testing and for remsh debugging
%
% Return the unpacked list of sequences from a raw update seq string. The input
% string is expected to include the N- prefix. The result looks like:
%  [{Node, Range, {SeqNum, Uuid, EpochNode}}, ...]
%
-spec decode_seq(binary()) -> [tuple()].
decode_seq(Packed) ->
    Opaque = unpack_seq_regex_match(Packed),
    unpack_seq_decode_term(Opaque).

% Returns fabric_dict with {Shard, Seq} entries
%
-spec unpack_seqs(pos_integer() | list() | binary(), binary()) ->
    orddict:orddict().
unpack_seqs(0, DbName) ->
    fabric_dict:init(mem3:shards(DbName), 0);
unpack_seqs("0", DbName) ->
    fabric_dict:init(mem3:shards(DbName), 0);
% deprecated
unpack_seqs([_SeqNum, Opaque], DbName) ->
    do_unpack_seqs(Opaque, DbName);
unpack_seqs(Packed, DbName) ->
    Opaque = unpack_seq_regex_match(Packed),
    do_unpack_seqs(Opaque, DbName).

do_unpack_seqs(Opaque, DbName) ->
    % A preventative fix for FB 13533 to remove duplicate shards.
    % This just picks each unique shard and keeps the largest seq
    % value recorded.
    Decoded = unpack_seq_decode_term(Opaque),
    DedupDict = lists:foldl(
        fun({Node, [A, B], Seq}, Acc) ->
            dict:append({Node, [A, B]}, Seq, Acc)
        end,
        dict:new(),
        Decoded
    ),
    Deduped = lists:map(
        fun({{Node, [A, B]}, SeqList}) ->
            {Node, [A, B], lists:max(SeqList)}
        end,
        dict:to_list(DedupDict)
    ),

    % Create a fabric_dict of {Shard, Seq} entries
    % TODO relies on internal structure of fabric_dict as keylist
    Unpacked = lists:flatmap(
        fun({Node, [A, B], Seq}) ->
            case mem3:get_shard(DbName, Node, [A, B]) of
                {ok, Shard} ->
                    [{Shard, Seq}];
                {error, not_found} ->
                    []
            end
        end,
        Deduped
    ),

    % This just handles the case if the ring in the unpacked sequence
    % received is not complete and in that case tries to fill in the
    % missing ranges with shards from the shard map
    case fabric_ring:is_progress_possible(Unpacked) of
        true ->
            Unpacked;
        false ->
            Uuids = get_db_uuid_shards(DbName),
            PotentialWorkers = lists:map(
                fun({Node, [A, B], Seq}) ->
                    case mem3:get_shard(DbName, Node, [A, B]) of
                        {ok, Shard} ->
                            {Shard, Seq};
                        {error, not_found} ->
                            Shard = replace_moved_shard(Node, [A, B], Seq, Uuids),
                            {Shard, Seq}
                    end
                end,
                Deduped
            ),
            Shards = mem3:shards(DbName),
            {Unpacked1, Dead, Reps} = find_replacements(PotentialWorkers, Shards),
            {Splits, Reps1} = find_split_shard_replacements(Dead, Reps),
            RepSeqs = lists:map(
                fun(#shard{} = S) ->
                    {S, get_old_seq(S, Deduped)}
                end,
                Reps1
            ),
            Unpacked1 ++ Splits ++ RepSeqs
    end.

get_old_seq(#shard{range = R} = Shard, SinceSeqs) ->
    case lists:keyfind(R, 2, SinceSeqs) of
        {Node, R, Seq} when is_number(Seq) ->
            % Unfortunately we don't have access to the db
            % uuid so we can't set a replacememnt here.
            couch_log:warning(
                "~p get_old_seq missing uuid "
                "node: ~p, range: ~p, seq: ~p",
                [?MODULE, Node, R, Seq]
            ),
            0;
        {Node, R, {Seq, Uuid}} ->
            % This update seq is using the old format that
            % didn't include the node. This information is
            % important for replacement.
            {Seq, Uuid, Node};
        {_Node, R, {Seq, Uuid, EpochNode}} ->
            % This is the newest sequence format that we
            % can use for replacement.
            {Seq, Uuid, EpochNode};
        Error ->
            couch_log:warning(
                "~p get_old_seq error: ~p, shard: ~p, seqs: ~p",
                [?MODULE, Error, Shard, SinceSeqs]
            ),
            0
    end.

get_db_uuid_shards(DbName) ->
    % Need to use an isolated process as we are performing a fabric call from
    % another fabric call and there is a good chance we'd polute the mailbox
    % with returned messages
    Timeout = fabric_util:request_timeout(),
    IsolatedFun = fun() -> fabric:db_uuids(DbName) end,
    try fabric_util:isolate(IsolatedFun, Timeout) of
        {ok, Uuids} ->
            % Trim uuids so we match exactly based on the currently configured
            % uuid_prefix_len. The assumption is that we are getting an older
            % sequence from the same cluster and we didn't tweak that
            % relatively obscure config option in the meantime.
            PrefixLen = fabric_util:get_uuid_prefix_len(),
            maps:fold(
                fun(Uuid, Shard, Acc) ->
                    TrimmedUuid = binary:part(Uuid, {0, PrefixLen}),
                    Acc#{TrimmedUuid => Shard}
                end,
                #{},
                Uuids
            );
        {error, Error} ->
            % Since we are doing a best-effort approach to match moved shards,
            % tolerate and log errors. This should also handle cases when the
            % cluster is partially upgraded, as some nodes will not have the
            % newer get_uuid fabric_rpc handler.
            ErrMsg = "~p : could not get db_uuids for Db:~p Error:~p",
            couch_log:error(ErrMsg, [?MODULE, DbName, Error]),
            #{}
    catch
        _Tag:Error ->
            ErrMsg = "~p : could not get db_uuids for Db:~p Error:~p",
            couch_log:error(ErrMsg, [?MODULE, DbName, Error]),
            #{}
    end.

%% Determine if the missing shard moved to a new node. Do that by matching the
%% uuids from the current shard map. If we cannot find a moved shard, we return
%% the original node and range as a shard and hope for the best.
replace_moved_shard(Node, Range, Seq, #{} = _UuidShards) when is_number(Seq) ->
    % Cannot figure out shard moves without uuid matching
    #shard{node = Node, range = Range};
replace_moved_shard(Node, Range, {Seq, Uuid}, #{} = UuidShards) ->
    % Compatibility case for an old seq format which didn't have epoch nodes
    replace_moved_shard(Node, Range, {Seq, Uuid, Node}, UuidShards);
replace_moved_shard(Node, Range, {_Seq, Uuid, _EpochNode}, #{} = UuidShards) ->
    case UuidShards of
        #{Uuid := #shard{range = Range} = Shard} ->
            % Found a moved shard by matching both the uuid and the range
            Shard;
        #{} ->
            % Did not find a moved shard, use the original node
            #shard{node = Node, range = Range}
    end.

changes_row(Props0) ->
    Props1 =
        case couch_util:get_value(deleted, Props0) of
            true ->
                Props0;
            _ ->
                lists:keydelete(deleted, 1, Props0)
        end,
    Allowed = [seq, id, changes, deleted, doc, error],
    Props2 = lists:filter(fun({K, _V}) -> lists:member(K, Allowed) end, Props1),
    {change, {Props2}}.

find_replacements(Workers, AllShards) ->
    % Build map [B, E] => [Worker1, Worker2, ...] for all workers
    WrkMap = lists:foldl(
        fun({#shard{range = [B, E]}, _} = W, Acc) ->
            maps:update_with({B, E}, fun(Ws) -> [W | Ws] end, [W], Acc)
        end,
        #{},
        fabric_dict:to_list(Workers)
    ),

    % Build map [B, E] => [Shard1, Shard2, ...] for all shards
    AllMap = lists:foldl(
        fun(#shard{range = [B, E]} = S, Acc) ->
            maps:update_with({B, E}, fun(Ss) -> [S | Ss] end, [S], Acc)
        end,
        #{},
        AllShards
    ),

    % Custom sort function will prioritize workers over other shards.
    % The idea is to not unnecessarily kill workers if we don't have to
    SortFun = fun
        (R1 = {B, E1}, R2 = {B, E2}) ->
            case {maps:is_key(R1, WrkMap), maps:is_key(R2, WrkMap)} of
                {true, true} ->
                    % Both are workers, larger interval wins
                    E1 >= E2;
                {true, false} ->
                    % First element is a worker range, it wins
                    true;
                {false, true} ->
                    % Second element is a worker range, it wins
                    false;
                {false, false} ->
                    % Neither one is a worker interval, pick larger one
                    E1 >= E2
            end;
        ({B1, _}, {B2, _}) ->
            B1 =< B2
    end,
    Ring = mem3_util:get_ring(maps:keys(AllMap), SortFun),

    % Keep only workers in the ring  and from one of the available nodes
    Keep = fun(#shard{range = [B, E], node = N}) ->
        lists:member({B, E}, Ring) andalso
            lists:keyfind(
                N,
                #shard.node,
                maps:get({B, E}, AllMap)
            ) =/= false
    end,
    Workers1 = fabric_dict:filter(fun(S, _) -> Keep(S) end, Workers),
    Removed = fabric_dict:filter(fun(S, _) -> not Keep(S) end, Workers),

    {Rep, _} = lists:foldl(
        fun(R, {RepAcc, AllMapAcc}) ->
            case maps:is_key(R, WrkMap) of
                true ->
                    % It's a worker and in the map of available shards. Make sure
                    % to keep it only if there is a range available on that node
                    % only (reuse Keep/1 predicate from above)
                    WorkersInRange = maps:get(R, WrkMap),
                    case lists:any(fun({S, _}) -> Keep(S) end, WorkersInRange) of
                        true ->
                            {RepAcc, AllMapAcc};
                        false ->
                            [Shard | Rest] = maps:get(R, AllMapAcc),
                            {[Shard | RepAcc], AllMapAcc#{R := Rest}}
                    end;
                false ->
                    % No worker for this range. Replace from available shards
                    [Shard | Rest] = maps:get(R, AllMapAcc),
                    {[Shard | RepAcc], AllMapAcc#{R := Rest}}
            end
        end,
        {[], AllMap},
        Ring
    ),

    % Return the list of workers that are part of ring, list of removed workers
    % and a list of replacement shards that could be used to make sure the ring
    % completes.
    {Workers1, Removed, Rep}.

% From the list of dead workers determine if any are a result of a split shard.
% In that case perhaps there is a way to not rewind the changes feed back to 0.
% Returns {NewWorkers, Available} where NewWorkers is the list of
% viable workers Available is the list of still unused input Shards
find_split_shard_replacements(DeadWorkers, Shards) ->
    Acc0 = {[], Shards},
    AccF = fabric_dict:fold(
        fun(#shard{node = WN, range = R}, Seq, Acc) ->
            [B, E] = R,
            {SplitWorkers, Available} = Acc,
            ShardsOnSameNode = [S || #shard{node = N} = S <- Available, N =:= WN],
            SplitShards = mem3_util:non_overlapping_shards(ShardsOnSameNode, B, E),
            RepCount = length(SplitShards),
            NewWorkers = [{S, make_split_seq(Seq, RepCount)} || S <- SplitShards],
            NewAvailable = [S || S <- Available, not lists:member(S, SplitShards)],
            {NewWorkers ++ SplitWorkers, NewAvailable}
        end,
        Acc0,
        DeadWorkers
    ),
    {Workers, Available} = AccF,
    {fabric_dict:from_list(Workers), Available}.

find_replacement_sequence(OriginalSeqs, R0) ->
    %% Find the original shard copy in the Seqs array
    case lists:dropwhile(fun({S, _}) -> S#shard.range =/= R0 end, OriginalSeqs) of
        [{#shard{}, {replace, _, _, _}} | _] ->
            % Don't attempt to replace a replacement
            0;
        [{#shard{node = OldNode}, OldSeq} | _] ->
            make_replacement_arg(OldNode, OldSeq);
        _ ->
            % TODO we don't currently attempt to replace a shard with split
            % replicas of that range on other nodes, so it's possible to end
            % up with an empty list here.
            0
    end.

make_split_seq({Num, Uuid, Node}, RepCount) when RepCount > 1 ->
    {Num, {split, Uuid}, Node};
make_split_seq(Seq, _) ->
    Seq.

validate_start_seq(_DbName, "now") ->
    ok;
validate_start_seq(_DbName, 0) ->
    ok;
validate_start_seq(_DbName, "0") ->
    ok;
validate_start_seq(_DbName, Seq) ->
    try
        case Seq of
            [_SeqNum, Opaque] ->
                unpack_seq_decode_term(Opaque);
            Seq ->
                Opaque = unpack_seq_regex_match(Seq),
                unpack_seq_decode_term(Opaque)
        end,
        ok
    catch
        _:_ ->
            Reason = <<"Malformed sequence supplied in 'since' parameter.">>,
            {error, {bad_request, Reason}}
    end.

get_changes_epoch() ->
    case application:get_env(fabric, changes_epoch) of
        undefined ->
            increment_changes_epoch(),
            get_changes_epoch();
        {ok, Epoch} ->
            Epoch
    end.

increment_changes_epoch() ->
    application:set_env(fabric, changes_epoch, os:timestamp()).

unpack_seq_setup() ->
    meck:new(mem3),
    meck:new(fabric_view),
    meck:expect(mem3, get_shard, fun(_, _, _) -> {ok, #shard{}} end),
    meck:expect(fabric_ring, is_progress_possible, fun(_) -> true end),
    ok.

unpack_seqs_test_() ->
    {
        setup,
        fun unpack_seq_setup/0,
        fun(_) -> meck:unload() end,
        [
            t_unpack_seqs()
        ]
    }.

t_unpack_seqs() ->
    ?_test(begin
        % BigCouch 0.3 style.
        assert_shards(
            "23423-g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
            "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
            "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA"
        ),

        % BigCouch 0.4 style.
        assert_shards([
            23423,
            <<
                "g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
                "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
                "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA"
            >>
        ]),

        % BigCouch 0.4 style (as string).
        assert_shards(
            "[23423,\"g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
            "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
            "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA\"]"
        ),
        assert_shards(
            "[23423 ,\"g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
            "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
            "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA\"]"
        ),
        assert_shards(
            "[23423, \"g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
            "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
            "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA\"]"
        ),
        assert_shards(
            "[23423 , \"g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
            "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
            "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA\"]"
        ),

        % with internal hypen
        assert_shards(
            "651-g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwNDLXMwBCwxygOFMiQ"
            "5L8____sxJTcalIUgCSSfZgReE4FTmAFMWDFYXgVJQAUlQPVuSKS1EeC5BkaABSQHXz8"
            "VgJUbgAonB_VqIPfoUHIArvE7T6AUQh0I1-WQAzp1XB"
        ),
        assert_shards([
            651,
            "g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwNDLXMwBCwxygOFMiQ"
            "5L8____sxJTcalIUgCSSfZgReE4FTmAFMWDFYXgVJQAUlQPVuSKS1EeC5BkaABSQHXz8"
            "VgJUbgAonB_VqIPfoUHIArvE7T6AUQh0I1-WQAzp1XB"
        ]),

        % CouchDB 1.2 style
        assert_shards(
            "\"23423-g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
            "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
            "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA\""
        )
    end).

assert_shards(Packed) ->
    ?assertMatch([{#shard{}, _} | _], unpack_seqs(Packed, <<"foo">>)).

find_replacements_test() ->
    % None of the workers are in the live list of shard but there is a
    % replacement on n3 for the full range. It should get picked instead of
    % the two smaller one on n2.
    Workers1 = mk_workers([{"n1", 0, 10}, {"n2", 11, ?RING_END}]),
    AllShards1 = [
        mk_shard("n1", 11, ?RING_END),
        mk_shard("n2", 0, 4),
        mk_shard("n2", 5, 10),
        mk_shard("n3", 0, ?RING_END)
    ],
    {WorkersRes1, Dead1, Reps1} = find_replacements(Workers1, AllShards1),
    ?assertEqual([], WorkersRes1),
    ?assertEqual(Workers1, Dead1),
    ?assertEqual([mk_shard("n3", 0, ?RING_END)], Reps1),

    % None of the workers are in the live list of shards and there is a
    % split replacement from n2 (range [0, 10] replaced with [0, 4], [5, 10])
    Workers2 = mk_workers([{"n1", 0, 10}, {"n2", 11, ?RING_END}]),
    AllShards2 = [
        mk_shard("n1", 11, ?RING_END),
        mk_shard("n2", 0, 4),
        mk_shard("n2", 5, 10)
    ],
    {WorkersRes2, Dead2, Reps2} = find_replacements(Workers2, AllShards2),
    ?assertEqual([], WorkersRes2),
    ?assertEqual(Workers2, Dead2),
    ?assertEqual(
        [
            mk_shard("n1", 11, ?RING_END),
            mk_shard("n2", 0, 4),
            mk_shard("n2", 5, 10)
        ],
        lists:sort(Reps2)
    ),

    % One worker is available and one needs to be replaced. Replacement will be
    % from two split shards
    Workers3 = mk_workers([{"n1", 0, 10}, {"n2", 11, ?RING_END}]),
    AllShards3 = [
        mk_shard("n1", 11, ?RING_END),
        mk_shard("n2", 0, 4),
        mk_shard("n2", 5, 10),
        mk_shard("n2", 11, ?RING_END)
    ],
    {WorkersRes3, Dead3, Reps3} = find_replacements(Workers3, AllShards3),
    ?assertEqual(mk_workers([{"n2", 11, ?RING_END}]), WorkersRes3),
    ?assertEqual(mk_workers([{"n1", 0, 10}]), Dead3),
    ?assertEqual(
        [
            mk_shard("n2", 0, 4),
            mk_shard("n2", 5, 10)
        ],
        lists:sort(Reps3)
    ),

    % All workers are available. Make sure they are not killed even if there is
    % a longer (single) shard to replace them.
    Workers4 = mk_workers([{"n1", 0, 10}, {"n1", 11, ?RING_END}]),
    AllShards4 = [
        mk_shard("n1", 0, 10),
        mk_shard("n1", 11, ?RING_END),
        mk_shard("n2", 0, 4),
        mk_shard("n2", 5, 10),
        mk_shard("n3", 0, ?RING_END)
    ],
    {WorkersRes4, Dead4, Reps4} = find_replacements(Workers4, AllShards4),
    ?assertEqual(Workers4, WorkersRes4),
    ?assertEqual([], Dead4),
    ?assertEqual([], Reps4).

mk_workers(NodesRanges) ->
    mk_workers(NodesRanges, nil).

mk_workers(NodesRanges, Val) ->
    orddict:from_list([{mk_shard(N, B, E), Val} || {N, B, E} <- NodesRanges]).

mk_shard(Name, B, E) ->
    Node = list_to_atom(Name),
    BName = list_to_binary(Name),
    #shard{name = BName, node = Node, range = [B, E]}.

find_split_shard_replacements_test() ->
    % One worker is can be replaced and one can't
    Dead1 = mk_workers([{"n1", 0, 10}, {"n2", 11, ?RING_END}], 42),
    Shards1 = [
        mk_shard("n1", 0, 4),
        mk_shard("n1", 5, 10),
        mk_shard("n3", 11, ?RING_END)
    ],
    {Workers1, ShardsLeft1} = find_split_shard_replacements(Dead1, Shards1),
    ?assertEqual(mk_workers([{"n1", 0, 4}, {"n1", 5, 10}], 42), Workers1),
    ?assertEqual([mk_shard("n3", 11, ?RING_END)], ShardsLeft1),

    % All workers can be replaced - one by 1 shard, another by 3 smaller shards
    Dead2 = mk_workers([{"n1", 0, 10}, {"n2", 11, ?RING_END}], 42),
    Shards2 = [
        mk_shard("n1", 0, 10),
        mk_shard("n2", 11, 12),
        mk_shard("n2", 13, 14),
        mk_shard("n2", 15, ?RING_END)
    ],
    {Workers2, ShardsLeft2} = find_split_shard_replacements(Dead2, Shards2),
    ?assertEqual(
        mk_workers(
            [
                {"n1", 0, 10},
                {"n2", 11, 12},
                {"n2", 13, 14},
                {"n2", 15, ?RING_END}
            ],
            42
        ),
        Workers2
    ),
    ?assertEqual([], ShardsLeft2),

    % No workers can be replaced. Ranges match but they are on different nodes
    Dead3 = mk_workers([{"n1", 0, 10}, {"n2", 11, ?RING_END}], 42),
    Shards3 = [
        mk_shard("n2", 0, 10),
        mk_shard("n3", 11, ?RING_END)
    ],
    {Workers3, ShardsLeft3} = find_split_shard_replacements(Dead3, Shards3),
    ?assertEqual([], Workers3),
    ?assertEqual(Shards3, ShardsLeft3).

find_replacement_sequence_test() ->
    Shards = [{"n2", 0, 10}, {"n3", 0, 5}],
    Uuid = <<"abc1234">>,
    Epoch = 'n1',

    % Not safe to use a plain integer sequence number
    Dead1 = mk_workers(Shards, 42),
    ?assertEqual(0, find_replacement_sequence(Dead1, [0, 10])),
    ?assertEqual(0, find_replacement_sequence(Dead1, [0, 5])),

    % {Seq, Uuid} should work
    Dead2 = mk_workers(Shards, {43, Uuid}),
    ?assertEqual(
        {replace, 'n2', Uuid, 43},
        find_replacement_sequence(Dead2, [0, 10])
    ),
    ?assertEqual(
        {replace, 'n3', Uuid, 43},
        find_replacement_sequence(Dead2, [0, 5])
    ),

    % Can't find the range at all
    ?assertEqual(0, find_replacement_sequence(Dead2, [0, 4])),

    % {Seq, Uuids, EpochNode} should work
    Dead3 = mk_workers(Shards, {44, Uuid, Epoch}),
    ?assertEqual(
        {replace, 'n1', Uuid, 44},
        find_replacement_sequence(Dead3, [0, 10])
    ),
    ?assertEqual(
        {replace, 'n1', Uuid, 44},
        find_replacement_sequence(Dead3, [0, 5])
    ),

    % Cannot replace a replacement
    Dead4 = mk_workers(Shards, {replace, 'n1', Uuid, 45}),
    ?assertEqual(0, find_replacement_sequence(Dead4, [0, 10])),
    ?assertEqual(0, find_replacement_sequence(Dead4, [0, 5])).
