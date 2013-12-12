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

%% exported for upgrade purposes.
-export([keep_sending_changes/8]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(fabric_db_update_listener, [wait_db_updated/1]).

go(DbName, Feed, Options, Callback, Acc0) when Feed == "continuous" orelse
        Feed == "longpoll" orelse Feed == "eventsource" ->
    Args = make_changes_args(Options),
    Since = get_start_seq(DbName, Args),
    case validate_start_seq(DbName, Since) of
    ok ->
        {ok, Acc} = Callback(start, Acc0),
        {Timeout, _} = couch_changes:get_changes_timeout(Args, Callback),
        Ref = make_ref(),
        Parent = self(),
        UpdateListener = {spawn_link(fabric_db_update_listener, go,
                                     [Parent, Ref, DbName, Timeout]),
                          Ref},
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
        #collector{counters=Seqs, user_acc=AccOut, offset=Offset} = Collector,
        Callback({stop, pack_seqs(Seqs), pending_count(Offset)}, AccOut);
    Error ->
        Callback(Error, Acc0)
    end.

keep_sending_changes(DbName, Args, Callback, Seqs, AccIn, Timeout, UpListen, T0) ->
    #changes_args{limit=Limit, feed=Feed, heartbeat=Heartbeat} = Args,
    {ok, Collector} = send_changes(DbName, Args, Callback, Seqs, AccIn, Timeout),
    #collector{
        limit = Limit2,
        counters = NewSeqs,
        offset = Offset,
        user_acc = AccOut
    } = Collector,
    LastSeq = pack_seqs(NewSeqs),
    MaintenanceMode = config:get("cloudant", "maintenance_mode"),
    if Limit > Limit2, Feed == "longpoll";
      MaintenanceMode == "true"; MaintenanceMode == "nolb" ->
        Callback({stop, LastSeq, pending_count(Offset)}, AccOut);
    true ->
        WaitForUpdate = wait_db_updated(UpListen),
        AccumulatedTime = timer:now_diff(os:timestamp(), T0) div 1000,
        Max = case config:get("fabric", "changes_duration") of
        undefined ->
            infinity;
        MaxStr ->
            list_to_integer(MaxStr)
        end,
        case {Heartbeat, AccumulatedTime > Max, WaitForUpdate} of
        {undefined, _, timeout} ->
            Callback({stop, LastSeq, pending_count(Offset)}, AccOut);
        {_, true, timeout} ->
            Callback({stop, LastSeq, pending_count(Offset)}, AccOut);
        _ ->
            {ok, AccTimeout} = Callback(timeout, AccOut),
            ?MODULE:keep_sending_changes(
                DbName,
                Args#changes_args{limit=Limit2},
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
    Seqs = lists:flatmap(fun({#shard{name=Name, node=N} = Shard, Seq}) ->
        case lists:member(Shard, AllLiveShards) of
        true ->
            Ref = rexi:cast(N, {fabric_rpc, changes, [Name,ChangesArgs,Seq]}),
            [{Shard#shard{ref = Ref}, Seq}];
        false ->
            % Find some replacement shards to cover the missing range
            % TODO It's possible in rare cases of shard merging to end up
            % with overlapping shard ranges from this technique
            lists:map(fun(#shard{name=Name2, node=N2} = NewShard) ->
                Ref = rexi:cast(N2, {fabric_rpc, changes, [Name2, ChangesArgs,
                    make_replacement_arg(N, Seq)]}),
                {NewShard#shard{ref = Ref}, 0}
            end, find_replacement_shards(Shard, AllLiveShards))
        end
    end, unpack_seqs(PackedSeqs, DbName)),
    {Workers0, _} = lists:unzip(Seqs),
    Repls = fabric_view:get_shard_replacements(DbName, Workers0),
    StartFun = fun(#shard{name=Name, node=N, range=R0}=Shard) ->
        %% Find the original shard copy in the Seqs array
        case lists:dropwhile(fun({S, _}) -> S#shard.range =/= R0 end, Seqs) of
            [{#shard{}, {replace, _, _, _}} | _] ->
                % Don't attempt to replace a replacement
                SeqArg = 0;
            [{#shard{node = OldNode}, OldSeq} | _] ->
                SeqArg = make_replacement_arg(OldNode, OldSeq);
            _ ->
                % TODO this clause is probably unreachable in the N>2
                % case because we compute replacements only if a shard has one
                % in the original set.
                couch_log:error("Streaming ~s from zero while replacing ~p",
                    [Name, PackedSeqs]),
                SeqArg = 0
        end,
        Ref = rexi:cast(N, {fabric_rpc, changes, [Name, ChangesArgs, SeqArg]}),
        Shard#shard{ref = Ref}
    end,
    RexiMon = fabric_util:create_monitors(Workers0),
    try
        case fabric_util:stream_start(Workers0, #shard.ref, StartFun, Repls) of
            {ok, Workers} ->
                try
                    LiveSeqs = lists:map(fun(W) ->
                        case lists:keyfind(W, 1, Seqs) of
                            {W, Seq} -> {W, Seq};
                            _ -> {W, 0}
                        end
                    end, Workers),
                    send_changes(DbName, Workers, LiveSeqs, ChangesArgs,
                            Callback, AccIn, Timeout)
                after
                    fabric_util:cleanup(Workers)
                end;
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
        rows = Seqs % store sequence positions instead
    },
    %% TODO: errors need to be handled here
    receive_results(Workers, State, Timeout, Callback).

receive_results(Workers, State, Timeout, Callback) ->
    case rexi_utils:recv(Workers, #shard.ref, fun handle_message/3, State,
            Timeout, infinity) of
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

handle_message({change, Props}, {Worker, _}, #collector{limit=0} = State) ->
    O0 = State#collector.offset,
    O1 = case fabric_dict:lookup_element(Worker, O0) of
        null ->
            % Use Pending+1 because we're ignoring this row in the response
            Pending = couch_util:get_value(pending, Props),
            fabric_dict:store(Worker, Pending+1, O0);
        _ ->
            O0
    end,
    maybe_stop(State#collector{offset = O1});

handle_message({complete, Props}, Worker, #collector{limit=0} = State) ->
    O0 = State#collector.offset,
    O1 = case fabric_dict:lookup_element(Worker, O0) of
        null ->
            fabric_dict:store(Worker, couch_util:get_value(pending,Props), O0);
        _ ->
            O0
    end,
    maybe_stop(State#collector{offset = O1});

handle_message(#change{} = Row, {Worker, From}, St) ->
    Change = {change, [
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
    if (Interval == undefined) orelse (Limit rem Interval == 0) ->
        Props2 = lists:keyreplace(seq, 1, Props, {seq, pack_seqs(S1)});
    true ->
        Props2 = lists:keyreplace(seq, 1, Props, {seq, null})
    end,
    {Go, Acc} = Callback(changes_row(Props2), AccIn),
    rexi:stream_ack(From),
    {Go, St#collector{counters=S1, offset=O1, limit=Limit-1, user_acc=Acc}};

handle_message({no_pass, Seq}, {Worker, From}, St) ->
    #collector{counters = S0} = St,
    true = fabric_dict:is_key(Worker, S0),
    S1 = fabric_dict:store(Worker, Seq, S0),
    rexi:stream_ack(From),
    {ok, St#collector{counters=S1}};

handle_message({complete, Props}, Worker, State) ->
    Key = couch_util:get_value(seq, Props),
    #collector{
        counters = S0,
        offset = O0,
        total_rows = Completed % override
    } = State,
    true = fabric_dict:is_key(Worker, S0),
    S1 = fabric_dict:store(Worker, Key, S0),
    O1 = fabric_dict:store(Worker, couch_util:get_value(pending, Props), O0),
    NewState = State#collector{counters=S1, offset=O1, total_rows=Completed+1},
    % We're relying on S1 having exactly the numnber of workers that
    % are participtaing in this response. With the new stream_start
    % that's a bit more obvious but historically it wasn't quite
    % so clear. The Completed variable is just a hacky override
    % of the total_rows field in the #collector{} record.
    NumWorkers = fabric_dict:size(S1),
    Go = case NumWorkers =:= (Completed+1) of
        true -> stop;
        false -> ok
    end,
    {Go, NewState}.


make_replacement_arg(Node, {Seq, Uuid}) ->
    {replace, Node, Uuid, Seq};
make_replacement_arg(Node, {Seq, Uuid, _}) ->
    %% TODO Deprecated, remove when we're confident no seqs with this format
    %% are in the wild
    {replace, Node, Uuid, Seq};
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

make_changes_args(#changes_args{style=Style, filter=undefined}=Args) ->
    Args#changes_args{filter = Style};
make_changes_args(Args) ->
    Args.

get_start_seq(DbName, #changes_args{dir=Dir, since=Since})
  when Dir == rev; Since == "now" ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_update_seq, []),
    {ok, Seqs} = fabric_util:recv(Workers, #shard.ref,
        fun collect_update_seqs/3, fabric_dict:init(Workers, -1)),
    Seqs;
get_start_seq(_DbName, #changes_args{dir=fwd, since=Since}) ->
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

pending_count(Dict) ->
    fabric_dict:fold(fun
        (_Worker, Count, Acc) when is_integer(Count), is_integer(Acc) ->
            Count + Acc;
        (_Worker, _Count, _Acc) ->
            null
    end, 0, Dict).

pack_seqs(Workers) ->
    SeqList = [{N,R,S} || {#shard{node=N, range=R}, S} <- Workers],
    SeqSum = lists:sum([seq(S) || {_,_,S} <- SeqList]),
    Opaque = couch_util:encodeBase64Url(term_to_binary(SeqList, [compressed])),
    [SeqSum, Opaque].

seq({Seq, _Uuid, _Node}) -> Seq; % downgrade clause
seq({Seq, _Uuid}) -> Seq;
seq(Seq)          -> Seq.

unpack_seqs(0, DbName) ->
    fabric_dict:init(mem3:shards(DbName), 0);

unpack_seqs("0", DbName) ->
    fabric_dict:init(mem3:shards(DbName), 0);

unpack_seqs([_SeqNum, Opaque], DbName) ->
    do_unpack_seqs(Opaque, DbName);

unpack_seqs(Packed, DbName) ->
    NewPattern = "^\\[[0-9]+\s*,\s*\"(?<opaque>.*)\"\\]$",
    OldPattern = "^\"?([0-9]+-)?(?<opaque>.*?)\"?$",
    Options = [{capture, [opaque], binary}],
    Opaque = case re:run(Packed, NewPattern, Options) of
    {match, Match} ->
        Match;
    nomatch ->
        {match, Match} = re:run(Packed, OldPattern, Options),
        Match
    end,
    do_unpack_seqs(Opaque, DbName).

do_unpack_seqs(Opaque, DbName) ->
    % A preventative fix for FB 13533 to remove duplicate shards.
    % This just picks each unique shard and keeps the largest seq
    % value recorded.
    Decoded = binary_to_term(couch_util:decodeBase64Url(Opaque)),
    DedupDict = lists:foldl(fun({Node, [A, B], Seq}, Acc) ->
        dict:append({Node, [A, B]}, Seq, Acc)
    end, dict:new(), Decoded),
    Deduped = lists:map(fun({{Node, [A, B]}, SeqList}) ->
        {Node, [A, B], lists:max(SeqList)}
    end, dict:to_list(DedupDict)),

    % Create a fabric_dict of {Shard, Seq} entries
    % TODO relies on internal structure of fabric_dict as keylist
    Unpacked = lists:flatmap(fun({Node, [A,B], Seq}) ->
        case mem3:get_shard(DbName, Node, [A,B]) of
        {ok, Shard} ->
            [{Shard, Seq}];
        {error, not_found} ->
            []
        end
    end, Deduped),

    % Fill holes in the since sequence. If/when we ever start
    % using overlapping shard ranges this will need to be updated
    % to not include shard ranges that overlap entries in Upacked.
    % A quick and dirty approach would be like such:
    %
    %   lists:foldl(fun(S, Acc) ->
    %       fabric_view:remove_overlapping_shards(S, Acc)
    %   end, mem3:shards(DbName), Unpacked)
    %
    % Unfortunately remove_overlapping_shards isn't reusable because
    % of its calls to rexi:kill/2. When we get to overlapping
    % shard ranges and have to rewrite shard range management
    % we can revisit this simpler algorithm.
    case fabric_view:is_progress_possible(Unpacked) of
        true ->
            Unpacked;
        false ->
            Extract = fun({Shard, _Seq}) -> Shard#shard.range end,
            Ranges = lists:usort(lists:map(Extract, Unpacked)),
            Filter = fun(S) -> not lists:member(S#shard.range, Ranges) end,
            Replacements = lists:filter(Filter, mem3:shards(DbName)),
            Unpacked ++ [{R, 0} || R <- Replacements]
    end.

changes_row(Props0) ->
    Props1 = case couch_util:get_value(deleted, Props0) of
        true ->
            Props0;
        _ ->
            lists:keydelete(deleted, 1, Props0)
    end,
    Allowed = [seq, id, changes, deleted, doc, error],
    Props2 = lists:filter(fun({K,_V}) -> lists:member(K, Allowed) end, Props1),
    {change, {Props2}}.

find_replacement_shards(#shard{range=Range}, AllShards) ->
    % TODO make this moar betta -- we might have split or merged the partition
    [Shard || Shard <- AllShards, Shard#shard.range =:= Range].

validate_start_seq(_DbName, "now") ->
    ok;
validate_start_seq(DbName, Seq) ->
    try unpack_seqs(Seq, DbName) of _Any ->
        ok
    catch
        error:database_does_not_exist ->
            {error, database_does_not_exist};
        _:_ ->
            Reason = <<"Malformed sequence supplied in 'since' parameter.">>,
            {error, {bad_request, Reason}}
    end.

unpack_seqs_test() ->
    meck:new(mem3),
    meck:new(fabric_view),
    meck:expect(mem3, get_shard, fun(_, _, _) -> {ok, #shard{}} end),
    meck:expect(fabric_view, is_progress_possible, fun(_) -> true end),

    % BigCouch 0.3 style.
    assert_shards("23423-g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
    "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
    "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA"),

    % BigCouch 0.4 style.
    assert_shards([23423,<<"g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
    "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
    "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA">>]),

    % BigCouch 0.4 style (as string).
    assert_shards("[23423,\"g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
    "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
    "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA\"]"),
    assert_shards("[23423 ,\"g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
    "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
    "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA\"]"),
    assert_shards("[23423, \"g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
    "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
    "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA\"]"),
    assert_shards("[23423 , \"g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
    "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
    "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA\"]"),

    % with internal hypen
    assert_shards("651-g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwNDLXMwBCwxygOFMiQ"
    "5L8____sxJTcalIUgCSSfZgReE4FTmAFMWDFYXgVJQAUlQPVuSKS1EeC5BkaABSQHXz8"
    "VgJUbgAonB_VqIPfoUHIArvE7T6AUQh0I1-WQAzp1XB"),
    assert_shards([651,"g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwNDLXMwBCwxygOFMiQ"
    "5L8____sxJTcalIUgCSSfZgReE4FTmAFMWDFYXgVJQAUlQPVuSKS1EeC5BkaABSQHXz8"
    "VgJUbgAonB_VqIPfoUHIArvE7T6AUQh0I1-WQAzp1XB"]),

    % CouchDB 1.2 style
    assert_shards("\"23423-g1AAAAE7eJzLYWBg4MhgTmHgS0ktM3QwND"
    "LXMwBCwxygOFMiQ5L8____sxIZcKlIUgCSSfZgRUw4FTmAFMWDFTHiVJQAUlSPX1Ee"
    "C5BkaABSQHXzsxKZ8StcAFG4H4_bIAoPQBTeJ2j1A4hCUJBkAQC7U1NA\""),

    meck:unload(fabric_view),
    meck:unload(mem3).

assert_shards(Packed) ->
    ?assertMatch([{#shard{},_}|_], unpack_seqs(Packed, <<"foo">>)).
