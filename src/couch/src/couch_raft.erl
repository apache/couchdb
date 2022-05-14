% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(couch_raft).
-behaviour(gen_statem).

-define(ELECTION_DELAY, 150).
-define(ELECTION_SPLAY, 150).
-define(LEADER_HEARTBEAT, 75).
-define(CLIENT_TIMEOUT, 5_000).

% maximum number of entries to send in one go.
-define(BATCH_SIZE, 10).

% public api

-export([
    start/2,
    start_link/2,
    stop/1,
    call/2
]).

% mandatory gen_statem callbacks

-export([
    init/1,
    callback_mode/0,
    handle_event/4
]).

%% public api

start(Name, Cohort) ->
    gen_statem:start({local, Name}, ?MODULE, new(Name, Cohort), []).

start_link(Name, Cohort) ->
    gen_statem:start_link({local, Name}, ?MODULE, new(Name, Cohort), []).

new(Name, Cohort) ->
    Peers = peers(Cohort),
    #{
        name => Name,
        cohort => Cohort,
        term => 0,
        votedFor => undefined,
        votesGranted => #{},
        nextIndex => maps:from_list([{Peer, 1} || Peer <- Peers]),
        matchIndex => maps:from_list([{Peer, 0} || Peer <- Peers]),
        log => couch_raft_log:new(),
        commitIndex => 0,
        froms => #{},
        lastApplied => 0,
        machine => <<0>>
    }.

stop(ServerRef) ->
    gen_statem:stop(ServerRef).

call(ServerRef, Value) ->
    gen_statem:call(ServerRef, #{type => 'ClientRequest', value => Value}, ?CLIENT_TIMEOUT).

init(Data) ->
    {ok, follower, Data}.

callback_mode() ->
    [handle_event_function, state_enter].

%% erlfmt-ignore
handle_event(cast, #{term := FutureTerm} = Msg, _State, #{term := CurrentTerm} = Data) when FutureTerm > CurrentTerm ->
    couch_log:notice("~p received message from future term ~B, moving to that term, becoming follower and clearing votedFor", [node(), FutureTerm]),
    {next_state, follower, Data#{term => FutureTerm, votedFor => undefined}, {next_event, cast, Msg}};

handle_event(enter, _OldState, follower, Data) ->
    #{term := Term, froms := Froms} = Data,
    couch_log:notice("~p became follower in term ~B", [node(), Term]),
    Replies = [{reply, From, {error, deposed}} || From <- maps:values(Froms)],
    {keep_state, Data#{votedFor => undefined, froms => #{}}, [restart_election_timeout() | Replies]};

handle_event(enter, _OldState, candidate, Data) ->
    #{term := Term} = Data,
    couch_log:notice("~p became candidate in term ~B", [node(), Term]),
    {keep_state, start_election(Data), restart_election_timeout()};

handle_event(enter, _OldState, leader, Data) ->
    #{log := Log, cohort := Cohort, term := Term} = Data,
    couch_log:notice("~p became leader in term ~B", [node(), Term]),
    Peers = peers(Cohort),
    {keep_state, Data#{
        nextIndex => maps:from_list([{Peer, couch_raft_log:index(couch_raft_log:last(Log)) + 1} || Peer <- Peers]),
        matchIndex => maps:from_list([{Peer, 0} || Peer <- Peers])
    }, restart_heartbeat_timeout()};

handle_event(cast, #{type := 'RequestVoteRequest', term := Term} = Msg, _State, #{term := CurrentTerm} = Data)
  when Term =< CurrentTerm ->
    #{
        source := MSource,
        lastLogIndex := MLastLogIndex,
        lastLogTerm := MLastLogTerm
    } = Msg,
    #{
        log := Log,
        votedFor := VotedFor
    } = Data,
    LogOk = MLastLogTerm > couch_raft_log:term(couch_raft_log:last(Log)) orelse (MLastLogTerm == couch_raft_log:term(couch_raft_log:last(Log)) andalso MLastLogIndex >= couch_raft_log:index(couch_raft_log:last(Log))),
    Grant = Term == CurrentTerm andalso LogOk andalso (VotedFor == undefined orelse VotedFor == MSource),
    couch_log:notice("~p received RequestVoteRequest from ~p in term ~B when in term ~B (Grant:~p, LogOk:~p, VotedFor:~p)", [node(), MSource, Term, CurrentTerm, Grant, LogOk, VotedFor]),
    Reply = #{
        type => 'RequestVoteResponse',
        term => CurrentTerm,
        voteGranted => Grant,
        source => node()
    },
    cast(MSource, Reply, Data),
    if
        Grant ->
            {keep_state, Data#{votedFor => MSource}, restart_election_timeout()};
        true ->
            {keep_state_and_data, restart_election_timeout()}
    end;

handle_event(cast, #{type := 'RequestVoteResponse', term := PastTerm}, _State, #{term := CurrentTerm}) when PastTerm < CurrentTerm ->
    couch_log:notice("~p ignored RequestVoteResponse from past term ~B", [node(), PastTerm]),
    keep_state_and_data;

handle_event(cast, #{type := 'RequestVoteResponse', term := Term} = Msg, _State, #{term := Term} = Data) ->
    #{source := MSource, voteGranted := MVoteGranted} = Msg,
    #{cohort := Cohort, votesGranted := VotesGranted0} = Data,
    VotesGranted1 = if MVoteGranted -> lists:usort([MSource | VotesGranted0]); true -> VotesGranted0 end,
    couch_log:notice("~p received RequestVoteResponse from ~p in current term ~B (VotesGranted:~p)", [node(), MSource, Term, VotesGranted1]),
    if
        length(VotesGranted1) >= length(Cohort) div 2 + 1 ->
            couch_log:notice("~p has enough votes to be leader in term ~B", [node(), Term]),
            {next_state, leader, Data#{votesGranted => VotesGranted1}};
        true ->
            {keep_state, Data#{votesGranted => VotesGranted1}}
    end;


handle_event(cast, #{type := 'AppendEntriesRequest', term := Term} = Msg, State, #{term := CurrentTerm} = Data)
  when Term =< CurrentTerm ->
    #{
        source := MSource,
        prevLogIndex := MPrevLogIndex,
        prevLogTerm := MPrevLogTerm,
        entries := MEntries,
        commitIndex := MCommitIndex
    } = Msg,
    #{
        log := Log
    } = Data,
    LogOk = MPrevLogIndex == 0 orelse (MPrevLogIndex > 0 andalso MPrevLogIndex =< couch_raft_log:index(couch_raft_log:last(Log)) andalso MPrevLogTerm == couch_raft_log:term(couch_raft_log:nth(MPrevLogIndex,Log))),
    if
        Term < CurrentTerm orelse (Term == CurrentTerm andalso State == follower andalso not LogOk) ->
            Reply = #{
                type => 'AppendEntriesResponse',
                term => CurrentTerm,
                success => false,
                matchIndex => 0,
                source => node()
            },
            cast(MSource, Reply, Data),
            if
                State == leader ->
                    keep_state_and_data;
                true ->
                    {keep_state_and_data, restart_election_timeout()}
            end;
        Term == CurrentTerm andalso State == candidate ->
            {next_state, follower, Data, {next_event, cast, Msg}};
        Term == CurrentTerm andalso State == follower andalso LogOk ->
            if
                MEntries == [] ->
                    Reply = #{
                        type => 'AppendEntriesResponse',
                        term => CurrentTerm,
                        success => true,
                        matchIndex => MPrevLogIndex,
                        source => node()
                    },
                    couch_log:debug("~p received heartbeat and everything matches, sending matchIndex:~p", [node(), MPrevLogIndex]),
                    cast(MSource, Reply, Data),
                    {keep_state, update_state_machine(Data#{commitIndex => MCommitIndex}), restart_election_timeout()};
                true ->
                    Index = MPrevLogIndex + 1,
                    LastLogIndex = couch_raft_log:index(couch_raft_log:last(Log)),
                    if
                        LastLogIndex >= Index ->
                            NthLogTerm = couch_raft_log:term(couch_raft_log:nth(Index, Log)),
                            FirstEntryTerm = couch_raft_log:term(hd(MEntries)),
                            if
                                NthLogTerm == FirstEntryTerm ->
                                    Reply = #{
                                        type => 'AppendEntriesResponse',
                                        term => CurrentTerm,
                                        success => true,
                                        matchIndex => MPrevLogIndex + length(MEntries),
                                        source => node()
                                    },
                                    couch_log:notice("~p received entry:~p that's already applied, sending matchIndex:~p", [node(), MEntries, MPrevLogIndex + length(MEntries)]),
                                    cast(MSource, Reply, Data),
                                    {keep_state, update_state_machine(Data#{commitIndex => MCommitIndex}), restart_election_timeout()};
                                NthLogTerm /= FirstEntryTerm ->
                                    couch_log:notice("~p received conflicting entry:~p, deleting it", [node(), MEntries]),
                                    {keep_state, Data#{log => lists:sublist(Log, LastLogIndex - 1)}, [{next_event, cast, Msg}, restart_election_timeout()]}
                            end;
                        LastLogIndex == MPrevLogIndex ->
                            couch_log:notice("~p received new entries:~p, appending it to log", [node(), MEntries]),
                            {keep_state, Data#{log => couch_raft_log:append(Log, MEntries)}, [{next_event, cast, Msg}, restart_election_timeout()]}
                    end
            end
    end;

handle_event(cast, #{type := 'AppendEntriesResponse', term := PastTerm}, _State, #{term := CurrentTerm}) when PastTerm < CurrentTerm ->
    couch_log:notice("~p ignored AppendEntriesResponse from past term ~B", [node(), PastTerm]),
    keep_state_and_data;

handle_event(cast, #{type := 'AppendEntriesResponse', term := Term} = Msg, _State, #{term := Term} = Data) ->
    #{success := MSuccess, matchIndex := MMatchIndex, source := MSource} = Msg,
    #{nextIndex := NextIndex, matchIndex := MatchIndex} = Data,
    couch_log:debug("~p received AppendEntriesResponse from ~p in current term ~B (Success:~p)", [node(), MSource, Term, MSuccess]),
    SourceNextIndex = maps:get(MSource, NextIndex),
    if
        MSuccess ->
            {keep_state, Data#{
                nextIndex => NextIndex#{MSource => MMatchIndex + 1},
                matchIndex => MatchIndex#{MSource => MMatchIndex}
            }};
        true ->
            {keep_state, Data#{
                nextIndex => NextIndex#{MSource => max(SourceNextIndex - 1, 1)}
            }}
    end;

handle_event({call, From}, #{type := 'ClientRequest'} = Msg, leader, Data) ->
    #{value := Value} = Msg,
    #{term := Term, log := Log, froms := Froms} = Data,
    EntryIndex = couch_raft_log:index(couch_raft_log:last(Log)) + 1,
    Entry = {EntryIndex, Term, Value},
    {keep_state, Data#{log => couch_raft_log:append(Log, [Entry]), froms => Froms#{EntryIndex => From}}};

handle_event({call, From}, #{type := 'ClientRequest'}, _State, _Data) ->
    {keep_state_and_data, {reply, From, {error, not_leader}}};

handle_event(state_timeout, new_election, State, Data) when State == follower; State == candidate ->
    #{term := Term} = Data,
    couch_log:notice("~p election timeout in state ~p, term ~B", [node(), State, Term]),
    {next_state, candidate, start_election(Data), restart_election_timeout()};

handle_event(state_timeout, heartbeat, leader, Data) ->
    #{term := Term} = Data,
    couch_log:debug("~p leader sending a heartbeat in term ~B", [node(), Term]),
    ok = send_append_entries(Data),
    {keep_state, advance_commit_index(Data), restart_heartbeat_timeout()};

handle_event(EventType, EventContent, State, Data) ->
    {stop, {unknown_event, EventType, EventContent, State, Data}}.


send_append_entries(#{cohort := Cohort} = Data) ->
    send_append_entries(peers(Cohort), Data).

send_append_entries([], _Data) ->
    ok;
send_append_entries([Peer | Rest], Data) ->
    #{term := Term, nextIndex := NextIndex, log := Log, commitIndex := CommitIndex} = Data,
    PrevLogIndex = maps:get(Peer, NextIndex) - 1,
    PrevLogTerm = if PrevLogIndex > 0 -> couch_raft_log:term(couch_raft_log:nth(PrevLogIndex, Log)); true -> 0 end,
    LastEntry = min(couch_raft_log:index(couch_raft_log:last(Log)), PrevLogIndex + 2),
    Entries = couch_raft_log:sublist(Log, PrevLogIndex + 1, ?BATCH_SIZE),
    Msg = #{
        type => 'AppendEntriesRequest',
        term => Term,
        source => node(),
        prevLogIndex => PrevLogIndex,
        prevLogTerm => PrevLogTerm,
        entries => Entries,
        commitIndex => min(CommitIndex, LastEntry)
    },
    cast(Peer, Msg, Data),
    send_append_entries(Rest, Data).

advance_commit_index(Data) ->
    #{matchIndex := MatchIndex, log := Log, cohort := Cohort, term := Term} = Data,
    LastTerm = couch_raft_log:term(couch_raft_log:last(Log)),
    LastIndexes = lists:sort([couch_raft_log:index(couch_raft_log:last(Log)) | maps:values(MatchIndex)]),
    NewCommitIndex = lists:nth(length(Cohort) div 2 + 1, LastIndexes),
    if
        LastTerm == Term ->
            update_state_machine(Data#{commitIndex => NewCommitIndex});
        true ->
            Data
    end.

update_state_machine(#{lastApplied := Same, commitIndex := Same} = Data) ->
    Data;
update_state_machine(#{lastApplied := LastApplied, commitIndex := CommitIndex} = Data) when LastApplied < CommitIndex ->
    #{log := Log, froms := Froms0, machine := Machine0} = Data,
    From = LastApplied + 1,
    To = min(couch_raft_log:index(couch_raft_log:last(Log)), CommitIndex),
    Fun = fun(Index, {Froms, Machine}) ->
        Value = couch_raft_log:value(couch_raft_log:nth(Index, Log)),
        Result = crypto:hash(sha256, <<Machine/binary, Value/binary>>),
        case maps:is_key(Index, Froms) of
            true ->
                gen_statem:reply(maps:get(Index, Froms), Result),
                {maps:remove(Index, Froms), Result};
            false ->
                {Froms, Result}
        end
    end,
    {Froms1, Machine1} = lists:foldl(Fun, {Froms0, Machine0}, lists:seq(From, To)),
    Data#{froms => Froms1, machine => Machine1, lastApplied => To}.

start_election(Data) ->
    #{term := Term, cohort := Cohort, log := Log} = Data,
    ElectionTerm = Term + 1,
    couch_log:notice("~p starting election in term ~B", [node(), ElectionTerm]),
    RequestVote = #{
        type => 'RequestVoteRequest',
        term => ElectionTerm,
        lastLogIndex => couch_raft_log:index(couch_raft_log:last(Log)),
        lastLogTerm => couch_raft_log:term(couch_raft_log:last(Log)),
        source => node()
    },
    lists:foreach(fun(Peer) -> cast(Peer, RequestVote, Data) end, peers(Cohort)),
    Data#{term => ElectionTerm, votedFor => node(), votesGranted => [node()]}.

cast(Node, Msg, #{name := Name}) ->
    gen_statem:cast({Name, Node}, Msg).

restart_election_timeout() ->
    {state_timeout, ?ELECTION_DELAY + rand:uniform(?ELECTION_SPLAY), new_election}.

restart_heartbeat_timeout() ->
    {state_timeout, ?LEADER_HEARTBEAT, heartbeat}.

peers(Cohort) ->
    Cohort -- [node()].
