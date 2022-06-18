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

%% a non-persistent implementation of the raft_log_store behaviour for testing purposes.

-module(couch_raft_store_sha256).
-behaviour(couch_raft_store).

-export([
    init/1,
    save_state/1,
    %% log
    last/1,
    lookup/2,
    range/3,
    append/2,
    truncate/2,
    discard/2,
    %% state machine
    apply/2
]).

init(Cohort) ->
    {ok, #{
        cohort => Cohort,
        commitIndex => 0,
        lastApplied => 0,
        log => [],
        machine => <<0>>,
        term => 0,
        votedFor => undefined
    }}.

% raft state callbacks

save_state(#{} = State) ->
    _WouldPersist = maps:with([cohort, term, votedFor, lastApplied, machine], State),
    ok.

%% log callbacks
last(#{log := []}) ->
    {0, 0};
last(#{log := Log}) ->
    {LastTerm, _} = lists:last(Log),
    {length(Log), LastTerm}.

lookup(0, #{}) ->
    {0, 0};
lookup(N, #{log := Log}) when N > 0 ->
    lists:nth(N, Log).

range(Start, Len, #{log := Log}) when Start > 0, Len > 0 ->
    lists:sublist(Log, Start, Len).

append(Entries, #{log := Log} = State) when is_list(Entries) ->
    NewLog = lists:append(Log, Entries),
    {ok, length(NewLog), State#{log => NewLog}}.

truncate(To, #{log := Log} = State) ->
    {ok, State#{log => lists:sublist(Log, To)}}.

discard(_UpTo, #{}) ->
    {error, not_implemented}.

%% state machine callbacks

apply(Bin, #{machine := Machine0} = State) when is_binary(Bin), is_binary(Machine0) ->
    Machine1 = crypto:hash(sha256, <<Machine0/binary, Bin/binary>>),
    {Machine1, State#{machine => Machine1}}.
