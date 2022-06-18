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

-module(couch_raft_store).

-callback init(Args :: term()) -> {ok, State :: #{}} | {stop, Reason :: term()}.

% raft state callbacks

-callback save_state(State :: #{}) -> ok | {error, Reason :: term()}.

%% log callbacks
-type log_entry() :: {Term :: non_neg_integer(), Value :: term()}.
-callback last(State :: #{}) -> {Index :: non_neg_integer(), Term :: non_neg_integer()}.
-callback lookup(N :: non_neg_integer(), State :: #{}) -> log_entry() | not_found.
-callback range(Start :: non_neg_integer(), Len :: non_neg_integer(), State :: #{}) -> [log_entry() | not_found].
-callback append(Entries :: [log_entry()], State :: #{}) ->
    {ok, Index :: non_neg_integer(), NewState :: #{}} | {error, Reason :: term()}.
-callback truncate(To :: non_neg_integer(), State :: #{}) -> {ok, NewState :: #{}} | {error, Reason :: term()}.
-callback discard(UpTo :: non_neg_integer(), State :: #{}) ->
    {ok, NewState :: #{}} | {error, Reason :: term()}.

%% state machine callbacks
-callback apply(Args :: term(), State :: #{}) -> {Result :: term(), NewState :: #{}}.
