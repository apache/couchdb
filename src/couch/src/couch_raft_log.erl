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

-module(couch_raft_log).

-export([
    new/0,
    append/2,
    sublist/3,
    nth/2,
    last/1,
    index/1,
    term/1,
    value/1
]).

new() ->
    [].

append(Log, Items) ->
    lists:append(Log, Items).

sublist(Log, Start, Len) ->
    lists:sublist(Log, Start, Len).

nth(N, Log) ->
    lists:nth(N, Log).

last([]) ->
    {0, 0, undefined};
last(Log) ->
    lists:last(Log).

index(Entry) ->
    element(1, Entry).

term(Entry) ->
    element(2, Entry).

value(Entry) ->
    element(3, Entry).
