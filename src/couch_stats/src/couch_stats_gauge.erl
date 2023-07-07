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

-module(couch_stats_gauge).

-export([
    new/0,
    update/2,
    read/1
]).

new() ->
    counters:new(1, [write_concurrency]).

update(Ctx, Val) when is_integer(Val) ->
    counters:put(Ctx, 1, Val).

read(Ctx) ->
    counters:get(Ctx, 1).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

counter_test() ->
    G = new(),

    ?assertEqual(0, read(G)),

    update(G, 0),
    ?assertEqual(0, read(G)),

    update(G, 1),
    ?assertEqual(1, read(G)),

    update(G, 0),
    ?assertEqual(0, read(G)),

    update(G, 1 bsl 10),
    ?assertEqual(1 bsl 10, read(G)),

    update(G, -1),
    ?assertEqual(-1, read(G)).

-endif.
