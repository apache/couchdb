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

-module(couch_views_batch_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("fabric/test/fabric2_test.hrl").
-include_lib("couch_views/include/couch_views.hrl").

batch_test_() ->
    {
        "Test view batch sizing",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            with([
                ?TDEF(basic),
                ?TDEF(search_success),
                ?TDEF(sense_success),
                ?TDEF(failure),
                ?TDEF(failure_switches_to_sense)
            ])
        }
    }.

setup() ->
    test_util:start_couch().

cleanup(Ctx) ->
    test_util:stop_couch(Ctx).

basic(_) ->
    erase(couch_views_batch),
    ?assertEqual(100, couch_views_batch:start(#mrst{})).

search_success(_) ->
    erase(couch_views_batch),
    couch_views_batch:start(#mrst{}),
    couch_views_batch:success(#mrst{}, ustats(0, 0, 0)),
    ?assertEqual(600, couch_views_batch:start(#mrst{})).

sense_success(_) ->
    erase(couch_views_batch),
    couch_views_batch:start(#mrst{}),
    % Exceeding our threshold switches from search to sense
    couch_views_batch:success(#mrst{}, ustats(5000, 10000000, 10000)),
    ?assertEqual(80, couch_views_batch:start(#mrst{})),
    couch_views_batch:success(#mrst{}, ustats(0, 0, 0)),
    ?assertEqual(180, couch_views_batch:start(#mrst{})).

failure(_) ->
    erase(couch_views_batch),
    couch_views_batch:start(#mrst{}),
    couch_views_batch:failure(#mrst{}),
    ?assertEqual(50, couch_views_batch:start(#mrst{})).

failure_switches_to_sense(_) ->
    erase(couch_views_batch),
    couch_views_batch:start(#mrst{}),
    couch_views_batch:failure(#mrst{}),
    couch_views_batch:start(#mrst{}),
    couch_views_batch:success(#mrst{}, ustats(0, 0, 0)),
    ?assertEqual(150, couch_views_batch:start(#mrst{})).

ustats(DocsRead, TxSize, TotalKVs) ->
    #{
        docs_read => DocsRead,
        tx_size => TxSize,
        total_kvs => TotalKVs
    }.
