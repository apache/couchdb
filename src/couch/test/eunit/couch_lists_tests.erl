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

-module(couch_lists_tests).

-include_lib("couch/include/couch_eunit.hrl").

uniq_test_() ->
    [
        ?_assertEqual([], couch_lists:uniq([])),
        ?_assertEqual(lists:seq(1, 100), couch_lists:uniq(lists:seq(1, 100))),
        ?_assertEqual(lists:seq(100, 1, -1), couch_lists:uniq(lists:seq(100, 1, -1))),
        ?_assertEqual(
            [1, 2, 3, 4],
            couch_lists:uniq([1, 2, 2, 3, 3, 3, 4, 4, 4, 4])
        ),
        ?_assertEqual(
            [5, 1, 3, 2, 8],
            couch_lists:uniq([5, 1, 1, 5, 5, 3, 2, 5, 3, 3, 2, 3, 8, 2, 2, 8])
        )
    ].
