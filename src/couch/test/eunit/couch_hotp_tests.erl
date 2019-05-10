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

-module(couch_hotp_tests).

-include_lib("eunit/include/eunit.hrl").

hotp_test() ->
    Key = <<"12345678901234567890">>,
    ?assertEqual(755224, couch_hotp:generate(sha, Key, 0, 6)),
    ?assertEqual(287082, couch_hotp:generate(sha, Key, 1, 6)),
    ?assertEqual(359152, couch_hotp:generate(sha, Key, 2, 6)),
    ?assertEqual(969429, couch_hotp:generate(sha, Key, 3, 6)),
    ?assertEqual(338314, couch_hotp:generate(sha, Key, 4, 6)),
    ?assertEqual(254676, couch_hotp:generate(sha, Key, 5, 6)),
    ?assertEqual(287922, couch_hotp:generate(sha, Key, 6, 6)),
    ?assertEqual(162583, couch_hotp:generate(sha, Key, 7, 6)),
    ?assertEqual(399871, couch_hotp:generate(sha, Key, 8, 6)),
    ?assertEqual(520489, couch_hotp:generate(sha, Key, 9, 6)).
