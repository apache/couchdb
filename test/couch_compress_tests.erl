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

-module(couch_compress_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TERM, {[{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}]}).
-define(NONE, <<131,104,1,108,0,0,0,5,104,2,100,0,1,97,97,1,
    104,2,100,0,1,98,97,2,104,2,100,0,1,99,97,3,104,2,100,0,
    1,100,97,4,104,2,100,0,1,101,97,5,106>>).
-define(DEFLATE, <<131,80,0,0,0,48,120,218,203,96,204,97,96,
    96,96,205,96,74,97,96,76,76,100,4,211,73,137,76,96,58,57,
    145,25,76,167,36,178,128,233,212,68,214,44,0,212,169,9,51>>).
-define(SNAPPY, <<1,49,64,131,104,1,108,0,0,0,5,104,2,100,0,
    1,97,97,1,104,1,8,8,98,97,2,5,8,8,99,97,3,5,8,44,100,97,
    4,104,2,100,0,1,101,97,5,106>>).


compress_test_() ->
    [
        ?_assertEqual(?NONE, couch_compress:compress(?TERM, none)),
        ?_assertEqual(?DEFLATE, couch_compress:compress(?TERM, {deflate, 9})),
        ?_assertEqual(?SNAPPY, couch_compress:compress(?TERM, snappy))
    ].

decompress_test_() ->
    [
        ?_assertEqual(?TERM, couch_compress:decompress(?NONE)),
        ?_assertEqual(?TERM, couch_compress:decompress(?DEFLATE)),
        ?_assertEqual(?TERM, couch_compress:decompress(?SNAPPY))
    ].

recompress_test_() ->
    [
        ?_assertEqual(?DEFLATE, couch_compress:compress(?NONE, {deflate, 9})),
        ?_assertEqual(?SNAPPY, couch_compress:compress(?NONE, snappy)),
        ?_assertEqual(?NONE, couch_compress:compress(?DEFLATE, none)),
        ?_assertEqual(?SNAPPY, couch_compress:compress(?DEFLATE, snappy)),
        ?_assertEqual(?NONE, couch_compress:compress(?SNAPPY, none)),
        ?_assertEqual(?DEFLATE, couch_compress:compress(?SNAPPY, {deflate, 9}))
    ].

is_compressed_test_() ->
    [
        ?_assert(couch_compress:is_compressed(?NONE, none)),
        ?_assert(couch_compress:is_compressed(?DEFLATE, {deflate, 9})),
        ?_assert(couch_compress:is_compressed(?SNAPPY, snappy)),
        ?_assertNot(couch_compress:is_compressed(?NONE, {deflate, 0})),
        ?_assertNot(couch_compress:is_compressed(?NONE, {deflate, 9})),
        ?_assertNot(couch_compress:is_compressed(?NONE, snappy)),
        ?_assertNot(couch_compress:is_compressed(?DEFLATE, none)),
        ?_assertNot(couch_compress:is_compressed(?DEFLATE, snappy)),
        ?_assertNot(couch_compress:is_compressed(?SNAPPY, none)),
        ?_assertNot(couch_compress:is_compressed(?SNAPPY, {deflate, 9}))
    ].
