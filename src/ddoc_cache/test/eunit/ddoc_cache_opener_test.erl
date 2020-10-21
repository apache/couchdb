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

-module(ddoc_cache_opener_test).


-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ddoc_cache_test.hrl").


empty_hull_test() ->
    InitExpect = {ok, nil},
    TermExpect = ok,
    CallExpect = {stop, {invalid_call, foo}, {invalid_call, foo}, baz},
    CastExpect = {stop, {invalid_cast, foo}, bar},
    InfoExpect = {stop, {invalid_info, foo}, bar},
    CCExpect = {ok, bar},
    ?assertEqual(InitExpect, ddoc_cache_opener:init(foo)),
    ?assertEqual(TermExpect, ddoc_cache_opener:terminate(foo, bar)),
    ?assertEqual(CallExpect, ddoc_cache_opener:handle_call(foo, bar, baz)),
    ?assertEqual(CastExpect, ddoc_cache_opener:handle_cast(foo, bar)),
    ?assertEqual(InfoExpect, ddoc_cache_opener:handle_info(foo, bar)),
    ?assertEqual(CCExpect, ddoc_cache_opener:code_change(foo, bar, baz)).
