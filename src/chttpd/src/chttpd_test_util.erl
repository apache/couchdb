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

-module(chttpd_test_util).

-export([start_couch/0, start_couch/1, stop_couch/1]).

-include_lib("couch/include/couch_eunit.hrl").


start_couch() ->
    start_couch(?CONFIG_CHAIN).

start_couch(IniFiles) ->
    test_util:start_couch(IniFiles, [chttpd]).

stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).
