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

-module(test_util).

-export([init_code_path/0]).
-export([source_file/1, build_file/1, config_files/0]).

init_code_path() ->
    code:load_abs("apps/couch/test/etap/etap").

source_file(Name) ->
    filename:join(["apps/couch", Name]).

build_file(Name) ->
    filename:join(["rel/overlay", Name]).

config_files() ->
    [
        build_file("etc/default.ini"),
        build_file("etc/local.ini"),
        source_file("test/etap/random_port.ini")
    ].

