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
-export([source_file/1, build_file/1]).

srcdir() ->
    "@abs_top_srcdir@".

builddir() ->
    "@abs_top_builddir@".

init_code_path() ->
    Paths = ["etap", "couch", "oauth", "ibrowse", "mochiweb"],
    lists:foreach(fun(Name) ->
        code:add_pathz(filename:join([builddir(), "ebin", Name]))
    end, Paths).

source_file(Name) ->
    filename:join([srcdir(), Name]).

build_file(Name) ->
    filename:join([builddir(), Name]).
