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

% couch_config module test suite

% Set up test suite
% ?MODULE_test() returns a list of functions
% that run the actual tests.
couch_config_test() ->
    [
        fun() -> store_strings() end
    ].

% test functions

store_strings() ->
    Filename = "test.ini",
    file:write_file(Filename, ""),

    Key = "foo",
    Value = "bar",

    {ok, Proc} = couch_config:start_link([Filename]),

    couch_config:set("test_module", Key, Value),
    Result = couch_config:get("test_module", Key),
    couch_config:delete("test_module", Key),

    exit(Proc, kill),
    receive {'EXIT', Proc, _} -> ok end,

    % clean up
    file:delete(Filename),

    Value = Result.
