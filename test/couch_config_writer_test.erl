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

% couch_config_writer module test suote

% Set up test suite
% ?MODULE_test() returns a list of functions
% that run the actual tests.
% todo, fix replace_existing_variable2 (i.e. reordering)
couch_config_writer_test() ->
    [
        fun() -> replace_existing_variable() end,
        fun() -> replace_existing_variable2() end,
        fun() -> replace_existing_variable3() end,
        fun() -> append_new_variable() end,
        fun() -> append_new_module() end,
        fun() -> overwrite_variable_further_down() end,
        fun() -> double_append_new_section_bug() end
    ].


% test functions
replace_existing_variable() ->
    % create file
    Contents = "[section]
variable = value

[another section]
another_var = another_value
",

    Expect = "[section]
variable = new_value

[another section]
another_var = another_value
",
    run_operation_and_compare_results(Contents, Expect, {{"section", "variable"}, "new_value"}).

replace_existing_variable2() ->
    % create file
    Contents = "[section]
variable = value
variable2 = value2
variable3 = value3
variable4 = value4

[another_section]
another_var = another_value
",

    Expect = "[section]
variable = value
variable2 = value2
variable3 = new_value3
variable4 = value4

[another_section]
another_var = another_value
",
    run_operation_and_compare_results(Contents, Expect, {{"section", "variable3"}, "new_value3"}).

replace_existing_variable3() ->
    % create file
    Contents = "[first_section]
var=val

[section]
variable = value
variable2 = value2
variable3 = value3
variable4 = value4

[another_section]
another_var = another_value
",

    Expect = "[first_section]
var=val

[section]
variable = value
variable2 = value2
variable3 = new_value3
variable4 = value4

[another_section]
another_var = another_value
",
    run_operation_and_compare_results(Contents, Expect, {{"section", "variable3"}, "new_value3"}).

append_new_variable() ->
    % create file
    Contents = "[section]
variable = value
variable2 = value

[another_section]
another_var = another_value
",

    Expect = "[section]
variable = value
variable2 = value

fantasy_variable = Citation Needed

[another_section]
another_var = another_value
",
    run_operation_and_compare_results(Contents, Expect, {{"section", "fantasy_variable"}, "Citation Needed"}).


append_new_module() ->
    % create file
    Contents = "[section]
variable = value

[another_section]
another_var = another_value
",

    Expect = "[section]
variable = value

[another_section]
another_var = another_value

[one_more_section]
favourite_food = cupcakes
",
    run_operation_and_compare_results(Contents, Expect, [{{"one_more_section", "favourite_food"}, "cupcakes"}]).

overwrite_variable_further_down() ->
    % create file
    Contents = "[section]
variable = value

[another_section]
another_var = another_value
",

    Expect = "[section]
variable = value

[another_section]
another_var = another_value

[erlang]
option = value

option2 = value2
",
    run_operation_and_compare_results(Contents, Expect, [{{"erlang", "option"}, "value"}, {{"erlang", "option2"}, "value2"}]).

double_append_new_section_bug() ->
    % create file
    Contents = "[section]
variable = value

[another_section]
another_var = another_value

[erlang]
option = value

option2 = value2
",

    Expect = "[section]
variable = value

[another_section]
another_var = another_value

[erlang]
option = value

option2 = value2
",
    run_operation_and_compare_results(Contents, Expect, [{{"another_section", "another_var"}, "another_value"}]).


run_operation_and_compare_results(Contents, Expect, Config) when not is_list(Config) ->
    run_operation_and_compare_results(Contents, Expect, [Config]);
run_operation_and_compare_results(Contents, Expect, Config) ->
    Filename = "local.ini",
    file:write_file(Filename, Contents),

    % call replace function
    [couch_config_writer:save_to_file(ConfigVar, Filename) || ConfigVar <- Config],

    % compare new file with expected file
    {ok, Result_} = file:read_file(Filename),
    Result = binary_to_list(Result_),

    % clean up
    % file:delete(Filename),

    Result = Expect.
