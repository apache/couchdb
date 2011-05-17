#!/usr/bin/env escript
%% -*- erlang -*-

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

default_config() ->
    test_util:build_file("etc/couchdb/default_dev.ini").

main(_) ->
    test_util:init_code_path(),
    etap:plan(12),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    % start couch_config with default
    couch_config:start_link([default_config()]),


    % Check that we can get values


    etap:fun_is(
        fun(List) -> length(List) > 0 end,
        couch_config:all(),
        "Data was loaded from the INI file."
    ),

    etap:fun_is(
        fun(List) -> length(List) > 0 end,
        couch_config:get("daemons"),
        "There are settings in the [daemons] section of the INI file."
    ),

    etap:is(
        couch_config:get("httpd_design_handlers", "_view"),
        "{couch_httpd_view, handle_view_req}",
        "The {httpd_design_handlers, view} is the expected default."
    ),

    etap:is(
        couch_config:get("httpd", "foo", "bar"),
        "bar",
        "Returns the default when key doesn't exist in config."
    ),

    etap:is(
        couch_config:get("httpd", "foo"),
        undefined,
        "The default default is the atom 'undefined'."
    ),

    etap:is(
        couch_config:get("httpd", "port", "bar"),
        "5984",
        "Only returns the default when the config setting does not exist."
    ),


    % Check that setting values works.


    ok = couch_config:set("log", "level", "severe", false),

    etap:is(
        couch_config:get("log", "level"),
        "severe",
        "Non persisted changes take effect."
    ),

    etap:is(
        couch_config:get("new_section", "bizzle"),
        undefined,
        "Section 'new_section' does not exist."
    ),

    ok = couch_config:set("new_section", "bizzle", "bang", false),

    etap:is(
        couch_config:get("new_section", "bizzle"),
        "bang",
        "New section 'new_section' was created for a new key/value pair."
    ),


    % Check that deleting works


    ok = couch_config:delete("new_section", "bizzle", false),
    etap:is(
        couch_config:get("new_section", "bizzle"),
        undefined,
        "Deleting sets the value to \"\""
    ),


    % Check ge/set/delete binary strings

    ok = couch_config:set(<<"foo">>, <<"bar">>, <<"baz">>, false),
    etap:is(
        couch_config:get(<<"foo">>, <<"bar">>),
        <<"baz">>,
        "Can get and set with binary section and key values."
    ),
    ok = couch_config:delete(<<"foo">>, <<"bar">>, false),
    etap:is(
        couch_config:get(<<"foo">>, <<"bar">>),
        undefined,
        "Deleting with binary section/key pairs sets the value to \"\""
    ),

    ok.
