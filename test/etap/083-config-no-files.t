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
    etap:plan(3),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_config:start_link([]),

    etap:fun_is(
        fun(KVPairs) -> length(KVPairs) == 0 end,
        couch_config:all(),
        "No INI files specified returns 0 key/value pairs."
    ),

    ok = couch_config:set("httpd", "port", "80", false),

    etap:is(
        couch_config:get("httpd", "port"),
        "80",
        "Created a new non-persisted k/v pair."
    ),

    ok = couch_config:set("httpd", "bind_address", "127.0.0.1"),
    etap:is(
        couch_config:get("httpd", "bind_address"),
        "127.0.0.1",
        "Asking for a persistent key/value pair doesn't choke."
    ),

    ok.
