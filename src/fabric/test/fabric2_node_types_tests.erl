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

-module(fabric2_node_types_tests).


-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


node_types_test_() ->
    {
        "Test node types",
        setup,
        fun() ->
            os:putenv("COUCHDB_NODE_TYPE_FOO", "false"),
            os:putenv("COUCHDB_NODE_TYPE_BAZ", "true"),
            os:putenv("COUCHDB_NODE_TYPE_ZIG", ""),
            % erlfdb, rexi and mem3 are all dependent apps for fabric. We make
            % sure to start them so when fabric is started during the test it
            % already has its dependencies
            test_util:start_couch([erlfdb, rexi, mem3, ctrace, fabric])
        end,
        fun(Ctx) ->
            test_util:stop_couch(Ctx),
            application:unset_env(fabric, node_types),
            os:unsetenv("COUCHDB_NODE_TYPE_FOO"),
            os:unsetenv("COUCHDB_NODE_TYPE_BAZ"),
            os:unsetenv("COUCHDB_NODE_TYPE_ZIG")
        end,
        with([
            ?TDEF(basics),
            ?TDEF(os_env_priority)
        ])
    }.


basics(_) ->
    % default is true for new types
    ?assert(fabric2_node_types:is_type(some_new_node_type)),

    % defined in os env
    ?assert(fabric2_node_types:is_type(baz)),
    ?assert(not fabric2_node_types:is_type(foo)),
    ?assert(fabric2_node_types:is_type(zig)),

    % defined in app env
    application:set_env(fabric, node_types, [{zag, true}, {bam, false}]),
    ?assert(fabric2_node_types:is_type(zag)),
    ?assert(not fabric2_node_types:is_type(bam)).


os_env_priority(_) ->
    % os env takes precedence
    application:set_env(fabric, node_types, [{foo, true}, {baz, false}]),
    ?assert(not fabric2_node_types:is_type(foo)),
    ?assert(fabric2_node_types:is_type(baz)).
