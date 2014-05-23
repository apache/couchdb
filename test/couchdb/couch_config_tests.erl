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

-module(couch_config_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(TIMEOUT, 1000).


setup() ->
    {ok, Pid} = couch_config:start_link(?CONFIG_CHAIN),
    Pid.

teardown(Pid) ->
    couch_config:stop(),
    erlang:monitor(process, Pid),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout_error, config_stop})
    end.


couch_config_test_() ->
    {
        "CouchDB config tests",
        [
            couch_config_get_tests(),
            couch_config_set_tests(),
            couch_config_del_tests()
        ]
    }.

couch_config_get_tests() ->
    {
        "Config get tests",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                should_load_all_configs(),
                should_locate_daemons_section(),
                should_locate_mrview_handler(),
                should_return_undefined_atom_on_missed_section(),
                should_return_undefined_atom_on_missed_option(),
                should_return_custom_default_value_on_missed_option(),
                should_only_return_default_on_missed_option(),
                should_get_binary_option()
            ]
        }
    }.

couch_config_set_tests() ->
    {
        "Config set tests",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                should_update_option(),
                should_create_new_section(),
                should_set_binary_option()
            ]
        }
    }.

couch_config_del_tests() ->
    {
        "Config deletion tests",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                should_return_undefined_atom_after_option_deletion(),
                should_be_ok_on_deleting_unknown_options(),
                should_delete_binary_option()
            ]
        }
    }.


should_load_all_configs() ->
    ?_assert(length(couch_config:all()) > 0).

should_locate_daemons_section() ->
    ?_assert(length(couch_config:get("daemons")) > 0).

should_locate_mrview_handler() ->
    ?_assertEqual("{couch_mrview_http, handle_view_req}",
                  couch_config:get("httpd_design_handlers", "_view")).

should_return_undefined_atom_on_missed_section() ->
    ?_assertEqual(undefined,
                  couch_config:get("foo", "bar")).

should_return_undefined_atom_on_missed_option() ->
    ?_assertEqual(undefined,
                  couch_config:get("httpd", "foo")).

should_return_custom_default_value_on_missed_option() ->
    ?_assertEqual("bar",
                  couch_config:get("httpd", "foo", "bar")).

should_only_return_default_on_missed_option() ->
    ?_assertEqual("0",
                  couch_config:get("httpd", "port", "bar")).

should_get_binary_option() ->
    ?_assertEqual(<<"baz">>,
                  couch_config:get(<<"foo">>, <<"bar">>, <<"baz">>)).

should_update_option() ->
    ?_assertEqual("severe",
        begin
            ok = couch_config:set("log", "level", "severe", false),
            couch_config:get("log", "level")
        end).

should_create_new_section() ->
    ?_assertEqual("bang",
        begin
            undefined = couch_config:get("new_section", "bizzle"),
            ok = couch_config:set("new_section", "bizzle", "bang", false),
            couch_config:get("new_section", "bizzle")
        end).

should_set_binary_option() ->
    ?_assertEqual(<<"baz">>,
        begin
            ok = couch_config:set(<<"foo">>, <<"bar">>, <<"baz">>, false),
            couch_config:get(<<"foo">>, <<"bar">>)
        end).

should_return_undefined_atom_after_option_deletion() ->
    ?_assertEqual(undefined,
        begin
            ok = couch_config:delete("log", "level", false),
            couch_config:get("log", "level")
        end).

should_be_ok_on_deleting_unknown_options() ->
    ?_assertEqual(ok,
        begin
            couch_config:delete("zoo", "boo", false)
        end).

should_delete_binary_option() ->
    ?_assertEqual(undefined,
        begin
            ok = couch_config:set(<<"foo">>, <<"bar">>, <<"baz">>, false),
            ok = couch_config:delete(<<"foo">>, <<"bar">>, false),
            couch_config:get(<<"foo">>, <<"bar">>)
        end).
