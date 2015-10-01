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

-module(couch_db_plugin_tests).

-export([
    validate_dbname/2,
    before_doc_update/2,
    after_doc_read/2,
    validate_docid/1,
    check_is_admin/1,
    on_delete/2
]).

-export([ %% couch_epi_plugin behaviour
    app/0,
    providers/0,
    services/0,
    data_providers/0,
    data_subscriptions/0,
    processes/0,
    notify/3
]).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-record(ctx, {pid, handle}).

%% couch_epi_plugin behaviour

app() -> test_app.
providers() -> [{couch_db, ?MODULE}].
services() -> [].
data_providers() -> [].
data_subscriptions() -> [].
processes() -> [].
notify(_, _, _) -> ok.

start_epi() ->
    application:load(couch_epi),
    Plugins = application:get_env(couch_epi, plugins, []),
    ok = application:set_env(couch_epi, plugins, append_if_missing(Plugins, ?MODULE)),
    ok = application:start(couch_epi).

append_if_missing(List, Value) ->
    case lists:member(Value, List) of
        true -> List;
        false -> [Value | List]
    end.

setup() ->
    error_logger:tty(false),
    start_epi(),
    #ctx{handle = couch_epi:get_handle(couch_db)}.

teardown(#ctx{}) ->
    Plugins = application:get_env(couch_epi, plugins, []),
    application:set_env(couch_epi, plugins, Plugins -- [?MODULE]),
    application:stop(couch_epi),
    ok.

validate_dbname({true, _Db}, _) -> true;
validate_dbname({false, _Db}, _) -> false;
validate_dbname({fail, _Db}, _) -> throw(validate_dbname).

before_doc_update({fail, _Doc}, _Db) -> throw(before_doc_update);
before_doc_update({true, Doc}, Db) -> [{true, [before_doc_update|Doc]}, Db];
before_doc_update({false, Doc}, Db) -> [{false, Doc}, Db].

after_doc_read({fail, _Doc}, _Db) -> throw(after_doc_read);
after_doc_read({true, Doc}, Db) -> [{true, [after_doc_read|Doc]}, Db];
after_doc_read({false, Doc}, Db) -> [{false, Doc}, Db].

validate_docid({true, _Id}) -> true;
validate_docid({false, _Id}) -> false;
validate_docid({fail, _Id}) -> throw(validate_docid).

check_is_admin({true, _Db}) -> true;
check_is_admin({false, _Db}) -> false;
check_is_admin({fail, _Db}) -> throw(check_is_admin).

on_delete(true, _Opts) -> true;
on_delete(false, _Opts) -> false;
on_delete(fail, _Opts) -> throw(on_delete).

callback_test_() ->
    {
        "callback tests",
        {
            foreach, fun setup/0, fun teardown/1,
            [
                fun validate_dbname_match/0,
                fun validate_dbname_no_match/0,
                fun validate_dbname_throw/0,

                fun before_doc_update_match/0,
                fun before_doc_update_no_match/0,
                fun before_doc_update_throw/0,

                fun after_doc_read_match/0,
                fun after_doc_read_no_match/0,
                fun after_doc_read_throw/0,

                fun validate_docid_match/0,
                fun validate_docid_no_match/0,
                fun validate_docid_throw/0,

                fun check_is_admin_match/0,
                fun check_is_admin_no_match/0,
                fun check_is_admin_throw/0,

                fun on_delete_match/0,
                fun on_delete_no_match/0,
                fun on_delete_throw/0
            ]
        }
    }.


validate_dbname_match() ->
    ?_assertMatch(
        {true, [validate_dbname, db]},
        couch_db_plugin:validate_dbname({true, [db]}, db)).

validate_dbname_no_match() ->
    ?_assertMatch(
        {false, [db]},
        couch_db_plugin:validate_dbname({false, [db]}, db)).

validate_dbname_throw() ->
    ?_assertThrow(
        validate_dbname,
        couch_db_plugin:validate_dbname({fail, [db]}, db)).

before_doc_update_match() ->
    ?_assertMatch(
        {true, [before_doc_update, doc]},
        couch_db_plugin:before_doc_update(#db{}, {true, [doc]})).

before_doc_update_no_match() ->
    ?_assertMatch(
        {false, [doc]},
        couch_db_plugin:before_doc_update(#db{}, {false, [doc]})).

before_doc_update_throw() ->
    ?_assertThrow(
        before_doc_update,
        couch_db_plugin:before_doc_update(#db{}, {fail, [doc]})).


after_doc_read_match() ->
    ?_assertMatch(
        {true, [after_doc_read, doc]},
        couch_db_plugin:after_doc_read(#db{}, {true, [doc]})).

after_doc_read_no_match() ->
    ?_assertMatch(
        {false, [doc]},
        couch_db_plugin:after_doc_read(#db{}, {false, [doc]})).

after_doc_read_throw() ->
    ?_assertThrow(
        after_doc_read,
        couch_db_plugin:after_doc_read(#db{}, {fail, [doc]})).


validate_docid_match() ->
    ?_assertMatch(
        {true, [validate_docid, doc]},
        couch_db_plugin:validate_docid({true, [doc]})).

validate_docid_no_match() ->
    ?_assertMatch(
        {false, [doc]},
        couch_db_plugin:validate_docid({false, [doc]})).

validate_docid_throw() ->
    ?_assertThrow(
        validate_docid,
        couch_db_plugin:validate_docid({fail, [doc]})).


check_is_admin_match() ->
    ?_assertMatch(
        true,
        couch_db_plugin:check_is_admin({true, [db]})).

check_is_admin_no_match() ->
    ?assertMatch(
        false,
        couch_db_plugin:check_is_admin({false, [db]})).

check_is_admin_throw() ->
    ?assertThrow(
        check_is_admin,
        couch_db_plugin:check_is_admin({fail, [db]})).

on_delete_match() ->
    ?_assertMatch(
        true,
        couch_db_plugin:on_delete(true, [])).

on_delete_no_match() ->
    ?_assertMatch(
        false,
        couch_db_plugin:on_delete(false, [])).

on_delete_throw() ->
    ?assertThrow(
        on_delete,
        couch_db_plugin:on_delete(fail, [])).
