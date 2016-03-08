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

-module(chttpd_plugin_tests).

-export([
    before_request/1,
    after_request/2,
    handle_error/1,
    before_response/4,
    before_serve_file/5
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

%% couch_epi_plugin behaviour

app() -> test_app.
providers() -> [{chttpd, ?MODULE}].
services() -> [].
data_providers() -> [].
data_subscriptions() -> [].
processes() -> [].
notify(_, _, _) -> ok.

setup() ->
    application:stop(couch_epi), % in case it's already running from other tests...
    application:unload(couch_epi),
    ok = application:load(couch_epi),
    ok = application:set_env(couch_epi, plugins, [?MODULE]), % only this plugin
    ok = application:start(couch_epi).

teardown(_) ->
    ok = application:stop(couch_epi),
    ok = application:unload(couch_epi).

before_request({true, Id}) -> [{true, [{before_request, Id}]}];
before_request({false, Id}) -> [{false, Id}];
before_request({fail, Id}) -> throw({before_request, Id}).

after_request({true, Id}, A) -> [{true, [{after_request, Id}]}, A];
after_request({false, Id}, A) ->  [{false, Id}, A];
after_request({fail, Id}, _A) -> throw({after_request, Id}).

handle_error({true, Id}) -> [{true, [{handle_error, Id}]}];
handle_error({false, Id}) -> [{false, Id}];
handle_error({fail, Id}) -> throw({handle_error, Id}).

before_response({true, Id}, A, B, C) ->
    [{true, [{before_response, Id}]}, A, B, C];
before_response({false, Id}, A, B, C) ->
    [{false, Id}, A, B, C];
before_response({fail, Id}, _A, _B, _C) ->
    throw({before_response, Id}).

before_serve_file({true, Id}, A, B, C, D) ->
    [{true, [{before_serve_file, Id}]}, A, B, C, D];
before_serve_file({false, Id}, A, B, C, D) ->
    [{false, Id}, A, B, C, D];
before_serve_file({fail, _Id}, _A, _B, _C, _D) ->
    throw(before_serve_file).

callback_test_() ->
    {
        "callback tests",
        {
            setup, fun setup/0, fun teardown/1,
            [
                fun before_request_match/0,
                fun before_request_no_match/0,
                fun before_request_throw/0,

                fun after_request_match/0,
                fun after_request_no_match/0,
                fun after_request_throw/0,

                fun handle_error_match/0,
                fun handle_error_no_match/0,
                fun handle_error_throw/0,

                fun before_response_match/0,
                fun before_response_no_match/0,
                fun before_response_throw/0,

                fun before_serve_file_match/0,
                fun before_serve_file_no_match/0,
                fun before_serve_file_throw/0
            ]
        }
    }.


before_request_match() ->
    ?assertEqual(
        {ok, {true, [{before_request, foo}]}},
        chttpd_plugin:before_request({true, foo})).

before_request_no_match() ->
    ?assertEqual(
        {ok, {false, foo}},
        chttpd_plugin:before_request({false, foo})).

before_request_throw() ->
    ?assertThrow(
        {before_request, foo},
        chttpd_plugin:before_request({fail, foo})).


after_request_match() ->
    ?assertEqual(
        {ok, bar},
        chttpd_plugin:after_request({true, foo}, bar)).

after_request_no_match() ->
    ?assertEqual(
        {ok, bar},
        chttpd_plugin:after_request({false, foo}, bar)).

after_request_throw() ->
    ?assertThrow(
        {after_request, foo},
        chttpd_plugin:after_request({fail, foo}, bar)).


handle_error_match() ->
    ?assertEqual(
        {true, [{handle_error, foo}]},
        chttpd_plugin:handle_error({true, foo})).

handle_error_no_match() ->
    ?assertEqual(
        {false, foo},
        chttpd_plugin:handle_error({false, foo})).

handle_error_throw() ->
    ?assertThrow(
        {handle_error, foo},
        chttpd_plugin:handle_error({fail, foo})).

before_response_match() ->
    ?assertEqual(
        {ok, {{true, [{before_response, foo}]}, 1, 2, 3}},
        chttpd_plugin:before_response({true, foo}, 1, 2, 3)).

before_response_no_match() ->
    ?assertEqual(
        {ok, {{false, foo}, 1, 2, 3}},
        chttpd_plugin:before_response({false, foo}, 1, 2, 3)).

before_response_throw() ->
    ?assertThrow(
        {before_response, foo},
        chttpd_plugin:before_response({fail, foo}, 1, 2, 3)).


before_serve_file_match() ->
    ?assertEqual(
        {ok, {{true, [{before_serve_file, foo}]}, 1, 2, 3, 4}},
        chttpd_plugin:before_serve_file({true, foo}, 1, 2, 3, 4)).

before_serve_file_no_match() ->
    ?assertEqual(
        {ok, {{false, foo}, 1, 2, 3, 4}},
        chttpd_plugin:before_serve_file({false, foo}, 1, 2, 3, 4)).

before_serve_file_throw() ->
    ?assertThrow(
        before_serve_file,
        chttpd_plugin:before_serve_file({fail, foo}, 1, 2, 3, 4)).
