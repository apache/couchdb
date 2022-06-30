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

-module(couch_tests_app_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    [mock(application)].

teardown(Mocks) ->
    [unmock(Mock) || Mock <- Mocks].

%% ------------------------------------------------------------------
%% Test callbacks definitions
%% ------------------------------------------------------------------

dummy_setup() ->
    couch_tests:new(
        ?MODULE,
        dummy_setup,
        fun(_Fixture, Ctx) -> Ctx end,
        fun(_Fixture, Ctx) -> Ctx end
    ).

setup1(Arg1) ->
    couch_tests:new(
        ?MODULE,
        setup1,
        fun(Fixture, Ctx0) ->
            Ctx1 = couch_tests:start_applications([asn1], Ctx0),
            couch_tests:set_state(Fixture, Ctx1, {Arg1})
        end,
        fun(_Fixture, Ctx) ->
            couch_tests:stop_applications([asn1], Ctx)
        end
    ).

setup2(Arg1, Arg2) ->
    couch_tests:new(
        ?MODULE,
        setup2,
        fun(Fixture, Ctx0) ->
            Ctx1 = couch_tests:start_applications([public_key], Ctx0),
            couch_tests:set_state(Fixture, Ctx1, {Arg1, Arg2})
        end,
        fun(_Fixture, Ctx) ->
            Ctx
        end
    ).

couch_tests_test_() ->
    {
        "couch_tests tests",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                {"chained setup", fun chained_setup/0}
            ]
        }
    }.

chained_setup() ->
    ?assert(meck:validate(application)),
    ?assertEqual([], history(application, start)),
    Ctx0 = couch_tests:setup(
        [
            setup1(foo),
            dummy_setup(),
            setup2(bar, baz)
        ],
        [],
        []
    ),

    ?assertEqual([asn1, public_key], history(application, start)),
    ?assertEqual([asn1, public_key], couch_tests:get(started_apps, Ctx0)),
    ?assertEqual([], couch_tests:get(stopped_apps, Ctx0)),

    Ctx1 = couch_tests:teardown(Ctx0),

    ?assertEqual([public_key, asn1], history(application, stop)),
    ?assertEqual([], couch_tests:get(started_apps, Ctx1)),
    ?assertEqual([public_key, asn1], couch_tests:get(stopped_apps, Ctx1)),

    ok.

mock(application) ->
    ok = meck:new(application, [unstick, passthrough]),
    ok = meck:expect(application, start, fun(_) -> ok end),
    ok = meck:expect(application, stop, fun(_) -> ok end),
    meck:validate(application),
    application.

unmock(application) ->
    catch meck:unload(application).

history(Module, Function) ->
    Self = self(),
    [
        A
     || {Pid, {M, F, [A]}, _Result} <- meck:history(Module),
        Pid =:= Self,
        M =:= Module,
        F =:= Function
    ].
