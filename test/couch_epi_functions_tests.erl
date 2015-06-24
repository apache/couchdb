% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_epi_functions_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(MODULE1(Name), "
    -export([foo/2, bar/0, inc/1]).
    foo(A1, A2) ->
        {A1, A2}.

    bar() ->
        [].

    inc(A) ->
        A + 1.
").

-define(MODULE2(Name), "
    -export([baz/1, inc/1]).
    baz(A1) ->
        A1.

    inc(A) ->
        A + 1.
").

setup() ->
    setup([{interval, 100}]).

setup(Opts) ->
    ServiceId = my_service,
    Module = my_test_module,
    ok = generate_module(Module, ?MODULE1(Module)),
    {ok, Pid} = couch_epi_functions:start_link(
        test_app, {epi_key, ServiceId}, {modules, [Module]}, Opts),
    ok = couch_epi_functions:wait(Pid),
    {Pid, Module, ServiceId, couch_epi_functions_gen:get_handle(ServiceId)}.

teardown({Pid, Module, _, _Handle}) ->
    code:purge(Module),
    couch_epi_functions:stop(Pid),
    catch meck:unload(compile),
    ok.

generate_module(Name, Body) ->
    Tokens = couch_epi_codegen:scan(Body),
    couch_epi_codegen:generate(Name, Tokens).

upgrade_release(Pid) ->
    sys:suspend(Pid),
    'ok' = sys:change_code(Pid, couch_epi_functions, 'undefined', []),
    sys:resume(Pid),
    ok.

epi_functions_test_() ->
    {
        "functions reload tests",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun ensure_reload_if_changed/1,
                fun ensure_no_reload_when_no_change/1
            ]
        }
    }.

epi_functions_manual_reload_test_() ->
    {
        "functions manual reload tests",
        {
            foreach,
            fun() -> setup([{interval, 10000}]) end,
            fun teardown/1,
            [
                fun ensure_reload_if_manually_triggered/1
            ]
        }
    }.

ensure_reload_if_manually_triggered({Pid, Module, _ServiceId, _Handle}) ->
    ?_test(begin
        ok = generate_module(Module, ?MODULE2(Module)),
        ok = meck:new(compile, [passthrough, unstick]),
        ok = meck:expect(compile, forms, fun(_, _) -> {error, reload} end),
        Result = couch_epi_functions:reload(Pid),
        ?assertMatch({error,{badmatch,{error,reload}}}, Result)
    end).

ensure_reload_if_changed({Pid, Module, ServiceId, _Handle}) ->
    ?_test(begin
        ?assertMatch(
            [{1, 2}],
            couch_epi_functions_gen:apply(ServiceId, foo, [1, 2], [])),
        ok = generate_module(Module, ?MODULE2(Module)),
        upgrade_release(Pid),
        ?assertMatch(
            [3],
            couch_epi_functions_gen:apply(ServiceId, baz, [3], []))
    end).

ensure_no_reload_when_no_change({Pid, _Module, ServiceId, _Handle}) ->
    ok = meck:new(compile, [passthrough, unstick]),
    ok = meck:expect(compile, forms, fun(_, _) ->
        {error, compile_should_not_be_called} end),
    ?_test(begin
        ?assertMatch(
            [{1, 2}],
            couch_epi_functions_gen:apply(ServiceId, foo, [1, 2], [])),
        upgrade_release(Pid),
        ?assertMatch(
            [],
            couch_epi_functions_gen:apply(ServiceId, baz, [3], []))
    end).
