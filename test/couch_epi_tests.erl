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

-module(couch_epi_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(DATA_FILE1, ?ABS_PATH("test/fixtures/app_data1.cfg")).
-define(DATA_FILE2, ?ABS_PATH("test/fixtures/app_data2.cfg")).

-export([notify_cb/5, save/3]).

-record(ctx, {file, handle, pid, kv, key}).

-define(TIMEOUT, 5000).

-define(MODULE1(Name), "
    -export([inc/2, fail/2]).

    inc(KV, A) ->
        Reply = A + 1,
        couch_epi_tests:save(KV, inc1, Reply),
        [KV, Reply].

    fail(KV, A) ->
        inc(KV, A).
").

-define(MODULE2(Name), "
    -export([inc/2, fail/2]).

    inc(KV, A) ->
        Reply = A + 1,
        couch_epi_tests:save(KV, inc2, Reply),
        [KV, Reply].

    fail(KV, _A) ->
        couch_epi_tests:save(KV, inc2, check_error),
        throw(check_error).
").

-define(DATA_MODULE1(Name), "
    -export([data/0]).

    data() ->
        [
            {[complex, key, 1], [
                {type, counter},
                {desc, foo}
            ]}
        ].
").

-define(DATA_MODULE2(Name), "
    -export([data/0]).

    data() ->
        [
            {[complex, key, 2], [
                {type, counter},
                {desc, bar}
            ]},
            {[complex, key, 1], [
                {type, counter},
                {desc, updated_foo}
            ]}
        ].
").

notify_cb(App, Key, OldData, Data, KV) ->
    save(KV, is_called, {App, Key, OldData, Data}).


setup(couch_epi_data_source) ->
    error_logger:tty(false),

    Key = {test_app, descriptions},
    File = ?tempfile(),
    {ok, _} = file:copy(?DATA_FILE1, File),
    application:start(couch_epi),
    {ok, Pid} = couch_epi_data_source:start_link(
        test_app, {epi_key, Key}, {file, File}, [{interval, 100}]),
    ok = couch_epi_data_source:wait(Pid),
    KV = state_storage(),
    #ctx{
        file = File,
        key = Key,
        handle = couch_epi:get_handle(Key),
        kv = KV,
        pid = Pid};
setup(couch_epi_data) ->
    error_logger:tty(false),

    Key = {test_app, descriptions},
    application:start(couch_epi),
    ok = generate_module(provider, ?DATA_MODULE1(provider)),

    {ok, Pid} = couch_epi_data:start_link(
        test_app, {epi_key, Key}, provider, []),
    ok = couch_epi_data:wait(Pid),
    KV = state_storage(),
    #ctx{
        key = Key,
        handle = couch_epi:get_handle(Key),
        kv = KV,
        pid = Pid};
setup(couch_epi_functions) ->
    Key = my_service,
    error_logger:tty(false),

    application:start(couch_epi),
    ok = generate_module(provider1, ?MODULE1(provider1)),
    ok = generate_module(provider2, ?MODULE2(provider2)),

    {ok, Pid} = couch_epi_functions:start_link(
        test_app, {epi_key, Key}, {modules, [provider1, provider2]},
        [{interval, 100}]),
    ok = couch_epi_functions:wait(Pid),
    KV = state_storage(),
    #ctx{
        key = Key,
        handle = couch_epi:get_handle(Key),
        kv = KV,
        pid = Pid};
setup(_Opts) ->
    setup(couch_epi_functions).

teardown(Module, #ctx{pid = Pid} = Ctx) when is_atom(Module) ->
    Module:stop(Pid),
    teardown(Ctx);
teardown(_Opts, #ctx{pid = Pid} = Ctx) ->
    couch_epi_functions:stop(Pid),
    teardown(Ctx).

teardown(#ctx{file = File} = Ctx) when File /= undefined ->
    file:delete(File),
    teardown(Ctx#ctx{file = undefined});
teardown(#ctx{kv = KV}) ->
    call(KV, stop),
    application:stop(couch_epi),
    ok.

upgrade_release(Pid, Module) ->
    sys:suspend(Pid),
    'ok' = sys:change_code(Pid, Module, 'undefined', []),
    sys:resume(Pid),
    ok.

epi_config_update_test_() ->
    Funs = [
        fun ensure_notified_when_changed/2,
        fun ensure_not_notified_when_no_change/2,
        fun ensure_not_notified_when_unsubscribed/2
    ],
    Modules= [
        couch_epi_data,
        couch_epi_data_source,
        couch_epi_functions
    ],
    {
        "config update tests",
        [make_case("Check notifications for: ", Modules, Funs)]
    }.

epi_data_source_test_() ->
    Funs = [
        fun check_dump/2,
        fun check_get/2,
        fun check_get_value/2,
        fun check_by_key/2,
        fun check_by_source/2,
        fun check_keys/2,
        fun check_subscribers/2
    ],
    Modules= [
        couch_epi_data,
        couch_epi_data_source
    ],
    {
        "epi data API tests",
        [make_case("Check query API for: ", Modules, Funs)]
    }.


epi_apply_test_() ->
    {
        "epi dispatch tests",
        {
            foreach,
            fun() -> setup(couch_epi_functions) end,
            fun teardown/1,
            [
                fun check_pipe/1,
                fun check_broken_pipe/1,
                fun ensure_fail/1,
                fun ensure_fail_pipe/1
            ]
        }
    }.


epi_subscription_test_() ->
    Funs = [
        fun ensure_unsubscribe_when_caller_die/2
    ],
    Modules= [
        couch_epi_data,
        couch_epi_data_source,
        couch_epi_functions
    ],
    {
        "epi subscription tests",
        [make_case("Check subscription API for: ", Modules, Funs)]
    }.


epi_reload_test_() ->
    Modules= [
        couch_epi_data,
        couch_epi_data_source,
        couch_epi_functions
    ],
    {
        "epi reload tests",
        {
            foreachx,
            fun setup/1,
            fun teardown/2,
            [{M, fun ensure_reloaded/2} || M <- Modules]
        }
    }.

apply_options_test_() ->
    Funs = [fun ensure_apply_is_called/2],
    make_case("Apply with options: ", valid_options_permutations(), Funs).


make_case(Msg, P, Funs) ->
    [{format_case_name(Msg, Case), [
        {
            foreachx, fun setup/1, fun teardown/2,
            [
                 {Case, Fun} || Fun <- Funs
            ]
        }
    ]} || Case <- P].

format_case_name(Msg, Case) ->
    lists:flatten(Msg ++ io_lib:format("~p", [Case])).

valid_options_permutations() ->
    [
        [],
        [ignore_errors],
        [pipe],
        [pipe, ignore_errors],
        [concurrent],
        [concurrent, ignore_errors]
    ].

ensure_notified_when_changed(couch_epi_functions, #ctx{key = Key} = Ctx) ->
    ?_test(begin
        subscribe(Ctx, test_app, Key),
        update(couch_epi_functions, Ctx),
        timer:sleep(200),
        Result = get(Ctx, is_called),
        Expected = {test_app, Key,
            {modules, [provider1, provider2]},
            {modules, [provider1, provider2]}},
        ?assertMatch({ok, Expected}, Result),
        ok
    end);
ensure_notified_when_changed(Module, #ctx{key = Key} = Ctx) ->
    ?_test(begin
        subscribe(Ctx, test_app, Key),
        update(Module, Ctx),
        timer:sleep(200),
        ExpectedData = lists:usort([
            {[complex, key, 1], [{type, counter}, {desc, updated_foo}]},
            {[complex, key, 2], [{type, counter}, {desc, bar}]}
        ]),
        Result = get(Ctx, is_called),
        ?assertMatch({ok, {test_app, Key, {data, _}, {data, _}}}, Result),
        {ok, {test_app, Key, {data, OldData}, {data, Data}}} = Result,
        ?assertMatch(ExpectedData, lists:usort(Data)),
        ?assertMatch(
            [{[complex, key, 1], [{type, counter}, {desc, foo}]}],
            lists:usort(OldData))
    end).

ensure_not_notified_when_no_change(_Module, #ctx{key = Key} = Ctx) ->
    ?_test(begin
        subscribe(Ctx, test_app, Key),
        timer:sleep(200),
        ?assertMatch(error, get(Ctx, is_called))
    end).

ensure_not_notified_when_unsubscribed(Module, #ctx{key = Key} = Ctx) ->
    ?_test(begin
        SubscriptionId = subscribe(Ctx, test_app, Key),
        couch_epi:unsubscribe(SubscriptionId),
        timer:sleep(100),
        update(Module, Ctx),
        timer:sleep(200),
        ?assertMatch(error, get(Ctx, is_called))
    end).

ensure_apply_is_called(Opts, #ctx{handle = Handle, kv = KV, key = Key} = Ctx) ->
    ?_test(begin
        couch_epi:apply(Handle, Key, inc, [KV, 2], Opts),
        maybe_wait(Opts),
        ?assertMatch({ok, _}, get(Ctx, inc1)),
        ?assertMatch({ok, _}, get(Ctx, inc2)),
        ok
    end).

check_pipe(#ctx{handle = Handle, kv = KV, key = Key}) ->
    ?_test(begin
        Result = couch_epi:apply(Handle, Key, inc, [KV, 2], [pipe]),
        ?assertMatch([KV, 4], Result),
        ok
    end).

check_broken_pipe(#ctx{handle = Handle, kv = KV, key = Key} = Ctx) ->
    ?_test(begin
        Result = couch_epi:apply(Handle, Key, fail, [KV, 2], [pipe, ignore_errors]),
        ?assertMatch([KV, 3], Result),
        ?assertMatch([3, check_error], pipe_state(Ctx)),
        ok
    end).

ensure_fail_pipe(#ctx{handle = Handle, kv = KV, key = Key}) ->
    ?_test(begin
        ?assertThrow(check_error,
            couch_epi:apply(Handle, Key, fail, [KV, 2], [pipe])),
        ok
    end).

ensure_fail(#ctx{handle = Handle, kv = KV, key = Key}) ->
    ?_test(begin
        ?assertThrow(check_error,
            couch_epi:apply(Handle, Key, fail, [KV, 2], [])),
        ok
    end).

ensure_unsubscribe_when_caller_die(_Module, #ctx{key = Key} = Ctx) ->
    ?_test(begin
        spawn(fun() ->
            subscribe(Ctx, test_app, Key)
        end),
        timer:sleep(200),
        ?assertMatch(error, get(Ctx, is_called))
    end).


pipe_state(Ctx) ->
    Trace = [get(Ctx, inc1), get(Ctx, inc2)],
    lists:usort([State || {ok, State} <- Trace]).

check_dump(_Module, #ctx{handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [[{type, counter}, {desc, foo}]],
            couch_epi:dump(Handle))
    end).

check_get(_Module, #ctx{handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [[{type, counter}, {desc, foo}]],
            couch_epi:get(Handle, [complex,key, 1]))
    end).

check_get_value(_Module, #ctx{handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [{type, counter}, {desc, foo}],
            couch_epi:get_value(Handle, test_app, [complex,key, 1]))
    end).

check_by_key(_Module, #ctx{handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [{[complex, key, 1],
                [{test_app, [{type, counter}, {desc, foo}]}]}],
            couch_epi:by_key(Handle)),
        ?assertMatch(
            [{test_app, [{type, counter}, {desc, foo}]}],
            couch_epi:by_key(Handle, [complex, key, 1]))
    end).

check_by_source(_Module, #ctx{handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [{test_app,
                [{[complex,key, 1], [{type, counter}, {desc, foo}]}]}],
            couch_epi:by_source(Handle)),
        ?assertMatch(
            [{[complex,key, 1], [{type, counter}, {desc, foo}]}],
            couch_epi:by_source(Handle, test_app))
    end).

check_keys(_Module, #ctx{handle = Handle}) ->
    ?_assertMatch([[complex,key,1]], couch_epi:keys(Handle)).

check_subscribers(_Module, #ctx{handle = Handle}) ->
    ?_assertMatch([test_app], couch_epi:subscribers(Handle)).


ensure_reloaded(Module, #ctx{pid = Pid, key = Key} = Ctx) ->
    ?_test(begin
        subscribe(Ctx, test_app, Key),
        update_definitions(Module, Ctx),
        Module:reload(Pid),
        timer:sleep(200),
        Result = get(Ctx, is_called),
        ?assertNotMatch(error, Result)
    end).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

generate_module(Name, Body) ->
    Tokens = couch_epi_codegen:scan(Body),
    couch_epi_codegen:generate(Name, Tokens).

update(Module, #ctx{pid = Pid} = Ctx) ->
    update_definitions(Module, Ctx),
    upgrade_release(Pid, Module).

update_definitions(couch_epi_data_source, #ctx{file = File}) ->
    {ok, _} = file:copy(?DATA_FILE2, File),
    ok;
update_definitions(couch_epi_data, #ctx{}) ->
    ok = generate_module(provider, ?DATA_MODULE2(provider));
update_definitions(couch_epi_functions, #ctx{}) ->
    ok = generate_module(provider1, ?MODULE2(provider1)).



subscribe(#ctx{kv = Kv}, App, Key) ->
    {ok, Pid} = couch_epi:subscribe(App, Key, ?MODULE, notify_cb, Kv),
    call(Kv, empty),
    Pid.

maybe_wait(Opts) ->
    case lists:member(concurrent, Opts) of
        true ->
            timer:sleep(100);
        false ->
            ok
    end.

%% ------------
%% State tracer

save(Kv, Key, Value) ->
    call(Kv, {set, Key, Value}).

get(#ctx{kv = Kv}, Key) ->
    call(Kv, {get, Key}).

call(Server, Msg) ->
    Ref = make_ref(),
    Server ! {{Ref, self()}, Msg},
    receive
        {reply, Ref, Reply} ->
            Reply
    after ?TIMEOUT ->
        {error, {timeout, Msg}}
    end.

reply({Ref, From}, Msg) ->
    From ! {reply, Ref, Msg}.

state_storage() ->
    spawn_link(fun() -> state_storage(dict:new()) end).

state_storage(Dict) ->
    receive
        {From, {set, Key, Value}} ->
            reply(From, ok),
            state_storage(dict:store(Key, Value, Dict));
        {From, {get, Key}} ->
            reply(From, dict:find(Key, Dict)),
            state_storage(Dict);
        {From, empty} ->
            reply(From, ok),
            state_storage(dict:new());
        {From, stop} ->
            reply(From, ok)
    end.
