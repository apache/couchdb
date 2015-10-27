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

-export([notify_cb/4, save/3]).

-record(ctx, {file, handle, pid, kv, key, modules = []}).

-define(TIMEOUT, 5000).

-define(temp_atom,
    fun() ->
        {A, B, C} = erlang:now(),
        list_to_atom(lists:flatten(io_lib:format("~p~p~p", [A, B, C])))
    end).

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

%% ------------------------------------------------------------------
%% couch_epi_plugin behaviour
%% ------------------------------------------------------------------

plugin_module([KV, Spec]) when is_tuple(Spec) ->
    SpecStr = io_lib:format("~w", [Spec]),
    KVStr = "'" ++ atom_to_list(KV) ++ "'",
    "
        -compile([export_all]).

        app() -> test_app.
        providers() ->
            [].

        services() ->
            [].

        data_providers() ->
            [
                {{test_app, descriptions}, " ++ SpecStr ++ ", [{interval, 100}]}
            ].

        data_subscriptions() ->
            [
                {test_app, descriptions}
            ].

        processes() -> [].

        notify(Key, OldData, Data) ->
            couch_epi_tests:notify_cb(Key, OldData, Data, " ++ KVStr ++ ").
    ";
plugin_module([KV, Provider]) when is_atom(Provider) ->
    KVStr = "'" ++ atom_to_list(KV) ++ "'",
    "
        -compile([export_all]).

        app() -> test_app.
        providers() ->
            [
                {my_service, " ++ atom_to_list(Provider) ++ "}
            ].

        services() ->
            [
                {my_service, " ++ atom_to_list(Provider) ++ "}
            ].

        data_providers() ->
            [].

        data_subscriptions() ->
            [].

        processes() -> [].

        notify(Key, OldData, Data) ->
            couch_epi_tests:notify_cb(Key, OldData, Data, " ++ KVStr ++ ").
    ".


notify_cb(Key, OldData, Data, KV) ->
    save(KV, is_called, {Key, OldData, Data}).

start_epi(Plugins) ->
    application:load(couch_epi),
    PluginsModules = lists:map(fun({Module, Body}) ->
        ok = generate_module(Module, Body),
        Module
    end, Plugins),
    application:set_env(couch_epi, plugins, PluginsModules),
    application:start(couch_epi).

setup(data_file) ->
    error_logger:tty(false),

    Key = {test_app, descriptions},
    File = ?tempfile(),
    {ok, _} = file:copy(?DATA_FILE1, File),
    KV = start_state_storage(),

    ok = start_epi([{provider_epi, plugin_module([KV, {file, File}])}]),

    Pid = whereis(couch_epi:get_handle(Key)),


    #ctx{
        file = File,
        key = Key,
        handle = couch_epi:get_handle(Key),
        kv = KV,
        pid = Pid};
setup(data_module) ->
    error_logger:tty(false),

    Key = {test_app, descriptions},

    ok = generate_module(provider, ?DATA_MODULE1(provider)),
    KV = start_state_storage(),

    ok = start_epi([{provider_epi, plugin_module([KV, {module, provider}])}]),

    Pid = whereis(couch_epi:get_handle(Key)),
    Handle = couch_epi:get_handle(Key),

    #ctx{
        key = Key,
        handle = Handle,
        modules = [Handle, provider],
        kv = KV,
        pid = Pid};
setup(functions) ->
    Key = my_service,
    error_logger:tty(false),

    ok = generate_module(provider1, ?MODULE1(provider1)),
    ok = generate_module(provider2, ?MODULE2(provider2)),

    KV = start_state_storage(),

    ok = start_epi([
        {provider_epi1, plugin_module([KV, provider1])},
        {provider_epi2, plugin_module([KV, provider2])}
    ]),

    Pid = whereis(couch_epi:get_handle(Key)),
    Handle = couch_epi:get_handle(Key),

    #ctx{
        key = Key,
        handle = Handle,
        modules = [Handle, provider1, provider2],
        kv = KV,
        pid = Pid};
setup({options, _Opts}) ->
    setup(functions).

teardown(_Case, #ctx{} = Ctx) ->
    teardown(Ctx).

teardown(#ctx{file = File} = Ctx) when File /= undefined ->
    file:delete(File),
    teardown(Ctx#ctx{file = undefined});
teardown(#ctx{kv = KV}) ->
    call(KV, stop),
    application:stop(couch_epi),
    ok.

upgrade_release(Pid, Modules) ->
    sys:suspend(Pid),
    [ok = sys:change_code(Pid, M, undefined, []) || M <- Modules],
    sys:resume(Pid),
    ok.

epi_config_update_test_() ->
    Funs = [
        fun ensure_notified_when_changed/2,
        fun ensure_not_notified_when_no_change/2
    ],
    Cases = [
        data_file,
        data_module,
        functions
    ],
    {
        "config update tests",
        [make_case("Check notifications for: ", Cases, Funs)]
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
    Cases = [
        data_file,
        data_module
    ],
    {
        "epi data API tests",
        [make_case("Check query API for: ", Cases, Funs)]
    }.


epi_apply_test_() ->
    {
        "epi dispatch tests",
        {
            foreach,
            fun() -> setup(functions) end,
            fun teardown/1,
            [
                fun check_pipe/1,
                fun check_broken_pipe/1,
                fun ensure_fail/1,
                fun ensure_fail_pipe/1
            ]
        }
    }.

epi_providers_order_test_() ->
    {
        "epi providers' order test",
        {
            foreach,
            fun() -> setup(functions) end,
            fun teardown/1,
            [
                fun check_providers_order/1
            ]
        }
    }.


epi_reload_test_() ->
    Cases = [
        data_file,
        data_module,
        functions
    ],
    Funs = [
        fun ensure_reload_if_manually_triggered/2,
        fun ensure_reload_if_changed/2,
        fun ensure_no_reload_when_no_change/2
    ],
    {
        "epi reload tests",
        [make_case("Check reload for: ", Cases, Funs)]
    }.

apply_options_test_() ->
    Funs = [fun ensure_apply_is_called/2],
    Setups = {options, valid_options_permutations()},
    {
        "apply options tests",
        [make_case("Apply with options: ", Setups, Funs)]
    }.


make_case(Msg, {Tag, P}, Funs) ->
    Cases = [{Tag, Case} || Case <- P],
    make_case(Msg, Cases, Funs);
make_case(Msg, P, Funs) ->
    [{format_case_name(Msg, Case), [
        {
            foreachx, fun setup/1, fun teardown/2,
            [
                 {Case, make_fun(Fun, 2)} || Fun <- Funs
            ]
        }
    ]} || Case <- P].

make_fun(Fun, Arity) ->
    {arity, A} = lists:keyfind(arity, 1, erlang:fun_info(Fun)),
    make_fun(Fun, Arity, A).

make_fun(Fun, A, A) -> Fun;
make_fun(Fun, 2, 1) -> fun(_, A) -> Fun(A) end;
make_fun(Fun, 1, 2) -> fun(A) -> Fun(undefined, A) end.

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

ensure_notified_when_changed(functions, #ctx{key = Key} = Ctx) ->
    ?_test(begin
        subscribe(Ctx, test_app, Key),
        update(functions, Ctx),
        timer:sleep(200),
        Result = get(Ctx, is_called),
        ExpectedDefs = [
            {provider1,[{inc,2},{fail,2}]},
            {provider2,[{inc,2},{fail,2}]}
        ],
        ?assertEqual({ok, {Key, ExpectedDefs, ExpectedDefs}}, Result),
        ok
    end);
ensure_notified_when_changed(Case, #ctx{key = Key} = Ctx) ->
    ?_test(begin
        subscribe(Ctx, test_app, Key),
        update(Case, Ctx),
        timer:sleep(200),
        ExpectedData = lists:usort([
            {[complex, key, 1], [{type, counter}, {desc, updated_foo}]},
            {[complex, key, 2], [{type, counter}, {desc, bar}]}
        ]),
        Result = get(Ctx, is_called),
        ?assertMatch({ok, {Key, _OldData, _Data}}, Result),
        {ok, {Key, OldData, Data}} = Result,
        ?assertMatch(ExpectedData, lists:usort(Data)),
        ?assertMatch(
            [{[complex, key, 1], [{type, counter}, {desc, foo}]}],
            lists:usort(OldData))
    end).

ensure_not_notified_when_no_change(_Case, #ctx{key = Key} = Ctx) ->
    ?_test(begin
        subscribe(Ctx, test_app, Key),
        timer:sleep(200),
        ?assertMatch(error, get(Ctx, is_called))
    end).

ensure_apply_is_called({options, Opts}, #ctx{handle = Handle, kv = KV, key = Key} = Ctx) ->
    ?_test(begin
        couch_epi:apply(Handle, Key, inc, [KV, 2], Opts),
        maybe_wait(Opts),
        ?assertMatch({ok, _}, get(Ctx, inc1)),
        ?assertMatch({ok, _}, get(Ctx, inc2)),
        ok
    end);
ensure_apply_is_called(undefined, #ctx{} = Ctx) ->
    ensure_apply_is_called({options, []}, Ctx).

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

pipe_state(Ctx) ->
    Trace = [get(Ctx, inc1), get(Ctx, inc2)],
    lists:usort([State || {ok, State} <- Trace]).

check_dump(_Case, #ctx{handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [[{type, counter}, {desc, foo}]],
            couch_epi:dump(Handle))
    end).

check_get(_Case, #ctx{handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [[{type, counter}, {desc, foo}]],
            couch_epi:get(Handle, [complex,key, 1]))
    end).

check_get_value(_Case, #ctx{handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [{type, counter}, {desc, foo}],
            couch_epi:get_value(Handle, test_app, [complex,key, 1]))
    end).

check_by_key(_Case, #ctx{handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [{[complex, key, 1],
                [{test_app, [{type, counter}, {desc, foo}]}]}],
            couch_epi:by_key(Handle)),
        ?assertMatch(
            [{test_app, [{type, counter}, {desc, foo}]}],
            couch_epi:by_key(Handle, [complex, key, 1]))
    end).

check_by_source(_Case, #ctx{handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [{test_app,
                [{[complex,key, 1], [{type, counter}, {desc, foo}]}]}],
            couch_epi:by_source(Handle)),
        ?assertMatch(
            [{[complex,key, 1], [{type, counter}, {desc, foo}]}],
            couch_epi:by_source(Handle, test_app))
    end).

check_keys(_Case, #ctx{handle = Handle}) ->
    ?_assertMatch([[complex,key,1]], couch_epi:keys(Handle)).

check_subscribers(_Case, #ctx{handle = Handle}) ->
    ?_assertMatch([test_app], couch_epi:subscribers(Handle)).


ensure_reload_if_manually_triggered(Case, #ctx{pid = Pid, key = Key} = Ctx) ->
    ?_test(begin
        subscribe(Ctx, test_app, Key),
        update_definitions(Case, Ctx),
        couch_epi_module_keeper:reload(Pid),
        timer:sleep(50),
        ?assertNotEqual(error, get(Ctx, is_called))
    end).

ensure_reload_if_changed(data_file =  Case,
        #ctx{key = Key, handle = Handle} = Ctx) ->
    ?_test(begin
        Version = Handle:version(),
        subscribe(Ctx, test_app, Key),
        update_definitions(Case, Ctx),
        timer:sleep(250),
        ?assertNotEqual(Version, Handle:version()),
        ?assertNotEqual(error, get(Ctx, is_called))
    end);
ensure_reload_if_changed(Case,
        #ctx{key = Key, handle = Handle} = Ctx) ->
    ?_test(begin
        Version = Handle:version(),
        subscribe(Ctx, test_app, Key),
        update(Case, Ctx),
        ?assertNotEqual(Version, Handle:version()),
        timer:sleep(100), %% Allow some time for notify to be called
        ?assertNotEqual(error, get(Ctx, is_called))
    end).

ensure_no_reload_when_no_change(functions,
        #ctx{pid = Pid, key = Key, handle = Handle, modules = Modules} = Ctx) ->
    ?_test(begin
        Version = Handle:version(),
        subscribe(Ctx, test_app, Key),
        upgrade_release(Pid, Modules),
        ?assertEqual(Version, Handle:version()),
        ?assertEqual(error, get(Ctx, is_called))
    end);
ensure_no_reload_when_no_change(_Case,
        #ctx{key = Key, handle = Handle} = Ctx) ->
    ?_test(begin
        Version = Handle:version(),
        subscribe(Ctx, test_app, Key),
        timer:sleep(450),
        ?assertEqual(Version, Handle:version()),
        ?assertEqual(error, get(Ctx, is_called))
    end).

check_providers_order(#ctx{handle = Handle, kv = KV, key = Key} = Ctx) ->
    ?_test(begin
        Result = couch_epi:apply(Handle, Key, inc, [KV, 2], [pipe]),
        ?assertMatch([KV, 4], Result),
        Order = [element(2, get(Ctx, K)) || K <- [inc1, inc2]],
        ?assertEqual(Order, [3, 4]),
        ok
    end).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

generate_module(Name, Body) ->
    Tokens = couch_epi_codegen:scan(Body),
    couch_epi_codegen:generate(Name, Tokens).

update(Case, #ctx{pid = Pid, modules = Modules} = Ctx) ->
    update_definitions(Case, Ctx),
    upgrade_release(Pid, Modules).

update_definitions(data_file, #ctx{file = File}) ->
    {ok, _} = file:copy(?DATA_FILE2, File),
    ok;
update_definitions(data_module, #ctx{}) ->
    ok = generate_module(provider, ?DATA_MODULE2(provider));
update_definitions(functions, #ctx{}) ->
    ok = generate_module(provider1, ?MODULE2(provider1)).

subscribe(#ctx{kv = Kv}, _App, _Key) ->
    call(Kv, empty),
    ok.

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

start_state_storage() ->
    Pid = state_storage(),
    Name = ?temp_atom(),
    register(Name, Pid),
    Name.

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
