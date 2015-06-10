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

-record(ctx, {
    file, data_handle, data_source,
    functions_handle, functions_source, kv}).

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


notify_cb(App, Key, OldData, Data, KV) ->
    save(KV, is_called, {App, Key, OldData, Data}).

setup() ->
    error_logger:tty(false),

    Key = {test_app, descriptions},
    File = ?tempfile(),
    {ok, _} = file:copy(?DATA_FILE1, File),
    application:start(couch_epi),
    {ok, DataPid} = couch_epi_data_source:start_link(
        test_app, {epi_key, Key}, {file, File}, [{interval, 100}]),
    ok = couch_epi_data_source:wait(DataPid),

    ok = generate_module(provider1, ?MODULE1(provider1)),
    ok = generate_module(provider2, ?MODULE2(provider2)),

    {ok, FunctionsPid} = couch_epi_functions:start_link(
        test_app, {epi_key, my_service}, {modules, [provider1, provider2]},
        [{interval, 100}]),
    ok = couch_epi_functions:wait(FunctionsPid),
    KV = state_storage(),
    #ctx{
        file = File,
        data_handle = couch_epi:get_handle(Key),
        functions_handle = couch_epi:get_handle(my_service),
        kv = KV,
        data_source = DataPid,
        functions_source = FunctionsPid}.

setup(_Opts) ->
    setup().

teardown(_, #ctx{} = Ctx) ->
    teardown(Ctx).

teardown(#ctx{data_source = DataPid,
              functions_source = FunctionsPid,
              kv = KV, file = File}) ->
    file:delete(File),
    couch_epi_data_source:stop(DataPid),
    couch_epi_functions:stop(FunctionsPid),
    catch meck:unload(compile),
    call(KV, stop),
    application:stop(couch_epi),
    ok.

epi_config_update_test_() ->
    Funs = [
        fun ensure_notified_when_changed/2,
        fun ensure_not_notified_when_no_change/2,
        fun ensure_not_notified_when_unsubscribed/2
    ],
    {
        "config update tests",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [make_case("Check notifications for: ", [module, file], Funs)]
        }
    }.

epi_data_source_test_() ->
    {
        "epi data_source tests",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun check_dump/1,
                fun check_get/1,
                fun check_get_value/1,
                fun check_by_key/1,
                fun check_by_source/1,
                fun check_keys/1,
                fun check_subscribers/1
            ]
        }
    }.


epi_apply_test_() ->
    {
        "epi dispatch tests",
        {
            foreach,
            fun setup/0,
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
    {
        "epi subscription tests",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun ensure_unsubscribe_when_caller_die/1
            ]
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

ensure_notified_when_changed(file, #ctx{file = File} = Ctx) ->
    ?_test(begin
        Key = {test_app, descriptions},
        subscribe(Ctx, test_app, Key),
        update(file, Ctx),
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
    end);
ensure_notified_when_changed(module, #ctx{file = File} = Ctx) ->
    ?_test(begin
        subscribe(Ctx, test_app, my_service),
        update(module, Ctx),
        timer:sleep(200),
        Result = get(Ctx, is_called),
        Expected = {test_app, my_service,
            {modules, [provider1, provider2]},
            {modules, [provider1, provider2]}},
        ?assertMatch({ok, Expected}, Result),
        ok
    end).

ensure_not_notified_when_no_change(Case, #ctx{} = Ctx) ->
    ?_test(begin
        Key = {test_app, descriptions},
        subscribe(Ctx, test_app, Key),
        timer:sleep(200),
        ?assertMatch(error, get(Ctx, is_called))
    end).

ensure_not_notified_when_unsubscribed(Case, #ctx{file = File} = Ctx) ->
    ?_test(begin
        Key = {test_app, descriptions},
        SubscriptionId = subscribe(Ctx, test_app, Key),
        couch_epi:unsubscribe(SubscriptionId),
        timer:sleep(100),
        update(Case, Ctx),
        timer:sleep(200),
        ?assertMatch(error, get(Ctx, is_called))
    end).

ensure_apply_is_called(Opts, #ctx{functions_handle = Handle, kv = KV} = Ctx) ->
    ?_test(begin
        couch_epi:apply(Handle, my_service, inc, [KV, 2], Opts),
        maybe_wait(Opts),
        ?assertMatch({ok, _}, get(Ctx, inc1)),
        ?assertMatch({ok, _}, get(Ctx, inc2)),
        ok
    end).

check_pipe(#ctx{functions_handle = Handle, kv = KV}) ->
    ?_test(begin
        Result = couch_epi:apply(Handle, my_service, inc, [KV, 2], [pipe]),
        ?assertMatch([KV, 4], Result),
        ok
    end).

check_broken_pipe(#ctx{functions_handle = Handle, kv = KV} = Ctx) ->
    ?_test(begin
        Result = couch_epi:apply(Handle, my_service, fail, [KV, 2], [pipe, ignore_errors]),
        ?assertMatch([KV, 3], Result),
        ?assertMatch([3, check_error], pipe_state(Ctx)),
        ok
    end).

ensure_fail_pipe(#ctx{functions_handle = Handle, kv = KV}) ->
    ?_test(begin
        ?assertThrow(check_error,
            couch_epi:apply(Handle, my_service, fail, [KV, 2], [pipe])),
        ok
    end).

ensure_fail(#ctx{functions_handle = Handle, kv = KV}) ->
    ?_test(begin
        ?assertThrow(check_error,
            couch_epi:apply(Handle, my_service, fail, [KV, 2], [])),
        ok
    end).

ensure_unsubscribe_when_caller_die(#ctx{} = Ctx) ->
    ?_test(begin
        Key = {test_app, descriptions},
        spawn(fun() ->
            subscribe(Ctx, test_app, Key)
        end),
        %%update(file, Ctx),
        timer:sleep(200),
        ?assertMatch(error, get(Ctx, is_called))
    end).


pipe_state(Ctx) ->
    Trace = [get(Ctx, inc1), get(Ctx, inc2)],
    lists:usort([State || {ok, State} <- Trace]).

check_dump(#ctx{data_handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [[{type, counter}, {desc, foo}]],
            couch_epi:dump(Handle))
    end).

check_get(#ctx{data_handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [[{type, counter}, {desc, foo}]],
            couch_epi:get(Handle, [complex,key, 1]))
    end).

check_get_value(#ctx{data_handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [{type, counter}, {desc, foo}],
            couch_epi:get_value(Handle, test_app, [complex,key, 1]))
    end).

check_by_key(#ctx{data_handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [{[complex, key, 1],
                [{test_app, [{type, counter}, {desc, foo}]}]}],
            couch_epi:by_key(Handle)),
        ?assertMatch(
            [{test_app, [{type, counter}, {desc, foo}]}],
            couch_epi:by_key(Handle, [complex, key, 1]))
    end).

check_by_source(#ctx{data_handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [{test_app,
                [{[complex,key, 1], [{type, counter}, {desc, foo}]}]}],
            couch_epi:by_source(Handle)),
        ?assertMatch(
            [{[complex,key, 1], [{type, counter}, {desc, foo}]}],
            couch_epi:by_source(Handle, test_app))
    end).

check_keys(#ctx{data_handle = Handle}) ->
    ?_assertMatch([[complex,key,1]], couch_epi:keys(Handle)).

check_subscribers(#ctx{data_handle = Handle}) ->
    ?_assertMatch([test_app], couch_epi:subscribers(Handle)).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

generate_module(Name, Body) ->
    Tokens = couch_epi_codegen:scan(Body),
    couch_epi_codegen:generate(Name, Tokens).

update(module, #ctx{}) ->
    ok = generate_module(provider1, ?MODULE2(provider1));
update(file, #ctx{file = File}) ->
    {ok, _} = file:copy(?DATA_FILE2, File),
    ok.

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
