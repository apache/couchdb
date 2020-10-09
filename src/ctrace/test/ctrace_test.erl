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

-module(ctrace_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ctrace/src/ctrace.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).


ctrace_config_test_() ->
    {
        "Test ctrace",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            [
                ?TDEF(is_enabled_cached),
                ?TDEF(simple_with_span),
                ?TDEF(with_span_exception),
                ?TDEF(simple_start_finish_span),
                ?TDEF(op_name_from_fun),
                ?TDEF(skipped_when_disabled),
                ?TDEF(include_or_skip_on_sampled),
                ?TDEF(set_tags_on_start_span),
                ?TDEF(set_time_on_start_span),
                ?TDEF(skip_on_filtered),
                ?TDEF(simple_child_span),
                ?TDEF(update_tags),
                ?TDEF(update_logs),
                ?TDEF(current_span_getters),
                ?TDEF(create_external_span),
                ?TDEF(use_external_span)
            ]
        }
    }.


setup() ->
    Ctx = test_util:start_couch([ctrace]),

    config_set("tracing", "enabled", "true"),

    Filter = "(#{}) -> true",
    config_set("tracing.filters", "all", Filter),

    ctrace_config:update(),

    MainReporter = passage_tracer_registry:get_reporter(?MAIN_TRACER),

    {MainReporter, Ctx}.


cleanup({MainReporter, Ctx}) ->
    passage_tracer_registry:set_reporter(?MAIN_TRACER, MainReporter),
    test_util:stop_couch(Ctx).


is_enabled_cached() ->
    erase(?IS_ENABLED_KEY),
    Result = ctrace:is_enabled(),
    ?assertEqual(Result, get(?IS_ENABLED_KEY)),
    ?assert(is_boolean(Result)),

    % Fake override to test that we're using the cached value
    put(?IS_ENABLED_KEY, not Result),
    ?assertEqual(not Result, ctrace:is_enabled()),

    % Revert to original to not mess with other tests
    put(?IS_ENABLED_KEY, Result).


simple_with_span() ->
    set_self_reporter(),

    Result = ctrace:with_span(zing, fun() ->
        a_result
    end),

    ?assertEqual(a_result, Result),

    receive
        {span, Span} ->
            ?assertEqual(zing, passage_span:get_operation_name(Span))
    end.


with_span_exception() ->
    set_self_reporter(),

    Result = try
        ctrace:with_span(zab, fun() ->
            throw(foo)
        end)
    catch T:R ->
        {T, R}
    end,

    ?assertEqual({throw, foo}, Result),

    receive
        {span, Span} ->
            ?assertEqual(zab, passage_span:get_operation_name(Span)),
            ?assertMatch(
                [
                    {#{
                        'error.kind' := throw,
                        event := error,
                        message := foo,
                        stack := [_ | _]
                    }, _TimeStamp}
                ],
                passage_span:get_logs(Span)
            )
    end.


simple_start_finish_span() ->
    set_self_reporter(),

    ctrace:start_span(foo),
    ctrace:finish_span(),

    receive
        {span, Span} ->
            ?assertEqual(foo, passage_span:get_operation_name(Span))
    end.


op_name_from_fun() ->
    set_self_reporter(),

    ctrace:start_span(fun ctrace:match/2),
    ctrace:finish_span(),

    receive
        {span, Span} ->
            OpName = passage_span:get_operation_name(Span),
            ?assertEqual('ctrace:match/2', OpName)
    end.


skipped_when_disabled() ->
    set_self_reporter(),

    ?assert(not ctrace:has_span()),
    ctrace:start_span(foo),
    ?assert(ctrace:has_span()),
    ctrace:finish_span(),
    ?assert(not ctrace:has_span()),
    receive {span, _Span} -> ok end,

    IsEnabled = get(?IS_ENABLED_KEY),
    try
        put(?IS_ENABLED_KEY, false),

        ?assert(not ctrace:has_span()),
        ctrace:start_span(foo),
        ?assert(not ctrace:has_span()),
        ctrace:finish_span(),
        ?assert(not ctrace:has_span())
    after
        put(?IS_ENABLED_KEY, IsEnabled)
    end.


set_tags_on_start_span() ->
    set_self_reporter(),

    Tags = #{foo => bar},
    ctrace:start_span(bang, [{tags, Tags}]),
    ctrace:finish_span(),

    receive
        {span, Span} ->
            ?assertEqual(bang, passage_span:get_operation_name(Span)),
            ?assertEqual(#{foo => bar}, passage_span:get_tags(Span))
    end.


set_time_on_start_span() ->
    set_self_reporter(),

    Time = os:timestamp(),
    timer:sleep(100),
    ctrace:start_span(bang, [{time, Time}]),
    ctrace:finish_span(),

    receive
        {span, Span} ->
            ?assertEqual(Time, passage_span:get_start_time(Span))
    end.


skip_on_filtered() ->
    set_self_reporter(),

    config_set("tracing.filters", "do_skip", "(#{}) -> false"),
    ctrace_config:update(),

    ?assert(not ctrace:has_span()),
    ctrace:start_span(do_skip),
    ?assert(not ctrace:has_span()),
    ctrace:finish_span(),
    ?assert(not ctrace:has_span()).


include_or_skip_on_sampled() ->
    set_self_reporter(),

    config_set("tracing.filters", "sample", "(#{}) -> 0.0"),
    ctrace_config:update(),

    ?assert(not ctrace:has_span()),
    ctrace:start_span(sample),
    ?assert(not ctrace:has_span()),
    ctrace:finish_span(),
    ?assert(not ctrace:has_span()),

    config_set("tracing.filters", "sample", "(#{}) -> 1.0"),
    ctrace_config:update(),

    ?assert(not ctrace:has_span()),
    ctrace:start_span(sample),
    ?assert(ctrace:has_span()),
    ctrace:finish_span(),
    ?assert(not ctrace:has_span()),

    receive
        {span, Span1} ->
            ?assertEqual(sample, passage_span:get_operation_name(Span1))
    end,

    config_set("tracing.filters", "sample", "(#{}) -> 0.5"),
    ctrace_config:update(),

    ?assert(not ctrace:has_span()),
    ctrace:start_span(sample),
    IsSampled = ctrace:has_span(),
    ctrace:finish_span(),
    ?assert(not ctrace:has_span()),

    if not IsSampled -> ok; true ->
        receive
            {span, Span2} ->
                ?assertEqual(
                        sample,
                        passage_span:get_operation_name(Span2)
                    )
        end
    end.


simple_child_span() ->
    set_self_reporter(),

    ctrace:start_span(parent),
    ctrace:start_span(child),
    ctrace:finish_span(),
    ctrace:finish_span(),

    receive
        {span, CSpan} ->
            ?assertEqual(child, passage_span:get_operation_name(CSpan))
    end,

    receive
        {span, PSpan} ->
            ?assertEqual(parent, passage_span:get_operation_name(PSpan))
    end.


update_tags() ->
    set_self_reporter(),

    ctrace:start_span(foo, [{tags, #{foo => bar}}]),
    ctrace:tag(#{bango => bongo}),
    ctrace:finish_span(),

    receive
        {span, Span} ->
            ?assertEqual(
                    #{foo => bar, bango => bongo},
                    passage_span:get_tags(Span)
                )
    end.


update_logs() ->
    set_self_reporter(),

    ctrace:start_span(foo),
    ctrace:log(#{foo => bar}),
    ctrace:finish_span(),

    receive
        {span, Span1} ->
            ?assertMatch(
                    [{#{foo := bar}, _TimeStamp}],
                    passage_span:get_logs(Span1)
                )
    end,

    ctrace:start_span(foo),
    ctrace:log(fun() ->
        #{foo => baz}
    end),
    ctrace:finish_span(),

    receive
        {span, Span2} ->
            ?assertMatch(
                    [{#{foo := baz}, _TimeStamp}],
                    passage_span:get_logs(Span2)
                )
    end.


current_span_getters() ->
    ?assertEqual(false, ctrace:has_span()),
    ?assertEqual(undefined, ctrace:tags()),
    ?assertEqual(undefined, ctrace:refs()),
    ?assertEqual(undefined, ctrace:operation_name()),
    ?assertEqual(undefined, ctrace:trace_id()),
    ?assertEqual(undefined, ctrace:span_id()),
    ?assertEqual(undefined, ctrace:tracer()),
    ?assertEqual(undefined, ctrace:context()),

    ctrace:start_span(parent),
    ctrace:start_span(child, [{tags, #{foo => oof}}]),

    ?assertEqual(true, ctrace:has_span()),
    ?assertEqual(#{foo => oof, origin => <<"parent">>}, ctrace:tags()),
    ?assertMatch([{child_of, _} | _], ctrace:refs()),
    ?assertEqual(child, ctrace:operation_name()),
    ?assert(is_integer(ctrace:trace_id())),
    ?assert(is_integer(ctrace:span_id())),
    ?assertEqual(?MAIN_TRACER, ctrace:tracer()),
    ?assertNotEqual(undefined, ctrace:context()),

    ctrace:finish_span(),
    ctrace:finish_span(),

    receive
        {span, CSpan} ->
            ?assertEqual(child, passage_span:get_operation_name(CSpan))
    end,

    receive
        {span, PSpan} ->
            ?assertEqual(parent, passage_span:get_operation_name(PSpan))
    end.


create_external_span() ->
    Span1 = ctrace:external_span(1, 2, 3),
    Ctx1 = passage_span:get_context(Span1),
    ?assertEqual(1, jaeger_passage_span_context:get_trace_id(Ctx1)),
    ?assertEqual(2, jaeger_passage_span_context:get_span_id(Ctx1)),

    Span2 = ctrace:external_span(42, undefined, undefined),
    Ctx2 = passage_span:get_context(Span2),
    ?assertEqual(42, jaeger_passage_span_context:get_trace_id(Ctx2)),
    ?assert(is_integer(jaeger_passage_span_context:get_span_id(Ctx2))).


use_external_span() ->
    Parent = ctrace:external_span(1, 2, 3),

    ?assert(not ctrace:has_span()),
    ctrace:start_span(foo, [{root, Parent}]),
    ?assert(ctrace:has_span()),
    ctrace:finish_span(),
    ?assert(not ctrace:has_span()),

    receive
        {span, Span} ->
            Ctx = passage_span:get_context(Span),
            TraceId = jaeger_passage_span_context:get_trace_id(Ctx),
            ?assertEqual(1, TraceId)
    end.


config_set(Section, Key, Value) ->
    PrevValue = config:get(Section, Key),
    if Value == PrevValue -> ok; true ->
        config:set(Section, Key, Value, false),
        test_util:wait_other_value(fun() ->
            config:get(Section, Key)
        end, PrevValue)
    end.


set_self_reporter() ->
    SelfReporter = passage_reporter_process:new(self(), span),
    passage_tracer_registry:set_reporter(?MAIN_TRACER, SelfReporter),
    test_util:wait_value(fun() ->
        {ok, Result} = passage_tracer_registry:get_reporter(?MAIN_TRACER),
        Result
    end, SelfReporter).