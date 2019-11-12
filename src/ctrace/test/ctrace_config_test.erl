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

-module(ctrace_config_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ctrace/src/ctrace.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).


ctrace_config_test_() ->
    {
        "Test ctrace_config",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            [
                ?TDEF(ensure_main_tracer_started),
                ?TDEF(ensure_all_supported),
                ?TDEF(handle_all_syntax_error_supported),
                ?TDEF(ensure_filter_updated),
                ?TDEF(ensure_filter_removed),
                ?TDEF(ensure_bad_filter_ignored)
            ]
        }
    }.


setup() ->
    Ctx = test_util:start_couch([ctrace]),

    config_set("tracing", "enabled", "true"),

    Filter = "(#{method := M}) when M == get -> true",
    config_set("tracing.filters", "base", Filter),

    ctrace_config:update(),

    Ctx.


cleanup(Ctx) ->
    test_util:stop_couch(Ctx).


ensure_main_tracer_started() ->
    ?assertMatch(
            {ok, _},
            passage_tracer_registry:get_reporter(?MAIN_TRACER)
        ).


ensure_all_supported() ->
    config:delete("tracing.filters", "all", false),
    test_util:wait_value(fun() ->
        config:get("tracing.filters", "all")
    end, undefined),
    ctrace_config:update(),

    ?assertEqual(false, ctrace:match(bam, #{gee => whiz})),

    Filter = "(#{}) -> true",
    config_set("tracing.filters", "all", Filter),
    ctrace_config:update(),

    ?assertEqual(true, ctrace:match(bam, #{gee => whiz})).


handle_all_syntax_error_supported() ->
    couch_log:error("XKCD: TEST START", []),
    config:delete("tracing.filters", "all", false),
    test_util:wait_value(fun() ->
        config:get("tracing.filters", "all")
    end, undefined),
    ctrace_config:update(),

    ?assertEqual(false, ctrace:match(bam, #{gee => whiz})),

    Filter = "( -> true.",
    config_set("tracing.filters", "all", Filter),
    ctrace_config:update(),

    % If there's a syntax in the `all` handler
    % then we default to not generating traces
    ?assertEqual(false, ctrace:match(bam, #{gee => whiz})),

    couch_log:error("XKCD: TEST END", []),
    config:delete("tracing.filters", "all", false).


ensure_filter_updated() ->
    Filter1 = "(#{}) -> true",
    config_set("tracing.filters", "bing", Filter1),
    ctrace_config:update(),

    ?assertEqual(true, ctrace:match(bing, #{gee => whiz})),

    Filter2 = "(#{}) -> false",
    config_set("tracing.filters", "bing", Filter2),
    ctrace_config:update(),

    ?assertEqual(false, ctrace:match(bing, #{gee => whiz})).


ensure_filter_removed() ->
    Filter = "(#{}) -> true",
    config_set("tracing.filters", "bango", Filter),
    ctrace_config:update(),

    ?assertEqual(true, ctrace:match(bango, #{gee => whiz})),

    config:delete("tracing.filters", "bango", false),
    test_util:wait_value(fun() ->
        config:get("tracing.filters", "bango")
    end, undefined),
    ctrace_config:update(),

    FilterMod = ctrace_config:filter_module_name("bango"),
    ?assertEqual(false, code:is_loaded(FilterMod)).


ensure_bad_filter_ignored() ->
    Filter = "#foo stuff",
    config_set("tracing.filters", "compile_error", Filter),
    ctrace_config:update(),

    FilterMod = ctrace_config:filter_module_name("compile_error"),
    ?assertEqual(false, code:is_loaded(FilterMod)),

    AllMod = ctrace_config:filter_module_name(all),
    ?assertMatch({file, _}, code:is_loaded(AllMod)).


config_set(Section, Key, Value) ->
    PrevValue = config:get(Section, Key),
    if Value == PrevValue -> ok; true ->
        config:set(Section, Key, Value, false),
        test_util:wait_other_value(fun() ->
            config:get(Section, Key)
        end, PrevValue)
    end.
