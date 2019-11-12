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

-module(ctrace).

-vsn(1).

-export([
    is_enabled/0,

    with_span/2,
    with_span/3,
    start_span/1,
    start_span/2,
    finish_span/0,
    finish_span/1,
    has_span/0,
    external_span/3,

    tag/1,
    log/1,

    tags/0,
    refs/0,
    operation_name/0,
    trace_id/0,
    span_id/0,
    tracer/0,
    context/0,

    match/2
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("passage/include/opentracing.hrl").
-include("ctrace.hrl").


-type operation()
    :: atom()
    | fun().

-type tags()
    :: #{atom() => term()}.

-type log_fields()
    :: #{atom() => term()}.

-type start_span_options()
    :: [start_span_option()].

-type start_span_option()
    :: {time, erlang:timespan()}
    | {tags, tags()}.

-type finish_span_options()
    :: [finish_span_option()].

-type finish_span_option()
    :: {time, erlang:timespan()}.


-spec is_enabled() -> boolean().

is_enabled() ->
    case get(?IS_ENABLED_KEY) of
        undefined ->
            Result = ctrace_config:is_enabled(),
            put(?IS_ENABLED_KEY, Result),
            Result;
        IsEnabled ->
            IsEnabled
    end.


%% @equiv with_span(Operation, [], Fun)
-spec with_span(
       Operation :: operation(),
       Fun
   ) -> Result when
       Fun :: fun (() -> Result),
       Result :: term().

with_span(Operation, Fun) ->
    with_span(Operation, #{}, Fun).

-spec with_span(
       Operation :: operation(),
       TagsOrOptions :: tags() | start_span_options(),
       Fun
   ) -> Result when
       Fun :: fun (() -> Result),
       Result :: term().

with_span(Operation, ExtraTags, Fun) when is_map(ExtraTags) ->
    with_span(Operation, [{tags, ExtraTags}], Fun);

with_span(Operation, Options, Fun)  ->
    try
        start_span(Operation, Options),
        Fun()
    catch Type:Reason ->
        Stack = erlang:get_stacktrace(),
        log(#{
            ?LOG_FIELD_ERROR_KIND => Type,
            ?LOG_FIELD_MESSAGE => Reason,
            ?LOG_FIELD_STACK => Stack
        }, [error]),
        erlang:raise(Type, Reason, Stack)
    after
        finish_span()
    end.

-spec start_span(
        Operation :: operation()
    ) -> ok.

start_span(Operation) ->
    start_span(Operation, []).

-spec start_span(
        Operation :: operation(),
        Options :: start_span_options()
    ) -> ok.

start_span(Operation, Options) ->
    case is_enabled() of
        true ->
            do_start_span(Operation, Options);
        false ->
            ok
    end.

do_start_span(Fun, Options) when is_function(Fun) ->
    start_span(fun_to_op(Fun), Options);

do_start_span(OperationName, Options0) ->
    Options1 = add_time(Options0),
    case passage_pd:current_span() of
        undefined ->
            put(?ORIGIN_KEY, atom_to_binary(OperationName, utf8)),
            Tags = case lists:keyfind(tags, 1, Options0) of
                {tags, T} ->
                    T;
                false ->
                    #{}
            end,
            case match(OperationName, Tags) of
                true ->
                    Options = [
                        {tracer, ?MAIN_TRACER}
                        | maybe_start_root(Options1)
                    ],
                    passage_pd:start_span(OperationName, Options);
                false ->
                    ok
            end;
        Span ->
            Options = add_tags([{child_of, Span} | Options1], #{
                origin => get(?ORIGIN_KEY)
            }),
            passage_pd:start_span(OperationName, Options)
    end.

-spec finish_span() -> ok.

finish_span() ->
    finish_span([]).

-spec finish_span(
        Options :: finish_span_options()
    ) -> ok.

finish_span(Options0) ->
    Options = add_time(Options0),
    passage_pd:finish_span(Options).

-spec tag(
        Tags :: tags()
    ) -> ok.

tag(Tags) ->
    passage_pd:set_tags(Tags).

-spec log(
        Fields :: log_fields() | fun (() -> log_fields())
    ) -> ok.

log(FieldsOrFun) ->
    log(FieldsOrFun, []).

log(FieldsOrFun, Options) ->
    passage_pd:log(FieldsOrFun, Options).

-spec tags() -> tags().

tags() ->
    case passage_pd:current_span() of
        undefined ->
            undefined;
        Span ->
            passage_span:get_tags(Span)
    end.

-spec refs() -> passage:refs().

refs() ->
    case passage_pd:current_span() of
        undefined ->
            undefined;
        Span ->
            passage_span:get_refs(Span)
    end.

-spec has_span() -> boolean().

has_span() ->
    passage_pd:current_span() =/= undefined.

-spec operation_name() -> atom().

operation_name() ->
    case passage_pd:current_span() of
        undefined ->
            undefined;
        Span ->
            passage_span:get_operation_name(Span)
    end.

-spec trace_id() -> 0..16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF.

trace_id() ->
    case passage_pd:current_span() of
        undefined ->
            undefined;
        Span ->
            Context = passage_span:get_context(Span),
            jaeger_passage_span_context:get_trace_id(Context)
    end.

-spec span_id() -> 0..16#FFFFFFFFFFFFFFFF.

span_id() ->
    case passage_pd:current_span() of
        undefined ->
            undefined;
        Span ->
            Context = passage_span:get_context(Span),
            jaeger_passage_span_context:get_span_id(Context)
    end.

-spec tracer() -> passage:tracer_id().

tracer() ->
    case passage_pd:current_span() of
        undefined ->
            undefined;
        Span ->
            passage_span:get_tracer(Span)
    end.

-spec context() -> passage_span_contest:context().

context() ->
    case passage_pd:current_span() of
        undefined ->
            undefined;
        Span ->
            passage_span:get_context(Span)
    end.

-spec external_span(
        TraceId :: passage:trace_id(),
        SpanId :: undefined | passage:span_id(),
        ParentSpanId :: undefined | passage:span_id()
    ) -> passage:maybe_span().

external_span(TraceId, undefined, ParentSpanId) ->
    external_span(TraceId, rand:uniform(16#FFFFFFFFFFFFFFFF), ParentSpanId);
external_span(TraceId, SpanId, undefined) ->
    external_span(TraceId, SpanId, rand:uniform(16#FFFFFFFFFFFFFFFF));
external_span(TraceId, SpanId, ParentSpanId) ->
    IterFun = fun(Val) -> Val end,
    Flags = <<0:32>>,
    BaggageItems = <<0:32>>,
    Binary = <<
        TraceId:128,
        SpanId:64,
        ParentSpanId:64,
        Flags/binary,
        BaggageItems/binary
    >>,
    State = {ok, <<"binary">>, Binary, error},
    passage:extract_span(?MAIN_TRACER, binary, IterFun, State).


match(OperationId, Tags) ->
    OpMod = ctrace_config:filter_module_name(OperationId),
    case erlang:function_exported(OpMod, match, 1) of
        true ->
            do_match(OpMod, Tags);
        false ->
            AllMod = ctrace_config:filter_module_name("all"),
            case erlang:function_exported(AllMod, match, 1) of
                true -> do_match(AllMod, Tags);
                false -> false
            end
    end.


do_match(Mod, Tags) ->
    case Mod:match(Tags) of
        true ->
            true;
        false ->
            false;
        Rate when is_float(Rate) ->
            rand:uniform() =< Rate
    end.


add_tags(Options, ExtraTags) ->
    case lists:keytake(tags, 1, Options) of
        {value, {tags, T}, Opts} ->
            [{tags, maps:merge(T, ExtraTags)} | Opts];
        false ->
            [{tags, ExtraTags} | Options]
    end.

add_time(Options) ->
    case lists:keymember(time, 1, Options) of
        true ->
            Options;
        false ->
            [{time, os:timestamp()} | Options]
    end.

maybe_start_root(Options) ->
    case lists:keytake(root, 1, Options) of
        {value, {root, Root}, NewOptions} ->
            [{child_of, Root} | NewOptions];
        false ->
            Options
    end.

fun_to_op(Fun) ->
     {module, M} = erlang:fun_info(Fun, module),
     {name, F} = erlang:fun_info(Fun, name),
     {arity, A} = erlang:fun_info(Fun, arity),
     Str = io_lib:format("~s:~s/~b", [M, F, A]),
     list_to_atom(lists:flatten(Str)).
