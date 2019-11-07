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

-behaviour(config_listener).

-export([
    update_tracers/0,
    new_request_ctx/0,
    start_span/1,
    start_span/2,
    start_span/3,
    finish_span/0,
    finish_span/1,
    finish_span/2,
    finish_lifetime/1,
    finish_lifetime/2,

    set_operation_name/2,

    trace/1,
    trace/2,
    trace/3,
    tag/2,
    log/2,

    tags/1,
    refs/1,
    operation_name/1,
    trace_id/1,
    span_id/1,
    tracer/1,
    context/1,

    match/2,
    filter_module_name/1
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("passage/include/opentracing.hrl").

-define(MAIN_TRACER, jaeger_passage_reporter).

-type request_ctx()
    :: #{
        '__struct__' := request_ctx,
        '__vsn__' := integer(),
        tracing := boolean()
    }.

-define(IS_SPAN(Tuple),
    is_tuple(Tuple)
        andalso element(1, Tuple) =:= passage_span
).

-record(span, {
    active = false,
    origin = undefined,
    span = undefined,
    tracer = undefined,
    backend = passage,
    actions = []
}).

-type trace_subject()
    :: #httpd{}
    | request_ctx()
    | #span{}
    | #{request_ctx => request_ctx()}.
    % | map().

-type tags()
    :: #{atom() => term()}.

-type log_fields()
    :: #{atom() => term()}.

new_request_ctx() ->
    #{
        '__struct__' => request_ctx,
        '__vsn__' => 1,
        tracing => is_enabled()
     }.

-spec start_span(
        OperationName :: atom()
    ) -> ok.

start_span(OperationName) ->
    passage_pd:start_span(OperationName, [{tracer, OperationName}]).

-spec start_span(
        Subject :: trace_subject(),
        OperationName :: atom()
    ) -> trace_subject().

start_span(Subject, OperationName) ->
    start_span(Subject, OperationName, []).

-spec start_span(
        Subject :: trace_subject(),
        OperationName :: atom(),
        Options :: list()
    ) -> trace_subject().

start_span(Subject, undefined, Options) ->
   start_span(Subject, ?MAIN_TRACER, Options);
start_span(Subject, OperationName, Options0) ->
    unwrap(Subject, fun
        (#span{active = true, span = PSpan0} = Span) when ?IS_SPAN(PSpan0) ->
            Options = [
                {child_of, PSpan0}
            | Options0],
            case passage:start_span(OperationName, Options) of
                undefined ->
                    Span#span{active = false};
                PSpan1 ->
                    Origin = atom_to_binary(Span#span.origin, utf8),
                    Tags = #{
                        origin => Origin
                    },
                    tag(Span#span{span = PSpan1}, Tags)
            end;
        (#span{} = Span) ->
            Span;
        (#{'__struct__' := request_ctx, tracing := true}) ->
            start_root_span(OperationName, Options0);
        (#{'__struct__' := request_ctx, tracing := false}) ->
            #span{}
    end).

 -spec trace(
        fun((#span{}) -> #span{})
    ) -> trace_subject().

trace(Fun) ->
    trace(#span{
        span = passage_pd:current_span(),
        backend = passage_pd,
        active = true
    }, Fun).

-spec trace(
        Subject :: trace_subject(),
        fun((#span{}) -> #span{})
    ) -> trace_subject().

trace(Subject, Fun) ->
    trace(Subject, undefined, Fun).

-spec trace(
        Subject :: trace_subject(),
        OperationName :: atom(),
        fun((#span{}) -> #span{})
    ) -> trace_subject().

trace(Subject, OperationName, Fun) ->
    unwrap(Subject, fun
        (#span{} = Span) ->
            Fun(Span);
        (_) ->
            couch_log:error("Unexpected subject in '~p' tracer: '~p'~n",
                [OperationName, Subject]),
            Fun(#span{})
    end).

-spec finish_span() -> ok.

finish_span() ->
    passage_pd:finish_span().

-spec finish_span(
        Subject :: trace_subject()
    ) -> trace_subject().

finish_span(Subject) ->
    finish_span(Subject, []).

-spec finish_span(
        Subject :: trace_subject(),
        Options :: list()
    ) -> trace_subject().

finish_span(Subject, Options0) ->
    Options = case lists:keymember(time, 1, Options0) of
        true ->
            Options0;
        false ->
            [{time, os:timestamp()} | Options0]
    end,
    unwrap(Subject, fun
        (#span{active = true, span = PassageSpan, actions = Actions} = Span) ->
            lists:takewhile(fun(Action) ->
                Action(PassageSpan, Options)
            end, Actions),
            Span;
        (Anything) ->
            Anything
    end).

%% finish_lifetime works only with passage backend

-spec finish_lifetime(
	        Subject :: trace_subject()
    ) -> ok.

finish_lifetime(Subject) ->
    finish_lifetime(Subject, self()).

-spec finish_lifetime(
        Subject :: trace_subject(),
        Pid :: pid()
    ) -> ok.

finish_lifetime(Subject, Pid) ->
    spawn(fun () ->
        Monitor = monitor(process, Pid),
        finish_span_when_process_exits(Monitor, Subject)
    end),
    ok.

-spec set_operation_name(
        Span :: #span{},
        OperationName :: atom()
    ) -> #span{}.

set_operation_name(#span{active = false} = Span, OperationName) ->
    Span#span{origin = OperationName};
set_operation_name(#span{span = PSpan0, backend = passage} = Span, OperationName) ->
    PSpan1 = passage:set_operation_name(PSpan0, OperationName),
    PSpan2 = passage:set_tracer(PSpan1, OperationName),
    Span#span{origin = OperationName, span = PSpan2};
set_operation_name(#span{backend = passage_pd} = Span, OperationName) ->
    passage_pd:set_operation_name(OperationName),
    passage_pd:set_tracer(OperationName),
    Span#span{origin = OperationName}.

-spec tag(
        Span :: #span{},
        Tags :: tags()
    ) -> #span{}.

tag(#span{active = false} = Span, _Tags) ->
    Span;
tag(Span, []) ->
    Span;
tag(#span{span = PSpan, backend = passage} = Span, Tags) ->
    Span#span{span = passage:set_tags(PSpan, Tags)};
tag(#span{backend = passage_pd} = Span, Tags) ->
    passage_pd:set_tags(Tags),
    Span.

-spec log(
        Span :: #span{},
        Fields :: log_fields() | fun (() -> log_fields())
    ) -> #span{}.

log(Span, FieldsOrFun) ->
    log(Span, FieldsOrFun, []).

log(#span{active = false} = Span, _FieldsOrFun, _Options) ->
    Span;
log(#span{span = PSpan, backend = passage} = Span, FieldsOrFun, Options) ->
    Span#span{span = passage:log(PSpan, FieldsOrFun, Options)};
log(#span{backend = passage_pd} = Span, FieldsOrFun, Options) ->
    passage_pd:log(FieldsOrFun, Options),
    Span.

-spec is_enabled() -> boolean().

is_enabled() ->
    ctrace_config:is_enabled().

-spec tags(#span{}) -> tags().

tags(#span{active = false}) ->
    [];
tags(#span{span = PSpan, backend = passage}) ->
    passage_span:get_tags(PSpan);
tags(#span{backend = passage_pd}) ->
    passage_span:get_tags(passage_pd:current_span()).

-spec refs(
        #span{} | passage_span:span()
    ) -> passage:refs().

refs(#span{span = PSpan, backend = passage}) ->
     refs(PSpan);
refs(#span{backend = passage_pd}) ->
    refs(passage_pd:current_span());
refs(PSpan) ->
    passage_span:get_refs(PSpan).

-spec operation_name(#span{}) -> atom().

operation_name(#span{active = false}) ->
	undefined;
operation_name(#span{span = Span, backend = passage}) ->
    passage_span:get_operation_name(Span);
operation_name(#span{backend = passage_pd}) ->
    passage_span:get_operation_name(passage_pd:current_span()).

-spec trace_id(
        #span{}
        | passage_span:span()
        | passage_span_contest:context()
    ) -> passage:trace_id().

trace_id(#span{span = PSpan, backend = passage}) ->
     trace_id(PSpan);
trace_id(#span{backend = passage_pd}) ->
    trace_id(passage_pd:current_span());
trace_id(Span) when ?IS_SPAN(Span) ->
    trace_id(context(Span));
trace_id(Context) ->
    jaeger_passage_span_context:get_trace_id(Context).

-spec span_id(
        #span{}
        | passage_span:span()
        | passage_span_contest:context()
    ) -> passage:span_id().
span_id(#span{span = PSpan, backend = passage}) ->
     span_id(PSpan);
span_id(#span{backend = passage_pd}) ->
    span_id(passage_pd:current_span());
span_id(Span) when ?IS_SPAN(Span) ->
    trace_id(context(Span));
span_id(Context) ->
    jaeger_passage_span_context:get_span_id(Context).

-spec tracer(
        passage_span:span()
    ) -> passage:tracer_id().

tracer(Span) ->
    passage_span:get_tracer(Span).

-spec context(
        #span{}
        | passage_span:span()
    ) -> passage_span_contest:context().

context(#span{span = PSpan, backend = passage}) ->
     context(PSpan);
context(#span{backend = passage_pd}) ->
    context(passage_pd:current_span());
context(Span) ->
    passage_span:get_context(Span).

-spec update_tracers() -> ok.

update_tracers() ->
    case is_enabled() of
        true ->
            maybe_start_main_tracer(?MAIN_TRACER)
                andalso config_update();
        false ->
            jaeger_passage:stop_tracer(?MAIN_TRACER)
    end,
    ok.

handle_config_change("tracing.samplers", _Key, _Val, _Persist, St) ->
    {ok, St};
handle_config_change("tracing." ++ OperationId, _Key, _Val, _Persist, St) ->
    compile_rules(list_to_atom(OperationId)),
    {ok, St};
handle_config_change("tracing", "enabled", _, _Persist, St) ->
    update_tracers(),
    {ok, St};
handle_config_change(_Sec, _Key, _Val, _Persist, St) ->
    {ok, St}.

handle_config_terminate(_Server, _Reason, _State) ->
    config_update().

-spec unwrap(Subject :: trace_subject(),
        fun((#span{}) -> trace_subject())
    ) -> trace_subject().

%% httpd
unwrap(#httpd{request_ctx = RequestCtx} = Req, Fun) when is_map(RequestCtx) ->
    NewRequestCtx = unwrap(RequestCtx, Fun),
    Req#httpd{request_ctx = NewRequestCtx};
%% db
unwrap(#{request_ctx := RequestCtx} = Subject, Fun) when is_map(RequestCtx) ->
    NewRequestCtx = unwrap(RequestCtx, Fun),
    Subject#{request_ctx => NewRequestCtx};
unwrap(#{trace_span := #span{} = Span} = RequestCtx, Fun) ->
    NewSpan = unwrap(Span, Fun),
    RequestCtx#{trace_span => NewSpan};
unwrap(#{'__struct__' := request_ctx} = RequestCtx, Fun) ->
    NewSpan = Fun(RequestCtx),
    RequestCtx#{trace_span => NewSpan};
unwrap(#span{} = Span, Fun) ->
    Fun(Span);
unwrap(Subject, Fun) ->
    Fun(Subject).

start_root_span(OperationName, Options0) ->
    Tags = case lists:keyfind(tags, 1, Options0) of
        {tags, T} ->
            T;
        false ->
            #{}
    end,
    case match(OperationName, Tags) of
        false ->
            #span{};
        Actions ->
            Options = [{tracer, ?MAIN_TRACER} | Options0],
            case passage:start_span(OperationName, Options) of
                undefined ->
                    #span{};
                PassageSpan ->
                    #span{
                        span = PassageSpan,
                        active = true,
                        tracer = OperationName,
                        origin = OperationName,
                        actions = Actions
                    }
            end
    end.

config_update() ->
    lists:foreach(fun(OperationId) ->
        compile_rules(list_to_atom(OperationId))
    end, ctrace_config:samplers()).

compile_rules(OperationId) ->
    case ctrace_config:get(OperationId) of
        {ok, {_Sampler, Rules0}} ->
            Rules = set_actions(Rules0),
            compile(OperationId, Rules);
        {error, Reason} = Else ->
            couch_log:error("cannot compile '~p', reason: ~p~n", [OperationId, Reason]),
            Else;
        Else ->
            couch_log:error("cannot compile '~p', reason: ~p~n", [OperationId, Else]),
            Else
    end.

compile(OperationId, Rules) ->
    try
        couch_log:info("compiling '~p' rules~n", [OperationId]),
        ctrace_dsl:compile(filter_module_name(OperationId), Rules)
    catch
        throw:{error, Reason} ->
            couch_log:error(
                "compile error for '~p': ~p~n", [OperationId, Reason])
    end.

set_actions(Rules) ->
    lists:map(fun(#{actions := Actions} = Rule) ->
        Rule#{actions => lists:map(fun set_action/1, Actions)}
    end, Rules).

set_action({sample, Rate}) ->
    {ctrace_action, sample, [Rate]};
set_action(report) ->
    {ctrace_action, report, [?MAIN_TRACER]}.

match(OperationId, Tags) ->
    Module = filter_module_name(OperationId),
    erlang:function_exported(Module, match, 1)
        andalso Module:match(Tags).

filter_module_name(OperationId) ->
    list_to_atom("ctrace_filter_" ++ atom_to_list(OperationId)).

maybe_start_main_tracer(TracerId) ->
    case passage_tracer_registry:get_reporter(TracerId) of
        error ->
            start_main_tracer(TracerId);
        _ ->
            true
    end.

start_main_tracer(TracerId) ->
    Sampler = passage_sampler_all:new(),
    Options = [
        {thrift_format,
            list_to_atom(config:get("tracing", "thrift_format", "compact"))},
        {agent_host, config:get("tracing", "agent_host", "127.0.0.1")},
        {agent_port, config:get_integer("tracing", "agent_port", 6831)},
        {default_service_name,
            list_to_atom(config:get("tracing", "app_name", "couchdb"))}
    ],
    case jaeger_passage:start_tracer(TracerId, Sampler, Options) of
        ok ->
            true;
        {error, Reason} ->
            couch_log:error("Cannot start main tracer: ~p~n", [Reason]),
            false
    end.

finish_span_when_process_exits(Monitor, Subject) ->
    receive
        {'DOWN', Monitor, _, _, normal} ->
            finish_span(Subject);
        {'DOWN', Monitor, _, _, shutdown} ->
            finish_span(Subject);
        {'DOWN', Monitor, _, _, {shutdown, _}} ->
            finish_span(Subject);
        {'DOWN', Monitor, _, _, Reason} ->
            finish_span(trace(fun(Span0) ->
                Span1 = log(Span0, #{event => exit, 'exit.reason' => Reason}),
                Span2 = tag(Span1, #{error => true}),
                Span2
            end));
        _ ->
            finish_span_when_process_exits(Monitor, Subject)
    end.
