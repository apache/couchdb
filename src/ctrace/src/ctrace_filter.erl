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

-module(ctrace_filter).
-include_lib("passage/include/opentracing.hrl").

-behaviour(passage_reporter).

-export([
    new/2,
    report/2,
    module/1
]).

-type state()
    :: #{
        '__struct__' := ?MODULE,
        id := passage:tracer_id(),
        module := module()
    }.

-spec new(
        Id :: passage:tracer_id(),
        Module :: module()
    ) -> State :: state().

new(Id, Module) ->
    #{
        '__struct__' => ?MODULE,
        id => Id,
        module => Module
    }.

-spec report(
        State :: state(),
        passage_span:span()
    ) -> ok.

report(#{'__struct__' := ?MODULE, module := Module} = _State, PSpan) ->
    Operation = passage_span:get_operation_name(PSpan),
    Tags = case passage_span:get_finish_time(PSpan) of
        {ok, FinishTime} ->
            StartTime =  passage_span:get_start_time(PSpan),
            Duration = timer:now_diff(FinishTime, StartTime),
            maps:merge(passage_span:get_tags(PSpan), #{
                duration => Duration,
                operation_id => Operation
            });
        error ->
            maps:put(operation_id, Operation, passage_span:get_tags(PSpan))
    end,

    %% FIXME handle the case when module is not compiled yet
    case Module:match(Tags) of
        false ->
            ok;
        Actions ->
            lists:takewhile(fun(Action) ->
                Action(PSpan)
            end, Actions),
            ok
    end.

-spec module(
        State :: state()
    ) -> module().

module(#{'__struct__' := ?MODULE, module := Module}) ->
    Module.

