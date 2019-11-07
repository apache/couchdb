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

-module(ctrace_action).

-export([
    sample/1,
    report/1,
    report_longer_than/1
]).

-type action_fun()
    :: fun((Span :: passage_span:span()) -> boolean()).

-spec sample(
        [SamplingRate :: float() | integer()]
    ) -> action_fun().

sample([SamplingRate]) ->
    fun(_Span) -> rand:uniform() < SamplingRate end.

-spec report(
        [ReporterId :: passage:tracer_id()]
    ) -> action_fun().

report([TracerId]) ->
    fun(Span, Options) ->
        passage:finish_span(Span, Options),
        true
    end.

report_longer_than([TracerId, Threshold]) ->
    fun(Span, Options) ->
        case longer_than(Span, Threshold) of
            true ->
                 passage:finish_span(Span, Options),
                true;
            false ->
                true
        end
    end.

longer_than(Span, Threshold) ->
    case passage_span:get_finish_time(Span) of
        {ok, FinishTime} ->
            StartTime =  passage_span:get_start_time(Span),
            timer:now_diff(FinishTime, StartTime) > Threshold;
        _ ->
            true
    end.

