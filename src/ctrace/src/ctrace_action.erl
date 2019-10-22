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
    report/1
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
    fun(Span) ->
        jaeger_passage_reporter:report(TracerId, Span),
        true
    end.