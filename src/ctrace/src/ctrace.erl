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
    new_request_ctx/0,
    start_span/2,
    start_span/3,
    finish_span/1,
    finish_span/2,

    set_operation_name/2,

    trace/2,
    trace/3,
    tag/2,
    log/2
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

-type request_ctx()
    :: #{
        '__struct__' := request_ctx,
        '__vsn__' := integer()
    }.

new_request_ctx() ->
    #{
        '__struct__' => request_ctx,
        '__vsn__' => 1
     }.


start_span(Subject, OperationName) ->
    start_span(Subject, OperationName, []).

start_span(Subject, _OperationName, _Options) ->
    Subject.

trace(Subject, Fun) ->
    trace(Subject, undefined, Fun).

trace(Subject, _OperationName, _Fun) ->
    Subject.

finish_span(Subject) ->
    finish_span(Subject, []).

finish_span(Subject, _Options) ->
    Subject.

set_operation_name(Span, _OperationName) ->
    Span.

tag(Span, _Tags) ->
    Span.

log(Span, _FieldsOrFun) ->
    Span.

handle_config_change(_Sec, _Key, _Val, _Persist, St) ->
    {ok, St}.

handle_config_terminate(_Server, _Reason, _State) ->
    ok.

