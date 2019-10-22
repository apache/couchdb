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

-module(ctrace_sampler).

-export([
    all/0,
    null/0,
    probalistic/1
]).

-spec all() -> passage_sampler:sampler().

all() ->
    passage_sampler_all:new().

-spec null() -> passage_sampler:sampler().

null() ->
    passage_sampler_null:new().

-spec probalistic(
        Rate :: float()
    ) -> passage_sampler:sampler().

probalistic(SamplingRate) ->
    passage_sampler_probalistic:new(SamplingRate).
