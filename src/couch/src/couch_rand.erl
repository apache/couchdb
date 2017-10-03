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

-module(couch_rand).


-export([
    uniform/0,
    uniform/1
]).


-ifdef(NORANDMODULE).


uniform() ->
    maybe_set_random_seed(),
    random:uniform().


uniform(N) ->
    maybe_set_random_seed(),
    random:uniform(N).


maybe_set_random_seed() ->
    case get(random_seed) of
        undefined ->
            {_, Sec, USec} = os:timestamp(),
            Seed = {erlang:phash2(self()), Sec, USec},
            random:seed(Seed);
        _ ->
            ok
    end.


-else.


uniform() ->
    rand:uniform().


uniform(N) ->
    rand:uniform(N).


-endif.
