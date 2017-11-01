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


-define(MIN_OTP_VERSION_WITH_RAND_MODULE, 18).


has_rand_module() ->
    OtpRelease = case erlang:system_info(otp_release) of
        % < R15 is excluded by ./configure already
        "R15" ++ _OtpVsn -> 15;
        "R16" ++ _OtpVsn -> 16;
        Release -> list_to_integer(Release)
    end,
    OtpRelease >= ?MIN_OTP_VERSION_WITH_RAND_MODULE.


uniform() ->
    case has_rand_module() of
        true -> rand_uniform();
        _False -> norand_uniform()
    end.

uniform(N) ->
    case has_rand_module() of
        true -> rand_uniform(N);
        _False -> norand_uniform(N)
    end.


norand_uniform() ->
    maybe_set_random_seed(),
    random:uniform().


norand_uniform(N) ->
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

rand_uniform() ->
    rand:uniform().


rand_uniform(N) ->
    rand:uniform(N).
