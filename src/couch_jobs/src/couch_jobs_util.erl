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

-module(couch_jobs_util).

-export([
    get_non_neg_int/2,
    get_float_0_1/2,
    get_timeout/2
]).

get_non_neg_int(Key, Default) when is_atom(Key), is_list(Default) ->
    StrVal = config:get("couch_jobs", atom_to_list(Key), Default),
    non_neg_int(Key, StrVal).

get_float_0_1(Key, Default) when is_atom(Key), is_list(Default) ->
    StrVal = config:get("couch_jobs", atom_to_list(Key), Default),
    float_0_1(Key, StrVal).

get_timeout(Key, Default) when is_atom(Key), is_list(Default) ->
    case config:get("couch_jobs", atom_to_list(Key), Default) of
        "infinity" -> infinity;
        StrVal -> non_neg_int(Key, StrVal)
    end.

non_neg_int(Name, Str) ->
    try
        Val = list_to_integer(Str),
        true = Val > 0,
        Val
    catch
        _:_ ->
            erlang:error({invalid_non_neg_integer, {couch_jobs, Name, Str}})
    end.

float_0_1(Name, Str) ->
    Val =
        try
            list_to_float(Str)
        catch
            error:badarg ->
                erlang:error({invalid_float, {couch_jobs, Name, Str}})
        end,
    if
        Val >= 0.0 andalso Val =< 1.0 -> Val;
        true -> erlang:error({float_out_of_range, {couch_jobs, Name, Str}})
    end.
