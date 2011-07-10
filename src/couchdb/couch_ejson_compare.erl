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

-module(couch_ejson_compare).

-export([less/2]).

-on_load(init/0).


init() ->
    LibDir = case couch_config:get("couchdb", "util_driver_dir") of
    undefined ->
        filename:join(couch_util:priv_dir(), "lib");
    LibDir0 ->
        LibDir0
    end,
    NumScheds = erlang:system_info(schedulers),
    (catch erlang:load_nif(filename:join([LibDir, ?MODULE]), NumScheds)),
    case erlang:system_info(otp_release) of
    "R13B03" -> true;
    _ -> ok
    end.


less(A, B) ->
    try
        less_nif(A, B)
    catch
    error:badarg ->
        % Maybe the EJSON structure is too deep, fallback to Erlang land.
        less_erl(A, B)
    end.


less_nif(A, B) ->
    less_erl(A, B).


less_erl(A,A)                                 -> 0;

less_erl(A,B) when is_atom(A), is_atom(B)     -> atom_sort(A) - atom_sort(B);
less_erl(A,_) when is_atom(A)                 -> -1;
less_erl(_,B) when is_atom(B)                 -> 1;

less_erl(A,B) when is_number(A), is_number(B) -> A - B;
less_erl(A,_) when is_number(A)               -> -1;
less_erl(_,B) when is_number(B)               -> 1;

less_erl(A,B) when is_binary(A), is_binary(B) -> couch_util:collate(A,B);
less_erl(A,_) when is_binary(A)               -> -1;
less_erl(_,B) when is_binary(B)               -> 1;

less_erl(A,B) when is_list(A), is_list(B)     -> less_list(A,B);
less_erl(A,_) when is_list(A)                 -> -1;
less_erl(_,B) when is_list(B)                 -> 1;

less_erl({A},{B}) when is_list(A), is_list(B) -> less_props(A,B);
less_erl({A},_) when is_list(A)               -> -1;
less_erl(_,{B}) when is_list(B)               -> 1.

atom_sort(null) -> 1;
atom_sort(false) -> 2;
atom_sort(true) -> 3.

less_props([], [_|_]) ->
    -1;
less_props(_, []) ->
    1;
less_props([{AKey, AValue}|RestA], [{BKey, BValue}|RestB]) ->
    case couch_util:collate(AKey, BKey) of
    0 ->
        case less_erl(AValue, BValue) of
        0 ->
            less_props(RestA, RestB);
        Result ->
            Result
        end;
    Result ->
        Result
    end.

less_list([], [_|_]) ->
    -1;
less_list(_, []) ->
    1;
less_list([A|RestA], [B|RestB]) ->
    case less_erl(A,B) of
    0 ->
        less_list(RestA, RestB);
    Result ->
        Result
    end.
