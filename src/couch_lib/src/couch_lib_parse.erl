% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_lib_parse).

-export([
    parse_boolean/1,
    parse_integer/1,
    parse_non_neg_integer/1
]).

parse_boolean(true) ->
    true;
parse_boolean(false) ->
    false;
parse_boolean(Val) when is_binary(Val) ->
    parse_boolean(binary_to_list(Val));
parse_boolean(Val) ->
    case string:to_lower(Val) of
        "true" ->
            true;
        "false" ->
            false;
        _ ->
            Msg = io_lib:format("Invalid boolean: ~p", [Val]),
            {error, list_to_binary(Msg)}
    end.

parse_integer(Val) when is_integer(Val) ->
    Val;
parse_integer(Val) when is_list(Val) ->
    case (catch list_to_integer(Val)) of
        IntVal when is_integer(IntVal) ->
            IntVal;
        _ ->
            Msg = io_lib:format("Invalid value for integer: ~p", [Val]),
            {error, list_to_binary(Msg)}
    end;
parse_integer(Val) when is_binary(Val) ->
    binary_to_list(Val).

parse_non_neg_integer(Val) ->
    case parse_integer(Val) of
        IntVal when IntVal >= 0 ->
            IntVal;
        _ ->
            Fmt = "Invalid value for non negative integer: ~p",
            Msg = io_lib:format(Fmt, [Val]),
            {error, list_to_binary(Msg)}
    end.
