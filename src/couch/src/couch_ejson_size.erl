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

-module(couch_ejson_size).

-export([encoded_size/1]).


%% View rows

encoded_size({EJson, DocId}) when is_binary(DocId) ->
    encoded_size(EJson) + encoded_size(DocId);

%% Compound objects

encoded_size({[]}) ->
    2;  % opening { and closing }

encoded_size({KVs}) ->
    % Would add 2 because opening { and closing }, but then inside the LC
    % would accumulate an extra , at the end so subtract 2 - 1
    1 + lists:sum([encoded_size(K) + encoded_size(V) + 2 || {K,V} <- KVs]);

encoded_size([]) ->
    2;  % opening [ and closing ]

encoded_size(List) when is_list(List) ->
    % 2 is for [ and ] but inside LC would accumulate an extra , so subtract
    % 2 - 1
    1 + lists:sum([encoded_size(V) + 1 || V <- List]);

%% Floats.

encoded_size(0.0) ->
    3;

encoded_size(1.0) ->
    3;

encoded_size(Float) when is_float(Float), Float < 0.0 ->
    encoded_size(-Float) + 1;

encoded_size(Float) when is_float(Float), Float < 1.0 ->
    if
        Float =< 1.0e-300 -> 3;  % close enough to 0.0
        Float =< 1.0e-100 -> 6;  % Xe-YYY
        Float =< 1.0e-10  -> 5;  % Xe-YY
        Float =< 0.01     -> 4;  % Xe-Y, 0.0X
        true              -> 3   % 0.X
    end;

encoded_size(Float) when is_float(Float) ->
    if
        Float >= 1.0e100  -> 5;  % XeYYY
        Float >= 1.0e10   -> 4;  % XeYY
        true              -> 3   % XeY, X.Y
    end;

%% Integers

encoded_size(0) ->
    1;

encoded_size(Integer) when is_integer(Integer), Integer < 0 ->
    encoded_size(-Integer) + 1;

encoded_size(Integer) when is_integer(Integer) ->
    if
        Integer < 10    -> 1;
        Integer < 100   -> 2;
        Integer < 1000  -> 3;
        Integer < 10000 -> 4;
        true            -> trunc(math:log10(Integer)) + 1
    end;

%% Strings

encoded_size(Binary) when is_binary(Binary) ->
    2 + byte_size(Binary);

%% Special terminal symbols as atoms

encoded_size(null) ->
    4;

encoded_size(true) ->
    4;

encoded_size(false) ->
    5;

%% Other atoms

encoded_size(Atom) when is_atom(Atom) ->
    encoded_size(atom_to_binary(Atom, utf8)).
