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

-module(couch_views_encoding_test).

-include_lib("eunit/include/eunit.hrl").

val_encoding_test() ->
    Values = [
        null,
        true,
        1.0,
        <<"a">>,
        {[{<<"a">>, 1.0}, {<<"b">>, <<"hello">>}]}
    ],
    lists:foreach(fun (Val) ->
        EncVal = couch_views_encoding:encode(Val),
        ?assertEqual(Val, couch_views_encoding:decode(EncVal))
    end, Values).


correct_ordering_test() ->
    % Load the ICU driver for couch_util:get_sort_key/1
    {ok, CfgPid} = gen_server:start_link(config, [], []),
    {ok, DrvPid} = gen_server:start_link(couch_drv, [], []),

    Ordered = [
        %  Special values sort before all other types
        null,
        false,
        true,

        % Then numbers
        1,
        2,
        3.0,
        4,

        % Then text, case sensitive
        <<"a">>,
        <<"A">>,
        <<"aa">>,
        <<"b">>,
        <<"B">>,
        <<"ba">>,
        <<"bb">>,

        % Then arrays, compared element by element until different.
        % Longer arrays sort after their prefixes
        [<<"a">>],
        [<<"b">>],
        [<<"b">>, <<"c">>],
        [<<"b">>, <<"c">>, <<"a">>],
        [<<"b">>, <<"d">>],
        [<<"b">>, <<"d">>, <<"e">>],

        % Then objects, compared each key value in the list until different.
        % Larger objects sort after their subset objects
        {[{<<"a">>, 1}]},
        {[{<<"a">>, 2}]},
        {[{<<"b">>, 1}]},
        {[{<<"b">>, 2}]},

        % Member order does matter for collation
        {[{<<"b">>, 2}, {<<"a">>, 1}]},
        {[{<<"b">>, 2}, {<<"c">>, 2}]}
    ],

    Encoded = lists:map(fun(Elem) ->
        K = couch_views_encoding:encode(Elem, key),
        V = couch_views_encoding:encode(Elem, value),
        {K, V}
    end, Ordered),
    Shuffled = shuffle(Encoded),
    Reordered = lists:sort(Shuffled),

    lists:foreach(fun({Original, {_K, ViewEncoded}}) ->
        ?assertEqual(Original, couch_views_encoding:decode(ViewEncoded))
    end, lists:zip(Ordered, Reordered)).


shuffle(List) when is_list(List) ->
    Tagged = [{rand:uniform(), Item} || Item <- List],
    {_, Randomized} = lists:unzip(lists:sort(Tagged)),
    Randomized.
