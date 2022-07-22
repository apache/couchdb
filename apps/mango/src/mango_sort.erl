% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(mango_sort).

-export([
    new/1,
    to_json/1,
    fields/1,
    directions/1
]).

-include("mango.hrl").

new(Fields) when is_list(Fields) ->
    Sort = {[sort_field(Field) || Field <- Fields]},
    validate(Sort),
    {ok, Sort};
new(Else) ->
    ?MANGO_ERROR({invalid_sort_json, Else}).

to_json({Fields}) ->
    to_json(Fields);
to_json([]) ->
    [];
to_json([{Name, Dir} | Rest]) ->
    [{[{Name, Dir}]} | to_json(Rest)].

fields({Props}) ->
    [Name || {Name, _Dir} <- Props].

directions({Props}) ->
    [Dir || {_Name, Dir} <- Props].

sort_field(<<"">>) ->
    ?MANGO_ERROR({invalid_sort_field, <<"">>});
sort_field(Field) when is_binary(Field) ->
    {Field, <<"asc">>};
sort_field({[{Name, <<"asc">>}]}) when is_binary(Name) ->
    {Name, <<"asc">>};
sort_field({[{Name, <<"desc">>}]}) when is_binary(Name) ->
    {Name, <<"desc">>};
sort_field({Name, BadDir}) when is_binary(Name) ->
    ?MANGO_ERROR({invalid_sort_dir, BadDir});
sort_field(Else) ->
    ?MANGO_ERROR({invalid_sort_field, Else}).

validate({Props}) ->
    % Assert each field is in the same direction
    % until we support mixed direction sorts.
    Dirs = [D || {_, D} <- Props],
    case lists:usort(Dirs) of
        [] ->
            ok;
        [_] ->
            ok;
        _ ->
            ?MANGO_ERROR({unsupported, mixed_sort})
    end.
