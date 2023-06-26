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

-module(mango_idx_special).

-export([
    validate/1,
    add/2,
    remove/2,
    from_ddoc/1,
    to_json/1,
    columns/1,
    is_usable/3,
    start_key/1,
    end_key/1
]).

-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").

validate(_) ->
    erlang:exit(invalid_call).

add(_, _) ->
    erlang:exit(invalid_call).

remove(_, _) ->
    erlang:exit(invalid_call).

from_ddoc(_) ->
    erlang:exit(invalid_call).

to_json(#idx{def = all_docs}) ->
    {[
        {ddoc, null},
        {name, <<"_all_docs">>},
        {type, <<"special">>},
        {def,
            {[
                {<<"fields">>, [
                    {[
                        {<<"_id">>, <<"asc">>}
                    ]}
                ]}
            ]}}
    ]}.

columns(#idx{def = all_docs}) ->
    [<<"_id">>].

is_usable(#idx{def = all_docs}, _Selector, []) ->
    {true, #{reason => []}};
is_usable(#idx{def = all_docs} = Idx, Selector, SortFields) ->
    Fields = mango_idx_view:indexable_fields(Selector),
    SelectorHasRequiredFields = lists:member(<<"_id">>, Fields),
    CanUseSort = can_use_sort(Idx, SortFields, Selector),
    Reason =
        [field_mismatch || not SelectorHasRequiredFields] ++
            [sort_order_mismatch || not CanUseSort],
    Details = #{reason => Reason},
    {SelectorHasRequiredFields and CanUseSort, Details}.

start_key([{'$gt', Key, _, _}]) ->
    case mango_json:special(Key) of
        true ->
            ?MIN_STR;
        false ->
            Key
    end;
start_key([{'$gte', Key, _, _}]) ->
    false = mango_json:special(Key),
    Key;
start_key([{'$eq', Key, '$eq', Key}]) ->
    false = mango_json:special(Key),
    Key.

end_key([{_, _, '$lt', Key}]) ->
    case mango_json:special(Key) of
        true ->
            ?MAX_STR;
        false ->
            Key
    end;
end_key([{_, _, '$lte', Key}]) ->
    false = mango_json:special(Key),
    Key;
end_key([{'$eq', Key, '$eq', Key}]) ->
    false = mango_json:special(Key),
    Key.

can_use_sort(_Idx, [], _Selector) ->
    true;
can_use_sort(Idx, SortFields, _Selector) ->
    Cols = columns(Idx),
    lists:prefix(SortFields, Cols).

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

usable(Index, undefined, Fields) ->
    is_usable(Index, undefined, Fields);
usable(Index, Selector, Fields) ->
    is_usable(Index, test_util:as_selector(Selector), Fields).

is_usable_test() ->
    Usable = {true, #{reason => []}},
    FieldMismatch = {false, #{reason => [field_mismatch]}},
    SortOrderMismatch = {false, #{reason => [sort_order_mismatch]}},

    Index = #idx{def = all_docs},
    ?assertEqual(FieldMismatch, usable(Index, #{}, [<<"_id">>])),
    ?assertEqual(SortOrderMismatch, usable(Index, #{<<"_id">> => 11}, [<<"field3">>])),
    ?assertEqual(Usable, usable(Index, #{}, [])).
-endif.
