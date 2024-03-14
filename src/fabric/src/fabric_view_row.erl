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

-module(fabric_view_row).
-export([
    from_props/2,
    get_id/1,
    get_key/1,
    get_value/1,
    get_doc/1,
    get_worker/1,
    set_key/2,
    set_doc/2,
    set_worker/2,
    transform/1
]).

-include_lib("fabric/include/fabric.hrl").

from_props(Props, Options) ->
    case couch_util:get_value(view_row_map, Options, false) of
        true ->
            Row = maps:from_list(Props),
            {view_row, Row};
        false ->
            #view_row{
                key = couch_util:get_value(key, Props),
                id = couch_util:get_value(id, Props),
                value = couch_util:get_value(value, Props),
                doc = couch_util:get_value(doc, Props),
                worker = couch_util:get_value(worker, Props)
            }
    end.

get_id(#view_row{id = Id}) ->
    Id;
get_id({view_row, #{id := Id}}) ->
    Id;
get_id({view_row, #{}}) ->
    undefined.

get_key(#view_row{key = Key}) ->
    Key;
get_key({view_row, #{key := Key}}) ->
    Key;
get_key({view_row, #{}}) ->
    undefined.

set_key(#view_row{} = Row, Key) ->
    Row#view_row{key = Key};
set_key({view_row, #{} = Row}, Key) ->
    {view_row, Row#{key => Key}}.

get_value(#view_row{value = Value}) ->
    Value;
get_value({view_row, #{value := Value}}) ->
    Value;
get_value({view_row, #{}}) ->
    undefined.

get_doc(#view_row{doc = Doc}) ->
    Doc;
get_doc({view_row, #{doc := Doc}}) ->
    Doc;
get_doc({view_row, #{}}) ->
    undefined.

set_doc(#view_row{} = Row, Doc) ->
    Row#view_row{doc = Doc};
set_doc({view_row, #{} = Row}, Doc) ->
    {view_row, Row#{doc => Doc}}.

get_worker(#view_row{worker = Worker}) ->
    Worker;
get_worker({view_row, #{worker := Worker}}) ->
    Worker;
get_worker({view_row, #{}}) ->
    undefined.

set_worker(#view_row{} = Row, Worker) ->
    Row#view_row{worker = Worker};
set_worker({view_row, #{} = Row}, Worker) ->
    {view_row, Row#{worker => Worker}}.

transform(#view_row{value = {[{reduce_overflow_error, Msg}]}}) ->
    {row, [{key, null}, {id, error}, {value, reduce_overflow_error}, {reason, Msg}]};
transform(#view_row{key = Key, id = reduced, value = Value}) ->
    {row, [{key, Key}, {value, Value}]};
transform(#view_row{key = Key, id = undefined}) ->
    {row, [{key, Key}, {id, error}, {value, not_found}]};
transform(#view_row{key = Key, id = Id, value = Value, doc = undefined}) ->
    {row, [{id, Id}, {key, Key}, {value, Value}]};
transform(#view_row{key = Key, doc = {error, Reason}}) ->
    {row, [{id, error}, {key, Key}, {value, Reason}]};
transform(#view_row{key = Key, id = Id, value = Value, doc = Doc}) ->
    {row, [{id, Id}, {key, Key}, {value, Value}, {doc, Doc}]};
transform({view_row, #{} = Row0}) ->
    Id = maps:get(id, Row0, undefined),
    Key = maps:get(key, Row0, undefined),
    Value = maps:get(value, Row0, undefined),
    Doc = maps:get(doc, Row0, undefined),
    Worker = maps:get(worker, Row0, undefined),
    Row = #view_row{id = Id, key = Key, value = Value, doc = Doc, worker = Worker},
    transform(Row).

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

from_props_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            ?TDEF_FE(t_from_props_record),
            ?TDEF_FE(t_from_props_map_empty),
            ?TDEF_FE(t_from_props_map)
        ]
    }.

t_from_props_record(_) ->
    Options1 = [],
    Options2 = [{view_row_map, false}],
    Props = [{id, id}, {key, key}, {value, value}],
    ViewRow = #view_row{id = id, key = key, value = value, doc = undefined, worker = undefined},
    ?assertEqual(ViewRow, from_props(Props, Options1)),
    ?assertEqual(ViewRow, from_props(Props, Options2)).

t_from_props_map_empty(_) ->
    Options = [{view_row_map, true}],
    Props = [],
    ViewRow = {view_row, #{}},
    ?assertEqual(ViewRow, from_props(Props, Options)).

t_from_props_map(_) ->
    Options = [{view_row_map, true}],
    Props = [{id, id}, {key, key}, {doc, doc}],
    ViewRow = {view_row, #{id => id, key => key, doc => doc}},
    ?assertEqual(ViewRow, from_props(Props, Options)).

getter_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            ?TDEF_FE(t_get_id_record),
            ?TDEF_FE(t_get_key_record),
            ?TDEF_FE(t_get_value_record),
            ?TDEF_FE(t_get_doc_record),
            ?TDEF_FE(t_get_worker_record),
            ?TDEF_FE(t_get_id_map),
            ?TDEF_FE(t_get_key_map),
            ?TDEF_FE(t_get_value_map),
            ?TDEF_FE(t_get_doc_map),
            ?TDEF_FE(t_get_worker_map)
        ]
    }.

t_get_id_record(_) ->
    ViewRow1 = #view_row{},
    ?assertEqual(undefined, get_id(ViewRow1)),
    ViewRow2 = #view_row{id = id},
    ?assertEqual(id, get_id(ViewRow2)).

t_get_id_map(_) ->
    ViewRow1 = {view_row, #{}},
    ?assertEqual(undefined, get_id(ViewRow1)),
    ViewRow2 = {view_row, #{id => id}},
    ?assertEqual(id, get_id(ViewRow2)).

t_get_key_record(_) ->
    ViewRow1 = #view_row{},
    ?assertEqual(undefined, get_key(ViewRow1)),
    ViewRow2 = #view_row{key = key},
    ?assertEqual(key, get_key(ViewRow2)).

t_get_key_map(_) ->
    ViewRow1 = {view_row, #{}},
    ?assertEqual(undefined, get_key(ViewRow1)),
    ViewRow2 = {view_row, #{key => key}},
    ?assertEqual(key, get_key(ViewRow2)).

t_get_value_record(_) ->
    ViewRow1 = #view_row{},
    ?assertEqual(undefined, get_value(ViewRow1)),
    ViewRow2 = #view_row{value = value},
    ?assertEqual(value, get_value(ViewRow2)).

t_get_value_map(_) ->
    ViewRow1 = {view_row, #{}},
    ?assertEqual(undefined, get_value(ViewRow1)),
    ViewRow2 = {view_row, #{value => value}},
    ?assertEqual(value, get_value(ViewRow2)).

t_get_doc_record(_) ->
    ViewRow1 = #view_row{},
    ?assertEqual(undefined, get_doc(ViewRow1)),
    ViewRow2 = #view_row{doc = doc},
    ?assertEqual(doc, get_doc(ViewRow2)).

t_get_doc_map(_) ->
    ViewRow1 = {view_row, #{}},
    ?assertEqual(undefined, get_doc(ViewRow1)),
    ViewRow2 = {view_row, #{doc => doc}},
    ?assertEqual(doc, get_doc(ViewRow2)).

t_get_worker_record(_) ->
    ViewRow1 = #view_row{},
    ?assertEqual(undefined, get_worker(ViewRow1)),
    ViewRow2 = #view_row{worker = worker},
    ?assertEqual(worker, get_worker(ViewRow2)).

t_get_worker_map(_) ->
    ViewRow1 = {view_row, #{}},
    ?assertEqual(undefined, get_worker(ViewRow1)),
    ViewRow2 = {view_row, #{worker => worker}},
    ?assertEqual(worker, get_worker(ViewRow2)).

setter_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            ?TDEF_FE(t_set_key_record),
            ?TDEF_FE(t_set_doc_record),
            ?TDEF_FE(t_set_worker_record),
            ?TDEF_FE(t_set_key_map),
            ?TDEF_FE(t_set_doc_map),
            ?TDEF_FE(t_set_worker_map)
        ]
    }.

t_set_key_record(_) ->
    ViewRow1 = #view_row{key = key},
    ViewRow2 = #view_row{key = updated_key},
    ?assertEqual(ViewRow2, set_key(ViewRow1, updated_key)).

t_set_key_map(_) ->
    ViewRow1 = {view_row, #{key => key}},
    ViewRow2 = {view_row, #{key => updated_key}},
    ?assertEqual(ViewRow2, set_key(ViewRow1, updated_key)).

t_set_doc_record(_) ->
    ViewRow1 = #view_row{doc = doc},
    ViewRow2 = #view_row{doc = updated_doc},
    ?assertEqual(ViewRow2, set_doc(ViewRow1, updated_doc)).

t_set_doc_map(_) ->
    ViewRow1 = {view_row, #{doc => doc}},
    ViewRow2 = {view_row, #{doc => updated_doc}},
    ?assertEqual(ViewRow2, set_doc(ViewRow1, updated_doc)).

t_set_worker_record(_) ->
    ViewRow1 = #view_row{worker = worker},
    ViewRow2 = #view_row{worker = updated_worker},
    ?assertEqual(ViewRow2, set_worker(ViewRow1, updated_worker)).

t_set_worker_map(_) ->
    ViewRow1 = {view_row, #{worker => worker}},
    ViewRow2 = {view_row, #{worker => updated_worker}},
    ?assertEqual(ViewRow2, set_worker(ViewRow1, updated_worker)).

transform_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            ?TDEF_FE(t_transform_record_reduce_overflow_error),
            ?TDEF_FE(t_transform_record_reduced),
            ?TDEF_FE(t_transform_record_id_undefined),
            ?TDEF_FE(t_transform_record_doc_undefined),
            ?TDEF_FE(t_transform_record_doc_error),
            ?TDEF_FE(t_transform_record),
            ?TDEF_FE(t_transform_map_reduce_overflow_error),
            ?TDEF_FE(t_transform_map_reduced),
            ?TDEF_FE(t_transform_map_id_undefined),
            ?TDEF_FE(t_transform_map_doc_undefined),
            ?TDEF_FE(t_transform_map_doc_error),
            ?TDEF_FE(t_transform_map)
        ]
    }.

t_transform_record_reduce_overflow_error(_) ->
    ViewRow = #view_row{value = {[{reduce_overflow_error, reason}]}},
    Props = {row, [{key, null}, {id, error}, {value, reduce_overflow_error}, {reason, reason}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_map_reduce_overflow_error(_) ->
    ViewRow = {view_row, #{value => {[{reduce_overflow_error, reason}]}}},
    Props = {row, [{key, null}, {id, error}, {value, reduce_overflow_error}, {reason, reason}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_record_reduced(_) ->
    ViewRow = #view_row{key = key, id = reduced, value = value},
    Props = {row, [{key, key}, {value, value}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_map_reduced(_) ->
    ViewRow = {view_row, #{key => key, id => reduced, value => value}},
    Props = {row, [{key, key}, {value, value}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_record_id_undefined(_) ->
    ViewRow = #view_row{key = key, id = undefined},
    Props = {row, [{key, key}, {id, error}, {value, not_found}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_map_id_undefined(_) ->
    ViewRow = {view_row, #{key => key, id => undefined}},
    Props = {row, [{key, key}, {id, error}, {value, not_found}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_record_doc_undefined(_) ->
    ViewRow = #view_row{key = key, id = id, value = value, doc = undefined},
    Props = {row, [{id, id}, {key, key}, {value, value}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_map_doc_undefined(_) ->
    ViewRow = {view_row, #{key => key, id => id, value => value, doc => undefined}},
    Props = {row, [{id, id}, {key, key}, {value, value}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_record_doc_error(_) ->
    ViewRow = #view_row{key = key, id = id, doc = {error, reason}},
    Props = {row, [{id, error}, {key, key}, {value, reason}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_map_doc_error(_) ->
    ViewRow = {view_row, #{key => key, id => id, doc => {error, reason}}},
    Props = {row, [{id, error}, {key, key}, {value, reason}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_record(_) ->
    ViewRow = #view_row{key = key, id = id, value = value, doc = doc},
    Props = {row, [{id, id}, {key, key}, {value, value}, {doc, doc}]},
    ?assertEqual(Props, transform(ViewRow)).

t_transform_map(_) ->
    ViewRow = {view_row, #{key => key, id => id, value => value, doc => doc}},
    Props = {row, [{id, id}, {key, key}, {value, value}, {doc, doc}]},
    ?assertEqual(Props, transform(ViewRow)).

-endif.
