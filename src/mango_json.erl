-module(mango_json).


-export([
    min/0,
    max/0,
    cmp/2,
    type/1,
    special/1
]).


-define(MIN_VAL, mango_json_min).
-define(MAX_VAL, mango_json_max).


min() ->
    ?MIN_VAL.


max() ->
    ?MAX_VAL.


cmp(?MIN_VAL, ?MIN_VAL) ->
    0;
cmp(?MIN_VAL, _) ->
    -1;
cmp(_, ?MIN_VAL) ->
    1;
cmp(?MAX_VAL, ?MAX_VAL) ->
    0;
cmp(?MAX_VAL, _) ->
    1;
cmp(_, ?MAX_VAL) ->
    -1;
cmp(A, B) ->
    couch_view:cmp_json(A, B).


type(null) ->
    <<"null">>;
type(Bool) when is_boolean(Bool) ->
    <<"boolean">>;
type(Num) when is_number(Num) ->
    <<"number">>;
type(Str) when is_binary(Str) ->
    <<"string">>;
type({Props}) when is_list(Props) ->
    <<"object">>;
type(Vals) when is_list(Vals) ->
    <<"array">>.


special(?MIN_VAL) ->
    true;
special(?MAX_VAL) ->
    true;
special(_) ->
    false.
