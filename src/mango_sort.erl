-module(mango_sort).

-export([
    new/1,
    to_json/1,
    fields/1,

    format_error/1
]).


-include("mango.hrl").


new(Fields) when is_list(Fields) ->
    {ok, {[sort_field(Field) || Field <- Fields]}};
new(Else) ->
    ?MANGO_ERROR({invalid_sort_json, Else}).


to_json({Fields}) ->
    to_json(Fields);
to_json([]) ->
    [];
to_json([{Name, Dir} | Rest]) ->
    [{[{Name, Dir}]} | to_json(Rest)].


fields({Fields}) ->
    Fields.


format_error({invalid_sort_json, BadSort}) ->
    mango_util:fmt("Sort must be an array of sort specs, not: ~w", [BadSort]);
format_error({invalid_sort_field, BadField}) ->
    mango_util:fmt("Invalid sort field: ~w", [BadField]);
format_error(Else) ->
    mango_util:fmt("Unknown error: ~w", [Else]).


sort_field(Field) when is_binary(Field) ->
    {Field, <<"asc">>};
sort_field({[{Name, <<"asc">>}]}) when is_binary(Name) ->
    {Name, <<"asc">>};
sort_field({[{Name, <<"desc">>}]}) when is_binary(Name) ->
    {Name, <<"desc">>};
sort_field(Else) ->
    ?MANGO_ERROR({invalid_sort_field, Else}).
