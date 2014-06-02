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
        
    

