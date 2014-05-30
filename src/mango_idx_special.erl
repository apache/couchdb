-module(mango_idx_special).


-export([
    validate/1,
    add/2,
    remove/2,
    from_ddoc/1,
    to_json/1,
    columns/1,
    start_key/1,
    end_key/1
]).


-include("mango_idx.hrl").


validate(_) ->
    erlang:exit(invalid_call).


add(_, _) ->
    erlang:exit(invalid_call).


remove(_, _) ->
    erlang:exit(invalid_call).


from_ddoc(_) ->
    erlang:exit(invalid_call).


to_json(#idx{def=all_docs}) ->
    {[
        {ddoc, null},
        {name, <<"_all_docs">>},
        {type, <<"special">>},
        {def, {[
            {<<"fields">>, [{[
                {<<"_id">>, <<"asc">>}
            ]}]}
        ]}}
    ]}.


columns(#idx{def=all_docs}) ->
    [<<"_id">>].


start_key([{'$gt', Key, _, _}]) ->
    Key;
start_key([{'$gte', Key, _, _}]) ->
    Key;
start_key([{'$eq', Key, '$eq', Key}]) ->
    Key.


end_key([{_, _, '$lt', Key}]) ->
    Key;
end_key([{_, _, '$lte', Key}]) ->
    Key;
end_key([{'$eq', Key, '$eq', Key}]) ->
    Key.
