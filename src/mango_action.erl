-module(mango_action).

-export([
    new/2,
    run/3,
    
    format_error/1
]).


-include("mango.hrl").


new(Db, {Props}) ->
    Mod = get_action_mod({Props}),
    {Mod, Mod:init(Db, {Props})};
new(_, _) ->
    ?MANGO_ERROR(action_must_be_an_object).


run(Writer, Db, {Mod, St}) ->
    {ok, Result} = Mod:run(Writer, Db, St),
    case mango_writer:is_writer(Result) of
        true ->
            {ok, Result};
        false ->
            mango_writer:add_value(Writer, Result)
    end.


format_error(action_must_be_an_object) ->
    <<"Each action must be a JSON object.">>;
format_error(no_action_specified) ->
    <<"The \"action\" key is required for all actions">>;
format_error({invalid_action, Action}) ->
    mango_util:fmt("Invalid action: ~p", [Action]);
format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).


get_action_mod({Props}) ->
    ActionMods = [
        {<<"delete">>, mango_act_crud_delete},
        {<<"find">>, mango_act_crud_find},
        {<<"insert">>, mango_act_crud_insert},
        {<<"update">>, mango_act_crud_update},
        
        {<<"create_index">>, mango_act_idx_create},
        {<<"list_indexes">>, mango_act_idx_list},
        {<<"delete_index">>, mango_act_idx_delete},
        
        {<<"describe_selector">>, mango_act_sel_describe}
    ],
    case lists:keyfind(<<"action">>, 1, Props) of
        {<<"action">>, Name} when is_binary(Name) ->
            case lists:keyfind(Name, 1, ActionMods) of
                {Name, Mod} ->
                    Mod;
                _ ->
                    ?MANGO_ERROR({invalid_action, Name})
            end;
        _ ->
            ?MANGO_ERROR(no_action_specified)
    end.


