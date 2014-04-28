-module(mango_idx_view).


-export([
    add/2,
    from_ddoc/1,
    columns/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").
-include("mango_idx.hrl").


add(#doc{body={Props0}}=DDoc, Idx) ->
    Views1 = case proplists:get_value(<<"views">>, Props0) of
        {Views0} -> Views0;
        _ -> []
    end,
    NewView = make_view(Idx),
    Views2 = lists:keystore(element(1, NewView), 1, Props0, Views1),
    Props1 = lists:keystore(<<"views">>, 1, Props0, {<<"views">>, Views2}),
    {ok, DDoc#doc{body={Props1}}}.


from_ddoc({Props}) ->
    case lists:keyfind(<<"views">>, 1, Props) of
        {<<"views">>, {Views}} when is_list(Views) ->
            lists:flatmap(fun({Name, {VProps}}) ->
                Def = proplists:get_value(<<"map">>, VProps),
                {Opts0} = proplists:get_value(<<"options">>, VProps),
                Opts = lists:keydelete(<<"sort">>, 1, Opts0),
                I = #idx{
                    type = view,
                    name = Name,
                    def = Def,
                    opts = Opts
                },
                % TODO: Validate the index definition
                [I]
            end, Views);
        _ ->
            []
    end.


columns(Idx) ->
    {Props} = Idx#idx.def,
    {<<"map">>, {Def}} = lists:keyfind(<<"map">>, 1, Props),
    [Key || {Key, _} <- Def].


make_view(Idx) ->
    Sort = to_sort(Idx#idx.def),
    View = {[
        {<<"map">>, Idx#idx.def},
        {<<"reduce">>, <<"_count">>},
        {<<"options">>, {[{<<"sort">>, Sort} | Idx#idx.opts]}}
    ]},
    {Idx#idx.name, View}.


to_sort({Props}) ->
    [Dir || {_, Dir} <- Props].
