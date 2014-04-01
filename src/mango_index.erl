-module(mango_index).


-export([
    create/3
]).


-include_lib("couch/include/couch_db.hrl").


create(DbName, {Opts0}, Ctx) ->
    Opts = validate_opts(Opts0),
    twig:log(err, "Valid Opts: ~p", [Opts]),
    DDoc = case mango_doc:open(DbName, ddoc_id(Opts0), Ctx) of
        {ok, DDoc0} ->
            DDoc0;
        not_found ->
            #doc{id = ddoc_id(Opts0), body = {[]}}
    end,
    twig:log(err, "DDoc: ~p", [DDoc]),
    case add_index(DDoc, Opts) of
        DDoc ->
            ok;
        #doc{}=NewDDoc ->
            twig:log(err, "New DDoc: ~s ~p", [DbName, NewDDoc]),
            case mango_doc:save(DbName, NewDDoc, Ctx) of
                {ok, _} ->
                    ok;
                {accepted, _} ->
                    twig:log(err, "saved accepted", []),
                    ok;
                {error, [Error]} ->
                    twig:log(err, "saved error ~p", [Error]),
                    {error, {doc_update_error, NewDDoc#doc.id, Error}}
            end;
        Else ->
            Else
    end.


add_index(#doc{body={Props}}=DDoc, Opts) ->
    AllViewProps = case proplists:get_value(<<"views">>, Props) of
        {AllViewProps0} ->
            AllViewProps0;
        _ ->
            []
    end,

    ViewName = view_name(Opts),

    NewView = {ViewName, make_view(Opts)},
    AllViewProps1 = lists:keystore(ViewName, 1, AllViewProps, NewView),

    NewViews = {<<"views">>, {AllViewProps1}},
    NewProps1 = lists:keystore(<<"views">>, 1, Props, NewViews),

    LangProp = {<<"language">>, <<"mongo">>},
    NewProps2 = lists:keystore(<<"language">>, 1, NewProps1, LangProp),

    DDoc#doc{body={NewProps2}}.


make_view(Opts) ->
    {<<"key">>, Key} = lists:keyfind(<<"key">>, 1, Opts),
    SortOrder = key_to_sort(Key),
    {[
        {<<"map">>, {Opts}},
        {<<"reduce">>, <<"_count">>},
        {<<"options">>, {[{<<"sort_order">>, SortOrder}]}}
    ]}.


ddoc_id(Opts) ->
    case proplists:get_value(<<"design_doc">>, Opts) of
        <<"_design/", _/binary>> = Id ->
            Id;
        Id when is_binary(Id) ->
            <<"_design/", Id/binary>>;
        _ ->
            <<"_design/mongo">>
    end.


view_name(Opts) ->
    case proplists:get_value(<<"name">>, Opts) of
        Name when is_binary(Name) ->
            Name;
        _ ->
            {<<"key">>, {Key}} = lists:keyfind(<<"key">>, 1, Opts),
            Parts = lists:foldl(fun({K, V}, Acc) ->
                [integer_to_list(V), binary_to_list(K) | Acc]
            end, [], Key),
            Name = string:join(lists:reverse(Parts), ""),
            list_to_binary(Name)
    end.


key_to_sort({Props}) ->
    [V || {_, V} <- Props].


validate_opts(Opts0) ->
    try
        Opts1 = lists:flatmap(fun validate/1, Opts0),
        case lists:keyfind(<<"key">>, 1, Opts1) of
            false ->
                throw({invalid_index, no_key});
            _ ->
                ok
        end,
        Opts2 = case lists:keyfind(<<"name">>, 1, Opts1) of
            {<<"name">>, _} ->
                Opts1;
            false ->
                [{<<"name">>, view_name(Opts1)} | Opts1]
        end,
        lists:usort(Opts2)
    catch throw:Reason ->
        Reason
    end.


validate({<<"key">>, {KeyProps}}) ->
    lists:foreach(fun({K, V}) ->
        if is_binary(K) -> ok; true ->
            throw({invalid_key_field, K})
        end,
        if V == -1 orelse V == 1 -> ok; true ->
            throw({invalid_key_sort, V})
        end
    end, KeyProps),
    [{<<"key">>, {KeyProps}}];
validate({<<"background">>, true}) ->
    [];
validate({<<"background">>, false}=KV) ->
    throw({not_supported, KV});
validate({<<"unique">>, false}) ->
    [];
validate({<<"unique">>, true}=KV) ->
    throw({not_supported, KV});
validate({<<"ddoc_id">>, Name}) when is_binary(Name) ->
    [];
validate({<<"name">>, Name}) when is_binary(Name) ->
    [{<<"name">>, Name}];
validate({<<"dropDups">>, false}) ->
    [];
validate({<<"dropDups">>, true}=KV) ->
    throw({not_supported, KV});
validate({<<"sparse">>, Value}) when is_boolean(Value) ->
    [{<<"sparse">>, Value}];
validate({<<"expiresAfterSeconds">>, _}=KV) ->
    throw({not_supported, KV});
validate({<<"v">>, _}=KV) ->
    throw({not_supported, KV});
validate({<<"weights">>, _}=KV) ->
    throw({not_supported, KV});
validate({<<"default_language">>, _}=KV) ->
    throw({not_supported, KV});
validate({<<"language_override">>, _}=KV) ->
    throw({not_supported, KV});
validate(KV) ->
    throw({invalid_index_option, KV}).
