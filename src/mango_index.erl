-module(mango_index).

-export([
    dbname/1,
    ddoc/1,
    name/1,
    type/1,
    cursor_mod/1,
    columns/1
]).

-export([
    create/3,
    list/2
]).


-include_lib("couch/include/couch_db.hrl").


-record(idx, {
    dbname,
    ddoc,
    name,
    type,
    def
}).


dbname(#idx{dbname=DbName}) ->
    DbName.


ddoc(#idx{ddoc=DDoc}) ->
    DDoc.


name(#idx{name=Name}) ->
    Name.


type(#idx{type=Type}) ->
    Type.


cursor_mod(#idx{type=view}) ->
    mango_cursor_view.


columns(#idx{type=view, def={Props}}) ->
    {<<"map">>, {MapProps}} = lists:keysearch(<<"map">>, 1, Props),
    {<<"key">>, {KeyProps}} = lists:keysearch(<<"key">>, 1, MapProps),
    [Key || {Key, _Dir} <- KeyProps].


create(DbName, {Opts0}, Ctx) ->
    Opts = validate_opts(Opts0),
    DDoc = case mango_doc:open(DbName, ddoc_id(Opts0), Ctx) of
        {ok, DDoc0} ->
            DDoc0;
        not_found ->
            #doc{id = ddoc_id(Opts0), body = {[]}}
    end,
    case add_index(DDoc, Opts) of
        DDoc ->
            ok;
        #doc{}=NewDDoc ->
            Results = case mango_doc:save(DbName, NewDDoc, Ctx) of
                {ok, Results0} ->
                    Results0;
                {accepted, Results0} ->
                    Results0;
                {error, Results0} ->
                    Results0
            end,
            case Results of
                [{ok, _Rev}] ->
                    ok;
                [Error] ->
                    throw({error, {doc_update_error, NewDDoc#doc.id, Error}})
            end;
        Else ->
            Else
    end.


list(DbName, Ctx) ->
    case mango_doc:open_ddocs(DbName, Ctx) of
        {ok, DDocs} ->
            lists:foldl(fun(Doc, Acc) ->
                list_doc_indexes(DbName, Doc) ++ Acc
            end, [], DDocs);
        not_found ->
            []
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


list_doc_indexes(DbName, Doc) ->
    % This is a bit silly but I'm planning ahead for
    % when we have Geo/Lucene based indices.
    ListFuns = [
        fun list_views/2
    ],
    lists:foldl(fun(Fun, Acc) ->
        Fun(DbName, Doc) ++ Acc
    end, ListFuns).


list_views(DbName, #doc{body={Props}}=Doc) ->
    case lists:keysearch(<<"language">>, 1, Props) of
        {<<"language">>, <<"mongo">>} ->
            case lists:keysearch(<<"views">>, 1, Props) of
                {<<"views">>, {Views}} when is_list(Views) ->
                    lists:foldl(fun({Name, View}, InAcc) ->
                        I = #idx{
                            dbname=DbName,
                            ddoc = Doc,
                            type = view,
                            name = Name,
                            def = View
                        },
                        [I | InAcc]
                    end, [], Views);
                _ ->
                    []
            end;
        _ ->
            []
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
