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

% This module is for the "index object" as in, the data structure
% representing an index. Not to be confused with mango_index which
% contains APIs for managing indexes.

-module(mango_idx).


-export([
    list/1,
    recover/1,

    new/2,
    validate_new/2,
    add/2,
    remove/2,
    from_ddoc/2,
    special/1,

    dbname/1,
    ddoc/1,
    name/1,
    type/1,
    def/1,
    opts/1,
    columns/1,
    is_usable/3,
    start_key/2,
    end_key/2,
    cursor_mod/1,
    idx_mod/1,
    to_json/1,
    delete/4,
    get_usable_indexes/3,
    get_partial_filter_selector/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").
-include("mango_idx.hrl").
-include_lib("couch_views/include/couch_views.hrl").


list(Db) ->
    DDocs = couch_views_ddoc:get_mango_list(Db),
    DbName = fabric2_db:name(Db),
    Indexes = lists:foldl(fun(DDoc, Acc) ->
        {Props} = couch_doc:to_json_obj(DDoc, []),

        case proplists:get_value(<<"language">>, Props) == <<"query">> of
            true ->
                {ok, Mrst} = couch_mrview_util:ddoc_to_mrst(DbName, DDoc),

                IsInteractive = couch_views_ddoc:is_interactive(DDoc),
                BuildState = couch_views_fdb:get_build_status(Db, Mrst),

                Idxs = lists:map(fun(Idx) ->
                    Idx#idx{
                        build_status = BuildState,
                        interactive = IsInteractive
                    }
                end, from_ddoc(Db, DDoc)),
                Acc ++ Idxs;
            false ->
                Acc
        end

    end, [], DDocs),
    Indexes ++ special(Db).


get_usable_indexes(Db, Selector, Opts) ->
    ExistingIndexes = mango_idx:list(Db),
    GlobalIndexes = mango_cursor:remove_indexes_with_partial_filter_selector(
            ExistingIndexes
        ),
    BuiltIndexes = mango_cursor:remove_unbuilt_indexes(GlobalIndexes),
    UserSpecifiedIndex = mango_cursor:maybe_filter_indexes_by_ddoc(ExistingIndexes, Opts),
    UsableIndexes0 = lists:usort(BuiltIndexes ++ UserSpecifiedIndex),

    SortFields = get_sort_fields(Opts),
    UsableFilter = fun(I) -> is_usable(I, Selector, SortFields) end,

    case lists:filter(UsableFilter, UsableIndexes0) of
        [] ->
            mango_sort_error(Db, Opts);
        UsableIndexes ->
            UsableIndexes
    end.


mango_sort_error(_Db, _Opts) ->
    ?MANGO_ERROR({no_usable_index, missing_sort_index}).


recover(Db) ->
    {ok, DDocs0} = mango_util:open_ddocs(Db),
    Pred = fun({Props}) ->
        case proplists:get_value(<<"language">>, Props) of
            <<"query">> -> true;
            _ -> false
        end
    end,
    DDocs = lists:filter(Pred, DDocs0),
    Special = special(Db),
    {ok, Special ++ lists:flatmap(fun(Doc) ->
        from_ddoc(Db, Doc)
    end, DDocs)}.


get_sort_fields(Opts) ->
    case lists:keyfind(sort, 1, Opts) of
        {sort, Sort} ->
            mango_sort:fields(Sort);
        _ ->
            []
    end.


new(Db, Opts) ->
    Def = get_idx_def(Opts),
    Type = get_idx_type(Opts),
    IdxName = get_idx_name(Def, Opts),
    DDoc = get_idx_ddoc(Def, Opts),
    {ok, #idx{
        dbname = db_to_name(Db),
        ddoc = DDoc,
        name = IdxName,
        type = Type,
        def = Def,
        opts = filter_opts(Opts)
    }}.


validate_new(Idx, Db) ->
    Mod = idx_mod(Idx),
    Mod:validate_new(Idx, Db).


add(DDoc, Idx) ->
    Mod = idx_mod(Idx),
    {ok, NewDDoc} = Mod:add(DDoc, Idx),
    % Round trip through JSON for normalization
    Body = ?JSON_DECODE(?JSON_ENCODE(NewDDoc#doc.body)),
    {ok, NewDDoc#doc{body = Body}}.


remove(DDoc, Idx) ->
    Mod = idx_mod(Idx),
    {ok, NewDDoc} = Mod:remove(DDoc, Idx),
    % Round trip through JSON for normalization
    Body = ?JSON_DECODE(?JSON_ENCODE(NewDDoc#doc.body)),
    {ok, NewDDoc#doc{body = Body}}.


delete(Filt, Db, Indexes, DelOpts) ->
    case lists:filter(Filt, Indexes) of
        [Idx] ->
            {ok, DDoc} = mango_util:load_ddoc(Db, mango_idx:ddoc(Idx)),
            {ok, NewDDoc} = mango_idx:remove(DDoc, Idx),
            FinalDDoc = case NewDDoc#doc.body of
                {[{<<"language">>, <<"query">>}]} ->
                    NewDDoc#doc{deleted = true, body = {[]}};
                _ ->
                    NewDDoc
            end,
            case mango_crud:insert(Db, FinalDDoc, DelOpts) of
                {ok, _} ->
                    {ok, true};
                Error ->
                    {error, Error}
            end;
        [] ->
            {error, not_found}
    end.


from_ddoc(Db, #doc{id = DDocId} = DDoc) ->
    {Props} = couch_doc:to_json_obj(DDoc, []),
    DbName = db_to_name(Db),
    DDocId = proplists:get_value(<<"_id">>, Props),

    case proplists:get_value(<<"language">>, Props) of
        <<"query">> -> ok;
        _ ->
            ?MANGO_ERROR(invalid_query_ddoc_language)
    end,
    IdxMods = case is_text_service_available() of
        true ->
            [mango_idx_view, mango_idx_text];
        false ->
            [mango_idx_view]
    end,
    Idxs = lists:flatmap(fun(Mod) -> Mod:from_ddoc({Props}) end, IdxMods),
    lists:map(fun(Idx) ->
        Idx#idx{
            dbname = DbName,
            ddoc = DDocId
        }
    end, Idxs).


special(Db) ->
    AllDocs = #idx{
        dbname = db_to_name(Db),
        name = <<"_all_docs">>,
        type = <<"special">>,
        def = all_docs,
        opts = [],
        build_status = ?INDEX_READY
    },
    % Add one for _update_seq
    [AllDocs].


dbname(#idx{dbname=DbName}) ->
    DbName.


ddoc(#idx{ddoc=DDoc}) ->
    DDoc.


name(#idx{name=Name}) ->
    Name.


type(#idx{type=Type}) ->
    Type.


def(#idx{def=Def}) ->
    Def.


opts(#idx{opts=Opts}) ->
    Opts.


to_json(#idx{}=Idx) ->
    Mod = idx_mod(Idx),
    Mod:to_json(Idx).


columns(#idx{}=Idx) ->
    Mod = idx_mod(Idx),
    Mod:columns(Idx).


is_usable(#idx{}=Idx, Selector, SortFields) ->
    Mod = idx_mod(Idx),
    Mod:is_usable(Idx, Selector, SortFields).


start_key(#idx{}=Idx, Ranges) ->
    Mod = idx_mod(Idx),
    Mod:start_key(Ranges).


end_key(#idx{}=Idx, Ranges) ->
    Mod = idx_mod(Idx),
    Mod:end_key(Ranges).


cursor_mod(#idx{type = <<"json">>}) ->
    mango_cursor_view;
cursor_mod(#idx{def = all_docs, type= <<"special">>}) ->
    mango_cursor_special;
cursor_mod(#idx{type = <<"text">>}) ->
    case clouseau_rpc:connected() of
        true ->
            mango_cursor_text;
        false ->
            ?MANGO_ERROR({index_service_unavailable, <<"text">>})
    end.


idx_mod(#idx{type = <<"json">>}) ->
    mango_idx_view;
idx_mod(#idx{type = <<"special">>}) ->
    mango_idx_special;
idx_mod(#idx{type = <<"text">>}) ->
    case clouseau_rpc:connected() of
        true ->
            mango_idx_text;
        false ->
            ?MANGO_ERROR({index_service_unavailable, <<"text">>})
    end.


db_to_name(Name) when is_binary(Name) ->
    Name;
db_to_name(Name) when is_list(Name) ->
    iolist_to_binary(Name);
db_to_name(Db) ->
    fabric2_db:name(Db).


get_idx_def(Opts) ->
    case proplists:get_value(def, Opts) of
        undefined ->
            ?MANGO_ERROR(no_index_definition);
        Def ->
            Def
    end.


get_idx_type(Opts) ->
    case proplists:get_value(type, Opts) of
        <<"json">> -> <<"json">>;
        <<"text">> -> case is_text_service_available() of
            true ->
                <<"text">>;
            false ->
                ?MANGO_ERROR({index_service_unavailable, <<"text">>})
            end;
        %<<"geo">> -> <<"geo">>;
        undefined -> <<"json">>;
        BadType ->
            ?MANGO_ERROR({invalid_index_type, BadType})
    end.


is_text_service_available() ->
    erlang:function_exported(clouseau_rpc, connected, 0) andalso
        clouseau_rpc:connected().


get_idx_ddoc(Idx, Opts) ->
    case proplists:get_value(ddoc, Opts) of
        <<"_design/", _Rest/binary>> = Name ->
            Name;
        Name when is_binary(Name) ->
            <<"_design/", Name/binary>>;
        _ ->
            Bin = gen_name(Idx, Opts),
            <<"_design/", Bin/binary>>
    end.


get_idx_name(Idx, Opts) ->
    case proplists:get_value(name, Opts) of
        Name when is_binary(Name) ->
            Name;
        _ ->
            gen_name(Idx, Opts)
    end.


gen_name(Idx, Opts0) ->
    Opts = lists:usort(Opts0),
    TermBin = term_to_binary({Idx, Opts}),
    Sha = crypto:hash(sha, TermBin),
    mango_util:enc_hex(Sha).


filter_opts([]) ->
    [];
filter_opts([{user_ctx, _} | Rest]) ->
    filter_opts(Rest);
filter_opts([{ddoc, _} | Rest]) ->
    filter_opts(Rest);
filter_opts([{name, _} | Rest]) ->
    filter_opts(Rest);
filter_opts([{type, _} | Rest]) ->
    filter_opts(Rest);
filter_opts([Opt | Rest]) ->
    [Opt | filter_opts(Rest)].


get_partial_filter_selector(#idx{def = Def}) when Def =:= all_docs; Def =:= undefined ->
    undefined;
get_partial_filter_selector(#idx{def = {Def}}) ->
    case proplists:get_value(<<"partial_filter_selector">>, Def) of
        undefined -> get_legacy_selector(Def);
        {[]} -> undefined;
        Selector -> Selector
    end.


% Partial filter selectors is supported in text indexes via the selector field
% This adds backwards support for existing indexes that might have a selector in it
get_legacy_selector(Def) ->
    case proplists:get_value(<<"selector">>, Def) of
        undefined -> undefined;
        {[]} -> undefined;
        Selector -> Selector
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

index(SelectorName, Selector) ->
    {
        idx,<<"mango_test_46418cd02081470d93290dc12306ebcb">>,
           <<"_design/57e860dee471f40a2c74ea5b72997b81dda36a24">>,
           <<"Selected">>,<<"json">>,
           {[{<<"fields">>,{[{<<"location">>,<<"asc">>}]}},
             {SelectorName,{Selector}}]},
           [{<<"def">>,{[{<<"fields">>,[<<"location">>]}]}}],
           <<"ready">>
    }.

get_partial_filter_all_docs_test() ->
    Idx = #idx{def = all_docs},
    ?assertEqual(undefined, get_partial_filter_selector(Idx)).

get_partial_filter_undefined_def_test() ->
    Idx = #idx{def = undefined},
    ?assertEqual(undefined, get_partial_filter_selector(Idx)).

get_partial_filter_selector_default_test() ->
    Idx = index(<<"partial_filter_selector">>, []),
    ?assertEqual(undefined, get_partial_filter_selector(Idx)).

get_partial_filter_selector_missing_test() ->
    Idx = index(<<"partial_filter_selector">>, []),
    ?assertEqual(undefined, get_partial_filter_selector(Idx)).

get_partial_filter_selector_with_selector_test() ->
    Selector = [{<<"location">>,{[{<<"$gt">>,<<"FRA">>}]}}],
    Idx = index(<<"partial_filter_selector">>, Selector),
    ?assertEqual({Selector}, get_partial_filter_selector(Idx)).

get_partial_filter_selector_with_legacy_selector_test() ->
    Selector = [{<<"location">>,{[{<<"$gt">>,<<"FRA">>}]}}],
    Idx = index(<<"selector">>, Selector),
    ?assertEqual({Selector}, get_partial_filter_selector(Idx)).

get_partial_filter_selector_with_legacy_default_selector_test() ->
    Idx = index(<<"selector">>, []),
    ?assertEqual(undefined, get_partial_filter_selector(Idx)).


get_idx_ddoc_name_only_test() ->
    Opts = [{ddoc, <<"foo">>}],
    ?assertEqual(<<"_design/foo">>, get_idx_ddoc({}, Opts)).

get_idx_ddoc_design_slash_name_test() ->
    Opts = [{ddoc, <<"_design/foo">>}],
    ?assertEqual(<<"_design/foo">>, get_idx_ddoc({}, Opts)).

-endif.
