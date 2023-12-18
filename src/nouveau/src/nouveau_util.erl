%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(nouveau_util).

-include("nouveau.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([
    index_name/1,
    design_doc_to_indexes/2,
    design_doc_to_index/3,
    verify_index_exists/2,
    ensure_local_purge_docs/2,
    maybe_create_local_purge_doc/2,
    get_local_purge_doc_id/1,
    get_local_purge_doc_body/3,
    nouveau_url/0,
    max_sessions/0,
    max_pipeline_size/0
]).

index_name(Path) when is_binary(Path) ->
    <<(node_prefix())/binary, "/", Path/binary>>;
index_name(#index{} = Index) ->
    <<(node_prefix())/binary, "/", (Index#index.dbname)/binary, "/", (Index#index.sig)/binary>>.

node_prefix() ->
    atom_to_binary(node(), utf8).

%% copied from dreyfus_index.erl
design_doc_to_indexes(DbName, #doc{body = {Fields}} = Doc) ->
    RawIndexes = couch_util:get_value(<<"nouveau">>, Fields, {[]}),
    case RawIndexes of
        {IndexList} when is_list(IndexList) ->
            {IndexNames, _} = lists:unzip(IndexList),
            lists:flatmap(
                fun(IndexName) ->
                    case (catch design_doc_to_index(DbName, Doc, IndexName)) of
                        {ok, #index{} = Index} -> [Index];
                        _ -> []
                    end
                end,
                IndexNames
            );
        _ ->
            []
    end.

%% copied from dreyfus_index.erl
design_doc_to_index(DbName, #doc{id = Id, body = {Fields}}, IndexName) ->
    Language = couch_util:get_value(<<"language">>, Fields, <<"javascript">>),
    {RawIndexes} = couch_util:get_value(<<"nouveau">>, Fields, {[]}),
    InvalidDDocError =
        {invalid_design_doc, <<"index `", IndexName/binary, "` must have parameter `index`">>},
    case lists:keyfind(IndexName, 1, RawIndexes) of
        false ->
            {error, {not_found, <<IndexName/binary, " not found.">>}};
        {IndexName, {Index}} ->
            DefaultAnalyzer = couch_util:get_value(<<"default_analyzer">>, Index, <<"standard">>),
            FieldAnalyzers = couch_util:get_value(<<"field_analyzers">>, Index, #{}),
            case couch_util:get_value(<<"index">>, Index) of
                undefined ->
                    {error, InvalidDDocError};
                Def ->
                    Sig = ?l2b(
                        couch_util:to_hex(
                            crypto:hash(
                                sha256,
                                ?term_to_bin(
                                    {DefaultAnalyzer, FieldAnalyzers, Def}
                                )
                            )
                        )
                    ),
                    {ok, #index{
                        dbname = DbName,
                        default_analyzer = DefaultAnalyzer,
                        field_analyzers = FieldAnalyzers,
                        ddoc_id = Id,
                        def = Def,
                        def_lang = Language,
                        name = IndexName,
                        sig = Sig
                    }}
            end;
        _ ->
            {error, InvalidDDocError}
    end.

verify_index_exists(DbName, Props) ->
    try
        Type = couch_util:get_value(<<"type">>, Props),
        if
            Type =/= <<"nouveau">> ->
                false;
            true ->
                DDocId = couch_util:get_value(<<"ddoc_id">>, Props),
                IndexName = couch_util:get_value(<<"indexname">>, Props),
                Sig = couch_util:get_value(<<"signature">>, Props),
                couch_util:with_db(DbName, fun(Db) ->
                    case couch_db:get_design_doc(Db, DDocId) of
                        {ok, #doc{} = DDoc} ->
                            {ok, IdxState} = design_doc_to_index(
                                DbName, DDoc, IndexName
                            ),
                            IdxState#index.sig == Sig;
                        {not_found, _} ->
                            false
                    end
                end)
        end
    catch
        _:_ ->
            false
    end.

ensure_local_purge_docs(DbName, DDocs) ->
    couch_util:with_db(DbName, fun(Db) ->
        lists:foreach(
            fun(DDoc) ->
                #doc{body = {Props}} = DDoc,
                case couch_util:get_value(<<"indexes">>, Props) of
                    undefined ->
                        false;
                    _ ->
                        try design_doc_to_indexes(DbName, DDoc) of
                            SIndexes -> ensure_local_purge_doc(Db, SIndexes)
                        catch
                            _:_ ->
                                ok
                        end
                end
            end,
            DDocs
        )
    end).

ensure_local_purge_doc(Db, SIndexes) ->
    if
        SIndexes =/= [] ->
            lists:map(
                fun(SIndex) ->
                    maybe_create_local_purge_doc(Db, SIndex)
                end,
                SIndexes
            );
        true ->
            ok
    end.

maybe_create_local_purge_doc(Db, Index) ->
    DocId = get_local_purge_doc_id(Index#index.sig),
    case couch_db:open_doc(Db, DocId) of
        {ok, _Doc} ->
            ok;
        {not_found, _} ->
            DbPurgeSeq = couch_db:get_purge_seq(Db),
            DocContent = get_local_purge_doc_body(
                DocId, DbPurgeSeq, Index
            ),
            couch_db:update_doc(Db, DocContent, [])
    end.

get_local_purge_doc_id(Sig) ->
    iolist_to_binary([?LOCAL_DOC_PREFIX, "purge-", "nouveau-", Sig]).

get_local_purge_doc_body(LocalDocId, PurgeSeq, Index) ->
    #index{
        name = IdxName,
        ddoc_id = DDocId,
        sig = Sig
    } = Index,
    NowSecs = os:system_time(second),
    JsonList =
        {[
            {<<"_id">>, LocalDocId},
            {<<"purge_seq">>, PurgeSeq},
            {<<"updated_on">>, NowSecs},
            {<<"indexname">>, IdxName},
            {<<"ddoc_id">>, DDocId},
            {<<"signature">>, Sig},
            {<<"type">>, <<"nouveau">>}
        ]},
    couch_doc:from_json_obj(JsonList).

nouveau_url() ->
    config:get("nouveau", "url", "http://127.0.0.1:5987").

max_sessions() ->
    config:get_integer("nouveau", "max_sessions", 100).

max_pipeline_size() ->
    config:get_integer("nouveau", "max_pipeline_size", 1000).
