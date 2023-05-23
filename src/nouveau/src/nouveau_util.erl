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
    nouveau_url/0
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

nouveau_url() ->
    config:get("nouveau", "url", "http://127.0.0.1:8080").
