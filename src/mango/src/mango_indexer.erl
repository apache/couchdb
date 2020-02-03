% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


-module(mango_indexer).


-export([
    create_doc/2,
    update_doc/3,
    delete_doc/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango_idx.hrl").


create_doc(Db, Doc) ->
    modify(Db, create, Doc, undefined).


update_doc(Db, Doc, PrevDoc) ->
    modify(Db, update, Doc, PrevDoc).


delete_doc(Db, PrevDoc) ->
    modify(Db, delete, undefined, PrevDoc).


modify(Db, Change, Doc, PrevDoc) ->
    try
        modify_int(Db, Change, Doc, PrevDoc)
    catch
        Error:Reason ->
            io:format("ERROR ~p ~p ~p ~n", [Error, Reason, erlang:display(erlang:get_stacktrace())]),
            #{
                name := DbName
            } = Db,

            Id = doc_id(Doc, PrevDoc),

            couch_log:error("Mango index error for Db ~s Doc ~p ~p ~p",
                [DbName, Id, Error, Reason])
    end,
    ok.


doc_id(undefined, #doc{id = DocId}) ->
    DocId;
doc_id(undefined, _) ->
    <<"unknown_doc_id">>;
doc_id(#doc{id = DocId}, _) ->
    DocId.


% Design doc
% Todo: Check if design doc is mango index and kick off background worker
% to build new index
modify_int(_Db, _Change, #doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc,
        _PrevDoc) ->
    io:format("DESIGN DOC SAVED ~p ~n", [Doc]),
    ok;

modify_int(Db, delete, _, PrevDoc)  ->
    remove_doc(Db, PrevDoc, json_indexes(Db));

modify_int(Db, update, Doc, PrevDoc) ->
    Indexes = json_indexes(Db),
    remove_doc(Db, PrevDoc, Indexes),
    write_doc(Db, Doc, Indexes);

modify_int(Db, create, Doc, _) ->
    write_doc(Db, Doc, json_indexes(Db)).


remove_doc(Db, #doc{} = Doc, Indexes) ->
    #doc{id = DocId} = Doc,
    PrevJSONDoc = mango_json:to_binary(couch_doc:to_json_obj(Doc, [])),
    PrevResults = index_doc(Indexes, PrevJSONDoc),
    mango_fdb:remove_doc(Db, DocId, PrevResults).


write_doc(Db, #doc{} = Doc, Indexes) ->
    #doc{id = DocId} = Doc,
    JSONDoc = mango_json:to_binary(couch_doc:to_json_obj(Doc, [])),
    Results = index_doc(Indexes, JSONDoc),
    mango_fdb:write_doc(Db, DocId, Results).


json_indexes(Db) ->
    lists:filter(fun (Idx) ->
        Idx#idx.type == <<"json">>
    end, mango_idx:list(Db)).


index_doc(Indexes, Doc) ->
    lists:foldl(fun(Idx, Acc) ->
        {IdxDef} = mango_idx:def(Idx),
        Results = get_index_entries(IdxDef, Doc),
        case lists:member(not_found, Results) of
            true ->
                Acc;
            false ->
                IdxResult = #{
                    name => mango_idx:name(Idx),
                    ddoc_id => mango_idx:ddoc(Idx),
                    results => Results
                },
                [IdxResult | Acc]
        end
    end, [], Indexes).


get_index_entries(IdxDef, Doc) ->
    {Fields} = couch_util:get_value(<<"fields">>, IdxDef),
    Selector = get_index_partial_filter_selector(IdxDef),
    case should_index(Selector, Doc) of
        false ->
            [not_found];
        true ->
            Values = get_index_values(Fields, Doc),
            Values
%%            case lists:member(not_found, Values) of
%%                true -> not_found;
%%                false -> [Values]
%%%%                false -> [[Values, null]]
%%            end
    end.


get_index_values(Fields, Doc) ->
    Out1 = lists:map(fun({Field, _Dir}) ->
        case mango_doc:get_field(Doc, Field) of
            not_found -> not_found;
            bad_path -> not_found;
            Value -> Value
        end
    end, Fields),
    io:format("OUT ~p ~p ~n", [Fields, Out1]),
    Out1.


get_index_partial_filter_selector(IdxDef) ->
    case couch_util:get_value(<<"partial_filter_selector">>, IdxDef, {[]}) of
        {[]} ->
            % this is to support legacy text indexes that had the partial_filter_selector
            % set as selector
            couch_util:get_value(<<"selector">>, IdxDef, {[]});
        Else ->
            Else
    end.


should_index(Selector, Doc) ->
    NormSelector = mango_selector:normalize(Selector),
    Matches = mango_selector:match(NormSelector, Doc),
    IsDesign = case mango_doc:get_field(Doc, <<"_id">>) of
        <<"_design/", _/binary>> -> true;
        _ -> false
    end,
    Matches and not IsDesign.


validate_index_info(IndexInfo) ->
    IdxTypes = [mango_idx_view, mango_idx_text],
    Results = lists:foldl(fun(IdxType, Results0) ->
        try
            IdxType:validate_index_def(IndexInfo),
            [valid_index | Results0]
        catch _:_ ->
            [invalid_index | Results0]
        end
    end, [], IdxTypes),
    lists:member(valid_index, Results).

