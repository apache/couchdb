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
    update/4
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango_idx.hrl").

% Design doc
% Todo: Check if design doc is mango index and kick off background worker
% to build new index
update(Db, Change, #doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, OldDoc) ->
    io:format("DESIGN DOC SAVED ~p ~n", [Doc]),
    ok;

update(Db, deleted, _, OldDoc)  ->
    ok;

update(Db, updated, Doc, OldDoc) ->
    ok;

update(Db, created, Doc, _) ->
    try
        io:format("CREATED ~p ~n", [Doc]),
        #doc{id = DocId} = Doc,
        Indexes = mango_idx:list(Db),
        Indexes1 = filter_json_indexes(Indexes),
        io:format("UPDATE INDEXES ~p ~n filtered ~p ~n", [Indexes, Indexes1]),
        JSONDoc = mango_json:to_binary(couch_doc:to_json_obj(Doc, [])),
        io:format("DOC ~p ~n", [Doc]),
        Results = index_doc(Indexes1, JSONDoc),
        io:format("Update ~p ~n, ~p ~n Results ~p ~n", [Doc, JSONDoc, Results]),
        mango_fdb:write_doc(Db, DocId, Results)
    catch
        Error:Reason ->
            io:format("ERROR ~p ~p ~p ~n", [Error, Reason, erlang:display(erlang:get_stacktrace())]),
            ok
    end,
    ok.


filter_json_indexes(Indexes) ->
    lists:filter(fun (Idx) ->
        Idx#idx.type == <<"json">>
    end, Indexes).


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

