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


update(Db, deleted, _, OldDoc) ->
    ok;
update(Db, updated, Doc, OldDoc) ->
    ok;
update(Db, created, Doc, _) ->
%%    Indexes = mango_idx:list(Db),
%%    Fun = fun (DDoc, Acc) ->
%%        io:format("DESIGN DOC ~p ~n", [DDoc]),
%%        Acc
%%    end,
%%    fabric2_db:fold_design_docs(Db, Fun, [], []),
%%    % maybe validate indexes here
%%    JSONDoc = mango_json:to_binary(couch_doc:to_json_obj(Doc, [])),
%%    io:format("Update ~p ~n, ~p ~n", [Doc, JSONDoc]),
%%    Results = index_doc(Indexes, JSONDoc),
    ok.


index_doc(Indexes, Doc) ->
    lists:map(fun(Idx) -> get_index_entries(Idx, Doc) end, Indexes).


get_index_entries({IdxProps}, Doc) ->
    {Fields} = couch_util:get_value(<<"fields">>, IdxProps),
    Selector = get_index_partial_filter_selector(IdxProps),
    case should_index(Selector, Doc) of
        false ->
            [];
        true ->
            Values = get_index_values(Fields, Doc),
            case lists:member(not_found, Values) of
                true -> [];
                false -> [[Values, null]]
            end
    end.


get_index_values(Fields, Doc) ->
    lists:map(fun({Field, _Dir}) ->
        case mango_doc:get_field(Doc, Field) of
            not_found -> not_found;
            bad_path -> not_found;
            Value -> Value
        end
    end, Fields).


get_index_partial_filter_selector(IdxProps) ->
    case couch_util:get_value(<<"partial_filter_selector">>, IdxProps, {[]}) of
        {[]} ->
            % this is to support legacy text indexes that had the partial_filter_selector
            % set as selector
            couch_util:get_value(<<"selector">>, IdxProps, {[]});
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

