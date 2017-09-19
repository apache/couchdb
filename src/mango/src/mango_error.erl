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

-module(mango_error).


-include_lib("couch/include/couch_db.hrl").


-export([
    info/2
]).


info(mango_idx, {no_usable_index, no_indexes_defined}) ->
    {
        400,
        <<"no_usable_index">>,
        <<"There are no indexes defined in this database.">>
    };
info(mango_idx, {no_usable_index, no_index_matching_name}) ->
    {
        400,
        <<"no_usable_index">>,
        <<"No index matches the index specified with \"use_index\"">>
    };
info(mango_idx, {no_usable_index, missing_sort_index}) ->
    {
        400,
        <<"no_usable_index">>,
        <<"No index exists for this sort, try indexing by the sort fields.">>
    };
info(mango_cursor, {no_usable_index, selector_unsupported}) ->
    {
        400,
        <<"no_usable_index">>,
        <<"There is no index available for this selector.">>
    };

info(mango_json_bookmark, {invalid_bookmark, BadBookmark}) ->
    {
        400,
        <<"invalid_bookmark">>,
        fmt("Invalid bookmark value: ~s", [?JSON_ENCODE(BadBookmark)])
    };

info(mango_cursor_text, {invalid_bookmark, BadBookmark}) ->
    {
        400,
        <<"invalid_bookmark">>,
        fmt("Invalid bookmark value: ~s", [?JSON_ENCODE(BadBookmark)])
    };
info(mango_cursor_text, multiple_text_indexes) ->
    {
        400,
        <<"multiple_text_indexes">>,
        <<"You must specify an index with the `use_index` parameter.">>
    };
info(mango_cursor_text, {text_search_error, {error, {bad_request, Msg}}})
        when is_binary(Msg) ->
    {
        400,
        <<"text_search_error">>,
        Msg
    };
info(mango_cursor_text, {text_search_error, {error, Error}}) ->
    {
        400,
        <<"text_search_error">>,
        fmt("~p", [Error])
    };

info(mango_fields, {invalid_fields_json, BadFields}) ->
    {
        400,
        <<"invalid_fields">>,
        fmt("Fields must be an array of strings, not: ~w", [BadFields])
    };
info(mango_fields, {invalid_field_json, BadField}) ->
    {
        400,
        <<"invalid_field">>,
        fmt("Invalid JSON for field spec: ~w", [BadField])
    };

info(mango_httpd, error_saving_ddoc) ->
    {
        500,
        <<"error_saving_ddoc">>,
        <<"Unknown error while saving the design document.">>
    };
info(mango_httpd, {error_saving_ddoc, <<"conflict">>}) ->
    {
        500,
        <<"error_saving_ddoc">>,
        <<"Encountered a conflict while saving the design document.">>
    };
info(mango_httpd, {error_saving_ddoc, Reason}) ->
    {
        500,
        <<"error_saving_ddoc">>,
        fmt("Unknown error while saving the design document: ~s", [Reason])
    };
info(mango_httpd, invalid_list_index_params) ->
    {
        500,
        <<"invalid_list_index_params">>,
        <<"Index parameter ranges: limit > 1, skip > 0" >>
    };

info(mango_idx, {invalid_index_type, BadType}) ->
    {
        400,
        <<"invalid_index">>,
        fmt("Invalid type for index: ~s", [BadType])
    };
info(mango_idx, invalid_query_ddoc_language) ->
    {
        400,
        <<"invalid_index">>,
        <<"Invalid design document query language.">>
    };
info(mango_idx, no_index_definition) ->
    {
        400,
        <<"invalid_index">>,
        <<"Index is missing its definition.">>
    };
info(mango_idx, {index_not_implemented, IndexName}) ->
    {
        501,
        <<"index_not_implemented">>,
        fmt("~s", [IndexName])
    };
info(mango_idx, {index_service_unavailable, IndexName}) ->
    {
        503,
        <<"required index service unavailable">>,
        fmt("~s", [IndexName])
    };

info(mango_idx_view, {invalid_index_json, BadIdx}) ->
    {
        400,
        <<"invalid_index">>,
        fmt("JSON indexes must be an object, not: ~w", [BadIdx])
    };
info(mango_idx_text, {invalid_index_fields_definition, Def}) ->
    {
        400,
        <<"invalid_index_fields_definition">>,
        fmt("Text Index field definitions must be of the form
            {\"name\": \"non-empty fieldname\", \"type\":
                \"boolean,number, or string\"}. Def: ~p", [Def])
    };
info(mango_idx_view, {index_not_found, BadIdx}) ->
    {
        404,
        <<"invalid_index">>,
        fmt("JSON index ~s not found in this design doc.", [BadIdx])
    };

info(mango_idx_text, {invalid_index_text, BadIdx}) ->
    {
        400,
        <<"invalid_index">>,
        fmt("Text indexes must be an object, not: ~w", [BadIdx])
    };
info(mango_idx_text, {index_not_found, BadIdx}) ->
    {
        404,
        <<"index_not_found">>,
        fmt("Text index ~s not found in this design doc.", [BadIdx])
    };
info(mango_idx_text, index_all_disabled) ->
    {
        403,
        <<"index_all_disabled">>,
        <<"New text indexes are forbidden to index all fields.">>
    };

info(mango_opts, {invalid_bulk_docs, Val}) ->
    {
        400,
        <<"invalid_bulk_docs">>,
        fmt("Bulk Delete requires an array of non-null docids. Docids: ~w",
            [Val])
    };
info(mango_opts, {invalid_ejson, Val}) ->
    {
        400,
        <<"invalid_ejson">>,
        fmt("Invalid JSON value: ~w", [Val])
    };
info(mango_opts, {invalid_key, Key}) ->
    {
        400,
        <<"invalid_key">>,
        fmt("Invalid key ~s for this request.", [Key])
    };
info(mango_opts, {missing_required_key, Key}) ->
    {
        400,
        <<"missing_required_key">>,
        fmt("Missing required key: ~s", [Key])
    };
info(mango_opts, {invalid_value, Name, Expect, Found}) ->
    {
        400,
        <<"invalid_value">>,
        fmt("Value for ~s is ~w, should be ~w", [Name, Found, Expect])
    };
info(mango_opts, {invalid_value, Name, Value}) ->
    {
        400,
        <<"invalid_value">>,
        fmt("Invalid value for ~s: ~w", [Name, Value])
    };
info(mango_opts, {invalid_string, Val}) ->
    {
        400,
        <<"invalid_string">>,
        fmt("Invalid string: ~w", [Val])
    };
info(mango_opts, {invalid_boolean, Val}) ->
    {
        400,
        <<"invalid_boolean">>,
        fmt("Invalid boolean value: ~w", [Val])
    };
info(mango_opts, {invalid_pos_integer, Val}) ->
    {
        400,
        <<"invalid_pos_integer">>,
        fmt("~w is not an integer greater than zero", [Val])
    };
info(mango_opts, {invalid_non_neg_integer, Val}) ->
    {
        400,
        <<"invalid_non_neg_integer">>,
        fmt("~w is not an integer greater than or equal to zero", [Val])
    };
info(mango_opts, {invalid_object, BadObj}) ->
    {
        400,
        <<"invalid_object">>,
        fmt("~w is not a JSON object", [BadObj])
    };
info(mango_opts, {invalid_selector_json, BadSel}) ->
    {
        400,
        <<"invalid_selector_json">>,
        fmt("Selector must be a JSON object, not: ~w", [BadSel])
    };
info(mango_opts, {invalid_index_name, BadName}) ->
    {
        400,
        <<"invalid_index_name">>,
        fmt("Invalid index name: ~w", [BadName])
    };

info(mango_opts, {multiple_text_operator, {invalid_selector, BadSel}}) ->
    {
        400,
        <<"multiple_text_selector">>,
        fmt("Selector cannot contain more than one $text operator: ~w",
            [BadSel])
    };

info(mango_selector, {invalid_selector, missing_field_name}) ->
    {
        400,
        <<"invalid_selector">>,
        <<"One or more conditions is missing a field name.">>
    };
info(mango_selector, {bad_arg, Op, Arg}) ->
    {
        400,
        <<"bad_arg">>,
        fmt("Bad argument for operator ~s: ~w", [Op, Arg])
    };
info(mango_selector, {not_supported, Op}) ->
    {
        400,
        <<"not_supported">>,
        fmt("Unsupported operator: ~s", [Op])
    };
info(mango_selector, {invalid_operator, Op}) ->
    {
        400,
        <<"invalid_operator">>,
        fmt("Invalid operator: ~s", [Op])
    };
info(mango_selector, {bad_field, BadSel}) ->
    {
        400,
        <<"bad_field">>,
        fmt("Invalid field normalization on selector: ~w", [BadSel])
    };

info(mango_selector_text, {invalid_operator, Op}) ->
    {
        400,
        <<"invalid_operator">>,
        fmt("Invalid text operator: ~s", [Op])
    };
info(mango_selector_text, {text_sort_error, Field}) ->
    S = binary_to_list(Field),
    Msg = "Unspecified or ambiguous sort type. Try appending :number or"
        " :string to the sort field. ~s",
    {
        400,
        <<"text_sort_error">>,
        fmt(Msg, [S])
    };

info(mango_sort, {invalid_sort_json, BadSort}) ->
    {
        400,
        <<"invalid_sort_json">>,
        fmt("Sort must be an array of sort specs, not: ~w", [BadSort])
    };
info(mango_sort, {invalid_sort_dir, BadSpec}) ->
    {
        400,
        <<"invalid_sort_dir">>,
        fmt("Invalid sort direction: ~w", BadSpec)
    };
info(mango_sort, {invalid_sort_field, BadField}) ->
    {
        400,
        <<"invalid_sort_field">>,
        fmt("Invalid sort field: ~w", [BadField])
    };
info(mango_sort, {unsupported, mixed_sort}) ->
    {
        400,
        <<"unsupported_mixed_sort">>,
        <<"Sorts currently only support a single direction for all fields.">>
    };

info(mango_util, {error_loading_doc, DocId}) ->
    {
        500,
        <<"internal_error">>,
        fmt("Error loading doc: ~s", [DocId])
    };
info(mango_util, error_loading_ddocs) ->
    {
        500,
        <<"internal_error">>,
        <<"Error loading design documents">>
    };
info(mango_util, {invalid_ddoc_lang, Lang}) ->
    {
        400,
        <<"invalid_ddoc_lang">>,
        fmt("Existing design doc has an invalid language: ~w", [Lang])
    };

info(Module, Reason) ->
    {
        500,
        <<"unknown_error">>,
        fmt("Unknown Error: ~s :: ~w", [Module, Reason])
    }.


fmt(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).
