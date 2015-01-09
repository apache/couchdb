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


-export([
    info/2
]).


info(mango_cursor, {no_usable_index, operator_unsupported}) ->
    {
        400,
        <<"no_usable_index">>,
        <<"There is no operator in this selector can used with an index.">>
    };
info(mango_cursor, {no_usable_index, selector_unsupported}) ->
    {
        400,
        <<"no_usable_index">>,
        <<"There is no index available for this selector.">>
    };
info(mango_cursor, {no_usable_index, sort_field}) ->
    {
        400,
        <<"no_usable_index">>,
        <<"No index can satisfy both the selector and sort specified.">>
    };
info(mango_cursor, {no_usable_index, {sort, Fields}}) ->
    S0 = [binary_to_list(F) || F <- Fields],
    S1 = string:join(S0, ", "),
    {
        400,
        <<"no_usable_index">>,
        fmt("No index exists for this sort, try indexing: ~s", [S1])
    };
info(mango_cursor, {no_usable_index, {fields, Possible}}) ->
    S0 = [binary_to_list(P) || P <- Possible],
    S1 = string:join(S0, ", "),
    {
        400,
        <<"no_usable_index">>,
        fmt("No index exists for this selector, try indexing one of: ~s", [S1])
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

info(mango_idx, {invalid_index_type, BadType}) ->
    {
        400,
        <<"invalid_index_type">>,
        fmt("Invalid type for index: ~s", [BadType])
    };

info(mango_idx_view, {invalid_index_json, BadIdx}) ->
    {
        400,
        <<"invalid_index_json">>,
        fmt("JSON indexes must be an object, not: ~w", [BadIdx])
    };
info(mango_idx_view, {index_not_found, BadIdx}) ->
    {
        404,
        <<"index_not_found">>,
        fmt("JSON index ~s not found in this design doc.", [BadIdx])
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
