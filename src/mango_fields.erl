-module(mango_fields).

-export([
    new/1,
    extract/2,
    
    format_error/1
]).


-include("mango.hrl").


new([]) ->
    {ok, all_fields};
new(Fields) when is_list(Fields) ->
    {ok, [field(F) || F <- Fields]};
new(Else) ->
    ?MANGO_ERROR({invalid_fields_json, Else}).


extract(Doc, Fields) ->
    lists:foldl(fun(F, NewDoc) ->
        case mango_doc:get_field(Doc, F) of
            not_found ->
                NewDoc;
            bad_path ->
                NewDoc;
            Value ->
                mango_doc:set_field(Doc, F, Value)
        end
    end, {[]}, Fields).


format_error({invalid_fields_json, BadFields}) ->
    mango_util:fmt("Fields must be an array of strings, not: ~w", [BadFields]);
format_error({invalid_field_json, BadField}) ->
    mango_util:fmt("Invalid JSON for field spec: ~w", [BadField]);
format_error(Else) ->
    mango_util:fmt("Unknown error: ~w", [Else]).


field(Val) when is_binary(Val) ->
    Val;
field(Else) ->
    ?MANGO_ERROR({invalid_field_json, Else}).
