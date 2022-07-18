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

-module(mango_fields).

-export([
    new/1,
    extract/2
]).

-include("mango.hrl").

new([]) ->
    {ok, all_fields};
new(Fields) when is_list(Fields) ->
    {ok, [field(F) || F <- Fields]};
new(Else) ->
    ?MANGO_ERROR({invalid_fields_json, Else}).

extract(Doc, undefined) ->
    Doc;
extract(Doc, all_fields) ->
    Doc;
extract(Doc, Fields) ->
    lists:foldl(
        fun(F, NewDoc) ->
            {ok, Path} = mango_util:parse_field(F),
            case mango_doc:get_field(Doc, Path) of
                not_found ->
                    NewDoc;
                bad_path ->
                    NewDoc;
                Value ->
                    mango_doc:set_field(NewDoc, Path, Value)
            end
        end,
        {[]},
        Fields
    ).

field(Val) when is_binary(Val) ->
    Val;
field({Val}) when is_list(Val) ->
    {Val};
field(Else) ->
    ?MANGO_ERROR({invalid_field_json, Else}).
