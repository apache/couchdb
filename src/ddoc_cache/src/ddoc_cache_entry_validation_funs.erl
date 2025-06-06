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

-module(ddoc_cache_entry_validation_funs).

-export([
    dbname/1,
    ddocid/1,
    recover/1,
    insert/2
]).

dbname(DbName) ->
    DbName.

ddocid(_) ->
    no_ddocid.

recover(DbName) ->
    %% The VDU function is used to validate documents update before
    %% storing them in the database.
    %% Raise an error when invalid instead of returning an empty list.
    DDocs =
        case fabric:design_docs(mem3:dbname(DbName)) of
            {ok, Resp} when is_list(Resp) ->
                Resp;
            {ok, Error} ->
                error(Error);
            {error, Error} ->
                error(Error)
        end,
    Funs = lists:flatmap(
        fun(DDoc) ->
            case couch_doc:get_validate_doc_fun(DbName, DDoc) of
                nil -> [];
                Fun -> [Fun]
            end
        end,
        DDocs
    ),
    {ok, Funs}.

insert(_, _) ->
    ok.
