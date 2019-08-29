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

-module(fabric2_db_plugin).

-export([
    validate_dbname/3,
    before_doc_update/3,
    after_doc_read/2,
    validate_docid/1,
    check_is_admin/1,
    is_valid_purge_client/2
]).


-include_lib("couch/include/couch_db.hrl").


-define(SERVICE_ID, fabric2_db).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

validate_dbname(DbName, Normalized, Default) ->
    maybe_handle(validate_dbname, [DbName, Normalized], Default).


before_doc_update(_, #doc{id = <<?LOCAL_DOC_PREFIX, _/binary>>} = Doc, _) ->
    Doc;

before_doc_update(Db, Doc0, UpdateType) ->
    Fun = fabric2_db:get_before_doc_update_fun(Db),
    case with_pipe(before_doc_update, [Doc0, Db, UpdateType]) of
        [Doc1, _Db, UpdateType1] when is_function(Fun) ->
            Fun(Doc1, Db, UpdateType1);
        [Doc1, _Db, _UpdateType] ->
            Doc1
    end.


after_doc_read(Db, Doc0) ->
    Fun = fabric2_db:get_after_doc_read_fun(Db),
    case with_pipe(after_doc_read, [Doc0, Db]) of
        [Doc1, _Db] when is_function(Fun) -> Fun(Doc1, Db);
        [Doc1, _Db] -> Doc1
    end.


validate_docid(Id) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    %% callbacks return true only if it specifically allow the given Id
    couch_epi:any(Handle, ?SERVICE_ID, validate_docid, [Id], []).


check_is_admin(Db) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    %% callbacks return true only if it specifically allow the given Id
    R = couch_epi:any(Handle, ?SERVICE_ID, check_is_admin, [Db], []),
    %io:format(standard_error, "~n FFFFFFF ~p check_is_admin Db:~p => ~p~n", [?MODULE, fabric2_db:name(Db), R]),
    R.


is_valid_purge_client(DbName, Props) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    %% callbacks return true only if it specifically allow the given Id
    couch_epi:any(Handle, ?SERVICE_ID, is_valid_purge_client, [DbName, Props], []).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

with_pipe(Func, Args) ->
    do_apply(Func, Args, [pipe]).

do_apply(Func, Args, Opts) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    couch_epi:apply(Handle, ?SERVICE_ID, Func, Args, Opts).

maybe_handle(Func, Args, Default) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    case couch_epi:decide(Handle, ?SERVICE_ID, Func, Args, []) of
       no_decision when is_function(Default) ->
           apply(Default, Args);
       no_decision ->
           Default;
       {decided, Result} ->
           Result
    end.
