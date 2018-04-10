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

-module(couch_db_plugin).

-export([
    validate_dbname/3,
    before_doc_update/2,
    after_doc_read/2,
    validate_docid/1,
    check_is_admin/1,
    on_delete/2,
    maybe_init_index_purge_state/2
]).

-define(SERVICE_ID, couch_db).

-include_lib("couch/include/couch_db.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

validate_dbname(DbName, Normalized, Default) ->
    maybe_handle(validate_dbname, [DbName, Normalized], Default).

before_doc_update(Db, Doc0) ->
    Fun = couch_db:get_before_doc_update_fun(Db),
    case with_pipe(before_doc_update, [Doc0, Db]) of
        [Doc1, _Db] when is_function(Fun) -> Fun(Doc1, Db);
        [Doc1, _Db] -> Doc1
    end.

after_doc_read(Db, Doc0) ->
    Fun = couch_db:get_after_doc_read_fun(Db),
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
    couch_epi:any(Handle, ?SERVICE_ID, check_is_admin, [Db], []).

on_delete(DbName, Options) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    couch_epi:apply(Handle, ?SERVICE_ID, on_delete, [DbName, Options], []).

maybe_init_index_purge_state(DbName, DDoc) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    couch_epi:apply(Handle, ?SERVICE_ID, maybe_init_index_purge_state,
        [DbName, DDoc], []).

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
