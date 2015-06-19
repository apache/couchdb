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
    validate_dbname/2,
    before_doc_update/2,
    after_doc_read/2
]).

-define(SERVICE_ID, couch_db).

-include_lib("couch/include/couch_db.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

validate_dbname(DbName, Normalized) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    %% callbacks return true only if it specifically allow the given Id
    couch_epi:any(Handle, ?SERVICE_ID, validate_dbname, [DbName, Normalized],
        [ignore_providers]).

before_doc_update(#db{before_doc_update = Fun} = Db, Doc0) ->
    case with_pipe(before_doc_update, [Doc0, Db]) of
        [Doc1, _Db] when is_function(Fun) -> Fun(Doc1, Db);
        [Doc1, _Db] -> Doc1
    end.

after_doc_read(#db{after_doc_read = Fun} = Db, Doc0) ->
    case with_pipe(after_doc_read, [Doc0, Db]) of
        [Doc1, _Db] when is_function(Fun) -> Fun(Doc1, Db);
        [Doc1, _Db] -> Doc1
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

with_pipe(Func, Args) ->
    do_apply(Func, Args, [ignore_providers, pipe]).

do_apply(Func, Args, Opts) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    couch_epi:apply(Handle, ?SERVICE_ID, Func, Args, Opts).
