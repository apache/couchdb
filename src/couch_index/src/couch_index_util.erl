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

-module(couch_index_util).

-export([root_dir/0, index_dir/2, index_file/3]).
-export([load_doc/3, sort_lib/1, hexsig/1]).

-include_lib("couch/include/couch_db.hrl").


root_dir() ->
    case couch_config:get("couchdb", "index_dir") of
        undefined -> couch_config:get("couchdb", "view_index_dir");
        Value -> Value
    end.


index_dir(Module, DbName) when is_binary(DbName) ->
    DbDir = "." ++ binary_to_list(DbName) ++ "_design",
    filename:join([root_dir(), DbDir, Module]);
index_dir(Module, #db{}=Db) ->
    index_dir(Module, couch_db:name(Db)).


index_file(Module, DbName, FileName) ->
    filename:join(index_dir(Module, DbName), FileName).


load_doc(Db, #doc_info{}=DI, Opts) ->
    Deleted = lists:member(deleted, Opts),
    case (catch couch_db:open_doc(Db, DI, Opts)) of
        {ok, #doc{deleted=false}=Doc} -> Doc;
        {ok, #doc{deleted=true}=Doc} when Deleted -> Doc;
        _Else -> null
    end;
load_doc(Db, {DocId, Rev}, Opts) ->
    case (catch load_doc(Db, DocId, Rev, Opts)) of
        #doc{deleted=false} = Doc -> Doc;
        _ -> null
    end.


load_doc(Db, DocId, Rev, Options) ->
    case Rev of
        nil -> % open most recent rev
            case (catch couch_db:open_doc(Db, DocId, Options)) of
                {ok, Doc} -> Doc;
                _Error -> null
            end;
        _ -> % open a specific rev (deletions come back as stubs)
            case (catch couch_db:open_doc_revs(Db, DocId, [Rev], Options)) of
                {ok, [{ok, Doc}]} -> Doc;
                {ok, [{{not_found, missing}, Rev}]} -> null;
                {ok, [_Else]} -> null
            end
    end.


sort_lib({Lib}) ->
    sort_lib(Lib, []).
sort_lib([], LAcc) ->
    lists:keysort(1, LAcc);
sort_lib([{LName, {LObj}}|Rest], LAcc) ->
    LSorted = sort_lib(LObj, []), % descend into nested object
    sort_lib(Rest, [{LName, LSorted}|LAcc]);
sort_lib([{LName, LCode}|Rest], LAcc) ->
    sort_lib(Rest, [{LName, LCode}|LAcc]).


hexsig(Sig) ->
    couch_util:to_hex(binary_to_list(Sig)).
