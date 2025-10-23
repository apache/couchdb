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
-export([get_purge_checkpoints/2, cleanup_purges/3]).

-include_lib("couch/include/couch_db.hrl").

root_dir() ->
    config:get("couchdb", "view_index_dir").

index_dir(Module, DbName) when is_binary(DbName) ->
    DbDir = "." ++ binary_to_list(DbName) ++ "_design",
    filename:join([root_dir(), DbDir, Module]);
index_dir(Module, Db) ->
    index_dir(Module, couch_db:name(Db)).

index_file(Module, DbName, FileName) ->
    filename:join(index_dir(Module, DbName), FileName).

load_doc(Db, #doc_info{} = DI, Opts) ->
    Deleted = lists:member(deleted, Opts),
    case (catch couch_db:open_doc(Db, DI, Opts)) of
        {ok, #doc{deleted = false} = Doc} -> Doc;
        {ok, #doc{deleted = true} = Doc} when Deleted -> Doc;
        _Else -> null
    end;
load_doc(Db, {DocId, Rev}, Opts) ->
    case (catch load_doc(Db, DocId, Rev, Opts)) of
        #doc{deleted = false} = Doc -> Doc;
        _ -> null
    end.

load_doc(Db, DocId, Rev, Options) ->
    case Rev of
        % open most recent rev
        nil ->
            case (catch couch_db:open_doc(Db, DocId, Options)) of
                {ok, Doc} -> Doc;
                _Error -> null
            end;
        % open a specific rev (deletions come back as stubs)
        _ ->
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
sort_lib([{LName, {LObj}} | Rest], LAcc) ->
    % descend into nested object
    LSorted = sort_lib(LObj, []),
    sort_lib(Rest, [{LName, LSorted} | LAcc]);
sort_lib([{LName, LCode} | Rest], LAcc) ->
    sort_lib(Rest, [{LName, LCode} | LAcc]).

hexsig(Sig) ->
    couch_util:to_hex(Sig).

% Helper function for indexes to get their purge checkpoints as signatures.
%
get_purge_checkpoints(DbName, Type) when is_binary(DbName), is_binary(Type) ->
    couch_util:with_db(DbName, fun(Db) -> get_purge_checkpoints(Db, Type) end);
get_purge_checkpoints(Db, Type) when is_binary(Type) ->
    Prefix = <<?LOCAL_DOC_PREFIX, "purge-", Type:(byte_size(Type))/binary, "-">>,
    PrefixSize = byte_size(Prefix),
    FoldFun = fun(#doc{id = Id}, Acc) ->
        case Id of
            <<Prefix:PrefixSize/binary, Sig/binary>> -> {ok, Acc#{Sig => Id}};
            _ -> {stop, Acc}
        end
    end,
    Opts = [{start_key, Prefix}],
    {ok, Signatures = #{}} = couch_db:fold_local_docs(Db, FoldFun, #{}, Opts),
    Signatures.

% Helper functions for indexes to clean their purge checkpoints.
%
cleanup_purges(DbName, #{} = Sigs, #{} = Checkpoints) when is_binary(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        cleanup_purges(Db, Sigs, Checkpoints)
    end);
cleanup_purges(Db, #{} = Sigs, #{} = CheckpointsMap) ->
    InactiveMap = maps:without(maps:keys(Sigs), CheckpointsMap),
    InactiveCheckpoints = maps:values(InactiveMap),
    DeleteFun = fun(DocId) -> delete_checkpoint(Db, DocId) end,
    lists:foreach(DeleteFun, InactiveCheckpoints).

delete_checkpoint(Db, DocId) ->
    DbName = couch_db:name(Db),
    LogMsg = "~p : deleting inactive purge checkpoint ~s : ~s",
    couch_log:debug(LogMsg, [?MODULE, DbName, DocId]),
    try couch_db:open_doc(Db, DocId, []) of
        {ok, Doc = #doc{}} ->
            Deleted = Doc#doc{deleted = true, body = {[]}},
            couch_db:update_doc(Db, Deleted, [?ADMIN_CTX]);
        {not_found, _} ->
            ok
    catch
        Tag:Error ->
            ErrLog = "~p : error deleting checkpoint ~s : ~s error: ~p:~p",
            couch_log:error(ErrLog, [?MODULE, DbName, DocId, Tag, Error]),
            ok
    end.
