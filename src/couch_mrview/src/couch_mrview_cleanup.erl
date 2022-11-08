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

-module(couch_mrview_cleanup).

-export([
    run/1,
    cleanup_purges/3,
    cleanup_indices/2
]).

-include_lib("couch/include/couch_db.hrl").

run(Db) ->
    Indices = couch_mrview_util:get_index_files(Db),
    Checkpoints = couch_mrview_util:get_purge_checkpoints(Db),
    {ok, Db1} = couch_db:reopen(Db),
    Sigs = couch_mrview_util:get_signatures(Db1),
    ok = cleanup_purges(Db1, Sigs, Checkpoints),
    ok = cleanup_indices(Sigs, Indices).

cleanup_purges(DbName, Sigs, Checkpoints) when is_binary(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        cleanup_purges(Db, Sigs, Checkpoints)
    end);
cleanup_purges(Db, #{} = Sigs, #{} = CheckpointsMap) ->
    InactiveMap = maps:without(maps:keys(Sigs), CheckpointsMap),
    InactiveCheckpoints = maps:values(InactiveMap),
    DeleteFun = fun(DocId) -> delete_checkpoint(Db, DocId) end,
    lists:foreach(DeleteFun, InactiveCheckpoints).

cleanup_indices(#{} = Sigs, #{} = IndexMap) ->
    Fun = fun(_, Files) -> lists:foreach(fun delete_file/1, Files) end,
    maps:map(Fun, maps:without(maps:keys(Sigs), IndexMap)),
    ok.

delete_file(File) ->
    RootDir = couch_index_util:root_dir(),
    couch_log:debug("~p : deleting inactive index : ~s", [?MODULE, File]),
    try
        couch_file:delete(RootDir, File, [sync])
    catch
        Tag:Error ->
            ErrLog = "~p : error deleting inactive index file ~s ~p:~p",
            couch_log:error(ErrLog, [?MODULE, File, Tag, Error]),
            ok
    end.

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
