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


%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(dreyfus_fabric_cleanup).

-include("dreyfus.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([go/1]).

go(DbName) ->
    {ok, DesignDocs} = fabric:design_docs(DbName),
    ActiveSigs = lists:usort(lists:flatmap(fun active_sigs/1,
        [couch_doc:from_json_obj(DD) || DD <- DesignDocs])),
    cleanup_local_purge_doc(DbName, ActiveSigs),
    clouseau_rpc:cleanup(DbName, ActiveSigs),
    ok.

active_sigs(#doc{body={Fields}}=Doc) ->
    try
        {RawIndexes} = couch_util:get_value(<<"indexes">>, Fields, {[]}),
        {IndexNames, _} = lists:unzip(RawIndexes),
        [begin
             {ok, Index} = dreyfus_index:design_doc_to_index(Doc, IndexName),
             Index#index.sig
         end || IndexName <- IndexNames]
    catch error:{badmatch, _Error} ->
        []
    end.

cleanup_local_purge_doc(DbName, ActiveSigs) ->
    {ok, BaseDir} = clouseau_rpc:get_root_dir(),
    DbNamePattern = <<DbName/binary, ".*">>,
    Pattern0 = filename:join([BaseDir, "shards", "*", DbNamePattern, "*"]),
    Pattern = binary_to_list(iolist_to_binary(Pattern0)),
    DirListStrs = filelib:wildcard(Pattern),
    DirList = [iolist_to_binary(DL) || DL <- DirListStrs],
    LocalShards = mem3:local_shards(DbName),
    ActiveDirs = lists:foldl(fun(LS, AccOuter) ->
        lists:foldl(fun(Sig, AccInner) ->
            DirName = filename:join([BaseDir, LS#shard.name, Sig]),
            [DirName | AccInner]
        end, AccOuter, ActiveSigs)
    end, [], LocalShards),

    DeadDirs = DirList -- ActiveDirs,
    lists:foreach(fun(IdxDir) ->
        Sig = dreyfus_util:get_signature_from_idxdir(IdxDir),
        case Sig of undefined -> ok; _ ->
            DocId = dreyfus_util:get_local_purge_doc_id(Sig),
            LocalShards = mem3:local_shards(DbName),
            lists:foreach(fun(LS) ->
                ShardDbName = LS#shard.name,
                {ok, ShardDb} = couch_db:open_int(ShardDbName, []),
                case couch_db:open_doc(ShardDb, DocId, []) of
                    {ok, LocalPurgeDoc} ->
                        couch_db:update_doc(ShardDb,
                            LocalPurgeDoc#doc{deleted=true}, [?ADMIN_CTX]);
                    {not_found, _} ->
                        ok
                end,
                couch_db:close(ShardDb)
            end, LocalShards)
        end
    end, DeadDirs).
