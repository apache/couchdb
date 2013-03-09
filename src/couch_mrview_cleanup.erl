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

-export([run/1]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


run(Db) ->
    RootDir = couch_index_util:root_dir(),
    DbName = couch_db:name(Db),

    {ok, DesignDocs} = couch_db:get_design_docs(Db),
    SigFiles = lists:foldl(fun(DDocInfo, SFAcc) ->
        {ok, DDoc} = couch_db:open_doc_int(Db, DDocInfo, [ejson_body]),
        {ok, InitState} = couch_mrview_util:ddoc_to_mrst(DbName, DDoc),
        Sig = InitState#mrst.sig,
        IFName = couch_mrview_util:index_file(DbName, Sig),
        CFName = couch_mrview_util:compaction_file(DbName, Sig),
        [IFName, CFName | SFAcc]
    end, [], [DD || DD <- DesignDocs, DD#full_doc_info.deleted == false]),

    IdxDir = couch_index_util:index_dir(mrview, DbName),
    DiskFiles = filelib:wildcard(filename:join(IdxDir, "*")),

    % We need to delete files that have no ddoc.
    ToDelete = DiskFiles -- SigFiles,

    lists:foreach(fun(FN) ->
        ?LOG_DEBUG("Deleting stale view file: ~s", [FN]),
        couch_file:delete(RootDir, FN, false)
    end, ToDelete),

    ok.
