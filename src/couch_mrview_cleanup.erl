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
        couch_log:debug("Deleting stale view file: ~s", [FN]),
        delete_view_file(RootDir, FN)
    end, ToDelete),

    ok.

delete_view_file(RootDir, FullFilePath) ->
    couch_server:delete_file(RootDir, FullFilePath, [sync]).

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

setup(rename) ->
    setup(true);
setup(delete) ->
    setup(false);
setup(RenameOnDelete) ->
    meck:new(config, [passthrough]),
    meck:expect(config, get_boolean, fun
        ("couchdb", "rename_on_delete", _Default) -> RenameOnDelete
    end),
    ViewFile = ?tempfile() ++ ".view",
    RootDir = filename:dirname(ViewFile),
    ok = couch_file:init_delete_dir(RootDir),
    ok = file:write_file(ViewFile, <<>>),
    {RootDir, ViewFile}.

teardown(_, {_, ViewFile}) ->
    (catch meck:unload(config)),
    (catch file:delete(ViewFile)).

delete_view_file_test_() ->
    Funs = [
        fun should_rename_on_delete/2,
        fun should_delete/2
    ],
    [
        make_test_case(rename, [fun should_rename_on_delete/2]),
        make_test_case(delete, [fun should_delete/2])
    ].

make_test_case(Mod, Funs) ->
    {
        lists:flatten(io_lib:format("~s", [Mod])),
        {foreachx, fun setup/1, fun teardown/2, [{Mod, Fun} || Fun <- Funs]}
    }.

should_rename_on_delete(_, {RootDir, ViewFile}) ->
    ?_test(begin
        ?assert(filelib:is_regular(ViewFile)),
        Result = delete_view_file(RootDir, ViewFile),
        ?assertNot(filelib:is_regular(ViewFile)),
        ?assertMatch({ok, {renamed, _}}, Result),
        {ok, {renamed, RenamedViewFile}} = Result,
        ?assert(filelib:is_regular(RenamedViewFile))
    end).

should_delete(_, {RootDir, ViewFile}) ->
    ?_test(begin
        ?assert(filelib:is_regular(ViewFile)),
        delete_view_file(RootDir, ViewFile),
        ?assertNot(filelib:is_regular(ViewFile)),
        ?assertMatch([], deleted_files(ViewFile))
    end).

deleted_files(ViewFile) ->
    filelib:wildcard(filename:rootname(ViewFile) ++ "*.deleted.*").

-endif.
