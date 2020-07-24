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
-module(couch_views_updater).

-export([
    index/6
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

% If the doc revision doesn't not match the NewRevId passed here we can ignore
% the document since it is then a conflict document and it doesn't need
% to be indexed.
index(Db, #doc{id = Id, revs = Revs} = Doc, _NewWinner, _OldWinner, NewRevId,
        Seq) ->
    try
        {Depth, [FirstRev | _]} = Revs,
        DocRev = {Depth, FirstRev},
        if DocRev /= NewRevId -> ok; true ->
            index_int(Db, Doc, Seq)
        end
    catch
        error:{erlfdb_error, ErrCode} when is_integer(ErrCode) ->
            Stack = erlang:get_stacktrace(),
            DbName = fabric2_db:name(Db),
            couch_log:error("Mango index erlfdb error Db ~s Doc ~p ~p",
                [DbName, Id, ErrCode]),
            erlang:raise(error, {erlfdb_error, ErrCode}, Stack);
        Error:Reason ->
            DbName = fabric2_db:name(Db),
            couch_log:error("Mango index error for Db ~s Doc ~p ~p ~p",
                [DbName, Id, Error, Reason])
    end.


% Check if design doc is an interactive index and kick off background worker
% to build the new index up to the creation_vs
index_int(Db, #doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>,
        deleted = false} = DDoc, Seq) ->
    DbName = fabric2_db:name(Db),

    case couch_views_ddoc:is_interactive(DDoc) of
        true ->
            {ok, Mrst} = couch_mrview_util:ddoc_to_mrst(DbName, DDoc),
            case couch_views_fdb:get_creation_vs(Db, Mrst) of
                not_found ->
                    couch_views_fdb:new_interactive_index(Db, Mrst, Seq),
                    {ok, _} = couch_views_jobs:build_view_async(Db, Mrst);
                _ ->
                    ok
            end;
        false ->
            ok
    end,
    write_doc(Db, DDoc);


index_int(Db, #doc{} = Doc, _Seq) ->
    write_doc(Db, Doc).


write_doc(Db, #doc{deleted = Deleted} = Doc) ->
    DbName = fabric2_db:name(Db),
    DDocs = couch_views_ddoc:get_interactive_list(Db),

    Result0 = [#{
        id => Doc#doc.id,
        results => [],
        deleted => Deleted,
        doc => Doc
    }],

    %% Interactive updates do not update the views update_seq
    State = #{
        last_seq => false
    },

    lists:foreach(fun(DDoc) ->
        {ok, Mrst0} = couch_mrview_util:ddoc_to_mrst(DbName, DDoc),
        Mrst1 = couch_views_fdb:set_trees(Db, Mrst0),

        case should_index_doc(Doc, Mrst1) of
            true ->
                {Mrst2, Result1} = couch_views_indexer:map_docs(Mrst1, Result0),
                DocNumber = couch_views_indexer:write_docs(Db, Mrst2,
                    Result1, State),
                couch_views_plugin:after_interactive_write(Db, Mrst2,
                    Result1, DocNumber),
                couch_eval:release_map_context(Mrst2#mrst.qserver);
            false ->
                ok
        end
    end, DDocs).


should_index_doc(<<?DESIGN_DOC_PREFIX, _/binary>>, Mrst) ->
    lists:keymember(<<"include_design">>, 1, Mrst#mrst.design_opts);

should_index_doc(_,  _) ->
    true.
