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

-module(couch_views).


-behavior(fabric2_index).


-export([
    query/6,

    % fabric2_index behavior
    build_indices/2,
    cleanup_indices/2,
    get_info/2
]).

-include("couch_views.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


query(Db, DDoc, ViewName, Callback, Acc0, Args0) ->
    case fabric2_db:is_users_db(Db) of
        true ->
            fabric2_users_db:after_doc_read(DDoc, Db);
        false ->
            ok
    end,

    DbName = fabric2_db:name(Db),
    IsInteractive = couch_views_ddoc:is_interactive(DDoc),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),

    #mrst{
        views = Views
    } = Mrst,

    Args1 = to_mrargs(Args0),
    Args2 = couch_mrview_util:set_view_type(Args1, ViewName, Views),
    Args3 = couch_mrview_util:validate_args(Args2),
    ok = check_range(Mrst, ViewName, Args3),

    try
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            ok = maybe_update_view(TxDb, Mrst, IsInteractive, Args3),
            read_view(TxDb, Mrst, ViewName, Callback, Acc0, Args3)
        end)
    catch throw:{build_view, WaitSeq} ->
        couch_views_jobs:build_view(Db, Mrst, WaitSeq),
        read_view(Db, Mrst, ViewName, Callback, Acc0, Args3)
    end.


build_indices(#{} = Db, DDocs) when is_list(DDocs) ->
    DbName = fabric2_db:name(Db),
    lists:filtermap(fun(DDoc) ->
        try couch_views_util:ddoc_to_mrst(DbName, DDoc) of
            {ok, #mrst{} = Mrst} ->
                {true, couch_views_jobs:build_view_async(Db, Mrst)}
        catch _:_ ->
            false
        end
    end, DDocs).


cleanup_indices(#{} = Db, DDocs) when is_list(DDocs) ->
    DbName = fabric2_db:name(Db),
    ActiveSigs = lists:filtermap(fun(DDoc) ->
        try couch_views_util:ddoc_to_mrst(DbName, DDoc) of
            {ok, #mrst{sig = Sig}} ->
                {true, Sig}
        catch _:_ ->
            false
        end
    end, DDocs),
    ExistingSigs = couch_views_fdb:list_signatures(Db),
    StaleSigs = ExistingSigs -- ActiveSigs,
    lists:foreach(fun(Sig) ->
        couch_views_jobs:remove(Db, Sig),
        couch_views_fdb:clear_index(Db, Sig)
    end, StaleSigs).


get_info(Db, DDoc) ->
    DbName = fabric2_db:name(Db),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    Sig = fabric2_util:to_hex(Mrst#mrst.sig),
    {UpdateSeq, DataSize, Status} = fabric2_fdb:transactional(Db, fun(TxDb) ->
        Mrst1 = couch_views_fdb:set_trees(TxDb, Mrst),
        Seq = couch_views_fdb:get_update_seq(TxDb, Mrst1),
        DataSize = get_total_view_size(TxDb, Mrst1),
        JobStatus = case couch_views_jobs:job_state(TxDb, Mrst1) of
            {ok, pending} -> true;
            {ok, running} -> true;
            {ok, finished} -> false;
            {error, not_found} -> false
        end,
        {Seq, DataSize, JobStatus}
    end),
    UpdateOptions = get_update_options(Mrst),
    {ok, [
        {language, Mrst#mrst.language},
        {signature, Sig},
        {sizes, {[
            {active, DataSize}
        ]}},
        {update_seq, UpdateSeq},
        {updater_running, Status},
        {update_options, UpdateOptions}
    ]}.


get_total_view_size(TxDb, Mrst) ->
    lists:foldl(fun(View, Total) ->
        Total + couch_views_fdb:get_kv_size(TxDb, View)
    end, 0, Mrst#mrst.views).


read_view(Db, Mrst, ViewName, Callback, Acc0, Args) ->
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        try
            couch_views_reader:read(TxDb, Mrst, ViewName, Callback, Acc0, Args)
        after
            UpdateAfter = Args#mrargs.update == lazy,
            if UpdateAfter == false -> ok; true ->
                couch_views_jobs:build_view_async(TxDb, Mrst)
            end
        end
    end).


maybe_update_view(_Db, _Mrst, _, #mrargs{update = false}) ->
    ok;

maybe_update_view(_Db, _Mrst, _, #mrargs{update = lazy}) ->
    ok;

maybe_update_view(TxDb, Mrst, true, _Args) ->
    BuildState = couch_views_fdb:get_build_status(TxDb, Mrst),
    if BuildState == ?INDEX_READY -> ok; true ->
        VS = couch_views_fdb:get_creation_vs(TxDb, Mrst),
        throw({build_view, fabric2_fdb:vs_to_seq(VS)})
    end;

maybe_update_view(TxDb, Mrst, false, _Args) ->
    DbSeq = fabric2_db:get_update_seq(TxDb),
    ViewSeq = couch_views_fdb:get_update_seq(TxDb, Mrst),
    case DbSeq == ViewSeq of
        true -> ok;
        false -> throw({build_view, DbSeq})
    end.


is_reduce_view(#mrargs{view_type = ViewType}) ->
    ViewType =:= red;
is_reduce_view({Reduce, _, _}) ->
    Reduce =:= red.


to_mrargs(#mrargs{} = Args) ->
    Args;

to_mrargs(#{} = Args) ->
    Fields = record_info(fields, mrargs),
    Indexes = lists:seq(2, record_info(size, mrargs)),
    LU = lists:zip(Fields, Indexes),

    maps:fold(fun(Key, Value, Acc) ->
        Index = fabric2_util:get_value(couch_util:to_existing_atom(Key), LU),
        setelement(Index, Acc, Value)
    end, #mrargs{}, Args).


check_range(Mrst, ViewName, Args) ->
    #mrst{
        language = Lang,
        views = Views
    } = Mrst,
    View = case couch_mrview_util:extract_view(Lang, Args, ViewName, Views) of
        {map, V, _} -> V;
        {red, {_, _, V}, _} -> V
    end,
    Cmp = couch_views_util:collate_fun(View),
    check_range(Args, Cmp).


check_range(#mrargs{start_key = undefined}, _Cmp) ->
    ok;

check_range(#mrargs{end_key = undefined}, _Cmp) ->
    ok;

check_range(#mrargs{start_key = K, end_key = K}, _Cmp) ->
    ok;

check_range(Args, Cmp) ->
    #mrargs{
        direction = Dir,
        start_key = SK,
        start_key_docid = SKD,
        end_key = EK,
        end_key_docid = EKD
    } = Args,

    case {Dir, Cmp({SK, SKD}, {EK, EKD})} of
        {fwd, gt} ->
            throw(check_range_error(<<"true">>));
        {rev, lt} ->
            throw(check_range_error(<<"false">>));
        _ ->
            ok
    end.


check_range_error(Descending) ->
    {query_parse_error,
        <<"No rows can match your key range, reverse your ",
            "start_key and end_key or set descending=",
            Descending/binary>>}.


get_update_options(#mrst{design_opts = Opts}) ->
    IncDesign = couch_util:get_value(<<"include_design">>, Opts, false),
    LocalSeq = couch_util:get_value(<<"local_seq">>, Opts, false),
    UpdateOptions = if IncDesign -> [include_design]; true -> [] end
        ++ if LocalSeq -> [local_seq]; true -> [] end,
    [atom_to_binary(O, latin1) || O <- UpdateOptions].
