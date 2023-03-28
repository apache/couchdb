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

-module(mem3_reshard_index).

-export([
    design_docs/1,
    target_indices/2,
    spawn_builders/1,
    build_index/2
]).

-define(MRVIEW, mrview).
-define(DREYFUS, dreyfus).
-define(HASTINGS, hastings).
-define(NOUVEAU, nouveau).

-include_lib("mem3/include/mem3.hrl").

%% Public API

design_docs(DbName) ->
    try
        case fabric_design_docs(mem3:dbname(DbName)) of
            {error, {maintenance_mode, _, _Node}} ->
                {ok, []};
            {ok, DDocs} ->
                JsonDocs = [couch_doc:from_json_obj(DDoc) || DDoc <- DDocs],
                {ok, JsonDocs};
            Else ->
                Else
        end
    catch
        error:database_does_not_exist ->
            {ok, []}
    end.

target_indices(Docs, Targets) ->
    Indices = [[indices(N, D) || D <- Docs] || #shard{name = N} <- Targets],
    lists:flatten(Indices).

spawn_builders(Indices) ->
    Retries = max_retries(),
    [spawn_link(?MODULE, build_index, [Idx, Retries]) || Idx <- Indices].

%% Private API

fabric_design_docs(DbName) ->
    case couch_util:with_proc(fabric, design_docs, [DbName], infinity) of
        {ok, Resp} -> Resp;
        {error, Error} -> Error
    end.

indices(DbName, Doc) ->
    mrview_indices(DbName, Doc) ++
        nouveau_indices(DbName, Doc) ++
        [dreyfus_indices(DbName, Doc) || has_app(dreyfus)] ++
        [hastings_indices(DbName, Doc) || has_app(hastings)].

mrview_indices(DbName, Doc) ->
    try
        {ok, MRSt} = couch_mrview_util:ddoc_to_mrst(DbName, Doc),
        Views = couch_mrview_index:get(views, MRSt),
        case Views =/= [] of
            true ->
                [{?MRVIEW, DbName, MRSt}];
            false ->
                []
        end
    catch
        Tag:Err ->
            Msg = "~p couldn't get mrview index ~p ~p ~p:~p",
            couch_log:error(Msg, [?MODULE, DbName, Doc, Tag, Err]),
            []
    end.

nouveau_indices(DbName, Doc) ->
    case nouveau:enabled() of
        false ->
            [];
        true ->
            try
                Indices = nouveau_util:design_doc_to_indexes(DbName, Doc),
                [{?NOUVEAU, DbName, Index} || Index <- Indices]
            catch
                Tag:Err ->
                    Msg = "~p couldn't get nouveau indices ~p ~p ~p:~p",
                    couch_log:error(Msg, [?MODULE, DbName, Doc, Tag, Err]),
                    []
            end
    end.

dreyfus_indices(DbName, Doc) ->
    try
        Indices = dreyfus_index:design_doc_to_indexes(Doc),
        [{?DREYFUS, DbName, Index} || Index <- Indices]
    catch
        Tag:Err ->
            Msg = "~p couldn't get dreyfus indices ~p ~p ~p:~p",
            couch_log:error(Msg, [?MODULE, DbName, Doc, Tag, Err]),
            []
    end.

hastings_indices(DbName, Doc) ->
    try
        Indices = hastings_index:design_doc_to_indexes(Doc),
        [{?HASTINGS, DbName, Index} || Index <- Indices]
    catch
        Tag:Err ->
            Msg = "~p couldn't get hasting indices ~p ~p ~p:~p",
            couch_log:error(Msg, [?MODULE, DbName, Doc, Tag, Err]),
            []
    end.

build_index({?MRVIEW, DbName, MRSt} = Ctx, Try) ->
    ioq:set_io_priority({reshard, DbName}),
    await_retry(
        couch_index_server:get_index(couch_mrview_index, MRSt),
        fun couch_index:get_state/2,
        Ctx,
        Try
    );
build_index({?NOUVEAU, _DbName, DIndex} = Ctx, Try) ->
    UpdateFun = fun() -> nouveau_index_updater:update(DIndex) end,
    retry_loop(Ctx, UpdateFun, Try);
build_index({?DREYFUS, DbName, DIndex} = Ctx, Try) ->
    await_retry(
        dreyfus_index_manager:get_index(DbName, DIndex),
        fun dreyfus_index:await/2,
        Ctx,
        Try
    );
build_index({?HASTINGS, DbName, HIndex} = Ctx, Try) ->
    await_retry(
        hastings_index_manager:get_index(DbName, HIndex),
        fun hastings_index:await/2,
        Ctx,
        Try
    ).

await_retry({ok, Pid}, AwaitIndex, {_, DbName, _} = Ctx, Try) ->
    UpdateFun = fun() ->
        case AwaitIndex(Pid, get_update_seq(DbName)) of
            {ok, _} -> ok;
            {ok, _, _} -> ok;
            AwaitError -> AwaitError
        end
    end,
    retry_loop(Ctx, UpdateFun, Try);
await_retry(OpenError, _AwaitIndex, Ctx, Try) ->
    maybe_retry(Ctx, OpenError, Try).

retry_loop(Ctx, UpdateFun, Try) ->
    try UpdateFun() of
        ok -> ok;
        UpdateError -> maybe_retry(Ctx, UpdateError, Try)
    catch
        _:CatchError ->
            maybe_retry(Ctx, CatchError, Try)
    end.

maybe_retry(Ctx, killed = Error, Try) ->
    retry(Ctx, Error, Try);
maybe_retry(Ctx, {killed, _} = Error, Try) ->
    retry(Ctx, Error, Try);
maybe_retry(Ctx, shutdown = Error, Try) ->
    retry(Ctx, Error, Try);
maybe_retry(Ctx, Error, 0) ->
    fail(Ctx, Error);
maybe_retry(Ctx, Error, Try) when is_integer(Try), Try > 0 ->
    retry(Ctx, Error, Try - 1).

retry(Ctx, Error, Try) ->
    IndexInfo = index_info(Ctx),
    LogMsg = "~p : error ~p when building ~p, retrying (~p)",
    couch_log:warning(LogMsg, [?MODULE, Error, IndexInfo, Try]),
    timer:sleep(retry_interval_sec() * 1000),
    build_index(Ctx, Try).

fail(Ctx, Error) ->
    IndexInfo = index_info(Ctx),
    LogMsg = "~p : error ~p when building ~p, max tries exceeded, failing",
    couch_log:error(LogMsg, [?MODULE, Error, IndexInfo]),
    exit({error_building_index, IndexInfo}).

index_info({?MRVIEW, DbName, MRSt}) ->
    GroupName = couch_mrview_index:get(idx_name, MRSt),
    {DbName, GroupName};
index_info({?DREYFUS, DbName, Index}) ->
    {DbName, Index};
index_info({?HASTINGS, DbName, Index}) ->
    {DbName, Index}.

has_app(App) ->
    code:lib_dir(App) /= {error, bad_name}.

get_update_seq(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        couch_db:get_update_seq(Db)
    end).

max_retries() ->
    config:get_integer("reshard", "index_max_retries", 5).

retry_interval_sec() ->
    config:get_integer("reshard", "index_retry_interval_sec", 10).
