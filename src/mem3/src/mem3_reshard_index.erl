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
    spawn_builders/1
]).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

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
    Results = [build_index(Index) || Index <- Indices],
    Oks = [{ok, Pid} || {ok, Pid} <- Results, is_pid(Pid)],
    case Results -- Oks of
        [] ->
            {ok, [Pid || {ok, Pid} <- Results]};
        Error ->
            % Do a all or nothing pattern, if some indices could not be
            % spawned, kill the spawned ones and and return the error.
            ErrMsg = "~p failed to spawn index builders: ~p ~p",
            couch_log:error(ErrMsg, [?MODULE, Error, Indices]),
            lists:foreach(
                fun({ok, Pid}) ->
                    catch unlink(Pid),
                    catch exit(Pid, kill)
                end,
                Oks
            ),
            {error, Error}
    end.

%% Private API

fabric_design_docs(DbName) ->
    case couch_util:with_proc(fabric, design_docs, [DbName], infinity) of
        {ok, Resp} -> Resp;
        {error, Error} -> Error
    end.

indices(DbName, Doc) ->
    mrview_indices(DbName, Doc) ++
        [dreyfus_indices(DbName, Doc) || has_app(dreyfus)] ++
        [hastings_indices(DbName, Doc) || has_app(hastings)].

mrview_indices(DbName, Doc) ->
    try
        {ok, MRSt} = couch_mrview_util:ddoc_to_mrst(DbName, Doc),
        Views = couch_mrview_index:get(views, MRSt),
        case Views =/= [] of
            true ->
                [{mrview, DbName, MRSt}];
            false ->
                []
        end
    catch
        Tag:Err ->
            Msg = "~p couldn't get mrview index ~p ~p ~p:~p",
            couch_log:error(Msg, [?MODULE, DbName, Doc, Tag, Err]),
            []
    end.

dreyfus_indices(DbName, Doc) ->
    try
        Indices = dreyfus_index:design_doc_to_indexes(Doc),
        [{dreyfus, DbName, Index} || Index <- Indices]
    catch
        Tag:Err ->
            Msg = "~p couldn't get dreyfus indices ~p ~p ~p:~p",
            couch_log:error(Msg, [?MODULE, DbName, Doc, Tag, Err]),
            []
    end.

hastings_indices(DbName, Doc) ->
    try
        Indices = hastings_index:design_doc_to_indexes(Doc),
        [{hastings, DbName, Index} || Index <- Indices]
    catch
        Tag:Err ->
            Msg = "~p couldn't get hasting indices ~p ~p ~p:~p",
            couch_log:error(Msg, [?MODULE, DbName, Doc, Tag, Err]),
            []
    end.

build_index({mrview, DbName, MRSt}) ->
    case couch_index_server:get_index(couch_mrview_index, MRSt) of
        {ok, Pid} ->
            Args = [Pid, get_update_seq(DbName)],
            WPid = spawn_link(couch_index, get_state, Args),
            {ok, WPid};
        Error ->
            Error
    end;
build_index({dreyfus, DbName, Index}) ->
    case dreyfus_index_manager:get_index(DbName, Index) of
        {ok, Pid} ->
            Args = [Pid, get_update_seq(DbName)],
            WPid = spawn_link(dreyfus_index, await, Args),
            {ok, WPid};
        Error ->
            Error
    end;
build_index({hastings, DbName, Index}) ->
    case hastings_index_manager:get_index(DbName, Index) of
        {ok, Pid} ->
            Args = [Pid, get_update_seq(DbName)],
            WPid = spawn_link(hastings_index, await, Args),
            {ok, WPid};
        Error ->
            Error
    end.

has_app(App) ->
    code:lib_dir(App) /= {error, bad_name}.

get_update_seq(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        couch_db:get_update_seq(Db)
    end).
