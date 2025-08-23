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

-module(nouveau_index_upgrader).
-behaviour(couch_scanner_plugin).

-export([
    start/2,
    resume/2,
    complete/1,
    checkpoint/1,
    db/2,
    ddoc/3
]).

-include("nouveau.hrl").
-include_lib("couch_scanner/include/couch_scanner_plugin.hrl").

start(ScanId, #{}) ->
    St = init_config(ScanId),
    case should_run(St) of
        true ->
            ?INFO("Starting.", [], St),
            {ok, St};
        false ->
            ?INFO("Not starting.", [], St),
            skip
    end.

resume(ScanId, #{}) ->
    St = init_config(ScanId),
    case should_run(St) of
        true ->
            ?INFO("Resuming.", [], St),
            {ok, St};
        false ->
            ?INFO("Not resuming.", [], St),
            skip
    end.

complete(St) ->
    ?INFO("Completed", [], St),
    {ok, #{}}.

checkpoint(_St) ->
    {ok, #{}}.

db(St, _DbName) ->
    {ok, St}.

ddoc(St, _DbName, #doc{id = <<"_design/_", _/binary>>}) ->
    {ok, St};
ddoc(St, DbName, #doc{} = DDoc0) ->
    case update_ddoc_versions(DDoc0) of
        DDoc0 ->
            ok;
        DDoc1 ->
            Indexes = nouveau_util:design_doc_to_indexes(DbName, DDoc1),
            case upgrade_indexes(DbName, Indexes) of
                true ->
                    save_ddoc(DbName, DDoc1);
                false ->
                    ok
            end
    end,
    {ok, St}.

upgrade_indexes(_DbName, []) ->
    true;
upgrade_indexes(DbName, [Index | Rest]) ->
    case upgrade_index(DbName, Index) of
        true ->
            upgrade_indexes(DbName, Rest);
        false ->
            false
    end.

upgrade_index(DbName, #index{} = Index) ->
    ?INFO("Upgrading ~s/~s/~s to version ~B", [
        DbName,
        Index#index.ddoc_id,
        Index#index.name,
        ?TARGET_LUCENE_VERSION
    ]),
    case
        nouveau_fabric_search:go(
            DbName,
            #{query => <<"*:*">>, bookmark => null, sort => null, limit => 1},
            Index#index{lucene_version = ?TARGET_LUCENE_VERSION}
        )
    of
        {ok, _SearchResults} ->
            true;
        {error, _Reason} ->
            false
    end.

update_ddoc_versions(#doc{} = Doc) ->
    #doc{body = {Fields0}} = Doc,
    {Indexes0} = couch_util:get_value(<<"nouveau">>, Fields0),
    Indexes1 = lists:map(fun update_version/1, Indexes0),
    Fields1 = couch_util:set_value(<<"nouveau">>, Fields0, {Indexes1}),
    Doc#doc{body = {Fields1}}.

save_ddoc(DbName, #doc{} = DDoc) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        case fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]) of
            {ok, _} ->
                exit(ok);
            Else ->
                exit(Else)
        end
    end),
    receive
        {'DOWN', Ref, process, Pid, ok} ->
            ?INFO(
                "Updated ~s/~s indexes to version ~B", [DbName, DDoc#doc.id, ?TARGET_LUCENE_VERSION]
            );
        {'DOWN', Ref, process, Pid, Else} ->
            ?INFO("Failed to update ~s/~s for reason ~p", [DbName, DDoc#doc.id, Else])
    end.

update_version({IndexName, {Index}}) ->
    {IndexName, {couch_util:set_value(<<"lucene_version">>, Index, ?TARGET_LUCENE_VERSION)}}.

init_config(ScanId) ->
    #{sid => ScanId}.

should_run(St) ->
    couch_scanner_util:on_first_node() andalso upgrade_supported(St).

upgrade_supported(St) ->
    case nouveau_api:supported_lucene_versions() of
        {ok, Versions} ->
            case lists:member(?TARGET_LUCENE_VERSION, Versions) of
                true ->
                    ?INFO(
                        "Nouveau server supports upgrades to Lucene ~B",
                        [?TARGET_LUCENE_VERSION],
                        St
                    ),
                    true;
                false ->
                    ?WARN(
                        "Nouveau server does not support upgrades to Lucene ~B",
                        [?TARGET_LUCENE_VERSION],
                        St
                    ),
                    false
            end;
        {error, Reason} ->
            ?ERR(
                "Nouveau server upgrade check failed for reason ~p", [Reason], St
            ),
            false
    end.
