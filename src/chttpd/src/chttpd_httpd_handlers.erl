% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_httpd_handlers).

-export([url_handler/1, db_handler/1, design_handler/1, handler_info/3]).

-export([
    not_supported/2,
    not_supported/3,
    not_implemented/2
]).


-include_lib("couch/include/couch_db.hrl").


url_handler(<<>>)                  -> fun chttpd_misc:handle_welcome_req/1;
url_handler(<<"favicon.ico">>)     -> fun chttpd_misc:handle_favicon_req/1;
url_handler(<<"_utils">>)          -> fun chttpd_misc:handle_utils_dir_req/1;
url_handler(<<"_all_dbs">>)        -> fun chttpd_misc:handle_all_dbs_req/1;
url_handler(<<"_deleted_dbs">>)    -> fun chttpd_misc:handle_deleted_dbs_req/1;
url_handler(<<"_dbs_info">>)       -> fun chttpd_misc:handle_dbs_info_req/1;
url_handler(<<"_active_tasks">>)   -> fun chttpd_misc:handle_task_status_req/1;
url_handler(<<"_scheduler">>)      -> fun couch_replicator_httpd:handle_scheduler_req/1;
url_handler(<<"_node">>)           -> fun chttpd_node:handle_node_req/1;
url_handler(<<"_reload_query_servers">>) -> fun chttpd_misc:handle_reload_query_servers_req/1;
url_handler(<<"_replicate">>)      -> fun chttpd_misc:handle_replicate_req/1;
url_handler(<<"_uuids">>)          -> fun chttpd_misc:handle_uuids_req/1;
url_handler(<<"_session">>)        -> fun chttpd_auth:handle_session_req/1;
url_handler(<<"_up">>)             -> fun chttpd_misc:handle_up_req/1;
url_handler(_) -> no_match.

db_handler(<<"_view_cleanup">>) -> fun chttpd_db:handle_view_cleanup_req/2;
db_handler(<<"_compact">>)      -> fun chttpd_db:handle_compact_req/2;
db_handler(<<"_design">>)       -> fun chttpd_db:handle_design_req/2;
db_handler(<<"_partition">>)    -> fun chttpd_db:handle_partition_req/2;
db_handler(<<"_temp_view">>)    -> fun ?MODULE:not_supported/2;
db_handler(<<"_changes">>)      -> fun chttpd_db:handle_changes_req/2;
db_handler(<<"_purge">>)        -> fun ?MODULE:not_implemented/2;
db_handler(<<"_purged_infos_limit">>) -> fun ?MODULE:not_implemented/2;
db_handler(_) -> no_match.

design_handler(<<"_view">>)    -> fun chttpd_view:handle_view_req/3;
design_handler(<<"_show">>)    -> fun ?MODULE:not_supported/3;
design_handler(<<"_list">>)    -> fun ?MODULE:not_supported/3;
design_handler(<<"_update">>)  -> fun chttpd_show:handle_doc_update_req/3;
design_handler(<<"_info">>)    -> fun chttpd_db:handle_design_info_req/3;
design_handler(<<"_rewrite">>) -> fun ?MODULE:not_supported/3;
design_handler(_) -> no_match.


handler_info('GET', [], _) ->
    {'welcome_message.read', #{}};

handler_info('GET', [<<"_active_tasks">>], _) ->
    {'active_tasks.read', #{}};

handler_info('GET', [<<"_all_dbs">>], _) ->
    {'all_dbs.read', #{}};

handler_info('GET', [<<"_deleted_dbs">>], _) ->
    {'account-deleted-dbs.read', #{}};

handler_info('POST', [<<"_deleted_dbs">>], _) ->
    {'account-deleted-dbs.undelete', #{}};

handler_info('DELETE', [<<"_deleted_dbs">>, Db], _) ->
    {'account-deleted-dbs.delete', #{'db.name' => Db}};

handler_info('POST', [<<"_dbs_info">>], _) ->
    {'dbs_info.read', #{}};

handler_info('GET', [<<"_node">>, <<"_local">>], _) ->
    {'node.name.read', #{}};

handler_info(Method, [<<"_node">>, <<"_local">> | Rest], HttpReq) ->
    handler_info(Method, [<<"_node">>, node() | Rest], HttpReq);

handler_info('GET', [<<"_node">>, Node, <<"_config">>], _) ->
    {'node.config.all.read', #{node => Node}};

handler_info('GET', [<<"_node">>, Node, <<"_config">>, Section], _) ->
    {'node.config.section.read', #{node => Node, 'config.section' => Section}};

handler_info('GET', [<<"_node">>, Node, <<"_config">>, Section, Key], _) ->
    {'node.config.key.read', #{
        node => Node,
        'config.section' => Section,
        'config.key' => Key
    }};

handler_info('PUT', [<<"_node">>, Node, <<"_config">>, Section, Key], _) ->
    {'node.config.key.write', #{
        node => Node,
        'config.section' => Section,
        'config.key' => Key
    }};

handler_info('DELETE', [<<"_node">>, Node, <<"_config">>, Section, Key], _) ->
    {'node.config.key.delete', #{
        node => Node,
        'config.section' => Section,
        'config.key' => Key
    }};

handler_info('GET', [<<"_node">>, Node, <<"_stats">> | Path], _) ->
    {'node.stats.read', #{node => Node, 'stat.path' => Path}};

handler_info('GET', [<<"_node">>, Node, <<"_system">>], _) ->
    {'node.system.read', #{node => Node}};

handler_info('POST', [<<"_node">>, Node, <<"_restart">>], _) ->
    {'node.restart.execute', #{node => Node}};

handler_info('POST', [<<"_reload_query_servers">>], _) ->
    {'query_servers.reload', #{}};

handler_info('POST', [<<"_replicate">>], _) ->
    {'replication.create', #{}};

handler_info('GET', [<<"_scheduler">>, <<"jobs">>], _) ->
    {'replication.jobs.read', #{}};

handler_info('GET', [<<"_scheduler">>, <<"jobs">>, JobId], _) ->
    {'replication.job.read', #{'job.id' => JobId}};

handler_info('GET', [<<"_scheduler">>, <<"docs">>], _) ->
    {'replication.docs.read', #{'db.name' => <<"_replicator">>}};

handler_info('GET', [<<"_scheduler">>, <<"docs">>, Db], _) ->
    {'replication.docs.read', #{'db.name' => Db}};

handler_info('GET', [<<"_scheduler">>, <<"docs">>, Db, DocId], _) ->
    {'replication.doc.read', #{'db.name' => Db, 'doc.id' => DocId}};

handler_info('GET', [<<"_scheduler">>, <<"docs">> | Path], _) ->
    case lists:splitwith(fun(Elem) -> Elem /= <<"_replicator">> end, Path) of
        {_, [<<"_replicator">>]} ->
            {'replication.docs.read', #{
                'db.name' => filename:join(Path)
            }};
        {DbParts, [<<"_replicator">>, DocId]} ->
            {'replication.doc.read', #{
                'db.name' => filename:join(DbParts ++ [<<"_replicator">>]),
                'doc.id' => DocId
            }};
        _ ->
            no_match
    end;

handler_info('GET', [<<"_session">>], _) ->
    {'session.read', #{}};

handler_info('POST', [<<"_session">>], _) ->
    {'session.create', #{}};

handler_info('DELETE', [<<"_session">>], _) ->
    {'session.delete', #{}};

handler_info('GET', [<<"_up">>], _) ->
    {'health.read', #{}};

handler_info('GET', [<<"_utils">> | Path], _) ->
    {'utils.read', #{'file.path' => filename:join(Path)}};

handler_info('GET', [<<"_uuids">>], _) ->
    {'uuids.read', #{}};

handler_info('GET', [<<"favicon.ico">>], _) ->
    {'favicon.ico.read', #{}};


handler_info(Method, [<<"_", _/binary>> = Part| Rest], Req) ->
    % Maybe bail here so that we don't trample over a
    % different url_handler plugin. However, we continue
    % on for known system databases.
    DbName = case Part of
        <<"_dbs">> -> '_dbs';
        <<"_global_changes">> -> '_global_changes';
        <<"_metadata">> -> '_metadata';
        <<"_nodes">> -> '_nodes';
        <<"_replicator">> -> '_replicator';
        <<"_users">> -> '_users';
        _ -> no_match
    end,
    if DbName == no_match -> no_match; true ->
        handler_info(Method, [DbName | Rest], Req)
    end;

handler_info('GET', [Db], _) ->
    {'db.info.read', #{'db.name' => Db}};

handler_info('PUT', [Db], _) ->
    {'db.create', #{'db.name' => Db}};

handler_info('POST', [Db], _) ->
    {'db.doc.write', #{'db.name' => Db}};

handler_info('DELETE', [Db], _) ->
    {'db.delete', #{'db.name' => Db}};

handler_info(M, [Db, <<"_all_docs">>], _) when M == 'GET'; M == 'POST' ->
    {'db.all_docs.read', #{'db.name' => Db}};

handler_info('POST', [Db, <<"_all_docs">>, <<"queries">>], _) ->
    {'db.all_docs.read', #{'db.name' => Db, multi => true}};

handler_info('POST', [Db, <<"_bulk_docs">>], _) ->
    {'db.docs.write', #{'db.name' => Db, bulk => true}};

handler_info('POST', [Db, <<"_bulk_get">>], _) ->
    {'db.docs.read', #{'db.name' => Db, bulk => true}};

handler_info('GET', [Db, <<"_changes">>], _) ->
    {'db.changes.read', #{'db.name' => Db}};

handler_info('POST', [Db, <<"_changes">>], _) ->
    {'db.changes.read', #{'db.name' => Db}};

handler_info('POST', [Db, <<"_compact">>], _) ->
    {'db.compact.execute', #{'db.name' => Db}};

handler_info('GET', [Db, <<"_design">>, Name], _) ->
    {'db.design.doc.read', #{'db.name' => Db, 'design.id' => Name}};

handler_info('POST', [Db, <<"_design">>, Name], _) ->
    {'db.design.doc.write', #{'db.name' => Db, 'design.id' => Name}};

handler_info('PUT', [Db, <<"_design">>, Name], _) ->
    {'db.design.doc.write', #{'db.name' => Db, 'design.id' => Name}};

handler_info('COPY', [Db, <<"_design">>, Name], Req) ->
    {'db.design.doc.write', #{
        'db.name' => Db,
        'design.id' => get_copy_destination(Req),
        'copy.source.doc.id' => <<"_design/", Name/binary>>
    }};

handler_info('DELETE', [Db, <<"_design">>, Name], _) ->
    {'db.design.doc.delete', #{'db.name' => Db, 'design.id' => Name}};

handler_info('GET', [Db, <<"_design">>, Name, <<"_info">>], _) ->
    {'db.design.info.read', #{'db.name' => Db, 'design.id' => Name}};

handler_info(M, [Db, <<"_design">>, Name, <<"_list">>, List, View], _)
        when M == 'GET'; M == 'POST', M == 'OPTIONS' ->
    {'db.design.list.read', #{
        'db.name' => Db,
        'design.id' => Name,
        'design.list.name' => List,
        'design.view.name' => View
    }};

handler_info(M, [Db, <<"_design">>, Name, <<"_list">>, List, Design, View], _)
        when M == 'GET'; M == 'POST', M == 'OPTIONS' ->
    {'db.design.list.read', #{
        'db.name' => Db,
        'design.id' => Name,
        'design.list.name' => List,
        'design.view.source.id' => Design,
        'design.view.name' => View
    }};

handler_info(_, [Db, <<"_design">>, Name, <<"_rewrite">> | Path], _) ->
    {'db.design.rewrite.execute', #{
        'db.name' => Db,
        'design.id' => Name,
        'rewrite.path' => filename:join(Path)
    }};

handler_info(_, [Db, <<"_design">>, Name, <<"_show">>, Show, DocId], _) ->
    {'db.design.show.execute', #{
        'db.name' => Db,
        'design.id' => Name,
        'design.show.name' => Show,
        'design.show.doc.id' => DocId
    }};

handler_info(_, [Db, <<"_design">>, Name, <<"_update">>, Update | Rest], _) ->
    BaseTags = #{
        'db.name' => Db,
        'design.id' => Name,
        'design.update.name' => Update
    },
    Tags = case Rest of
        [] ->
            BaseTags;
        _ ->
            DocId = filename:join(Rest),
            maps:put('design.update.doc.id', DocId, BaseTags)
    end,
    {'db.design.update.execute', Tags};

handler_info('POST', [Db, <<"_design">>, Name, <<"_view">>, View, <<"queries">>], _) ->
    {'db.design.view.multi.read', #{
        'db.name' => Db,
        'design.id' => Name,
        'design.view.name' => View
    }};

handler_info(M, [Db, <<"_design">>, Name, <<"_view">>, View], _)
        when M == 'GET'; M == 'POST' ->
    {'db.design.view.read', #{
        'db.name' => Db,
        'design.id' => Name,
        'design.view.name' => View
    }};

handler_info(_, [_Db, <<"_design">>, _Name, <<"_", _/binary>> | _], _) ->
    % Bail here so that we don't treat a plugin
    % design handler in place of a design attachment
    no_match;

handler_info('GET', [Db, <<"_design">>, Name | Path], _) ->
    {'db.design.doc.attachment.read', #{
        'db.name' => Db,
        'design.id' => Name,
        'attachment.name' => filename:join(Path)
    }};

handler_info('PUT', [Db, <<"_design">>, Name | Path], _) ->
    {'db.design.doc.attachment.write', #{
        'db.name' => Db,
        'design.id' => Name,
        'attachment.name' => filename:join(Path)
    }};

handler_info('DELETE', [Db, <<"_design">>, Name | Path], _) ->
    {'db.design.doc.attachment.delete', #{
        'db.name' => Db,
        'design.id' => Name,
        'attachment.name' => filename:join(Path)
    }};

handler_info(_, [Db, <<"_design/", Name/binary>> | Rest], Req) ->
    % Recurse if someone sent us `_design%2Fname`
    chttpd_handlers:handler_info(Req#httpd{
        path_parts = [Db, <<"_design">>, Name | Rest]
    });

handler_info(M, [Db, <<"_design_docs">>], _) when M == 'GET'; M == 'POST' ->
    {'db.design_docs.read', #{'db.name' => Db}};

handler_info('POST', [Db, <<"_design_docs">>, <<"queries">>], _) ->
    {'db.design_docs.read', #{'db.name' => Db, multi => true}};

handler_info('POST', [Db, <<"_ensure_full_commit">>], _) ->
    {'db.ensure_full_commit.execute', #{'db.name' => Db}};

handler_info('GET', [Db, <<"_local">>, Name], _) ->
    {'db.local.doc.read', #{'db.name' => Db, 'local.id' => Name}};

handler_info('POST', [Db, <<"_local">>, Name], _) ->
    {'db.local.doc.write', #{'db.name' => Db, 'local.id' => Name}};

handler_info('PUT', [Db, <<"_local">>, Name], _) ->
    {'db.local.doc.write', #{'db.name' => Db, 'local.id' => Name}};

handler_info('COPY', [Db, <<"_local">>, Name], Req) ->
    {'db.local.doc.write', #{
        'db.name' => Db,
        'local.id' => get_copy_destination(Req),
        'copy.source.doc.id' => <<"_local/", Name/binary>>
    }};

handler_info('DELETE', [Db, <<"_local">>, Name], _) ->
    {'db.local.doc.delete', #{'db.name' => Db, 'local.id' => Name}};

handler_info(_, [Db, <<"_local">>, Name | _Path], _) ->
    {'db.local.doc.invalid_attachment_req', #{
        'db.name' => Db,
        'local.id' => Name
    }};

handler_info(M, [Db, <<"_local_docs">>], _) when M == 'GET'; M == 'POST' ->
    {'db.local_docs.read', #{'db.name' => Db}};

handler_info('POST', [Db, <<"_local_docs">>, <<"queries">>], _) ->
    {'db.local_docs.read', #{'db.name' => Db, multi => true}};

handler_info('POST', [Db, <<"_missing_revs">>], _) ->
    {'db.docs.missing_revs.execute', #{'db.name' => Db}};

handler_info('GET', [Db, <<"_partition">>, Partition], _) ->
    {'db.partition.info.read', #{'db.name' => Db, partition => Partition}};

handler_info(_, [Db, <<"_partition">>, Partition | Rest], Req) ->
    NewPath = case Rest of
        [<<"_all_docs">> | _] ->
            [Db | Rest];
        [<<"_index">> | _] ->
            [Db | Rest];
        [<<"_find">> | _] ->
            [Db | Rest];
        [<<"_explain">> | _] ->
            [Db | Rest];
        [<<"_design">>, _Name, <<"_", _/binary>> | _] ->
            [Db | Rest];
        _ ->
            no_match
    end,
    if NewPath == no_match -> no_match; true ->
        {OpName, Tags} = chttpd_handlers:handler_info(Req#httpd{
            path_parts = NewPath
        }),
        NewOpName = case atom_to_list(OpName) of
            "db." ++ Name -> list_to_atom("db.partition." ++ Name);
            Else -> list_to_atom(Else ++ ".partition")
        end,
        {NewOpName, maps:put(partition, Partition, Tags)}
    end;

handler_info('POST', [Db, <<"_purge">>], _) ->
    {'db.docs.purge', #{'db.name' => Db}};

handler_info('GET', [Db, <<"_purged_infos_limit">>], _) ->
    {'db.purged_infos_limit.read', #{'db.name' => Db}};

handler_info('PUT', [Db, <<"_purged_infos_limit">>], _) ->
    {'db.purged_infos_limit.write', #{'db.name' => Db}};

handler_info('POST', [Db, <<"_revs_diff">>], _) ->
    {'db.docs.revs_diff.execute', #{'db.name' => Db}};

handler_info('GET', [Db, <<"_revs_limit">>], _) ->
    {'db.revs_limit.read', #{'db.name' => Db}};

handler_info('PUT', [Db, <<"_revs_limit">>], _) ->
    {'db.revs_limit.write', #{'db.name' => Db}};

handler_info('GET', [Db, <<"_security">>], _) ->
    {'db.security.read', #{'db.name' => Db}};

handler_info('PUT', [Db, <<"_security">>], _) ->
    {'db.security.write', #{'db.name' => Db}};

handler_info(_, [Db, <<"_view_cleanup">>], _) ->
    {'views.cleanup.execute', #{'db.name' => Db}};

handler_info(_, [_Db, <<"_", _/binary>> | _], _) ->
    % Bail here for other possible db_handleres
    no_match;

handler_info('GET', [Db, DocId], _) ->
    {'db.doc.read', #{'db.name' => Db, 'doc.id' => DocId}};

handler_info('POST', [Db, DocId], _) ->
    {'db.doc.write', #{'db.name' => Db, 'design.id' => DocId}};

handler_info('PUT', [Db, DocId], _) ->
    {'db.doc.write', #{'db.name' => Db, 'design.id' => DocId}};

handler_info('COPY', [Db, DocId], Req) ->
    {'db.doc.write', #{
        'db.name' => Db,
        'doc.id' => get_copy_destination(Req),
        'copy.source.doc.id' => DocId
    }};

handler_info('DELETE', [Db, DocId], _) ->
    {'db.doc.delete', #{'db.name' => Db, 'doc.id' => DocId}};

handler_info('GET', [Db, DocId | Path], _) ->
    {'db.doc.attachment.read', #{
        'db.name' => Db,
        'doc.id' => DocId,
        'attachment.name' => filename:join(Path)
    }};

handler_info('PUT', [Db, DocId | Path], _) ->
    {'db.doc.attachment.write', #{
        'db.name' => Db,
        'doc.id' => DocId,
        'attachment.name' => filename:join(Path)
    }};

handler_info('DELETE', [Db, DocId | Path], _) ->
    {'db.doc.attachment.delete', #{
        'db.name' => Db,
        'doc.id' => DocId,
        'attachment.name' => filename:join(Path)
    }};

handler_info(_, _, _) ->
    no_match.


get_copy_destination(Req) ->
    try
        {DocIdStr, _} = couch_httpd_db:parse_copy_destination_header(Req),
        list_to_binary(mochiweb_util:unquote(DocIdStr))
    catch _:_ ->
        unknown
    end.


not_supported(#httpd{} = Req, Db, _DDoc) ->
    not_supported(Req, Db).


not_supported(#httpd{} = Req, _Db) ->
    Msg = <<"resource is not supported in CouchDB >= 4.x">>,
    chttpd:send_error(Req, 410, gone, Msg).


not_implemented(#httpd{} = Req, _Db) ->
    Msg = <<"resource is not implemented">>,
    chttpd:send_error(Req, 501, not_implemented, Msg).
