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

-module(chttpd_misc).

-export([
    handle_all_dbs_req/1,
    handle_dbs_info_req/1,
    handle_node_req/1,
    handle_favicon_req/1,
    handle_favicon_req/2,
    handle_replicate_req/1,
    handle_reload_query_servers_req/1,
    handle_system_req/1,
    handle_task_status_req/1,
    handle_up_req/1,
    handle_utils_dir_req/1,
    handle_utils_dir_req/2,
    handle_uuids_req/1,
    handle_welcome_req/1,
    handle_welcome_req/2,
    get_stats/0
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-import(chttpd,
    [send_json/2,send_json/3,send_method_not_allowed/2,
    send_chunk/2,start_chunked_response/3]).

-define(MAX_DB_NUM_FOR_DBS_INFO, 100).

% httpd global handlers

handle_welcome_req(Req) ->
    handle_welcome_req(Req, <<"Welcome">>).

handle_welcome_req(#httpd{method='GET'}=Req, WelcomeMessage) ->
    send_json(Req, {[
        {couchdb, WelcomeMessage},
        {version, list_to_binary(couch_server:get_version())},
        {git_sha, list_to_binary(couch_server:get_git_sha())},
        {uuid, couch_server:get_uuid()},
        {features, config:features()}
        ] ++ case config:get("vendor") of
        [] ->
            [];
        Properties ->
            [{vendor, {[{?l2b(K), ?l2b(V)} || {K, V} <- Properties]}}]
        end
    });
handle_welcome_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_favicon_req(Req) ->
    handle_favicon_req(Req, get_docroot()).

handle_favicon_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    {DateNow, TimeNow} = calendar:universal_time(),
    DaysNow = calendar:date_to_gregorian_days(DateNow),
    DaysWhenExpires = DaysNow + 365,
    DateWhenExpires = calendar:gregorian_days_to_date(DaysWhenExpires),
    CachingHeaders = [
        %favicon should expire a year from now
        {"Cache-Control", "public, max-age=31536000"},
        {"Expires", couch_util:rfc1123_date({DateWhenExpires, TimeNow})}
    ],
    chttpd:serve_file(Req, "favicon.ico", DocumentRoot, CachingHeaders);
handle_favicon_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_utils_dir_req(Req) ->
    handle_utils_dir_req(Req, get_docroot()).

handle_utils_dir_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    "/" ++ UrlPath = chttpd:path(Req),
    case chttpd:partition(UrlPath) of
    {_ActionKey, "/", RelativePath} ->
        % GET /_utils/path or GET /_utils/
        CachingHeaders = [{"Cache-Control", "private, must-revalidate"}],
        EnableCsp = config:get("csp", "enable", "false"),
        Headers = maybe_add_csp_headers(CachingHeaders, EnableCsp),
        chttpd:serve_file(Req, RelativePath, DocumentRoot, Headers);
    {_ActionKey, "", _RelativePath} ->
        % GET /_utils
        RedirectPath = chttpd:path(Req) ++ "/",
        chttpd:send_redirect(Req, RedirectPath)
    end;
handle_utils_dir_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

maybe_add_csp_headers(Headers, "true") ->
    DefaultValues = "default-src 'self'; img-src 'self' data:; font-src 'self'; "
                    "script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline';",
    Value = config:get("csp", "header_value", DefaultValues),
    [{"Content-Security-Policy", Value} | Headers];
maybe_add_csp_headers(Headers, _) ->
    Headers.

handle_all_dbs_req(#httpd{method='GET', request_ctx=RequestCtx}=Req) ->
    #mrargs{
        start_key = StartKey,
        end_key = EndKey,
        direction = Dir,
        limit = Limit,
        skip = Skip
    } = couch_mrview_http:parse_params(Req, undefined),

    Options = [
        {start_key, StartKey},
        {end_key, EndKey},
        {dir, Dir},
        {limit, Limit},
        {skip, Skip},
        {request_ctx, RequestCtx}
    ],

    % Eventually the Etag for this request will be derived
    % from the \xFFmetadataVersion key in fdb
    Etag = <<"foo">>,

    {ok, Resp} = chttpd:etag_respond(Req, Etag, fun() ->
        {ok, Resp} = chttpd:start_delayed_json_response(Req, 200, [{"ETag",Etag}]),
        Callback = fun all_dbs_callback/2,
        Acc = #vacc{req=Req,resp=Resp},
        fabric2_db:list_dbs(Callback, Acc, Options)
    end),
    case is_record(Resp, vacc) of
        true -> {ok, Resp#vacc.resp};
        _ -> {ok, Resp}
    end;
handle_all_dbs_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

all_dbs_callback({meta, _Meta}, #vacc{resp=Resp0}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "["),
    {ok, Acc#vacc{resp=Resp1}};
all_dbs_callback({row, Row}, #vacc{resp=Resp0}=Acc) ->
    Prepend = couch_mrview_http:prepend_val(Acc),
    DbName = couch_util:get_value(id, Row),
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, [Prepend, ?JSON_ENCODE(DbName)]),
    {ok, Acc#vacc{prepend=",", resp=Resp1}};
all_dbs_callback(complete, #vacc{resp=Resp0}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "]"),
    {ok, Resp2} = chttpd:end_delayed_json_response(Resp1),
    {ok, Acc#vacc{resp=Resp2}};
all_dbs_callback({error, Reason}, #vacc{resp=Resp0}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_error(Resp0, Reason),
    {ok, Acc#vacc{resp=Resp1}}.

handle_dbs_info_req(#httpd{method='POST', user_ctx=UserCtx}=Req) ->
    chttpd:validate_ctype(Req, "application/json"),
    Props = chttpd:json_body_obj(Req),
    Keys = couch_mrview_util:get_view_keys(Props),
    case Keys of
        undefined -> throw({bad_request, "`keys` member must exist."});
        _ -> ok
    end,
    MaxNumber = config:get_integer("chttpd",
        "max_db_number_for_dbs_info_req", ?MAX_DB_NUM_FOR_DBS_INFO),
    case length(Keys) =< MaxNumber of
        true -> ok;
        false -> throw({bad_request, too_many_keys})
    end,
    {ok, Resp} = chttpd:start_json_response(Req, 200),
    send_chunk(Resp, "["),
    lists:foldl(fun(DbName, AccSeparator) ->
        try
            {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
            {ok, Info} = fabric2_db:get_db_info(Db),
            Json = ?JSON_ENCODE({[{key, DbName}, {info, {Info}}]}),
            send_chunk(Resp, AccSeparator ++ Json)
        catch error:database_does_not_exist ->
            ErrJson = ?JSON_ENCODE({[{key, DbName}, {error, not_found}]}),
            send_chunk(Resp, AccSeparator ++ ErrJson)
        end,
        "," % AccSeparator now has a comma
    end, "", Keys),
    send_chunk(Resp, "]"),
    chttpd:end_json_response(Resp);
handle_dbs_info_req(Req) ->
    send_method_not_allowed(Req, "POST").

handle_task_status_req(#httpd{method='GET'}=Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    {Replies, _BadNodes} = gen_server:multi_call(couch_task_status, all),
    Response = lists:flatmap(fun({Node, Tasks}) ->
        [{[{node,Node} | Task]} || Task <- Tasks]
    end, Replies),
    send_json(Req, lists:sort(Response));
handle_task_status_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_replicate_req(#httpd{method='POST', user_ctx=Ctx} = Req) ->
    chttpd:validate_ctype(Req, "application/json"),
    %% see HACK in chttpd.erl about replication
    PostBody = get(post_body),
    case replicate(PostBody, Ctx) of
        {ok, {continuous, RepId}} ->
            send_json(Req, 202, {[{ok, true}, {<<"_local_id">>, RepId}]});
        {ok, {cancelled, RepId}} ->
            send_json(Req, 200, {[{ok, true}, {<<"_local_id">>, RepId}]});
        {ok, {JsonResults}} ->
            send_json(Req, {[{ok, true} | JsonResults]});
        {ok, stopped} ->
            send_json(Req, 200, {[{ok, stopped}]});
        {error, not_found=Error} ->
            chttpd:send_error(Req, Error);
        {error, {_, _}=Error} ->
            chttpd:send_error(Req, Error);
        {_, _}=Error ->
            chttpd:send_error(Req, Error)
    end;
handle_replicate_req(Req) ->
    send_method_not_allowed(Req, "POST").

replicate({Props} = PostBody, Ctx) ->
    case couch_util:get_value(<<"cancel">>, Props) of
    true ->
        cancel_replication(PostBody, Ctx);
    _ ->
        Node = choose_node([
            couch_util:get_value(<<"source">>, Props),
            couch_util:get_value(<<"target">>, Props)
        ]),
        case rpc:call(Node, couch_replicator, replicate, [PostBody, Ctx]) of
        {badrpc, Reason} ->
            erlang:error(Reason);
        Res ->
            Res
        end
    end.

cancel_replication(PostBody, Ctx) ->
    {Res, _Bad} = rpc:multicall(couch_replicator, replicate, [PostBody, Ctx]),
    case [X || {ok, {cancelled, _}} = X <- Res] of
    [Success|_] ->
        % Report success if at least one node canceled the replication
        Success;
    [] ->
        case lists:usort(Res) of
        [UniqueReply] ->
            % Report a universally agreed-upon reply
            UniqueReply;
        [] ->
            {error, badrpc};
        Else ->
            % Unclear what to do here -- pick the first error?
            % Except try ignoring any {error, not_found} responses
            % because we'll always get two of those
            hd(Else -- [{error, not_found}])
        end
    end.

choose_node(Key) when is_binary(Key) ->
    Checksum = erlang:crc32(Key),
    Nodes = lists:sort([node()|erlang:nodes()]),
    lists:nth(1 + Checksum rem length(Nodes), Nodes);
choose_node(Key) ->
    choose_node(term_to_binary(Key)).

handle_reload_query_servers_req(#httpd{method='POST'}=Req) ->
    chttpd:validate_ctype(Req, "application/json"),
    ok = couch_proc_manager:reload(),
    send_json(Req, 200, {[{ok, true}]});
handle_reload_query_servers_req(Req) ->
    send_method_not_allowed(Req, "POST").

handle_uuids_req(Req) ->
    couch_httpd_misc_handlers:handle_uuids_req(Req).


% Node-specific request handler (_config and _stats)
% Support _local meaning this node
handle_node_req(#httpd{path_parts=[_, <<"_local">>]}=Req) ->
    send_json(Req, 200, {[{name, node()}]});
handle_node_req(#httpd{path_parts=[A, <<"_local">>|Rest]}=Req) ->
    handle_node_req(Req#httpd{path_parts=[A, node()] ++ Rest});
% GET /_node/$node/_config
handle_node_req(#httpd{method='GET', path_parts=[_, Node, <<"_config">>]}=Req) ->
    Grouped = lists:foldl(fun({{Section, Key}, Value}, Acc) ->
        case dict:is_key(Section, Acc) of
        true ->
            dict:append(Section, {list_to_binary(Key), list_to_binary(Value)}, Acc);
        false ->
            dict:store(Section, [{list_to_binary(Key), list_to_binary(Value)}], Acc)
        end
    end, dict:new(), call_node(Node, config, all, [])),
    KVs = dict:fold(fun(Section, Values, Acc) ->
        [{list_to_binary(Section), {Values}} | Acc]
    end, [], Grouped),
    send_json(Req, 200, {KVs});
handle_node_req(#httpd{path_parts=[_, _Node, <<"_config">>]}=Req) ->
    send_method_not_allowed(Req, "GET");
% GET /_node/$node/_config/Section
handle_node_req(#httpd{method='GET', path_parts=[_, Node, <<"_config">>, Section]}=Req) ->
    KVs = [{list_to_binary(Key), list_to_binary(Value)}
            || {Key, Value} <- call_node(Node, config, get, [Section])],
    send_json(Req, 200, {KVs});
handle_node_req(#httpd{path_parts=[_, _Node, <<"_config">>, _Section]}=Req) ->
    send_method_not_allowed(Req, "GET");
% PUT /_node/$node/_config/Section/Key
% "value"
handle_node_req(#httpd{method='PUT', path_parts=[_, Node, <<"_config">>, Section, Key]}=Req) ->
    couch_util:check_config_blacklist(Section),
    Value = couch_util:trim(chttpd:json_body(Req)),
    Persist = chttpd:header_value(Req, "X-Couch-Persist") /= "false",
    OldValue = call_node(Node, config, get, [Section, Key, ""]),
    case call_node(Node, config, set, [Section, Key, ?b2l(Value), Persist]) of
        ok ->
            send_json(Req, 200, list_to_binary(OldValue));
        {error, Reason} ->
            chttpd:send_error(Req, {bad_request, Reason})
    end;
% GET /_node/$node/_config/Section/Key
handle_node_req(#httpd{method='GET', path_parts=[_, Node, <<"_config">>, Section, Key]}=Req) ->
    case call_node(Node, config, get, [Section, Key, undefined]) of
    undefined ->
        throw({not_found, unknown_config_value});
    Value ->
        send_json(Req, 200, list_to_binary(Value))
    end;
% DELETE /_node/$node/_config/Section/Key
handle_node_req(#httpd{method='DELETE',path_parts=[_, Node, <<"_config">>, Section, Key]}=Req) ->
    couch_util:check_config_blacklist(Section),
    Persist = chttpd:header_value(Req, "X-Couch-Persist") /= "false",
    case call_node(Node, config, get, [Section, Key, undefined]) of
    undefined ->
        throw({not_found, unknown_config_value});
    OldValue ->
        case call_node(Node, config, delete, [Section, Key, Persist]) of
            ok ->
                send_json(Req, 200, list_to_binary(OldValue));
            {error, Reason} ->
                chttpd:send_error(Req, {bad_request, Reason})
        end
    end;
handle_node_req(#httpd{path_parts=[_, _Node, <<"_config">>, _Section, _Key]}=Req) ->
    send_method_not_allowed(Req, "GET,PUT,DELETE");
handle_node_req(#httpd{path_parts=[_, _Node, <<"_config">>, _Section, _Key | _]}=Req) ->
    chttpd:send_error(Req, not_found);
% GET /_node/$node/_stats
handle_node_req(#httpd{method='GET', path_parts=[_, Node, <<"_stats">> | Path]}=Req) ->
    flush(Node, Req),
    Stats0 = call_node(Node, couch_stats, fetch, []),
    Stats = couch_stats_httpd:transform_stats(Stats0),
    Nested = couch_stats_httpd:nest(Stats),
    EJSON0 = couch_stats_httpd:to_ejson(Nested),
    EJSON1 = couch_stats_httpd:extract_path(Path, EJSON0),
    chttpd:send_json(Req, EJSON1);
handle_node_req(#httpd{path_parts=[_, _Node, <<"_stats">>]}=Req) ->
    send_method_not_allowed(Req, "GET");
% GET /_node/$node/_system
handle_node_req(#httpd{method='GET', path_parts=[_, Node, <<"_system">>]}=Req) ->
    Stats = call_node(Node, chttpd_misc, get_stats, []),
    EJSON = couch_stats_httpd:to_ejson(Stats),
    send_json(Req, EJSON);
handle_node_req(#httpd{path_parts=[_, _Node, <<"_system">>]}=Req) ->
    send_method_not_allowed(Req, "GET");
% POST /_node/$node/_restart
handle_node_req(#httpd{method='POST', path_parts=[_, Node, <<"_restart">>]}=Req) ->
    call_node(Node, init, restart, []),
    send_json(Req, 200, {[{ok, true}]});
handle_node_req(#httpd{path_parts=[_, _Node, <<"_restart">>]}=Req) ->
    send_method_not_allowed(Req, "POST");
handle_node_req(#httpd{path_parts=[_]}=Req) ->
    chttpd:send_error(Req, {bad_request, <<"Incomplete path to _node request">>});
handle_node_req(#httpd{path_parts=[_, _Node]}=Req) ->
    chttpd:send_error(Req, {bad_request, <<"Incomplete path to _node request">>});
handle_node_req(Req) ->
    chttpd:send_error(Req, not_found).


call_node(Node0, Mod, Fun, Args) when is_binary(Node0) ->
    Node1 = try
                list_to_existing_atom(?b2l(Node0))
            catch
                error:badarg ->
                    throw({not_found, <<"no such node: ", Node0/binary>>})
            end,
    call_node(Node1, Mod, Fun, Args);
call_node(Node, Mod, Fun, Args) when is_atom(Node) ->
    case rpc:call(Node, Mod, Fun, Args) of
        {badrpc, nodedown} ->
            Reason = ?l2b(io_lib:format("~s is down", [Node])),
            throw({error, {nodedown, Reason}});
        Else ->
            Else
    end.

flush(Node, Req) ->
    case couch_util:get_value("flush", chttpd:qs(Req)) of
        "true" ->
            call_node(Node, couch_stats_aggregator, flush, []);
        _Else ->
            ok
    end.

% Note: this resource is exposed on the backdoor interface, but it's in chttpd
% because it's not couch trunk
handle_system_req(Req) ->
    Stats = get_stats(),
    EJSON = couch_stats_httpd:to_ejson(Stats),
    send_json(Req, EJSON).

get_stats() ->
    Other = erlang:memory(system) - lists:sum([X || {_,X} <-
        erlang:memory([atom, code, binary, ets])]),
    Memory = [{other, Other} | erlang:memory([atom, atom_used, processes,
        processes_used, binary, code, ets])],
    {NumberOfGCs, WordsReclaimed, _} = statistics(garbage_collection),
    {{input, Input}, {output, Output}} = statistics(io),
    {CF, CDU} = db_pid_stats(),
    MessageQueues0 = [{couch_file, {CF}}, {couch_db_updater, {CDU}}],
    MessageQueues = MessageQueues0 ++ message_queues(registered()),
    [
        {uptime, couch_app:uptime() div 1000},
        {memory, {Memory}},
        {run_queue, statistics(run_queue)},
        {ets_table_count, length(ets:all())},
        {context_switches, element(1, statistics(context_switches))},
        {reductions, element(1, statistics(reductions))},
        {garbage_collection_count, NumberOfGCs},
        {words_reclaimed, WordsReclaimed},
        {io_input, Input},
        {io_output, Output},
        {os_proc_count, couch_proc_manager:get_proc_count()},
        {stale_proc_count, couch_proc_manager:get_stale_proc_count()},
        {process_count, erlang:system_info(process_count)},
        {process_limit, erlang:system_info(process_limit)},
        {message_queues, {MessageQueues}},
        {internal_replication_jobs, mem3_sync:get_backlog()},
        {distribution, {get_distribution_stats()}}
    ].

db_pid_stats() ->
    {monitors, M} = process_info(whereis(couch_stats_process_tracker), monitors),
    Candidates = [Pid || {process, Pid} <- M],
    CouchFiles = db_pid_stats(couch_file, Candidates),
    CouchDbUpdaters = db_pid_stats(couch_db_updater, Candidates),
    {CouchFiles, CouchDbUpdaters}.

db_pid_stats(Mod, Candidates) ->
    Mailboxes = lists:foldl(
        fun(Pid, Acc) ->
            case process_info(Pid, [message_queue_len, dictionary]) of
                undefined ->
                    Acc;
                PI ->
                    Dictionary = proplists:get_value(dictionary, PI, []),
                    case proplists:get_value('$initial_call', Dictionary) of
                        {Mod, init, 1} ->
                            case proplists:get_value(message_queue_len, PI) of
                                undefined -> Acc;
                                Len -> [Len|Acc]
                            end;
                        _  ->
                            Acc
                    end
            end
        end, [], Candidates
    ),
    format_pid_stats(Mailboxes).

format_pid_stats([]) ->
    [];
format_pid_stats(Mailboxes) ->
    Sorted = lists:sort(Mailboxes),
    Count = length(Sorted),
    [
        {count, Count},
        {min, hd(Sorted)},
        {max, lists:nth(Count, Sorted)},
        {'50', lists:nth(round(Count * 0.5), Sorted)},
        {'90', lists:nth(round(Count * 0.9), Sorted)},
        {'99', lists:nth(round(Count * 0.99), Sorted)}
    ].

get_distribution_stats() ->
    lists:map(fun({Node, Socket}) ->
        {ok, Stats} = inet:getstat(Socket),
        {Node, {Stats}}
    end, erlang:system_info(dist_ctrl)).

handle_up_req(#httpd{method='GET'} = Req) ->
    case config:get("couchdb", "maintenance_mode") of
    "true" ->
        send_json(Req, 404, {[{status, maintenance_mode}]});
    "nolb" ->
        send_json(Req, 404, {[{status, nolb}]});
    _ ->
        {ok, {Status}} = mem3_seeds:get_status(),
        case couch_util:get_value(status, Status) of
            ok ->
                send_json(Req, 200, {Status});
            seeding ->
                send_json(Req, 404, {Status})
        end
    end;

handle_up_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

message_queues(Registered) ->
    lists:map(fun(Name) ->
        Type = message_queue_len,
        {Type, Length} = process_info(whereis(Name), Type),
        {Name, Length}
    end, Registered).

get_docroot() ->
    % if the env var isn’t set, let’s not throw an error, but
    % assume the current working dir is what we want
    os:getenv("COUCHDB_FAUXTON_DOCROOT", "").
