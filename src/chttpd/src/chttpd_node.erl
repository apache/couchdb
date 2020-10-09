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

-module(chttpd_node).
-compile(tuple_calls).

-export([
    handle_node_req/1,
    get_stats/0,
    run_queues/0
]).

-include_lib("couch/include/couch_db.hrl").

-import(chttpd,
    [send_json/2,send_json/3,send_method_not_allowed/2,
    send_chunk/2,start_chunked_response/3]).

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
% POST /_node/$node/_config/_reload - Flushes unpersisted config values from RAM
handle_node_req(#httpd{method='POST', path_parts=[_, Node, <<"_config">>, <<"_reload">>]}=Req) ->
    case call_node(Node, config, reload, []) of
        ok ->
            send_json(Req, 200, {[{ok, true}]});
        {error, Reason} ->
            chttpd:send_error(Req, {bad_request, Reason})
    end;
handle_node_req(#httpd{path_parts=[_, _Node, <<"_config">>, <<"_reload">>]}=Req) ->
    send_method_not_allowed(Req, "POST");
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
    IsSensitive = Section == <<"admins">>,
    Opts = #{persisit => Persist, sensitive => IsSensitive},
    case call_node(Node, config, set, [Section, Key, ?b2l(Value), Opts]) of
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
    Stats = call_node(Node, chttpd_node, get_stats, []),
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
handle_node_req(#httpd{path_parts=[_, Node | PathParts],
                       mochi_req=MochiReq0}) ->
    % strip /_node/{node} from Req0 before descending further
    RawUri = MochiReq0:get(raw_path),
    {_, Query, Fragment} = mochiweb_util:urlsplit_path(RawUri),
    NewPath0 = "/" ++ lists:join("/", [couch_util:url_encode(P) || P <- PathParts]),
    NewRawPath = mochiweb_util:urlunsplit_path({NewPath0, Query, Fragment}),
    MaxSize =  config:get_integer("httpd", "max_http_request_size", 4294967296),
    NewOpts = [{body, MochiReq0:recv_body(MaxSize)} | MochiReq0:get(opts)],
    Ref = erlang:make_ref(),
    MochiReq = mochiweb_request:new({remote, self(), Ref},
                               NewOpts,
                               MochiReq0:get(method),
                               NewRawPath,
                               MochiReq0:get(version),
                               MochiReq0:get(headers)),
    call_node(Node, couch_httpd, handle_request, [MochiReq]),
    recv_loop(Ref, MochiReq0);
handle_node_req(#httpd{path_parts=[_]}=Req) ->
    chttpd:send_error(Req, {bad_request, <<"Incomplete path to _node request">>});
handle_node_req(Req) ->
    chttpd:send_error(Req, not_found).

recv_loop(Ref, ReqResp) ->
    receive
        {Ref, Code, Headers, _Args, start_response} ->
            recv_loop(Ref, ReqResp:start({Code, Headers}));
        {Ref, Code, Headers, Len, start_response_length} ->
            recv_loop(Ref, ReqResp:start_response_length({Code, Headers, Len}));
        {Ref, Code, Headers, chunked, respond} ->
            Resp = ReqResp:respond({Code, Headers, chunked}),
            recv_loop(Ref, Resp);
        {Ref, Code, Headers, Args, respond} ->
            Resp = ReqResp:respond({Code, Headers, Args}),
            {ok, Resp};
        {Ref, send, Data} ->
            ReqResp:send(Data),
            {ok, ReqResp};
        {Ref, chunk, <<>>} ->
            ReqResp:write_chunk(<<>>),
            {ok, ReqResp};
        {Ref, chunk, Data} ->
            ReqResp:write_chunk(Data),
            recv_loop(Ref, ReqResp);
        _Else ->
            recv_loop(Ref, ReqResp)
    end.

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
    {SQ, DCQ} = run_queues(),
    [
        {uptime, couch_app:uptime() div 1000},
        {memory, {Memory}},
        {run_queue, SQ},
        {run_queue_dirty_cpu, DCQ},
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

message_queues(Registered) ->
    lists:map(fun(Name) ->
        Type = message_queue_len,
        {Type, Length} = process_info(whereis(Name), Type),
        {Name, Length}
    end, Registered).

%% Workaround for https://bugs.erlang.org/browse/ERL-1355
run_queues() ->
    case erlang:system_info(dirty_cpu_schedulers) > 0 of
        false ->
            {statistics(run_queue), 0};
        true ->
            [DCQ | SQs] = lists:reverse(statistics(run_queue_lengths)),
            {lists:sum(SQs), DCQ}
    end.
