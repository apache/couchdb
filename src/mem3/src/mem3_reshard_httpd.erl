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

-module(mem3_reshard_httpd).

-export([
    handle_reshard_req/1
]).

-import(couch_httpd, [
    send_json/2,
    send_json/3,
    send_method_not_allowed/2
]).

-include_lib("couch/include/couch_db.hrl").

-define(JOBS, <<"jobs">>).
-define(STATE, <<"state">>).
-define(S_RUNNING, <<"running">>).
-define(S_STOPPED, <<"stopped">>).

% GET /_reshard
handle_reshard_req(#httpd{method = 'GET', path_parts = [_]} = Req) ->
    reject_if_disabled(),
    State = mem3_reshard_api:get_summary(),
    send_json(Req, State);
handle_reshard_req(#httpd{path_parts = [_]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD");
% GET /_reshard/state
handle_reshard_req(
    #httpd{
        method = 'GET',
        path_parts = [_, ?STATE]
    } = Req
) ->
    reject_if_disabled(),
    {State, Reason} = mem3_reshard_api:get_shard_splitting_state(),
    send_json(Req, {[{state, State}, {reason, Reason}]});
% PUT /_reshard/state
handle_reshard_req(
    #httpd{
        method = 'PUT',
        path_parts = [_, ?STATE]
    } = Req
) ->
    reject_if_disabled(),
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body_obj(Req),
    State = couch_util:get_value(<<"state">>, Props),
    Reason = couch_util:get_value(<<"reason">>, Props),
    case {State, Reason} of
        {undefined, _} ->
            throw({bad_request, <<"Expected a `state` field">>});
        {?S_RUNNING, _} ->
            case mem3_reshard_api:start_shard_splitting() of
                {ok, JsonResult} ->
                    send_json(Req, 200, JsonResult);
                {error, JsonError} ->
                    send_json(Req, 500, JsonError)
            end;
        {?S_STOPPED, Reason} ->
            Reason1 =
                case Reason =:= undefined of
                    false -> Reason;
                    true -> <<"Cluster-wide resharding stopped by the user">>
                end,
            case mem3_reshard_api:stop_shard_splitting(Reason1) of
                {ok, JsonResult} ->
                    send_json(Req, 200, JsonResult);
                {error, JsonError} ->
                    send_json(Req, 500, JsonError)
            end;
        {_, _} ->
            throw({bad_request, <<"State field not `running` or `stopped`">>})
    end;
handle_reshard_req(#httpd{path_parts = [_, ?STATE]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,PUT");
handle_reshard_req(#httpd{path_parts = [_, ?STATE | _]} = _Req) ->
    throw(not_found);
% GET /_reshard/jobs
handle_reshard_req(#httpd{method = 'GET', path_parts = [_, ?JOBS]} = Req) ->
    reject_if_disabled(),
    Jobs = mem3_reshard_api:get_jobs(),
    Total = length(Jobs),
    send_json(Req, {[{total_rows, Total}, {offset, 0}, {jobs, Jobs}]});
% POST /_reshard/jobs {"node": "...", "shard": "..."}
handle_reshard_req(
    #httpd{
        method = 'POST',
        path_parts = [_, ?JOBS]
    } = Req
) ->
    reject_if_disabled(),
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body_obj(Req),
    Node = validate_node(couch_util:get_value(<<"node">>, Props)),
    Shard = validate_shard(couch_util:get_value(<<"shard">>, Props)),
    Db = validate_db(couch_util:get_value(<<"db">>, Props)),
    Range = validate_range(couch_util:get_value(<<"range">>, Props)),
    Type = validate_type(couch_util:get_value(<<"type">>, Props)),
    Res = mem3_reshard_api:create_jobs(Node, Shard, Db, Range, Type),
    case Res of
        [] -> throw(not_found);
        _ -> ok
    end,
    Oks = length([R || {ok, _, _, _} = R <- Res]),
    Code =
        case {Oks, length(Res)} of
            {Oks, Oks} -> 201;
            {Oks, _} when Oks > 0 -> 202;
            {0, _} -> 500
        end,
    EJson = lists:map(
        fun
            ({ok, Id, N, S}) ->
                {[{ok, true}, {id, Id}, {node, N}, {shard, S}]};
            ({error, E, N, S}) ->
                {[{error, couch_util:to_binary(E)}, {node, N}, {shard, S}]}
        end,
        Res
    ),
    send_json(Req, Code, EJson);
handle_reshard_req(#httpd{path_parts = [_, ?JOBS]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,POST");
handle_reshard_req(#httpd{path_parts = [_, _]}) ->
    throw(not_found);
% GET /_reshard/jobs/$jobid
handle_reshard_req(
    #httpd{
        method = 'GET',
        path_parts = [_, ?JOBS, JobId]
    } = Req
) ->
    reject_if_disabled(),
    case mem3_reshard_api:get_job(JobId) of
        {ok, JobInfo} ->
            send_json(Req, JobInfo);
        {error, not_found} ->
            throw(not_found)
    end;
% DELETE /_reshard/jobs/$jobid
handle_reshard_req(
    #httpd{
        method = 'DELETE',
        path_parts = [_, ?JOBS, JobId]
    } = Req
) ->
    reject_if_disabled(),
    case mem3_reshard_api:get_job(JobId) of
        {ok, {Props}} ->
            NodeBin = couch_util:get_value(node, Props),
            Node = binary_to_atom(NodeBin, utf8),
            case rpc:call(Node, mem3_reshard, remove_job, [JobId]) of
                ok ->
                    send_json(Req, 200, {[{ok, true}]});
                {error, not_found} ->
                    throw(not_found)
            end;
        {error, not_found} ->
            throw(not_found)
    end;
handle_reshard_req(#httpd{path_parts = [_, ?JOBS, _]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,DELETE");
% GET /_reshard/jobs/$jobid/state
handle_reshard_req(
    #httpd{
        method = 'GET',
        path_parts = [_, ?JOBS, JobId, ?STATE]
    } = Req
) ->
    reject_if_disabled(),
    case mem3_reshard_api:get_job(JobId) of
        {ok, {Props}} ->
            JobState = couch_util:get_value(job_state, Props),
            {SIProps} = couch_util:get_value(state_info, Props),
            Reason =
                case couch_util:get_value(reason, SIProps) of
                    undefined -> null;
                    Val -> couch_util:to_binary(Val)
                end,
            send_json(Req, 200, {[{state, JobState}, {reason, Reason}]});
        {error, not_found} ->
            throw(not_found)
    end;
% PUT /_reshard/jobs/$jobid/state
handle_reshard_req(
    #httpd{
        method = 'PUT',
        path_parts = [_, ?JOBS, JobId, ?STATE]
    } = Req
) ->
    reject_if_disabled(),
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body_obj(Req),
    State = couch_util:get_value(<<"state">>, Props),
    Reason = couch_util:get_value(<<"reason">>, Props),
    case {State, Reason} of
        {undefined, _} ->
            throw({bad_request, <<"Expected a `state` field">>});
        {?S_RUNNING, _} ->
            case mem3_reshard_api:resume_job(JobId) of
                ok ->
                    send_json(Req, 200, {[{ok, true}]});
                {error, not_found} ->
                    throw(not_found);
                {error, JsonError} ->
                    send_json(Req, 500, JsonError)
            end;
        {?S_STOPPED, Reason} ->
            Reason1 =
                case Reason =:= undefined of
                    false -> Reason;
                    true -> <<"Stopped by user">>
                end,
            case mem3_reshard_api:stop_job(JobId, Reason1) of
                ok ->
                    send_json(Req, 200, {[{ok, true}]});
                {error, not_found} ->
                    throw(not_found);
                {error, JsonError} ->
                    send_json(Req, 500, JsonError)
            end;
        {_, _} ->
            throw({bad_request, <<"State field not `running` or `stopped`">>})
    end;
handle_reshard_req(#httpd{path_parts = [_, ?JOBS, _, ?STATE]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,PUT").

reject_if_disabled() ->
    case mem3_reshard:is_disabled() of
        true -> throw(not_implemented);
        false -> ok
    end.

validate_type(<<"split">>) ->
    split;
validate_type(_Type) ->
    throw({bad_request, <<"`job type must be `split`">>}).

validate_node(undefined) ->
    undefined;
validate_node(Node0) when is_binary(Node0) ->
    Nodes = mem3_util:live_nodes(),
    try binary_to_existing_atom(Node0, utf8) of
        N1 ->
            case lists:member(N1, Nodes) of
                true -> N1;
                false -> throw({bad_request, <<"Not connected to `node`">>})
            end
    catch
        error:badarg ->
            throw({bad_request, <<"`node` is not a valid node name">>})
    end;
validate_node(_Node) ->
    throw({bad_request, <<"Invalid `node`">>}).

validate_shard(undefined) ->
    undefined;
validate_shard(Shard) when is_binary(Shard) ->
    case Shard of
        <<"shards/", _:8/binary, "-", _:8/binary, "/", _/binary>> ->
            Shard;
        _ ->
            throw({bad_request, <<"`shard` is invalid">>})
    end;
validate_shard(_Shard) ->
    throw({bad_request, <<"Invalid `shard`">>}).

validate_db(undefined) ->
    undefined;
validate_db(DbName) when is_binary(DbName) ->
    try mem3:shards(DbName) of
        [_ | _] -> DbName;
        _ -> throw({bad_request, <<"`No shards in `db`">>})
    catch
        _:_ ->
            throw({bad_request, <<"Invalid `db`">>})
    end;
validate_db(_bName) ->
    throw({bad_request, <<"Invalid `db`">>}).

validate_range(undefined) ->
    undefined;
validate_range(<<BBin:8/binary, "-", EBin:8/binary>>) ->
    {B, E} =
        try
            {
                httpd_util:hexlist_to_integer(binary_to_list(BBin)),
                httpd_util:hexlist_to_integer(binary_to_list(EBin))
            }
        catch
            _:_ ->
                invalid_range()
        end,
    if
        B < 0 -> invalid_range();
        E < 0 -> invalid_range();
        B > (2 bsl 31) - 1 -> invalid_range();
        E > (2 bsl 31) - 1 -> invalid_range();
        B >= E -> invalid_range();
        true -> ok
    end,
    % Use a list format here to make it look the same as #shard's range
    [B, E];
validate_range(_Range) ->
    invalid_range().

invalid_range() ->
    throw({bad_request, <<"Invalid `range`">>}).
