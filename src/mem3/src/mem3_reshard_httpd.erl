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
%
handle_reshard_req(#httpd{method='GET', path_parts=[_]} = Req) ->
    State = mem3_reshard_httpd_util:get_summary(),
    send_json(Req, State);

handle_reshard_req(#httpd{path_parts=[_]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD");


% GET /_reshard/state
%
handle_reshard_req(#httpd{method='GET',
        path_parts=[_, ?STATE]} = Req) ->
    {State, Reason} = mem3_reshard_httpd_util:get_shard_splitting_state(),
    send_json(Req, {[{state, State}, {reason, Reason}]});

% PUT /_reshard/state
%
handle_reshard_req(#httpd{method='PUT',
        path_parts=[_, ?STATE]} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body_obj(Req),
    State = couch_util:get_value(<<"state">>, Props),
    Reason = couch_util:get_value(<<"reason">>, Props),
    case {State, Reason} of
        {undefined, _} ->
            throw({bad_request, <<"Expected a `state` field">>});
        {?S_RUNNING, _} ->
            case mem3_reshard_httpd_util:start_shard_splitting() of
                {ok, JsonResult} ->
                    send_json(Req, 200, JsonResult);
                {error, JsonError} ->
                    send_json(Req, 500, JsonError)
            end;
        {?S_STOPPED, Reason} ->
            Reason1 = case Reason =:= undefined of false -> Reason; true ->
                <<"Shard splitting stopped on the cluster by user">>
            end,
            case mem3_reshard_httpd_util:stop_shard_splitting(Reason1) of
                {ok, JsonResult} ->
                    send_json(Req, 200, JsonResult);
                {error, JsonError} ->
                    send_json(Req, 500, JsonError)
            end;
        {_, _} ->
            throw({bad_request, <<"State field not `running` or `stopped`">>})
     end;

handle_reshard_req(#httpd{path_parts=[_, ?STATE]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,PUT");

handle_reshard_req(#httpd{path_parts=[_, ?STATE | _]} = Req) ->
    throw(not_found);


% GET /_reshard/jobs
%
handle_reshard_req(#httpd{method='GET', path_parts=[_,?JOBS]}=Req) ->
    Jobs = mem3_reshard_httpd_util:get_jobs(),
    Total = length(Jobs),
    send_json(Req, {[{total_rows, Total}, {offset, 0}, {jobs, Jobs}]});

% POST /_reshard/jobs {"node": "...", "shard": "..."}
%
handle_reshard_req(#httpd{method = 'POST',
        path_parts=[_, ?JOBS]} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body_obj(Req),
    Node = mem3_reshard_httpd_util:validate_node(
        couch_util:get_value(<<"node">>, Props)),
    Shard = mem3_reshard_httpd_util:validate_shard(
        couch_util:get_value(<<"shard">>, Props)),
    Db = mem3_reshard_httpd_util:validate_db(
        couch_util:get_value(<<"db">>, Props)),
    Range = mem3_reshard_httpd_util:validate_range(
        couch_util:get_value(<<"range">>, Props)),
    Type = mem3_reshard_httpd_util:validate_type(
        couch_util:get_value(<<"type">>, Props)),
    Res = mem3_reshard_httpd_util:create_jobs(Node, Shard, Db, Range, Type),
    case Res of
        [] -> throw(not_found);
        _ -> ok
    end,
    Oks = length([R || {ok, _, _, _} = R <- Res]),
    Code = case {Oks, length(Res)} of
        {Oks, Oks} -> 201;
        {Oks, _} when Oks > 0 -> 202;
        {0, _} -> 500
    end,
    EJson = lists:map(fun
        ({ok, Id, N, S}) ->
            {[{ok, true}, {id, Id}, {node, N}, {shard, S}]};
        ({error, E, N, S}) ->
            {[{error, couch_util:to_binary(E)}, {node, N}, {shard, S}]}
    end, Res),
    send_json(Req, Code, EJson);

handle_reshard_req(#httpd{path_parts=[_, ?JOBS]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,POST");

handle_reshard_req(#httpd{path_parts=[_, _]}) ->
    throw(not_found);


% GET /_reshard/jobs/$jobid
%
handle_reshard_req(#httpd{method='GET',
        path_parts=[_, ?JOBS, JobId]}=Req) ->
    case mem3_reshard_httpd_util:get_job(JobId) of
        {ok, JobInfo} ->
            send_json(Req, JobInfo);
        {error, not_found} ->
            throw(not_found)
    end;

% DELETE /_reshard/jobs/$jobid
%
handle_reshard_req(#httpd{method='DELETE',
        path_parts=[_, ?JOBS, JobId]}=Req) ->
    case mem3_reshard_httpd_util:get_job(JobId) of
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


handle_reshard_req(#httpd{path_parts=[_, ?JOBS, _]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,DELETE");


% GET /_reshard/jobs/$jobid/state
%

handle_reshard_req(#httpd{method='GET',
        path_parts=[_, ?JOBS, JobId, ?STATE]} = Req) ->
   case mem3_reshard_httpd_util:get_job(JobId) of
        {ok, {Props}} ->
             JobState = couch_util:get_value(job_state, Props),
             {SIProps} = couch_util:get_value(state_info, Props),
             Reason = case couch_util:get_value(reason, SIProps) of
                 undefined -> null;
                 Val -> couch_util:to_binary(Val)
             end,
             send_json(Req, 200, {[{state, JobState}, {reason, Reason}]});
        {error, not_found} ->
            throw(not_found)
    end;

% PUT /_reshard/jobs/$jobid/state

handle_reshard_req(#httpd{method='PUT',
        path_parts=[_, ?JOBS, JobId, ?STATE]} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body_obj(Req),
    State = couch_util:get_value(<<"state">>, Props),
    Reason = couch_util:get_value(<<"reason">>, Props),
    case {State, Reason} of
        {undefined, _} ->
            throw({bad_request, <<"Expected a `state` field">>});
        {?S_RUNNING, _} ->
            case mem3_reshard_httpd_util:resume_job(JobId) of
                ok ->
                    send_json(Req, 200, {[{ok, true}]});
                {error, not_found} ->
                    throw(not_found);
                {error, JsonError} ->
                    send_json(Req, 500, JsonError)
            end;
        {?S_STOPPED, Reason} ->
            Reason1 = case Reason =:= undefined of false -> Reason; true ->
                <<"Stopped by user">>
            end,
            case mem3_reshard_httpd_util:stop_job(JobId, Reason1) of
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

handle_reshard_req(#httpd{path_parts=[_, ?JOBS ,_, ?STATE ]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,PUT").
