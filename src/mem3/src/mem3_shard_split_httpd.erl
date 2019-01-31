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

-module(mem3_shard_split_httpd).

-export([
    handle_shard_splits_req/1
]).

-import(couch_httpd, [
    send_json/2,
    send_json/3,
    send_method_not_allowed/2
]).

-import(couch_util, [
    to_binary/1
]).


-include_lib("couch/include/couch_db.hrl").


-define(JOBS, <<"jobs">>).
-define(STATE, <<"state">>).
-define(S_RUNNING, <<"running">>).
-define(S_STOPPED, <<"stopped">>).

% GET /_shard_splits
%
handle_shard_splits_req(#httpd{method='GET', path_parts=[_]} = Req) ->
    State = get_summary(),
    send_json(Req, State);

handle_shard_splits_req(#httpd{path_parts=[_]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD");


% GET /_shard_splits/state
%
handle_shard_splits_req(#httpd{method='GET',
        path_parts=[_, ?STATE]} = Req) ->
    State = get_shard_splitting_state(),
    send_json(Req, {[{state, State}]});

% PUT /_shard_splits/state
%
handle_shard_splits_req(#httpd{method='PUT',
        path_parts=[_, ?STATE]} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body_obj(Req),
    State = couch_util:get_value(<<"state">>, Props),
    Reason = couch_util:get_value(<<"reason">>, Props),
    case {State, Reason} of
        {undefined, _} ->
            throw({bad_request, <<"Expected a `state` field">>});
        {?S_RUNNING, _} ->
            case start_shard_splitting() of
                {ok, JsonResult} ->
                    send_json(Req, 200, JsonResult);
                {error, JsonError} ->
                    send_json(Req, 500, JsonError)
            end;
        {?S_STOPPED, Reason} ->
            Reason1 = case Reason =:= undefined of false -> Reason; true ->
                <<"Shard splitting stopped on the cluster by user">>
            end,
            case stop_shard_splitting(Reason1) of
                {ok, JsonResult} ->
                    send_json(Req, 200, JsonResult);
                {error, JsonError} ->
                    send_json(Req, 500, JsonError)
            end;
        {_, _} ->
            throw({bad_request, <<"State field not `running` or `stopped`">>})
     end;


handle_shard_splits_req(#httpd{path_parts=[_, ?STATE]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,PUT");


% GET /_shard_splits/jobs
%
handle_shard_splits_req(#httpd{method='GET', path_parts=[_,?JOBS]}=Req) ->
    Jobs = get_jobs(),
    Total = length(Jobs),
    send_json(Req, {[{total_rows, Total}, {offset, 0}, {jobs, Jobs}]});

% POST /_shard_splits/jobs {"node": "...", "shard": "..."}
%
handle_shard_splits_req(#httpd{method = 'POST',
        path_parts=[_, ?JOBS]} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    JobProps = couch_httpd:json_body_obj(Req),
    Node = get_jobs_post_node(JobProps),
    Shard = get_jobs_post_shard(JobProps),
    case rpc:call(Node, mem3_shard_split, start_job, [Shard]) of
        {ok, JobId} ->
            send_json(Req, 200, {[{ok, true}, {<<"id">>, JobId}]});
        {error, {Error, Reason}} ->
            ErrJson = {[
                {error, to_binary(Error)},
                {reason, to_binary(Reason)}
            ]},
            send_json(Req, 500, ErrJson);
        {error, Error} ->
            send_json(Req, 500, {[{error, to_binary(Error)}]})
    end;

handle_shard_splits_req(#httpd{path_parts=[_, ?JOBS]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,POST");


% GET /_shard_splits/jobs/$jobid
%
handle_shard_splits_req(#httpd{method='GET',
        path_parts=[_, ?JOBS, JobId]}=Req) ->
    case get_job(JobId) of
        {ok, JobInfo} ->
            send_json(Req, JobInfo);
        {error, not_found} ->
            throw(not_found)
    end;

% DELETE /_shard_splits/jobs/$jobid
%
handle_shard_splits_req(#httpd{method='DELETE',
        path_parts=[_, ?JOBS, JobId]}=Req) ->
    case get_job(JobId) of
        {ok, {Props}} ->
            NodeBin = couch_util:get_value(node, Props),
            Node = binary_to_atom(NodeBin, utf8),
            case rpc:call(Node, mem3_shard_split, remove_job, [JobId]) of
                ok ->
                    send_json(Req, 200, {[{ok, true}]});
                {error, not_found} ->
                    throw(not_found)
            end;
        {error, not_found} ->
            throw(not_found)
    end;


handle_shard_splits_req(#httpd{path_parts=[_, ?JOBS, _]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,DELETE");


% GET /_shard_splits/jobs/$jobid/state
%

handle_shard_splits_req(#httpd{method='GET',
        path_parts=[_, ?JOBS, JobId, ?STATE]} = Req) ->
   case get_job(JobId) of
        {ok, {Props}} ->
             JobState = couch_util:get_value(job_state, Props),
             {InfoProps} = couch_util:get_value(state_info, Props, {[]}),
             send_json(Req, 200, {[{state, JobState}]});
        {error, not_found} ->
            throw(not_found)
    end;

% PUT /_shard_splits/jobs/$jobid/state

handle_shard_splits_req(#httpd{method='PUT',
        path_parts=[_, ?JOBS, JobId, ?STATE]} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body_obj(Req),
    State = couch_util:get_value(<<"state">>, Props),
    Reason = couch_util:get_value(<<"reason">>, Props),
    case {State, Reason} of
        {undefined, _} ->
            throw({bad_request, <<"Expected a `state` field">>});
        {?S_RUNNING, _} ->
            case resume_job(JobId) of
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
            case stop_job(JobId, Reason1) of
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

handle_shard_splits_req(#httpd{path_parts=[_, ?JOBS ,_, ?STATE ]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,PUT").


% Private helper functions

get_jobs_post_node({Props}) ->
    Node0 = case couch_util:get_value(<<"node">>, Props) of
        N0 when is_binary(N0) -> N0;
        true -> throw({bad_request, <<"invalid `node` field">>})
    end,
    try binary_to_existing_atom(Node0, utf8) of
        N1 ->
            case lists:member(N1, [node() | nodes()]) of
                true -> N1;
                false -> throw({bad_request, <<"Not connected to `node`">>})
            end
    catch
        error:badarg ->
            throw({bad_request, <<"`node` is not a valid node name">>})
   end.


get_jobs_post_shard({Props}) ->
    Shard0 = case couch_util:get_value(<<"shard">>, Props) of
        S when is_binary(S) -> S;
        true -> throw({bad_request, <<"invalid `shard` field">>})
    end,
    case Shard0 of
        <<"shards/", _:8/binary,"-", _:8/binary, "/", _/binary>> ->  Shard0;
        true ->  throw({bad_request, <<"`shard` is invalid">>})
    end.


get_jobs() ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_shard_split, jobs, []),
    lists:flatten(Replies).


get_job(JobId) ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_shard_split, job, [JobId]),
    case [JobInfo || {ok, JobInfo} <- Replies] of
        [JobInfo] ->
            {ok, JobInfo};
        [] ->
            {error, not_found}
    end.


resume_job(JobId) ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_shard_split, resume_job,
        [JobId]),
    WithoutNotFound = [R || R <- Replies, R =/= {error, not_found}],
    case lists:usort(WithoutNotFound) of
        [ok] ->
            ok;
        [{error, Error} | _] ->
            {error, {[{error, to_binary(Error)}]}};
        [] ->
            {error, not_found}
   end.


stop_job(JobId, Reason) ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_shard_split, stop_job,
        [JobId, Reason]),
    WithoutNotFound = [R || R <- Replies, R =/= {error, not_found}],
    case lists:usort(WithoutNotFound) of
        [ok] ->
            ok;
        [{error, Error} | _] ->
            {error, {[{error, to_binary(Error)}]}};
        [] ->
            {error, not_found}
    end.


get_summary() ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_shard_split, get_state, []),
    Stats0 = #{running => 0, total => 0, completed => 0, failed => 0,
        stopped => 0},
    {Stats, States} = lists:foldl(fun({Res}, {Stats, States}) ->
       Stats1 = maps:map(fun(Stat, OldVal) ->
           OldVal + couch_util:get_value(Stat, Res, 0)
       end, Stats),
       NewStates = [couch_util:get_value(state, Res) | States],
       {Stats1, NewStates}
    end, {Stats0, []}, Replies),
    State = case lists:member(?S_RUNNING, States) of
        true -> running;
        false -> stopped
    end,
    {[{state, State}] ++ lists:sort(maps:to_list(Stats))}.


get_shard_splitting_state() ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_shard_split, get_state, []),
    Running = lists:foldl(fun({Res}, R) ->
       case couch_util:get_value(state, Res) of
           ?S_RUNNING -> R + 1;
           _ -> R
       end
    end, 0, Replies),
    % If at least one node is "running", then overall state is "running"
    case Running > 0 of
        true -> running;
        false -> stopped
    end.


start_shard_splitting() ->
    {Replies, _Bad} = rpc:multicall(mem3_shard_split, start, []),
    case lists:usort(lists:flatten(Replies)) of
        [ok] ->
            {ok, {[{ok, true}]}};
        [Error | _] ->
            {error, {[{error, to_binary(Error)}]}}
    end.


stop_shard_splitting(Reason) ->
    {Replies, _Bad} = rpc:multicall(mem3_shard_split, stop, [Reason]),
    case lists:usort(lists:flatten(Replies)) of
        [ok] ->
            {ok, {[{ok, true}]}};
        [Error | _] ->
            {error, {[{error, to_binary(Error)}]}}
    end.
