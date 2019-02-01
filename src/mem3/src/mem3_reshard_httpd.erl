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

-import(couch_util, [
    to_binary/1
]).


-include_lib("couch/include/couch_db.hrl").


-define(JOBS, <<"jobs">>).
-define(STATE, <<"state">>).
-define(S_RUNNING, <<"running">>).
-define(S_STOPPED, <<"stopped">>).

% GET /_reshard
%
handle_reshard_req(#httpd{method='GET', path_parts=[_]} = Req) ->
    State = get_summary(),
    send_json(Req, State);

handle_reshard_req(#httpd{path_parts=[_]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD");


% GET /_reshard/state
%
handle_reshard_req(#httpd{method='GET',
        path_parts=[_, ?STATE]} = Req) ->
    State = get_shard_splitting_state(),
    send_json(Req, {[{state, State}]});

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


handle_reshard_req(#httpd{path_parts=[_, ?STATE]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,PUT");


% GET /_reshard/jobs
%
handle_reshard_req(#httpd{method='GET', path_parts=[_,?JOBS]}=Req) ->
    Jobs = get_jobs(),
    Total = length(Jobs),
    send_json(Req, {[{total_rows, Total}, {offset, 0}, {jobs, Jobs}]});

% POST /_reshard/jobs {"node": "...", "shard": "..."}
%
handle_reshard_req(#httpd{method = 'POST',
        path_parts=[_, ?JOBS]} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body_obj(Req),
    Node = validate_node(couch_util:get_value(<<"node">>, Props)),
    Shard = validate_shard(couch_util:get_value(<<"shard">>, Props)),
    Db = validate_db(couch_util:get_value(<<"db">>, Props)),
    Range = validate_range(couch_util:get_value(<<"range">>, Props)),
    Type = validate_type(couch_util:get_value(<<"type">>, Props)),
    Res = create_jobs(Node, Shard, Db, Range, Type),
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
            {[{error, to_binary(E)}, {node, N}, {shard, S}]}
    end, Res),
    send_json(Req, Code, EJson);

handle_reshard_req(#httpd{path_parts=[_, ?JOBS]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,POST");


% GET /_reshard/jobs/$jobid
%
handle_reshard_req(#httpd{method='GET',
        path_parts=[_, ?JOBS, JobId]}=Req) ->
    case get_job(JobId) of
        {ok, JobInfo} ->
            send_json(Req, JobInfo);
        {error, not_found} ->
            throw(not_found)
    end;

% DELETE /_reshard/jobs/$jobid
%
handle_reshard_req(#httpd{method='DELETE',
        path_parts=[_, ?JOBS, JobId]}=Req) ->
    case get_job(JobId) of
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
   case get_job(JobId) of
        {ok, {Props}} ->
             JobState = couch_util:get_value(job_state, Props),
             send_json(Req, 200, {[{state, JobState}]});
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

handle_reshard_req(#httpd{path_parts=[_, ?JOBS ,_, ?STATE ]} = Req) ->
    send_method_not_allowed(Req, "GET,HEAD,PUT").


% Private helper functions


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
    end.


validate_shard(undefined) ->
    undefined;

validate_shard(Shard) when is_binary(Shard) ->
    case Shard of
        <<"shards/", _:8/binary,"-", _:8/binary, "/", _/binary>> ->
            Shard;
        _ ->
            throw({bad_request, <<"`shard` is invalid">>})
    end.


validate_db(undefined) ->
    undefined;

validate_db(DbName) when is_binary(DbName) ->
    try mem3:shards(DbName) of
        [_ | _] -> DbName;
        _ ->  throw({bad_request, <<"`No shards in `db`">>})
    catch
        _:_ ->
            throw({bad_request, <<"Invalid `db`">>})
    end.


validate_range(undefined) ->
    undefined;

validate_range(<<BBin:8/binary,"-", EBin:8/binary>>) ->
    {B, E} = try
        {
            httpd_util:hexlist_to_integer(binary_to_list(BBin)),
            httpd_util:hexlist_to_integer(binary_to_list(EBin))
        }
    catch
        _:_ ->
            throw({bad_request, <<"Invalid `range`">>})
    end,
    if
        B < 0 -> throw({bad_request, <<"Invalid `range`">>});
        E < 0 -> throw({bad_request, <<"Invalid `range`">>});
        B > (2 bsl 31) - 1 -> throw({bad_request, <<"Invalid `range`">>});
        E > (2 bsl 31) - 1 -> throw({bad_request, <<"invalid `range`">>});
        B >= E -> throw({bad_request, <<"Invalid `range`">>});
        true -> ok
    end,
    % Use a list format here to make it look the same as #shard's range
    [B, E];

validate_range(_Range) ->
     throw({bad_request, <<"Invalid `range`">>}).


create_jobs(Node, Shard, Db, Range, split) ->
    lists:map(fun(S) ->
        N = mem3:node(S),
        Name = mem3:name(S),
        case rpc:call(N, mem3_reshard, start_split_job, [Name]) of
            {badrpc, Error} ->
                {error, Error, N, Name};
            {ok, JobId} ->
                {ok, JobId, N, Name};
            {error, Error} ->
                {error, Error, N, Name}
        end
    end,  pick_shards(Node, Shard, Db, Range)).


pick_shards(undefined, undefined, Db, undefined) ->
    mem3:shards(Db);

pick_shards(Node, undefined, Db, undefined) when is_atom(Node) ->
    [S || S <- mem3:shards(Db), mem3:node(S) == Node];

pick_shards(undefined, undefined, Db, [_B, _E] = Range) ->
    [S || S <- mem3:shards(Db), mem3:range(S) == Range];

pick_shards(Node, undefined, Db, [_B, _E] = Range) when is_atom(Node) ->
    [S || S <- mem3:shards(Db), mem3:node(S) == Node, mem3:range(S) == Range];

pick_shards(undefined, Shard, undefined, undefined) when is_binary(Shard) ->
    Db = mem3:dbname(Shard),
    [S || S <- mem3:shards(Db), mem3:name(S) == Shard];

pick_shards(Node, Shard, undefined, undefined) when is_atom(Node),
        is_binary(Shard) ->
    Db = mem3:dbname(Shard),
    [S || S <- mem3:shards(Db), mem3:name(S) == Shard, mem3:node(S) == Node];

pick_shards(_, undefined, undefined, _) ->
    throw({bad_request, <<"Must specify at least `db` or `shard`">>});

pick_shards(_, Db, Shard, _) when is_binary(Db), is_binary(Shard) ->
    throw({bad_request, <<"`db` and `shard` are mutually exclusive">>}).


get_jobs() ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_reshard, jobs, []),
    lists:flatten(Replies).


get_job(JobId) ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_reshard, job, [JobId]),
    case [JobInfo || {ok, JobInfo} <- Replies] of
        [JobInfo] ->
            {ok, JobInfo};
        [] ->
            {error, not_found}
    end.


resume_job(JobId) ->
    Nodes = mem3_util:live_nodes(),
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_reshard, resume_job,
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
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_reshard, stop_job,
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
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_reshard, get_state, []),
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
    {Replies, _Bad} = rpc:multicall(Nodes, mem3_reshard, get_state, []),
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
    {Replies, _Bad} = rpc:multicall(mem3_reshard, start, []),
    case lists:usort(lists:flatten(Replies)) of
        [ok] ->
            {ok, {[{ok, true}]}};
        [Error | _] ->
            {error, {[{error, to_binary(Error)}]}}
    end.


stop_shard_splitting(Reason) ->
    {Replies, _Bad} = rpc:multicall(mem3_reshard, stop, [Reason]),
    case lists:usort(lists:flatten(Replies)) of
        [ok] ->
            {ok, {[{ok, true}]}};
        [Error | _] ->
            {error, {[{error, to_binary(Error)}]}}
    end.
