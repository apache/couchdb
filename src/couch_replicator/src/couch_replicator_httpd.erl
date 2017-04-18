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

-module(couch_replicator_httpd).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([
    handle_req/1,
    handle_scheduler_req/1
]).

-import(couch_httpd, [
    send_json/2,
    send_json/3,
    send_method_not_allowed/2
]).

-import(couch_util, [
    to_binary/1
]).


-define(DEFAULT_TASK_LIMIT, 100).
-define(REPDB, <<"_replicator">>).


handle_scheduler_req(#httpd{method='GET', path_parts=[_,<<"jobs">>]}=Req) ->
    Limit = couch_replicator_httpd_util:parse_int_param(Req, "limit",
        ?DEFAULT_TASK_LIMIT, 0, infinity),
    Skip = couch_replicator_httpd_util:parse_int_param(Req, "skip", 0, 0,
        infinity),
    {Replies, _BadNodes} = rpc:multicall(couch_replicator_scheduler, jobs, []),
    Flatlist = lists:concat(Replies),
    % couch_replicator_scheduler:job_ejson/1 guarantees {id, Id} to be the
    % the first item in the list
    Sorted = lists:sort(fun({[{id,A}|_]},{[{id,B}|_]}) -> A =< B end, Flatlist),
    Total = length(Sorted),
    Offset = min(Skip, Total),
    Sublist = lists:sublist(Sorted, Offset+1, Limit),
    Sublist1 = [couch_replicator_httpd_util:update_db_name(Task)
        || Task <- Sublist],
    send_json(Req, {[{total_rows, Total}, {offset, Offset}, {jobs, Sublist1}]});
handle_scheduler_req(#httpd{method='GET', path_parts=[_,<<"jobs">>,JobId]}=Req) ->
    case couch_replicator:job(JobId) of
        {ok, JobInfo} ->
            send_json(Req, couch_replicator_httpd_util:update_db_name(JobInfo));
        {error, not_found} ->
            throw(not_found)
    end;
handle_scheduler_req(#httpd{method='GET', path_parts=[_,<<"docs">>]}=Req) ->
    VArgs0 = couch_mrview_http:parse_params(Req, undefined),
    StatesQs = chttpd:qs_value(Req, "states"),
    States = couch_replicator_httpd_util:parse_replication_state_filter(StatesQs),
    VArgs1 = VArgs0#mrargs{
        view_type = map,
        include_docs = true,
        reduce = false,
        extra = [{filter_states, States}]
    },
    VArgs2 = couch_mrview_util:validate_args(VArgs1),
    Opts = [{user_ctx, Req#httpd.user_ctx}],
    Db = ?REPDB,
    Max = chttpd:chunked_response_buffer_size(),
    Acc = couch_replicator_httpd_util:docs_acc_new(Req, Db, Max),
    Cb = fun couch_replicator_httpd_util:docs_cb/2,
    {ok, RAcc} = couch_replicator_fabric:docs(Db, Opts, VArgs2, Cb, Acc),
    {ok,  couch_replicator_httpd_util:docs_acc_response(RAcc)};
handle_scheduler_req(#httpd{method='GET', path_parts=[_,<<"docs">>,DocId]}=Req) ->
    UserCtx = Req#httpd.user_ctx,
    case couch_replicator:doc(?REPDB, DocId, UserCtx#user_ctx.roles) of
        {ok, DocInfo} ->
            send_json(Req, couch_replicator_httpd_util:update_db_name(DocInfo));
        {error, not_found} ->
            throw(not_found)
    end;
handle_scheduler_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").


handle_req(#httpd{method = 'POST', user_ctx = UserCtx} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    RepDoc = {Props} = couch_httpd:json_body_obj(Req),
    couch_replicator_httpd_utils:validate_rep_props(Props),
    case couch_replicator:replicate(RepDoc, UserCtx) of
    {error, {Error, Reason}} ->
        send_json(
            Req, 500,
            {[{error, to_binary(Error)}, {reason, to_binary(Reason)}]});
    {error, not_found} ->
        % Tried to cancel a replication that didn't exist.
        send_json(Req, 404, {[{error, <<"not found">>}]});
    {error, Reason} ->
        send_json(Req, 500, {[{error, to_binary(Reason)}]});
    {ok, {cancelled, RepId}} ->
        send_json(Req, 200, {[{ok, true}, {<<"_local_id">>, RepId}]});
    {ok, {continuous, RepId}} ->
        send_json(Req, 202, {[{ok, true}, {<<"_local_id">>, RepId}]});
    {ok, {HistoryResults}} ->
        send_json(Req, {[{ok, true} | HistoryResults]})
    end;

handle_req(Req) ->
    send_method_not_allowed(Req, "POST").
