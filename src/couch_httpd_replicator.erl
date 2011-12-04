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

-module(couch_httpd_replicator).

-include("couch_db.hrl").

-import(couch_httpd, [
    send_json/2,
    send_json/3,
    send_method_not_allowed/2
]).

-import(couch_util, [
    to_binary/1
]).

-export([handle_req/1]).


handle_req(#httpd{method = 'POST', user_ctx = UserCtx} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    RepDoc = {Props} = couch_httpd:json_body_obj(Req),
    validate_rep_props(Props),
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(RepDoc, UserCtx),
    case couch_replicator:replicate(Rep) of
    {error, {Error, Reason}} ->
        send_json(
            Req, 404,
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

validate_rep_props([]) ->
    ok;
validate_rep_props([{<<"query_params">>, {Params}}|Rest]) ->
    lists:foreach(fun
        ({_,V}) when is_binary(V) -> ok;
        ({K,_}) -> throw({bad_request,
            <<K/binary," value must be a string.">>})
        end, Params),
    validate_rep_props(Rest);
validate_rep_props([_|Rest]) ->
    validate_rep_props(Rest).
