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

-module(fabric_index_cleanup).

-export([
    cleanup_all_nodes/0,
    cleanup_all_nodes/1,
    cleanup_this_node/0,
    cleanup_this_node/1
]).

cleanup_all_nodes() ->
    Fun = fun(DbName, _) -> cleanup_all_nodes(DbName) end,
    mem3:fold_dbs(Fun, nil),
    ok.

cleanup_all_nodes(DbName) ->
    cleanup_indexes(DbName, mem3_util:live_nodes()).

cleanup_this_node() ->
    Fun = fun(DbName, _) ->
        case mem3:local_shards(DbName) of
            [_ | _] -> cleanup_this_node(DbName);
            [] -> ok
        end
    end,
    mem3:fold_dbs(Fun, nil),
    ok.

cleanup_this_node(DbName) ->
    cleanup_indexes(DbName, [config:node_name()]).

cleanup_indexes(DbName, Nodes) ->
    try fabric_util:get_design_doc_records(DbName) of
        {ok, DDocs} when is_list(DDocs) ->
            VSigs = couch_mrview_util:get_signatures_from_ddocs(DbName, DDocs),
            DSigs = dreyfus_util:get_signatures_from_ddocs(DbName, DDocs),
            NSigs = nouveau_util:get_signatures_from_ddocs(DbName, DDocs),
            Shards = [S || S <- mem3:shards(DbName), lists:member(mem3:node(S), Nodes)],
            ByNode = maps:groups_from_list(fun mem3:node/1, fun mem3:name/1, Shards),
            Fun = fun(Node, Dbs, Acc) ->
                Acc1 = send(Node, couch_mrview_cleanup, cleanup, [Dbs, VSigs], Acc),
                Acc2 = send(Node, dreyfus_fabric_cleanup, go_local, [DbName, Dbs, DSigs], Acc1),
                Acc3 = send(Node, nouveau_fabric_cleanup, go_local, [DbName, Dbs, NSigs], Acc2),
                Acc3
            end,
            Reqs = maps:fold(Fun, erpc:reqids_new(), ByNode),
            recv(DbName, Reqs, fabric_util:abs_request_timeout());
        Error ->
            couch_log:error("~p : error fetching ddocs db:~p ~p", [?MODULE, DbName, Error]),
            Error
    catch
        error:database_does_not_exist ->
            ok
    end.

send(Node, M, F, A, Reqs) ->
    Label = {Node, M, F},
    erpc:send_request(Node, M, F, A, Label, Reqs).

recv(DbName, Reqs, Timeout) ->
    case erpc:receive_response(Reqs, Timeout, true) of
        {ok, _Label, Reqs1} ->
            recv(DbName, Reqs1, Timeout);
        {Error, Label, Reqs1} ->
            ErrMsg = "~p : error cleaning indexes db:~p req:~p error:~p",
            couch_log:error(ErrMsg, [?MODULE, DbName, Label, Error]),
            recv(DbName, Reqs1, Timeout);
        no_request ->
            ok
    end.
