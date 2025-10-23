% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(dreyfus_fabric_cleanup).

-export([go/1, go_local/3]).

go(DbName) ->
    case fabric_util:get_design_doc_records(DbName) of
        {ok, DDocs} when is_list(DDocs) ->
            Sigs = dreyfus_util:get_signatures_from_ddocs(DbName, DDocs),
            Shards = mem3:shards(DbName),
            ByNode = maps:groups_from_list(fun mem3:node/1, fun mem3:name/1, Shards),
            Fun = fun(Node, Dbs, Acc) ->
                erpc:send_request(Node, ?MODULE, go_local, [DbName, Dbs, Sigs], Node, Acc)
            end,
            Reqs = maps:fold(Fun, erpc:reqids_new(), ByNode),
            recv(DbName, Reqs, fabric_util:abs_request_timeout());
        Error ->
            couch_log:error("~p : error fetching ddocs db:~p ~p", [?MODULE, DbName, Error]),
            Error
    end.

% erpc endpoint for go/1 and fabric_index_cleanup:cleanup_indexes/2
%
go_local(DbName, Dbs, #{} = Sigs) ->
    try
        lists:foreach(
            fun(Db) ->
                Checkpoints = dreyfus_util:get_purge_checkpoints(Db),
                ok = couch_index_util:cleanup_purges(Db, Sigs, Checkpoints)
            end,
            Dbs
        ),
        clouseau_rpc:cleanup(DbName, Sigs),
        ok
    catch
        error:database_does_not_exist ->
            ok
    end.

recv(DbName, Reqs, Timeout) ->
    case erpc:receive_response(Reqs, Timeout, true) of
        {ok, _Lable, Reqs1} ->
            recv(DbName, Reqs1, Timeout);
        {Error, Label, Reqs1} ->
            ErrMsg = "~p : error cleaning dreyfus indexes db:~p req:~p error:~p",
            couch_log:error(ErrMsg, [?MODULE, DbName, Label, Error]),
            recv(DbName, Reqs1, Timeout);
        no_request ->
            ok
    end.
