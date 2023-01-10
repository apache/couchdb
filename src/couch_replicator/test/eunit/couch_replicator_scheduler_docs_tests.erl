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

-module(couch_replicator_scheduler_docs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

scheduler_docs_test_() ->
    {
        foreach,
        fun() ->
            Ctx = couch_replicator_test_helper:test_setup(),
            ok = config:set("replicator", "cluster_start_period", "0", false),
            Opts = [{q, 1}, {n, 1}, ?ADMIN_CTX],
            case fabric:create_db(<<"_replicator">>, Opts) of
                ok -> ok;
                {error, file_exists} -> ok
            end,
            Ctx
        end,
        fun(Ctx) ->
            ok = config:delete("replicator", "cluster_start_period"),
            ok = fabric:delete_db(<<"_replicator">>, [?ADMIN_CTX]),
            couch_replicator_test_helper:test_teardown(Ctx)
        end,
        [
            ?TDEF_FE(t_scheduler_docs_total_rows, 10)
        ]
    }.

t_scheduler_docs_total_rows({_Ctx, {Source, Target}}) ->
    SourceUrl = couch_replicator_test_helper:cluster_db_url(Source),
    TargetUrl = couch_replicator_test_helper:cluster_db_url(Target),
    RepDoc = jiffy:encode(
        {[
            {<<"source">>, SourceUrl},
            {<<"target">>, TargetUrl}
        ]}
    ),
    RepDocUrl = couch_replicator_test_helper:cluster_db_url(
        list_to_binary("/_replicator/" ++ ?docid())
    ),
    {ok, 201, _, _} = test_request:put(binary_to_list(RepDocUrl), [], RepDoc),
    SchedulerDocsUrl =
        couch_replicator_test_helper:cluster_db_url(<<"/_scheduler/docs">>),
    Body = test_util:wait(
        fun() ->
            case test_request:get(binary_to_list(SchedulerDocsUrl), []) of
                {ok, 200, _, JsonBody} ->
                    Decoded = jiffy:decode(JsonBody, [return_maps]),
                    case maps:get(<<"docs">>, Decoded) of
                        [] ->
                            wait;
                        _ ->
                            Decoded
                    end;
                _ ->
                    wait
            end
        end,
        10000,
        1000
    ),
    Docs = maps:get(<<"docs">>, Body),
    TotalRows = maps:get(<<"total_rows">>, Body),
    ?assertEqual(TotalRows, length(Docs)),
    ok.
