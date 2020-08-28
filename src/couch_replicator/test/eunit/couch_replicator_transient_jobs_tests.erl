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

-module(couch_replicator_transient_jobs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


transient_jobs_test_() ->
    {
        "Transient jobs tests",
        {
            setup,
            fun couch_replicator_test_helper:start_couch/0,
            fun couch_replicator_test_helper:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(transient_job_is_removed, 10),
                    ?TDEF_FE(posting_same_job_is_a_noop, 10)
                ]
            }
        }
    }.


setup() ->
    Source = couch_replicator_test_helper:create_db(),
    couch_replicator_test_helper:create_docs(Source, [
        #{<<"_id">> => <<"doc1">>}
    ]),
    Target = couch_replicator_test_helper:create_db(),
    config:set("replicator", "stats_update_interval_sec", "0", false),
    config:set("replicator", "transient_job_max_age_sec", "9999", false),
    {Source, Target}.


teardown({Source, Target}) ->
    config:delete("replicator", "stats_update_interval_sec", false),
    config:delete("replicator", "transient_job_max_age_sec", false),
    couch_replicator_test_helper:delete_db(Source),
    couch_replicator_test_helper:delete_db(Target).


transient_job_is_removed({Source, Target}) ->
    {ok, #{}} = replicate(Source, Target),
    JobId = get_rep_id(Source, Target),

    couch_replicator_job_server:reschedule(),

    % Still there after clean up attempt ran
    ?assertMatch({200, #{}}, scheduler_jobs(JobId)),

    config:set("replicator", "transient_job_max_age_sec", "0", false),
    couch_replicator_job_server:reschedule(),

    % Should be gone now
    ?assertMatch({404, #{}}, scheduler_jobs(JobId)).


posting_same_job_is_a_noop({Source, Target}) ->
    {ok, Pid1, RepId1} = replicate_continuous(Source, Target),
    {ok, Pid2, RepId2} = replicate_continuous(Source, Target),
    ?assertEqual(RepId1, RepId2),
    ?assertEqual(Pid1, Pid2),
    couch_replicator_test_helper:cancel(RepId1).

   
get_rep_id(Source, Target) ->
    {ok, Id, _} = couch_replicator_parse:parse_transient_rep(#{
        <<"source">> => couch_replicator_test_helper:db_url(Source),
        <<"target">> => couch_replicator_test_helper:db_url(Target)
    }, null),
    Id.


replicate(Source, Target) ->
    couch_replicator:replicate(#{
        <<"source">> => couch_replicator_test_helper:db_url(Source),
        <<"target">> => couch_replicator_test_helper:db_url(Target)
    }, ?ADMIN_USER).


replicate_continuous(Source, Target) ->
    couch_replicator_test_helper:replicate_continuous(Source, Target).


scheduler_jobs(Id) ->
    SUrl = couch_replicator_test_helper:server_url(),
    Url = lists:flatten(io_lib:format("~s/_scheduler/jobs/~s", [SUrl, Id])),
    {ok, Code, _, Body} = test_request:get(Url, []),
    {Code, jiffy:decode(Body, [return_maps])}.
