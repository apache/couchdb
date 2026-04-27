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

-module(couch_replicator_dns_override_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("ibrowse/include/ibrowse.hrl").

dns_override_replication_test_() ->
    {
        "DNS override replication tests",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(should_replicate_with_dns_override)
            ]
        }
    }.

setup() ->
    couch_replicator_test_helper:test_setup().

teardown(Ctx) ->
    config:delete("replicator", "dns_overrides", false),
    couch_replicator_test_helper:test_teardown(Ctx).

should_replicate_with_dns_override({_Ctx, {Source, Target}}) ->
    create_doc(Source),
    
    SourceUrl = db_url(Source),
    #url{host = SourceHost} = ibrowse_lib:parse_url(binary_to_list(SourceUrl)),
    
    % configure DNS override: example.com -> actual source host
    OverrideConfig = "example.com:" ++ SourceHost,
    config:set("replicator", "dns_overrides", OverrideConfig, false),
    
    % replace source host with example.com
    OverrideUrl = re:replace(SourceUrl, SourceHost, "example.com", [{return, binary}]),
    
    % replicate using overridden URL
    replicate(OverrideUrl, db_url(Target)),
    
    % verify replication succeeded by comparing doc counts
    ?assertEqual(ok, compare(Source, Target)).

create_doc(DbName) ->
    Doc = couch_doc:from_json_obj({[{<<"_id">>, <<"test-doc">>}, {<<"value">>, 42}]}),
    {ok, _} = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

compare(Source, Target) ->
    couch_replicator_test_helper:cluster_compare_dbs(Source, Target).

replicate(SourceUrl, TargetUrl) ->
    couch_replicator_test_helper:replicate(SourceUrl, TargetUrl).
