#!/usr/bin/env escript
%% -*- erlang -*-

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

% Test that we can load each module.

main(_) ->
    test_util:init_code_path(),
    Modules = [
        couch_replicator_api_wrap,
        couch_replicator_httpc,
        couch_replicator_httpd,
        couch_replicator_manager,
        couch_replicator_notifier,
        couch_replicator,
        couch_replicator_worker,
        couch_replicator_utils,
        couch_replicator_job_sup
    ],

    etap:plan(length(Modules)),
    lists:foreach(
        fun(Module) ->
            etap:loaded_ok(Module, lists:concat(["Loaded: ", Module]))
        end, Modules),
    etap:end_tests().
