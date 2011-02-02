#!/usr/bin/env escript
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

main(_) ->
    test_util:init_code_path(),
    couch_config:start_link(test_util:config_files()),
    etap:plan(3),
    etap:is(
        element(1, couch_drv:start_link()),
        ok,
        "Started couch_icu_driver."
    ),
    etap:is(
        couch_util:collate(<<"foo">>, <<"bar">>),
        1,
        "Can collate stuff"
    ),
    etap:is(
        couch_util:collate(<<"A">>, <<"aa">>),
        -1,
        "Collate's non-ascii style."
    ),
    etap:end_tests().
