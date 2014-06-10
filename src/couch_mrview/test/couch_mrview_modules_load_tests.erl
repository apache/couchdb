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

-module(couch_mrview_modules_load_tests).

-include("couch_eunit.hrl").


modules_load_test_() ->
    {
        "Verify that all modules loads",
        should_load_modules()
    }.


should_load_modules() ->
    Modules = [
        couch_mrview,
        couch_mrview_compactor,
        couch_mrview_http,
        couch_mrview_index,
        couch_mrview_updater,
        couch_mrview_util
    ],
    [should_load_module(Mod) || Mod <- Modules].

should_load_module(Mod) ->
    {atom_to_list(Mod), ?_assertMatch({module, _}, code:load_file(Mod))}.
