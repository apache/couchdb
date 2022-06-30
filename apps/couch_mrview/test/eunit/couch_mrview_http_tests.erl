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

-module(couch_mrview_http_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

mrview_http_test_() ->
    [
        ?_assertEqual(
            #mrargs{group_level = undefined, group = true},
            couch_mrview_http:parse_params(
                [{"group", "true"}],
                undefined,
                #mrargs{}
            )
        ),

        ?_assertEqual(
            #mrargs{group_level = 1, group = undefined},
            couch_mrview_http:parse_params(
                [{"group_level", "1"}],
                undefined,
                #mrargs{}
            )
        )
    ].
