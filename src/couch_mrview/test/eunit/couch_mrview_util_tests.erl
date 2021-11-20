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

-module(couch_mrview_util_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

couch_mrview_util_test_() ->
    [
        ?_assertEqual(0, validate_group_level(undefined, undefined)),
        ?_assertEqual(exact, validate_group_level(true, undefined)),
        ?_assertEqual(0, validate_group_level(false, undefined)),
        ?_assertEqual(1, validate_group_level(undefined, 1)),
        ?_assertEqual(0, validate_group_level(true, 0)),
        ?_assertEqual(0, validate_group_level(undefined, 0)),
        ?_assertEqual(1, validate_group_level(true, 1)),
        ?_assertEqual(0, validate_group_level(false, 0)),
        ?_assertThrow(
            {query_parse_error, <<"Can't specify group=false and group_level>0 at the same time">>},
            validate_group_level(false, 1)
        )
    ].

validate_group_level(Group, GroupLevel) ->
    Args0 = #mrargs{group = Group, group_level = GroupLevel, view_type = red},
    Args1 = couch_mrview_util:validate_args(Args0),
    Args1#mrargs.group_level.
