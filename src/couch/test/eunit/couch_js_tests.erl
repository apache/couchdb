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

-module(couch_js_tests).
-include_lib("eunit/include/eunit.hrl").


-define(FUNC, <<
  "function(doc) {\n"
  "  var val = \"0123456789ABCDEF\";\n"
  "  while(true) {emit(val, val);}\n"
  "}\n"
>>).


couch_js_test_() ->
    {
        "Test couchjs",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            [
                fun should_recover_from_oom/0
            ]
        }
    }.


should_recover_from_oom() ->
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, ?FUNC]),
    ?assertThrow(
        {<<"oom">>, <<"out of memory">>},
        couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, <<"{}">>])
    ).
