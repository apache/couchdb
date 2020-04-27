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


couch_js_test_() ->
    {
        "Test couchjs",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            [
                fun should_create_sandbox/0,
                fun should_roundtrip_utf8/0,
                fun should_roundtrip_modified_utf8/0,
                fun should_replace_broken_utf16/0,
                fun should_allow_js_string_mutations/0,
                {timeout, 60000, fun should_exit_on_oom/0}
            ]
        }
    }.


should_create_sandbox() ->
    % Try and detect whether we can see out of the
    % sandbox or not.
    Src = <<
      "function(doc) {\n"
      "  try {\n"
      "    emit(false, typeof(Couch.compile_function));\n"
      "  } catch (e) {\n"
      "    emit(true, e.message);\n"
      "  }\n"
      "}\n"
    >>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Result = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, <<"{}">>]),
    ?assertEqual([[[true, <<"Couch is not defined">>]]], Result).


should_roundtrip_utf8() ->
    % Try round tripping UTF-8 both directions through
    % couchjs. These tests use hex encoded values of
    % Ä (C384) and Ü (C39C) so as to avoid odd editor/Erlang encoding
    % strangeness.
    Src = <<
      "function(doc) {\n"
      "  emit(doc.value, \"", 16#C3, 16#9C, "\");\n"
      "}\n"
    >>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Doc = {[
        {<<"value">>, <<16#C3, 16#84>>}
    ]},
    Result = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[<<16#C3, 16#84>>, <<16#C3, 16#9C>>]]], Result).


should_roundtrip_modified_utf8() ->
    % Mimicing the test case from the mailing list
    Src = <<
      "function(doc) {\n"
      "  emit(doc.value.toLowerCase(), \"", 16#C3, 16#9C, "\");\n"
      "}\n"
    >>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Doc = {[
        {<<"value">>, <<16#C3, 16#84>>}
    ]},
    Result = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[<<16#C3, 16#A4>>, <<16#C3, 16#9C>>]]], Result).


should_replace_broken_utf16() ->
    % This test reverse the surrogate pair of
    % the Boom emoji U+1F4A5
    Src = <<
      "function(doc) {\n"
      "  emit(doc.value.split(\"\").reverse().join(\"\"), 1);\n"
      "}\n"
    >>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Doc = {[
        {<<"value">>, list_to_binary(xmerl_ucs:to_utf8([16#1F4A5]))}
    ]},
    Result = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    % Invalid UTF-8 gets replaced with the 16#FFFD replacement
    % marker
    Markers = list_to_binary(xmerl_ucs:to_utf8([16#FFFD, 16#FFFD])),
    ?assertEqual([[[Markers, 1]]], Result).


should_allow_js_string_mutations() ->
    % This binary corresponds to this string: мама мыла раму
    % Which I'm told translates to: "mom was washing the frame"
    MomWashedTheFrame = <<
        16#D0, 16#BC, 16#D0, 16#B0, 16#D0, 16#BC, 16#D0, 16#B0, 16#20,
        16#D0, 16#BC, 16#D1, 16#8B, 16#D0, 16#BB, 16#D0, 16#B0, 16#20,
        16#D1, 16#80, 16#D0, 16#B0, 16#D0, 16#BC, 16#D1, 16#83
    >>,
    Mom = <<16#D0, 16#BC, 16#D0, 16#B0, 16#D0, 16#BC, 16#D0, 16#B0>>,
    Washed = <<16#D0, 16#BC, 16#D1, 16#8B, 16#D0, 16#BB, 16#D0, 16#B0>>,
    Src1 = <<
      "function(doc) {\n"
      "  emit(\"length\", doc.value.length);\n"
      "}\n"
    >>,
    Src2 = <<
      "function(doc) {\n"
      "  emit(\"substring\", doc.value.substring(5, 9));\n"
      "}\n"
    >>,
    Src3 = <<
      "function(doc) {\n"
      "  emit(\"slice\", doc.value.slice(0, 4));\n"
      "}\n"
    >>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src1]),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src2]),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src3]),
    Doc = {[{<<"value">>, MomWashedTheFrame}]},
    Result = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    Expect = [
        [[<<"length">>, 14]],
        [[<<"substring">>, Washed]],
        [[<<"slice">>, Mom]]
    ],
    ?assertEqual(Expect, Result).


should_exit_on_oom() ->
    Src = <<
      "var state = [];\n"
      "function(doc) {\n"
      "  var val = \"0123456789ABCDEF\";\n"
      "  for(var i = 0; i < 165535; i++) {\n"
      "    state.push([val, val]);\n"
      "  }\n"
      "}\n"
    >>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    trigger_oom(Proc).

trigger_oom(Proc) ->
    Status = try
        couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, <<"{}">>]),
        continue
    catch throw:{os_process_error, {exit_status, 1}} ->
        done
    end,
    case Status of
        continue -> trigger_oom(Proc);
        done -> ok
    end.
