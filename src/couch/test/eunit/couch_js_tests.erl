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
                fun should_reset_properly/0,
                fun should_freeze_doc_object/0,
                fun should_roundtrip_utf8/0,
                fun should_roundtrip_modified_utf8/0,
                fun should_replace_broken_utf16/0,
                fun should_allow_js_string_mutations/0,
                {timeout, 60000, fun should_exit_on_oom/0},
                {timeout, 60000, fun should_exit_on_internal_error/0}
            ]
        }
    }.

%% erlfmt-ignore
should_create_sandbox() ->
    % Try and detect whether we can see out of the
    % sandbox or not.
    Src = <<"
        function(doc) {
            try {
                emit(false, typeof(Couch.compile_function));
            } catch (e) {
                emit(true, e.message);
            }
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Result = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, {[]}]),
    ?assertMatch([[[true, <<_/binary>>]]], Result),
    [[[true, ErrMsg]]] = Result,
    ?assertNotEqual([], binary:matches(ErrMsg, <<"not defined">>)),
    ?assert(couch_stats:sample([couchdb, query_server, process_starts]) > 0),
    ?assert(couch_stats:sample([couchdb, query_server, process_prompts]) > 0),
    ?assert(couch_stats:sample([couchdb, query_server, acquired_processes]) > 0),
    ?assert(couch_stats:sample([couchdb, query_server, process_exists]) >= 0),
    ?assert(couch_stats:sample([couchdb, query_server, process_errors]) >= 0),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_reset_properly() ->
    Src = <<"
        function(doc) {
            var a = [0,1,2];
            emit(a.indexOf(0), Object.foo);
            Object.foo = 43;
            [].constructor.prototype.indexOf = function(x) {return 42;};
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Doc = {[]},
    Result1 = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[0, null]]], Result1),
    Result2 = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[42, 43]]], Result2),
    true = couch_query_servers:proc_prompt(Proc, [<<"reset">>]),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Result3 = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[0, null]]], Result3),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_freeze_doc_object() ->
    Src = <<"
        function(doc) {
            emit(doc.foo, doc.bar);
            doc.foo = 1042;
            doc.bar = 1043;
            emit(doc.foo, doc.bar);
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Doc = {[{<<"bar">>, 1041}]},
    Result1 = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[null, 1041], [null, 1041]]], Result1),
    Result2 = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[null, 1041], [null, 1041]]], Result2),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_roundtrip_utf8() ->
    % Try round tripping UTF-8 both directions through
    % couchjs. These tests use hex encoded values of
    % Ä (C384) and Ü (C39C) so as to avoid odd editor/Erlang encoding
    % strangeness.
    Src = <<
        "function(doc) {
          emit(doc.value, \"", 16#C3,  16#9C, "\");
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Doc =
        {[
            {<<"value">>, <<16#C3, 16#84>>}
        ]},
    Result = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[<<16#C3, 16#84>>, <<16#C3, 16#9C>>]]], Result),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_roundtrip_modified_utf8() ->
    % Mimicking the test case from the mailing list
    Src = <<"
        function(doc) {
          emit(doc.value.toLowerCase(), \"", 16#C3, 16#9C, "\");
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Doc =
        {[
            {<<"value">>, <<16#C3, 16#84>>}
        ]},
    Result = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[<<16#C3, 16#A4>>, <<16#C3, 16#9C>>]]], Result),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_replace_broken_utf16() ->
    % This test reverse the surrogate pair of
    % the Boom emoji U+1F4A5
    Src = <<"
        function(doc) {
            emit(doc.value.split(\"\").reverse().join(\"\"), 1);
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Doc =
        {[
            {<<"value">>, list_to_binary(xmerl_ucs:to_utf8([16#1F4A5]))}
        ]},
    Result = couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc]),
    % Invalid UTF-8 gets replaced with the 16#FFFD replacement
    % marker
    Markers = list_to_binary(xmerl_ucs:to_utf8([16#FFFD, 16#FFFD])),
    ?assertEqual([[[Markers, 1]]], Result),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_allow_js_string_mutations() ->
    % This binary corresponds to this string: мама мыла раму
    % Which I'm told translates to: "mom was washing the frame"
    MomWashedTheFrame = <<
        16#D0,
        16#BC,
        16#D0,
        16#B0,
        16#D0,
        16#BC,
        16#D0,
        16#B0,
        16#20,
        16#D0,
        16#BC,
        16#D1,
        16#8B,
        16#D0,
        16#BB,
        16#D0,
        16#B0,
        16#20,
        16#D1,
        16#80,
        16#D0,
        16#B0,
        16#D0,
        16#BC,
        16#D1,
        16#83
    >>,
    Mom = <<16#D0, 16#BC, 16#D0, 16#B0, 16#D0, 16#BC, 16#D0, 16#B0>>,
    Washed = <<16#D0, 16#BC, 16#D1, 16#8B, 16#D0, 16#BB, 16#D0, 16#B0>>,
    Src1 = <<"
        function(doc) {
          emit(\"length\", doc.value.length);
        }
    ">>,
    Src2 = <<"
        function(doc) {
          emit(\"substring\", doc.value.substring(5, 9));
        }
    ">>,
    Src3 = <<"
        function(doc) {
          emit(\"slice\", doc.value.slice(0, 4));
        }
    ">>,
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
    ?assertEqual(Expect, Result),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_exit_on_oom() ->
    Src = <<"
        var state = [];
        function(doc) {
            var val = \"0123456789ABCDEF\";
            for(var i = 0; i < 665535; i++) {
                state.push([val, val]);
                emit(null, null);
             }
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    trigger_oom(Proc).

%% erlfmt-ignore
should_exit_on_internal_error() ->
    % A different way to trigger OOM which previously used to
    % throw an InternalError on SM. Check that we still exit on that
    % type of error
    Src = <<"
        function(doc) {
            function mkstr(l) {
                var r = 'r';
                while (r.length < l) {r = r + r;}
                return r;
            }
            var s = mkstr(96*1024*1024);
            var a = [s,s,s,s,s,s,s,s];
            var j = JSON.stringify(a);
            emit(42, j.length);}
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = couch_query_servers:proc_prompt(Proc, [<<"reset">>]),
    true = couch_query_servers:proc_prompt(Proc, [<<"add_fun">>, Src]),
    Doc = {[]},
    try
        couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, Doc])
    catch
        % Expect either an internal error thrown if it catches it and
        % emits an error log before dying
        throw:{<<"InternalError">>, _} ->
            ok;
        % gen_server may die before replying
        exit:{noproc, {gen_server,call, _}} ->
            ok;
        % or it may die with an epipe if it crashes while we send/recv data to it
        exit:{epipe, {gen_server, call, _}} ->
            ok;
        % It may fail and just exit the process. That's expected as well
        throw:{os_process_error, _} ->
            ok
    end,
    ?assert(couch_stats:sample([couchdb, query_server, process_errors]) > 0).

trigger_oom(Proc) ->
    Status =
        try
            couch_query_servers:proc_prompt(Proc, [<<"map_doc">>, <<"{}">>]),
            continue
        catch
            throw:{os_process_error, {exit_status, 1}} ->
                done
        end,
    case Status of
        continue -> trigger_oom(Proc);
        done -> ok
    end.
