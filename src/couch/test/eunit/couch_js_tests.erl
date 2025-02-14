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
-include_lib("couch/include/couch_eunit.hrl").

couch_js_test_() ->
    {
        "Test couchjs",
        {
            setup,
            fun() ->
                test_util:start_couch(),
                meck:new(couch_log, [passthrough])
            end,
            fun(Ctx) ->
                meck:unload(),
                test_util:stop_couch(Ctx)
            end,
            with([
                ?TDEF(should_create_sandbox),
                ?TDEF(should_reset_properly),
                ?TDEF(should_freeze_doc_object),
                ?TDEF(should_emit_error_details),
                ?TDEF(should_log_error_details),
                ?TDEF(should_roundtrip_utf8),
                ?TDEF(should_roundtrip_modified_utf8),
                ?TDEF(should_replace_broken_utf16),
                ?TDEF(should_allow_js_string_mutations),
                ?TDEF(should_bump_timing_and_call_stats),
                ?TDEF(should_exit_on_internal_error, 60)
            ])
        }
    }.

%% erlfmt-ignore
should_create_sandbox(_) ->
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
    true = prompt(Proc, [<<"add_fun">>, Src]),
    Result = prompt(Proc, [<<"map_doc">>, {[]}]),
    ?assertMatch([[[true, <<_/binary>>]]], Result),
    [[[true, ErrMsg]]] = Result,
    ?assertNotEqual([], binary:matches(ErrMsg, <<"not defined">>)),
    ?assert(couch_stats:sample([couchdb, query_server, process_starts]) > 0),
    ?assert(couch_stats:sample([couchdb, query_server, process_prompts]) > 0),
    ?assert(couch_stats:sample([couchdb, query_server, acquired_processes]) > 0),
    ?assert(couch_stats:sample([couchdb, query_server, process_exits]) >= 0),
    ?assert(couch_stats:sample([couchdb, query_server, process_errors]) >= 0),
    ?assert(couch_stats:sample([couchdb, query_server, process_error_exits]) >= 0),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_reset_properly(_) ->
    Src = <<"
        function(doc) {
            var a = [0,1,2];
            emit(a.indexOf(0), Object.foo);
            Object.foo = 43;
            [].constructor.prototype.indexOf = function(x) {return 42;};
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = prompt(Proc, [<<"add_fun">>, Src]),
    Doc = {[]},
    Result1 = prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[0, null]]], Result1),
    Result2 = prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[42, 43]]], Result2),
    true = prompt(Proc, [<<"reset">>]),
    true = prompt(Proc, [<<"add_fun">>, Src]),
    Result3 = prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[0, null]]], Result3),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_freeze_doc_object(_) ->
    Src = <<"
        function(doc) {
            emit(doc.foo, doc.bar);
            doc.foo = 1042;
            doc.bar = 1043;
            emit(doc.foo, doc.bar);
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = prompt(Proc, [<<"add_fun">>, Src]),
    Doc = {[{<<"bar">>, 1041}]},
    Result1 = prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[null, 1041], [null, 1041]]], Result1),
    Result2 = prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[null, 1041], [null, 1041]]], Result2),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_emit_error_details(_) ->
    Src = <<"
        function(doc) {
            try {
               non_existent.fun_call
            } catch (e) {
               emit('err', e);
            }
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = prompt(Proc, [<<"add_fun">>, Src]),
    Result = prompt(Proc, [<<"map_doc">>,  {[{<<"foo">>, 42}]}]),
    ?assertMatch([[[<<"err">>, {[_ | _]}]]], Result),
    [[[<<"err">>, {ErrProps}]]] = Result,
    ?assertEqual(<<"ReferenceError">>, couch_util:get_value(<<"error">>, ErrProps)),
    ?assertMatch(<<_/binary>>, couch_util:get_value(<<"message">>, ErrProps)),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_log_error_details(_) ->
    Src = <<"
        function(doc) {
            try {
               non_existent.fun_call
            } catch (e) {
               log(e);
               emit('err', 1);
            }
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = prompt(Proc, [<<"add_fun">>, Src]),
    meck:reset(couch_log),
    % Don't check too specifically just that we emitted ReferenceError somewhere
    % in there, otherwise each engine has its own error message format
    meck:expect(couch_log, info, fun
        ("OS Process " ++ _, [_Port, <<Msg/binary>>]) ->
            ?assertNotEqual(nomatch, binary:match(Msg, [<<"ReferenceError">>])),
            ok;
        (_, _) ->
            ok
    end),
    Result = prompt(Proc, [<<"map_doc">>,  {[{<<"foo">>, 42}]}]),
    ?assertEqual([[[<<"err">>, 1]]], Result),
    ?assert(meck:num_calls(couch_log, info, 2) >= 1),
    meck:expect(couch_log, info, 2, meck:passthrough()),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_roundtrip_utf8(_) ->
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
    true = prompt(Proc, [<<"add_fun">>, Src]),
    Doc =
        {[
            {<<"value">>, <<16#C3, 16#84>>}
        ]},
    Result = prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[<<16#C3, 16#84>>, <<16#C3, 16#9C>>]]], Result),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_roundtrip_modified_utf8(_) ->
    % Mimicking the test case from the mailing list
    Src = <<"
        function(doc) {
          emit(doc.value.toLowerCase(), \"", 16#C3, 16#9C, "\");
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = prompt(Proc, [<<"add_fun">>, Src]),
    Doc =
        {[
            {<<"value">>, <<16#C3, 16#84>>}
        ]},
    Result = prompt(Proc, [<<"map_doc">>, Doc]),
    ?assertEqual([[[<<16#C3, 16#A4>>, <<16#C3, 16#9C>>]]], Result),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_replace_broken_utf16(_) ->
    % This test reverse the surrogate pair of
    % the Boom emoji U+1F4A5
    Src = <<"
        function(doc) {
            emit(doc.value.split(\"\").reverse().join(\"\"), 1);
        }
    ">>,
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    true = prompt(Proc, [<<"add_fun">>, Src]),
    Doc =
        {[
            {<<"value">>, list_to_binary(xmerl_ucs:to_utf8([16#1F4A5]))}
        ]},
    Result = prompt(Proc, [<<"map_doc">>, Doc]),
    % Invalid UTF-8 gets replaced with the 16#FFFD replacement
    % marker
    Markers = list_to_binary(xmerl_ucs:to_utf8([16#FFFD, 16#FFFD])),
    ?assertEqual([[[Markers, 1]]], Result),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_allow_js_string_mutations(_) ->
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
    true = prompt(Proc, [<<"add_fun">>, Src1]),
    true = prompt(Proc, [<<"add_fun">>, Src2]),
    true = prompt(Proc, [<<"add_fun">>, Src3]),
    Doc = {[{<<"value">>, MomWashedTheFrame}]},
    Result = prompt(Proc, [<<"map_doc">>, Doc]),
    Expect = [
        [[<<"length">>, 14]],
        [[<<"substring">>, Washed]],
        [[<<"slice">>, Mom]]
    ],
    ?assertEqual(Expect, Result),
    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_bump_timing_and_call_stats(_) ->
    Proc = couch_query_servers:get_os_process(<<"javascript">>),
    ?assert(sample_time(spawn_proc) > 0),

    ResetTime = sample_time(reset),
    ResetCalls = sample_calls(reset),
    true = prompt(Proc, [<<"reset">>]),
    ?assert(sample_time(reset) > ResetTime),
    ?assertEqual(ResetCalls + 1, sample_calls(reset)),

    AddFunTime = sample_time(add_fun),
    AddFunCalls = sample_calls(add_fun),
    true = prompt(Proc, [
        <<"add_fun">>,
        <<"function(doc) {emit(doc.x, doc.y);}">>
    ]),
    ?assert(sample_time(add_fun) > AddFunTime),
    ?assertEqual(AddFunCalls + 1, sample_calls(add_fun)),

    MapTime = sample_time(map),
    MapCalls = sample_calls(map),
    [[[1, 2]]] = prompt(Proc, [
        <<"map_doc">>,
       {[{<<"x">>, 1}, {<<"y">>, 2}]}
    ]),
    ?assert(sample_time(map) > MapTime),
    ?assertEqual(MapCalls + 1, sample_calls(map)),

    ReduceTime = sample_time(reduce),
    ReduceCalls = sample_calls(reduce),
    [true, [2]] = prompt(Proc, [
        <<"reduce">>,
        [<<"function(k, v) {return sum(v);}">>], [[1, 2]]
    ]),
    ?assert(sample_time(reduce)> ReduceTime),
    ?assertEqual(ReduceCalls + 1, sample_calls(reduce)),

    ReduceTime1 = sample_time(reduce),
    ReduceCalls1 = sample_calls(reduce),
    [true, [7]] = prompt(Proc, [
        <<"rereduce">>,
        [<<"function(k, v) {return sum(v);}">>], [3, 4]
    ]),
    ?assert(sample_time(reduce) > ReduceTime1),
    ?assertEqual(ReduceCalls1 + 1, sample_calls(reduce)),

    FilterFun = <<"function(doc, req) {return true;}">>,
    UpdateFun =  <<"function(doc, req) {return [null, 'something'];}">>,
    VduFun = <<"function(cur, old, ctx, sec) {return true;}">>,
    DDocId = <<"_design/ddoc1">>,
    DDoc = #{
        <<"_id">> => DDocId,
        <<"_rev">> => <<"1-a">>,
        <<"filters">> => #{<<"f1">> => FilterFun},
        <<"updates">> => #{<<"u1">> => UpdateFun},
        <<"validate_doc_update">> => VduFun
    },

    NewDDocTime = sample_time(ddoc_new),
    NewDDocCalls = sample_calls(ddoc_new),
    true = prompt(Proc, [
        <<"ddoc">>,
        <<"new">>,
        DDocId,
        DDoc
    ]),
    ?assert(sample_time(ddoc_new) > NewDDocTime),
    ?assertEqual(NewDDocCalls + 1, sample_calls(ddoc_new)),

    VduTime = sample_time(ddoc_vdu),
    VduCalls = sample_calls(ddoc_vdu),
    1 = prompt(Proc, [
        <<"ddoc">>,
        DDocId,
        [<<"validate_doc_update">>],
        [#{}, #{}]
    ]),
    ?assert(sample_time(ddoc_vdu) > VduTime),
    ?assertEqual(VduCalls + 1, sample_calls(ddoc_vdu)),

    FilterTime = sample_time(ddoc_filter),
    FilterCalls = sample_calls(ddoc_filter),
    [true, [true]] = prompt(Proc, [
        <<"ddoc">>,
        DDocId,
        [<<"filters">>, <<"f1">>],
        [[#{}], #{}]
    ]),
    ?assert(sample_time(ddoc_filter) > FilterTime),
    ?assertEqual(FilterCalls + 1, sample_calls(ddoc_filter)),

    DDocOtherTime = sample_time(ddoc_other),
    DDocOtherCalls = sample_calls(ddoc_other),
    prompt(Proc, [
        <<"ddoc">>,
        DDocId,
        [<<"updates">>, <<"u1">>],
        [null, #{}]
    ]),
    ?assert(sample_time(ddoc_other) > DDocOtherTime),
    ?assertEqual(DDocOtherCalls + 1, sample_calls(ddoc_other)),

    OtherTime = sample_time(other),
    OtherCalls = sample_calls(other),
    true = prompt(Proc, [
        <<"add_lib">>,
        #{<<"foo">> => <<"exports.bar = 42;">>}
    ]),
    ?assert(sample_time(other) > OtherTime),
    ?assertEqual(OtherCalls + 1, sample_calls(other)),

    couch_query_servers:ret_os_process(Proc).

%% erlfmt-ignore
should_exit_on_internal_error(_) ->
    % A different way to trigger OOM which previously used to
    % throw an InternalError on SM. Check that we still exit on that
    % type of error
    config:set("couchdb", "os_process_timeout", "15000", _Persist = false),
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
    true = prompt(Proc, [<<"reset">>]),
    true = prompt(Proc, [<<"add_fun">>, Src]),
    Doc = {[]},
    try
        prompt(Proc, [<<"map_doc">>, Doc])
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

sample_time(Stat) ->
    couch_stats:sample([couchdb, query_server, time, Stat]).

sample_calls(Stat) ->
    couch_stats:sample([couchdb, query_server, calls, Stat]).

prompt(Proc, Cmd) ->
    couch_query_servers:proc_prompt(Proc, Cmd).
