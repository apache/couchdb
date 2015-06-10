% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_epi_data_source_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(DATA_FILE1, ?ABS_PATH("test/fixtures/app_data1.cfg")).
-define(DATA_FILE2, ?ABS_PATH("test/fixtures/app_data2.cfg")).

-record(ctx, {file, handle, pid}).

setup() ->
    Key = {test_app, descriptions},
    File = ?tempfile(),
    {ok, _} = file:copy(?DATA_FILE1, File),
    {ok, Pid} = couch_epi_data_source:start_link(
        test_app, {epi_key, Key}, {file, File}, [{interval, 100}]),
    ok = couch_epi_data_source:wait(Pid),
    #ctx{
        pid = Pid,
        file = File,
        handle = couch_epi_data_gen:get_handle(Key)}.


teardown(#ctx{pid = Pid, file = File}) ->
    file:delete(File),
    couch_epi_data_source:stop(Pid),
    catch meck:unload(compile),
    ok.


epi_data_source_reload_test_() ->
    {
        "data_source reload tests",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun ensure_reload_if_manually_triggered/1,
                fun ensure_reload_if_changed/1,
                fun ensure_no_reload_when_no_change/1
            ]
        }
    }.

ensure_reload_if_manually_triggered(#ctx{pid = Pid, file = File}) ->
    ?_test(begin
        ok = meck:new(compile, [passthrough, unstick]),
        ok = meck:expect(compile, forms, fun(_, _) -> {error, reload} end),
        {ok, _} = file:copy(?DATA_FILE2, File),
        Result = couch_epi_data_source:reload(Pid),
        ?assertMatch({error,{badmatch,{error,reload}}}, Result)
    end).

ensure_reload_if_changed(#ctx{file = File, handle = Handle}) ->
    ?_test(begin
        ?assertMatch(
            [[{type,counter},{desc,foo}]],
            couch_epi_data_gen:get(Handle, [complex, key, 1])),
        {ok, _} = file:copy(?DATA_FILE2, File),
        timer:sleep(150),
        ?assertMatch(
            [[{type,counter},{desc,bar}]],
            couch_epi_data_gen:get(Handle, [complex, key, 2]))
    end).

ensure_no_reload_when_no_change(#ctx{handle = Handle}) ->
    ok = meck:new(compile, [passthrough, unstick]),
    ok = meck:expect(compile, forms, fun(_, _) ->
        {error, compile_should_not_be_called} end),
    ?_test(begin
        ?assertMatch(
            [[{type,counter},{desc,foo}]],
            couch_epi_data_gen:get(Handle, [complex, key, 1])),
        timer:sleep(200),
        ?assertMatch(
            [],
            couch_epi_data_gen:get(Handle, [complex, key, 2]))
    end).
