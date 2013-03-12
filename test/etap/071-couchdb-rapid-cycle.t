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

main(_) ->
    test_util:init_code_path(),

    etap:plan(2),
    try
        ok = test()
    catch T:R ->
        Stack = erlang:get_stacktrace(),
        etap:diag("Test died abnormally: ~p~n    ~p", [{T, R}, Stack]),
        timer:sleep(250),
        etap:bail(io_lib:format("Error: ~p", [{T, R}]))
    end,
    ok.

dbname() -> <<"etap-test-db">>.

test() ->
    ok = test_util:start_couch(),
    timer:sleep(500),

    couch_server:delete(dbname(), []),
    etap:diag("Test starting"),
    timer:sleep(250),

    Pid = spawn_link(fun() -> open_loop() end),

    Result = lists:foldl(fun(_, N) ->
        {ok, Db} = couch_db:create(dbname(), []),
        ok = couch_db:close(Db),
        ok = couch_server:delete(dbname(), []),
        N + 1
    end, 0, lists:seq(1, 100)),

    etap:is(Result, 100, "Cycled the database 1000 times successfully."),
    etap:is(is_process_alive(Pid), true, "The open loop lives"),

    Pid ! {self(), close},
    receive
        {Pid, ok} -> ok
    after 1000 ->
        open_loop_didnt_die
    end.

open_loop() ->
    receive
        {Parent, close} ->
            Parent ! {self(), ok}
        after 0 ->
            case couch_db:open_int(dbname(), []) of
                {ok, Db} ->
                    ok = couch_db:close(Db);
                {not_found, no_db_file} ->
                    ok;
                Other ->
                    etap:diag("WHIBBLE? ~p", [Other])
            end,
            open_loop()
    end.

