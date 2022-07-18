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

-module(couch_log_monitor_test).

-include_lib("eunit/include/eunit.hrl").

-define(HANDLER, couch_log_error_logger_h).

couch_log_monitor_test_() ->
    {setup, fun couch_log_test_util:start/0, fun couch_log_test_util:stop/1, [
        fun monitor_ignores_unknown_messages/0,
        fun monitor_restarts_handler/0,
        fun coverage_test/0
    ]}.

monitor_ignores_unknown_messages() ->
    Pid1 = get_monitor_pid(),

    ?assertEqual(ignored, gen_server:call(Pid1, do_foo_please)),

    gen_server:cast(Pid1, do_bar_please),
    Pid1 ! do_baz_please,
    timer:sleep(250),
    ?assert(is_process_alive(Pid1)).

monitor_restarts_handler() ->
    Pid1 = get_monitor_pid(),
    error_logger:delete_report_handler(?HANDLER),
    timer:sleep(250),

    ?assert(not is_process_alive(Pid1)),

    Pid2 = get_monitor_pid(),
    ?assert(is_process_alive(Pid2)),

    Handlers = gen_event:which_handlers(error_logger),
    ?assert(lists:member(?HANDLER, Handlers)).

coverage_test() ->
    Resp = couch_log_monitor:code_change(foo, bazinga, baz),
    ?assertEqual({ok, bazinga}, Resp).

get_monitor_pid() ->
    Children = supervisor:which_children(couch_log_sup),
    [MonPid] = [Pid || {couch_log_monitor, Pid, _, _} <- Children, is_pid(Pid)],
    MonPid.
