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

-module(couch_log_writer_stderr_test).


-include_lib("couch_log/include/couch_log.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(WRITER, couch_log_writer_stderr).


couch_log_writer_stderr_test_() ->
    {setup,
        fun couch_log_test_util:start/0,
        fun couch_log_test_util:stop/1,
        [
            fun check_init_terminate/0,
            fun() ->
                couch_log_test_util:with_meck(
                    [{io, [unstick]}],
                    fun check_write/0
                )
            end
        ]
    }.


check_init_terminate() ->
    {ok, St} = ?WRITER:init(),
    ok = ?WRITER:terminate(stop, St).


check_write() ->
    meck:expect(io, format, 3, ok),

    Entry = #log_entry{
        level = debug,
        pid = list_to_pid("<0.1.0>"),
        msg = "stuff",
        msg_id = "msg_id",
        time_stamp = "time_stamp"
    },
    {ok, St} = ?WRITER:init(),
    {ok, NewSt} = ?WRITER:write(Entry, St),
    ok = ?WRITER:terminate(stop, NewSt),

    ?assert(meck:validate(io)).
