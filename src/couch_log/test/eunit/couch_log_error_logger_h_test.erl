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

-module(couch_log_error_logger_h_test).


-include_lib("eunit/include/eunit.hrl").


-define(HANDLER, couch_log_error_logger_h).


couch_log_error_logger_h_test_() ->
    {setup,
        fun couch_log_test_util:start/0,
        fun couch_log_test_util:stop/1,
        [
            fun handler_ignores_unknown_messages/0,
            fun coverage_test/0
        ]
    }.


handler_ignores_unknown_messages() ->
    Handlers1 = gen_event:which_handlers(error_logger),
    ?assert(lists:member(?HANDLER, Handlers1)),
    ?assertEqual(ignored, gen_event:call(error_logger, ?HANDLER, foo)),

    error_logger ! this_is_a_message,
    Handlers2 = gen_event:which_handlers(error_logger),
    ?assert(lists:member(?HANDLER, Handlers2)).


coverage_test() ->
    Resp = couch_log_error_logger_h:code_change(foo, bazinga, baz),
    ?assertEqual({ok, bazinga}, Resp).
