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

-module(couch_log_trunc_io_fmt_test).


-include_lib("eunit/include/eunit.hrl").


format_test_() ->
    lists:map(fun({Fmt, Args, Expect}) ->
        Name = io_lib:format("~p", [Expect]),
        {lists:flatten(Name),
            ?_assertEqual(
                Expect,
                lists:flatten(couch_log_trunc_io_fmt:format(Fmt, Args, 1024))
            )
        }
    end, cases()).


cases() ->
    [
        {"", [], ""},
        {"~w", [foo], "foo"},
        {"~p", [bar], "bar"},
        {"~W", [{{{2}}}, 2], "{{...}}"},
        {"~P", [{{{ohai}}}, 1], "{...}"},
        {"~s", [[$s, [$t, [$u, [$f, [$f]]]]]], "stuff"},
        {"~r", [{reason, [{x, k, [c, d]}]}], "reason at x:k/2"},
        {"~e", [1.0], "1.00000e+0"},
        {"~f", [1.0], "1.000000"},
        {"~g", [1.0], "1.00000"},
        {"~b", [15], "15"},
        {"~B", [15], "15"},
        {"~.16b", [15], "f"},
        {"~.16B", [15], "F"},
        {"~.16x", [15, "16#"], "16#f"},
        {"~.16x", [15, '16#'], "16#f"},
        {"~.16X", [15, "16#"], "16#F"},
        {"~.16X", [15, '16#'], "16#F"},
        {"~.16#", [15], "16#F"},
        {"~.16+", [15], "16#f"},
        {"~c", [$z], "z"},
        {"~tc", [$g], "g"},
        {"~~", [], "\~"},
        {"~n", [], "\n"},
        {"~i", [ignored], ""},
        {"~2.w", [1], " 1"},
        {"~*w", [2, 1], " 1"},
        {"~-2.w", [1], "1 "},
        {"~2.0. w", [1], "  "},
        {"~2.1. w", [1], " 1"},
        {"~2.0.|w", [1], "||"},
        {"~2.1.|w", [1], "|1"},
        {"~2.1.*w", [$q, 1], "q1"}
    ].
