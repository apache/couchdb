#!/usr/bin/env escript

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

    etap:plan(3),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag("Test died abnormally: ~p", [Other]),
            etap:bail("Bad return value.")
    end,
    ok.


test() ->
    test_single_line_parse(), 
    test_multiple_line_parsing(),
    test_incomplete_lines_ignored(),
    ok.


test_single_line_parse() ->
    Chunk = <<"[Mon, 12 Nov 2012 15:24:19 GMT] [info] [<0.31.0>] Apache CouchDB has started on http://127.0.0.1:5984/\n">>,
    Expected = [{[{date,<<"Mon, 12 Nov 2012 15:24:19 GMT">>},
               {loglevel,<<"info">>},
               {pid,<<"<0.31.0>">>},
               {text,<<"Apache CouchDB has started on http://127.0.0.1:5984/">>}]}],

    Output = couch_log:parse_to_json(Chunk),
    
    etap:is(Output, Expected, "Parse one line correctly").

test_multiple_line_parsing() ->
    % Erlang doesn't suppot a nice multi-line string so have to do it in one line.
    Chunk = <<"[Tue, 13 Nov 2012 12:09:33 GMT] [info] [<0.129.0>] 127.0.0.1 - - GET /_utils/document.html?surge/24b601d1c0bea3080662ff8506024361 200\n[Tue, 13 Nov 2012 12:09:33 GMT] [info] [<0.136.0>] 127.0.0.1 - - GET / 200\n[Tue, 13 Nov 2012 12:09:33 GMT] [info] [<0.131.0>] 127.0.0.1 - - GET /_session 200">>,
    Expected = [{[{date,<<"Tue, 13 Nov 2012 12:09:33 GMT">>},
                    {loglevel,<<"info">>},
                    {pid,<<"<0.129.0>">>},
                    {text,<<"127.0.0.1 - - GET /_utils/document.html?surge/24b601d1c0bea3080662ff8506024361 200">>}]},

                  {[{date,<<"Tue, 13 Nov 2012 12:09:33 GMT">>},
                    {loglevel,<<"info">>},
                    {pid,<<"<0.136.0>">>},
                    {text,<<"127.0.0.1 - - GET / 200">>}]},

                  {[{date,<<"Tue, 13 Nov 2012 12:09:33 GMT">>},
                    {loglevel,<<"info">>},
                    {pid,<<"<0.131.0>">>},
                    {text,<<"127.0.0.1 - - GET /_session 200">>}]}],

    Output = couch_log:parse_to_json(Chunk),
    etap:is(Output, Expected, "Parse multiple lines correctly").

test_incomplete_lines_ignored() ->
    Chunk = <<"12:09:33 GMT] [info] [<0.129.0>] 127.0.0.1 - - GET /_utils/document.html\n[Tue, 13 Nov 2012 12:09:33 GMT] [info] [<0.129.0>] 127.0.0.1 - - GET /_utils/document.html?surge/24b601d1c0bea3080662ff8506024361 200\n[Tue, 13 Nov 2012 12:09:33 GMT] [info] [<0.136.0>] 127.0.0.1 - - GET / 200\n[Tue, 13 Nov 2012 12:09:33 GMT] [info] [<0.131.0>] 127.0.0.1 - - GET /_session 200">>,
    Expected = [{[{date,<<"Tue, 13 Nov 2012 12:09:33 GMT">>},
                    {loglevel,<<"info">>},
                    {pid,<<"<0.129.0>">>},
                    {text,<<"127.0.0.1 - - GET /_utils/document.html?surge/24b601d1c0bea3080662ff8506024361 200">>}]},

                  {[{date,<<"Tue, 13 Nov 2012 12:09:33 GMT">>},
                    {loglevel,<<"info">>},
                    {pid,<<"<0.136.0>">>},
                    {text,<<"127.0.0.1 - - GET / 200">>}]},

                  {[{date,<<"Tue, 13 Nov 2012 12:09:33 GMT">>},
                    {loglevel,<<"info">>},
                    {pid,<<"<0.131.0>">>},
                    {text,<<"127.0.0.1 - - GET /_session 200">>}]}],

    Output = couch_log:parse_to_json(Chunk),
    etap:is(Output, Expected, "Ingores incomplete lines").
