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

    etap:plan(5),
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
    test_match_multiline_stack_trace_log_line(),
    test_full_couchdb_test_file(),
    ok.


test_single_line_parse() ->
    Chunk = <<"[Mon, 12 Nov 2012 15:24:19 GMT] [info] [<0.31.0>] Apache CouchDB has started on http://127.0.0.1:5984/">>,
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
    Chunk = <<"  nfo] [<0.129.0>] 127.0.0.1 - - GET /_utils/document.html\n[Tue, 13 Nov 2012 12:09:33 GMT] [info] [<0.129.0>] 127.0.0.1 - - GET /_utils/document.html?surge/24b601d1c0bea3080662ff8506024361 200\n[Tue, 13 Nov 2012 12:09:33 GMT] [info] [<0.136.0>] 127.0.0.1 - - GET / 200\n[Tue, 13 Nov 2012 12:09:33 GMT] [info] [<0.131.0>] 127.0.0.1 - - GET /_session 200">>,
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

test_match_multiline_stack_trace_log_line() ->
    Chunk = <<"[Tue, 13 Nov 2012 14:23:02 GMT] [debug] [<0.138.0>] Stacktrace: [{couch_httpd_db,create_db_req,2,\n[{file,\"couch_httpd_db.erl\"},{line,214}]},\n{couch_httpd,handle_request_int,5,\n[{file,\"couch_httpd.erl\"},{line,317}]},\n{mochiweb_http,headers,5,\n[{file,\"mochiweb_http.erl\"},{line,136}]},\n{proc_lib,init_p_do_apply,3,[{file,\"proc_lib.erl\"},{line,227}]}]">>,
    Expected = [{[{date,<<"Tue, 13 Nov 2012 14:23:02 GMT">>},
                    {loglevel,<<"debug">>},
                    {pid,<<"<0.138.0>">>},
                    {text,<<"Stacktrace: [{couch_httpd_db,create_db_req,2,\n[{file,\"couch_httpd_db.erl\"},{line,214}]},\n{couch_httpd,handle_request_int,5,\n[{file,\"couch_httpd.erl\"},{line,317}]},\n{mochiweb_http,headers,5,\n[{file,\"mochiweb_http.erl\"},{line,136}]},\n{proc_lib,init_p_do_apply,3,[{file,\"proc_lib.erl\"},{line,227}]}]">>}]}],
    Output = couch_log:parse_to_json(Chunk),
    etap:is(Output, Expected, "Matches multiline stack track log line").

test_full_couchdb_test_file() ->
    Expected = [{[{date,<<"Tue, 13 Nov 2012 14:22:43 GMT">>},
                    {loglevel,<<"debug">>},
                    {pid,<<"<0.113.0>">>},
                    {text,<<"Include Doc: <<\"_design/_replicator\">> {1,\n                                                            <<91,250,44,153,\n                                                              238,254,43,46,\n                                                              180,150,45,181,\n                                                              10,163,207,212>>}">>}]},
                  {[{date,<<"Tue, 13 Nov 2012 14:22:43 GMT">>},
                    {loglevel,<<"info">>},
                    {pid,<<"<0.31.0>">>},
                    {text,<<"Apache CouchDB has started on http://127.0.0.1:5984/">>}]},
                  {[{date,<<"Tue, 13 Nov 2012 14:22:58 GMT">>},
                    {loglevel,<<"debug">>},
                    {pid,<<"<0.124.0>">>},
                    {text,<<"'GET' /_utils/couch_tests.html?script/couch_tests.js {1,1} from \"127.0.0.1\"\nHeaders: [{'Accept',\"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\"},\n          {'Accept-Encoding',\"gzip, deflate\"},\n          {'Accept-Language',\"en-US,en;q=0.5\"},\n          {'Connection',\"keep-alive\"},\n          {'Cookie',\"AuthSession=amFuOjUwQTI1MjM0Ol899Tg8MVYsmnB6ruczSIFpjfhd\"},\n          {'Host',\"localhost:5984\"},\n          {'Referer',\"http://localhost:5984/_utils/config.html\"},\n          {'User-Agent',\"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:16.0) Gecko/20100101 Firefox/16.0\"}]">>}]},
                  {[{date,<<"Tue, 13 Nov 2012 14:22:58 GMT">>},
                    {loglevel,<<"debug">>},
                    {pid,<<"<0.124.0>">>},
                    {text,<<"OAuth Params: [{\"script/couch_tests.js\",[]}]">>}]},
                  {[{date,<<"Tue, 13 Nov 2012 14:22:58 GMT">>},
                    {loglevel,<<"info">>},
                    {pid,<<"<0.124.0>">>},
                    {text,<<"127.0.0.1 - - GET /_utils/couch_tests.html?script/couch_tests.js 200">>}]},
                  {[{date,<<"Tue, 13 Nov 2012 14:22:58 GMT">>},
                    {loglevel,<<"debug">>},
                    {pid,<<"<0.136.0>">>},
                    {text,<<"'GET' /_utils/script/jquery.dialog.js?0.11.0 {1,1} from \"127.0.0.1\"">>}]},
                  {[{date,<<"Tue, 13 Nov 2012 14:23:02 GMT">>},
                    {loglevel,<<"debug">>},
                    {pid,<<"<0.138.0>">>},
                    {text,<<"Stacktrace: [{couch_httpd_db,create_db_req,2,\n                                     [{file,\"couch_httpd_db.erl\"},{line,214}]},\n                                 {couch_httpd,handle_request_int,5,\n                                     [{file,\"couch_httpd.erl\"},{line,317}]},\n                                 {mochiweb_http,headers,5,\n                                     [{file,\"mochiweb_http.erl\"},{line,136}]},\n                                 {proc_lib,init_p_do_apply,3,\n                                     [{file,\"proc_lib.erl\"},{line,227}]}]">>}]},
                  {[{date,<<"Tue, 13 Nov 2012 14:23:02 GMT">>},
                    {loglevel,<<"info">>},
                    {pid,<<"<0.138.0>">>},
                    {text,<<"127.0.0.1 - - PUT /test_suite_db/ 412">>}]},
                  {[{date,<<"Tue, 13 Nov 2012 14:23:02 GMT">>},
                    {loglevel,<<"debug">>},
                    {pid,<<"<0.138.0>">>},
                    {text,<<"httpd 412 error response:\n {\"error\":\"file_exists\",\"reason\":\"The database could not be created, the file already exists.\"}\n">>}]},
                  {[{date,<<"Tue, 13 Nov 2012 14:23:02 GMT">>},
                    {loglevel,<<"debug">>},
                    {pid,<<"<0.138.0>">>},
                    {text,<<"'DELETE' /test_suite_db {1,1} from \"127.0.0.1\"\n">>}]}],

    LogFileName = './test/etap/240-json-logs.log',
    LogFileSize = filelib:file_size(LogFileName),
    {ok, Fd} = file:open(LogFileName, [read]),
    {ok, Chunk} = file:pread(Fd,0, LogFileSize),
    Out = couch_log:parse_to_json(Chunk),
    ok = file:close(Fd),
    etap:is(Out, Expected, "Correctly format multiline log file with stacktraces and debug").
