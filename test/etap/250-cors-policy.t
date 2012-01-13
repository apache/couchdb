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

% Test only the policy decisions of couch_cors_policy:check/3--no servers
% or other couch_* stuff.

-record(httpd,
    {mochi_req,
    peer,
    method,
    requested_path_parts,
    path_parts,
    db_url_handlers,
    user_ctx,
    req_body = undefined,
    design_url_handlers,
    auth,
    default_fun,
    url_handlers
    }).

main(_) ->
    test_util:init_code_path(),

    etap:plan(7),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    test_bad_api_calls(),
    test_good_api_calls(),
    ok.

test_bad_api_calls() ->
    etap_threw(fun() -> couch_cors_policy:check() end,
            true, "Policy check with zero parameters"),
    etap_threw(fun() -> couch_cors_policy:check([]) end,
            true, "Policy check with one parameter"),
    etap_threw(fun() -> couch_cors_policy:check([], []) end,
            true, "Policy check with two parameters"),

    etap_threw(fun() -> couch_cors_policy:check(notList, [], #httpd{}) end,
            true, "Policy check with non-list first parameter"),
    etap_threw(fun() -> couch_cors_policy:check([], notList, #httpd{}) end,
            true, "Policy check with non-list second parameter"),
    etap_threw(fun() -> couch_cors_policy:check([], [], {not_httpd}) end,
            true, "Policy check with non-#httpd{} third parameter"),
    ok.

test_good_api_calls() ->
    etap_threw(fun() -> couch_cors_policy:check([], [], #httpd{}) end,
               false, "Policy check with three valid parameters"),
    ok.

%
% Utilities
%

etap_threw(Function, Expected, Description) ->
    Result = try Function() of
        _NoThrow -> false
        catch _:_ -> true
    end,
    etap:is(Result, Expected, Description).

% vim: sts=4 sw=4 et
