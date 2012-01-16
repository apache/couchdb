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

    etap_threw(fun() ->
        couch_cors_policy:check(config(), config(), #httpd{})
    end, false, "Policy check with noop configs"),

    % And the shortcut function.
    etap_threw(fun() -> check([], [], req()) end,
               false, "Policy check with three valid parameters"),
    etap_threw(fun() -> check(config(), config(), req()) end,
               false, "Policy check with noop configs"),
    ok.

%
% Utilities
%

check(A, B, C) ->
    couch_cors_policy:check(A, B, C).

etap_threw(Function, Expected, Description) ->
    Result = try Function() of
        _NoThrow -> false
        catch _:_ -> true
    end,
    etap:is(Result, Expected, Description).

httpd() ->
    httpd('GET').

httpd(Method) when is_atom(Method) ->
    httpd(Method, "/db/doc");

httpd(Path) ->
    httpd('GET', Path).

httpd(Method, Path) ->
    Parts = [ list_to_binary(Part) || Part <- string:tokens(Path, "/") ],
    #httpd{method=Method, requested_path_parts=Parts, path_parts=Parts}.

req() ->
    req(httpd()).
req(Origin) when is_list(Origin) ->
    req(httpd(), Origin);
req(Httpd) ->
    req(Httpd, "http://origin.com").
req(#httpd{method=Method, path_parts=Parts}=Req, Origin) ->
    % Give this request the Origin.
    Method = Req#httpd.method,
    Path = filename:join(Parts),
    Version = {1,1},
    Headers = mochiweb_headers:make([{"Origin", Origin}]),
    MochiReq = mochiweb_request:new(nil, Method, Path, Version, Headers),
    Req#httpd{ mochi_req=MochiReq }.

% Example, CORS enabled, mydomain.com allows http://origin.com with a max-age:
% config([enabled,
%        {"mydomain.com","http://origin.com"},
%        ["http://origin.com", {"max-age",3600}]])
config() ->
    config([]).
config(Opts) ->
    config(Opts, []).
config([], Config) ->
    Config;

config([enabled | Opts], Config) ->
    Config1 = lists:keystore(<<"httpd">>, 1, Config,
        {<<"httpd">>, {[ {<<"cors_enabled">>,true} ]}}),
    config(Opts, Config1);

config([ {Dom, Orig} | Opts ], Config) ->
    Domain = list_to_binary(Dom),
    Origin = list_to_binary(Orig),
    {Origins} = case lists:keyfind(<<"origins">>, 1, Config) of
        {<<"origins">>, FoundOrigins} -> FoundOrigins;
        false -> {[]}
    end,
    Origins1 = lists:keystore(Domain, 1, Origins, {Domain, Origin}),
    Config1 = lists:keystore(<<"origins">>, 1, Config, {<<"origins">>, {Origins1}}),
    config(Opts, Config1);

config([ [Orig|KVs] | Opts ], Config) ->
    Origin = list_to_binary(Orig),
    Configs = lists:foldl(fun({KeyStr, ValStr}, OriginCfg) ->
        Key = list_to_binary(KeyStr),
        Val = case ValStr of
            "true" -> true;
            "false" -> false;
            Num when is_number(Num) -> Num;
            _ -> list_to_binary(ValStr)
        end,
        lists:keystore(Key, 1, OriginCfg, {Key, Val})
    end, [], KVs),
    Config1 = lists:keystore(Origin, 1, Config, {Origin, {Configs}}),
    config(Opts, Config1).

% vim: sts=4 sw=4 et
