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

main(_) ->
    test_util:init_code_path(),

    etap:plan(27),
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
    test_disabling(),
    test_enabled(),
    test_default_policy(),
    test_duels(),
    ok.

test_bad_api_calls() ->
    etap_threw(fun() -> couch_cors_policy:check() end,
            true, "Policy check with zero parameters"),
    etap_threw(fun() -> couch_cors_policy:check([]) end,
            true, "Policy check with one parameter"),
    etap_threw(fun() -> couch_cors_policy:check([], []) end,
            true, "Policy check with two parameters"),

    etap_threw(fun() -> couch_cors_policy:check(notList, [], {'GET', []}) end,
            true, "Policy check with non-list first parameter"),
    etap_threw(fun() -> couch_cors_policy:check([], notList, {'GET', []}) end,
            true, "Policy check with non-list second parameter"),
    etap_threw(fun() -> couch_cors_policy:check([], [], {{not_method}, []}) end,
            true, "Policy check with non-method third parameter"),
    etap_threw(fun() -> couch_cors_policy:check([], [], {'GET', not_list}) end,
            true, "Policy check with non-list fourth parameter"),
    ok.

test_good_api_calls() ->
    etap_threw(fun() -> couch_cors_policy:check([], [], {'GET', []}) end,
               false, "Policy check with three valid parameters"),

    etap_threw(fun() ->
        couch_cors_policy:check(config(), config(), {'GET', []})
    end, false, "Policy check with noop configs"),

    % And the shortcut function.
    etap_threw(fun() -> check([], [], req()) end,
               false, "Policy check with three valid parameters"),
    etap_threw(fun() -> check(config(), config(), req()) end,
               false, "Policy check with noop configs"),
    ok.

test_disabling() ->
    Default = config(),
    Enabled = config([enabled]), % Enabled only, nothing else.
    Config = config([enabled, {"example.com", "http://origin.com"}]),
    Deactivated = config([{"example.com", "http://origin.com"}]),

    etap:is(check(Default, Default, req()),
            [], "By default, CORS is disabled"),
    etap:is(check(Default, Config, req()),
            [], "By default, CORS is disabled, despite _security"),
    etap:is(check(Deactivated, Default, req()),
            [], "Globally disabling CORS overrides everything else"),
    etap:is(check(Deactivated, Config, req()),
            [], "Deactivated CORS still overrides _security"),

    etap:is(check(Config, [], httpd()),
            [], "CORS fail for request with no Origin header"),
    etap:is(check(Enabled, Config, httpd()),
            [], "CORS fail from _security for request with no Origin"),

    etap:isnt(check(Enabled, Config, req()),
              [], "Globally enabled CORS, config in _security: passes"),
    etap:isnt(check(Config, Default, req()),
              [], "Global CORS config, nothing in _security: passes"),
    ok.

test_enabled() ->
    Enabled = config([enabled]),
    Config = config([enabled, {"example.com","http://origin.com"}]),
    etap:ok(is_list(check(Enabled, Config, req())),
            "Good CORS from _security returns a list"),
    etap:ok(is_list(check(Config, [], req())),
            "Good CORS from _config returns a list"),
    ok.


test_default_policy() ->
    Enabled = config([enabled]),
    Config = config([enabled, {"example.com","http://origin.com"}]),

    HeaderIs = fun(Global, Local, Req, Suffix, Expected, Description) ->
        Result = couch_cors_policy:check(Global, Local, Req),
        etap:ok(is_list(Result), "List returned: " ++ Description),

        % Protect against crashing in case the result was not a list.
        case is_list(Result) of
            false ->
                etap:ok(false, "No headers either: " ++ Description);
            true ->
                Key = "Access-Control-" ++ Suffix,
                Value = case lists:keyfind(Key, 1, Result) of
                    {Key, Val} -> Val;
                    _ -> undefined
                end,
                etap:is(Value, Expected, Description)
        end
    end,

    HeaderIs(Enabled, [], req(), "Allow-Origin",
             "http://origin.com", "Default CORS policy echoes the header"),
    HeaderIs(Enabled, Config, req(), "Allow-Origin",
             "http://origin.com", "Satisfied CORS policy echoes the header"),

    HeaderIs(Enabled, [], req(), "Allow-Methods",
             undefined, "Actual response does not send preflight headers"),
    ok.

test_duels() ->
    % Configs from _security and _config have a duel!
    %TODO
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
    Headers = [{"Stuff","I am stuff"}],
    {Method, Headers}.

req() ->
    req(httpd()).
req(Origin) when is_list(Origin) ->
    req(httpd(), Origin);
req(Httpd) ->
    req(Httpd, "http://origin.com").
req({Method, Headers}, Origin) ->
    % Give this request the Origin.
    OriginHeaders = lists:keystore("Origin", 1, Headers, {"Origin", Origin}),
    HostHeaders = lists:keystore("Host", 1, OriginHeaders, {"Host", "example.com"}),
    {Method, HostHeaders}.

% Example, CORS enabled, mydomain.com allows http://origin.com with a max-age:
% config([enabled,
%        {"mydomain.com","http://origin.com"},
%        {"mydomain.com","https://origin.com", [{"allow_credentials",true}]} ])
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
    config([ {Dom, Orig, []} | Opts ], Config);

config([ {Dom, Orig, Policy} | Opts ], Config) ->
    Domain = list_to_binary(Dom),
    Origin = list_to_binary(Orig),

    {Domains} = case lists:keyfind(<<"origins">>, 1, Config) of
        {<<"origins">>, FoundDomains} -> FoundDomains;
        false -> {[]}
    end,
    {Origins} = case lists:keyfind(Domain, 1, Domains) of
        {Domain, FoundOrigins} -> FoundOrigins;
        false -> {[]}
    end,

    Policy1 = [{list_to_binary(Key), Val} || {Key, Val} <- Policy],
    Origins1 = lists:keystore(Origin, 1, Origins, {Origin, {Policy1}}),
    Domains1 = lists:keystore(Domain, 1, Domains, {Domain, {Origins1}}),
    Config1 = lists:keystore(<<"origins">>, 1, Config,
                             {<<"origins">>, {Domains1}}),
    config(Opts, Config1).

% vim: sts=4 sw=4 et
