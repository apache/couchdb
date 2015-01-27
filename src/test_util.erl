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

-module(test_util).

-include_lib("couch/include/couch_eunit.hrl").

-export([init_code_path/0]).
-export([source_file/1, build_file/1]).
%% -export([run/2]).
-export([request/3, request/4]).
-export([start_couch/0, start_couch/1, start_couch/2, stop_couch/0, stop_couch/1]).
-export([start_config/1, stop_config/1]).
-export([start_applications/1]).


srcdir() ->
    code:priv_dir(couch) ++ "/../../".

builddir() ->
    code:priv_dir(couch) ++ "/../../../".

init_code_path() ->
    Paths = [
        "etap",
        "couchdb",
        "ejson",
        "oauth",
        "ibrowse",
        "mochiweb",
        "snappy"
    ],
    lists:foreach(fun(Name) ->
        code:add_patha(filename:join([builddir(), "src", Name]))
    end, Paths).

source_file(Name) ->
    filename:join([srcdir(), Name]).

build_file(Name) ->
    filename:join([builddir(), Name]).


request(Url, Headers, Method) ->
    request(Url, Headers, Method, []).

request(Url, Headers, Method, Body) ->
    request(Url, Headers, Method, Body, 3).

request(_Url, _Headers, _Method, _Body, 0) ->
    {error, request_failed};
request(Url, Headers, Method, Body, N) ->
    case code:is_loaded(ibrowse) of
    false ->
        {ok, _} = ibrowse:start();
    _ ->
        ok
    end,
    case ibrowse:send_req(Url, Headers, Method, Body) of
    {ok, Code0, RespHeaders, RespBody0} ->
        Code = list_to_integer(Code0),
        RespBody = iolist_to_binary(RespBody0),
        {ok, Code, RespHeaders, RespBody};
    {error, {'EXIT', {normal, _}}} ->
        % Connection closed right after a successful request that
        % used the same connection.
        request(Url, Headers, Method, Body, N - 1);
    Error ->
        Error
    end.


start_couch() ->
    start_couch(?CONFIG_CHAIN, []).

start_couch(ExtraApps) ->
    start_couch(?CONFIG_CHAIN, ExtraApps).

start_couch(IniFiles, ExtraApps) ->
    ok = application:set_env(config, ini_files, IniFiles),
    ok = lager:start(),
    Ctx = start_applications([inets, ibrowse, ssl, config, couch] ++ ExtraApps),
    couch_stats:reload(),
    Ctx.

stop_couch() ->
    ok = application:stop(couch),
    ok = application:stop(lager),
    ok = application:stop(goldrush),
    ok = application:stop(config),
    ok = application:stop(ssl),
    ok = application:stop(ibrowse),
    ok = application:stop(inets),
    ok.


stop_couch(_) ->
    stop_couch().


start_applications([]) ->
    ok;
start_applications([App|Apps]) ->
    case application:start(App) of
        {error, {already_started, _}} ->
            ok;
        {error, {not_started, Dep}} ->
            start_applications([Dep, App | Apps]);
        {error, {not_running, Dep}} ->
            start_applications([Dep, App | Apps]);
        ok ->
            ok
    end,
    start_applications(Apps).


start_config(Chain) ->
    case config:start_link(Chain) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, OldPid}}  ->
            ok = stop_config(OldPid),
            start_config(Chain)
    end.


stop_config(Pid) ->
    Timeout = 1000,
    erlang:monitor(process, Pid),
    config:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after Timeout ->
        throw({timeout_error, config_stop})
    end.
