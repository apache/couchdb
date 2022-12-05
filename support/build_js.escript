%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.
%%
%%

-export([main/1]).


main([]) ->

    CouchConfig = case filelib:is_file(os:getenv("COUCHDB_CONFIG")) of
        true ->
            {ok, Result} = file:consult(os:getenv("COUCHDB_CONFIG")),
            Result;
        false ->
            []
    end,

    SMVsn = case lists:keyfind(spidermonkey_version, 1, CouchConfig) of
        {_, Vsn} -> Vsn;
        undefined -> "1.8.5"
    end,

    JsFiles =  [
        "share/server/dreyfus.js",
        "share/server/nouveau.js",
        "share/server/filter.js",
        "share/server/mimeparse.js",
        "share/server/render.js",
        "share/server/state.js",
        "share/server/util.js",
        "share/server/validate.js",
        "share/server/views.js",
        "share/server/loop.js"
    ],

    CoffeeFiles = [
        "share/server/dreyfus.js",
        "share/server/nouveau.js",
        "share/server/filter.js",
        "share/server/mimeparse.js",
        "share/server/render.js",
        "share/server/state.js",
        "share/server/util.js",
        "share/server/validate.js",
        "share/server/views.js",
        "share/server/coffee-script.js",
        "share/server/loop.js"
    ],

    ExtraFiles = case SMVsn of
        "1.8.5" ->
            [
                "share/server/rewrite_fun.js"
            ];
        _ ->
            [
                "share/server/60/esprima.js",
                "share/server/60/escodegen.js"
            ]
    end,

    RewriteFunFile = ["share/server/60/rewrite_fun.js"],
    RewriteFunWithASTBypassFile = ["share/server/60/rewrite_fun_ast_bypass.js"],

    Pre = "(function () {\n",
    Post = "})();\n",

    Concat = fun(Files, To) ->
            AccBin = lists:foldl(fun(Path, Acc) ->
                            {ok, Bin} = file:read_file(Path),
                            [Bin | Acc]
                    end, [], Files),
            FinalBin = iolist_to_binary(Pre ++ lists:reverse(AccBin) ++ Post),
            file:write_file(To, FinalBin)
    end,

    case SMVsn of
        "1.8.5" ->
            ok = Concat(ExtraFiles ++ JsFiles, "share/server/main.js"),
            ok = Concat(ExtraFiles ++ JsFiles, "share/server/main-ast-bypass.js");
        _ ->
            ok = Concat(RewriteFunFile ++ ExtraFiles ++ JsFiles, "share/server/main.js"),
            ok = Concat(RewriteFunWithASTBypassFile ++ ExtraFiles ++ JsFiles, "share/server/main-ast-bypass.js")
    end,
    ok = Concat(ExtraFiles ++ CoffeeFiles, "share/server/main-coffee.js"),
    ok.
