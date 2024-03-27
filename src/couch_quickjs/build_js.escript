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

%% Utility script to compile query server .js files into C arrays. Main JS and
%% Coffeescript files are treated separately. First each is bundled into a
%% single .js file. Then the file is passed to qjsc to produce the bytecode
%% array.
%%

-export([main/1]).

-mode(compile).

main(["compile"]) ->
    concat_js_files("bundle_mainjs.js", "bundle_coffee.js"),
    Changed1 = compile_bytecode("bundle_mainjs.js", "couchjs_mainjs_bytecode.c"),
    Changed2 = compile_bytecode("bundle_coffee.js", "couchjs_coffee_bytecode.c"),
    case Changed1 orelse Changed2 of
        true ->
            % A stupid hack. The compile step is often too quick and
            % generates .o timestamps with the same 1 second timestamp
            % as the .c file. During dev work, it means it will
            % re-compile and re-link everything uncessarily at least
            % one more time as the port compiler compares timestamps
            % with the >= operator.
            timer:sleep(1000),
            ok;
        false ->
            ok
    end;
main(["clean"]) ->
    rm("priv/bundle_*.js"),
    rm("c_src/couchjs_*_bytecode.c");
main(Arg) ->
    io:format(standard_error, "Expected a 'compile' or 'clean' arg. Got:~p", [Arg]),
    halt(1).

concat_js_files(JsScript, CoffeeScript) ->
    Prefix = "../../share/server/",
    JsFiles = [
        "rewrite_fun.js",
        "dreyfus.js",
        "nouveau.js",
        "filter.js",
        "mimeparse.js",
        "render.js",
        "state.js",
        "util.js",
        "validate.js",
        "views.js"
    ],
    Main = JsFiles ++ ["dispatch-quickjs.js"],
    Coffee = JsFiles ++ ["coffee-script.js", "dispatch-quickjs.js"],
    concat([Prefix ++ File || File <- Main], "priv/" ++ JsScript),
    concat([Prefix ++ File || File <- Coffee], "priv/" ++ CoffeeScript),
    ok.

compile_bytecode(Js, CBytecode) ->
    % cp_if_different/2 is used to to avoid triggering a re-compile if nothing changed
    Tmp = CBytecode ++ ".tmp",
    os:cmd("quickjs/qjsc -c -N bytecode -o c_src/" ++ Tmp ++ " priv/" ++ Js),
    Changed = cp_if_different("c_src/" ++ Tmp, "c_src/" ++ CBytecode),
    rm("c_src/" ++ Tmp),
    Changed.

cp_if_different(From, To) ->
    Bin = fread(From),
    case filelib:is_file(To) of
        true ->
            case fread(To) of
                Bin ->
                    false;
                <<_/binary>> ->
                    ok = fwrite(To, Bin),
                    true
            end;
        false ->
            ok = fwrite(To, Bin),
            true
    end.

concat(Sources, Target) ->
    SourceBins = [fread(P) || P <- Sources],
    TargetBin =  iolist_to_binary(["(function () {\n"] ++ SourceBins ++ ["})();\n"]),
    fwrite(Target, TargetBin).

fread(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

fwrite(Path, Bin) when is_binary(Bin) ->
    ok = file:write_file(Path, Bin).

rm(Path) ->
    Fun = fun(F) ->
        case filelib:is_file(F) of
            true -> ok = file:delete(F);
            false -> ok
        end
    end,
    lists:foreach(Fun, filelib:wildcard(Path)).
