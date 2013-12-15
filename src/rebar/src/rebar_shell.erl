%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Trifork
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------

-module(rebar_shell).
-author("Kresten Krab Thorup <krab@trifork.com>").

-include("rebar.hrl").

-export([shell/2]).

shell(_Config, _AppFile) ->
    ?CONSOLE("NOTICE: Using experimental 'shell' command~n", []),
    %% backwards way to say we only want this executed
    %% for the "top level" directory
    case is_deps_dir(rebar_utils:get_cwd()) of
        false ->
            true = code:add_pathz(rebar_utils:ebin_dir()),
            user_drv:start(),
            %% this call never returns (until user quits shell)
            shell:server(false, false);
        true ->
            ok
    end,
    ok.

is_deps_dir(Dir) ->
    case lists:reverse(filename:split(Dir)) of
        [_, "deps" | _] ->
            true;
        _V ->
            false
    end.
