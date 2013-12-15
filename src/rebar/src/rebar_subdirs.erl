%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
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
-module(rebar_subdirs).

-include("rebar.hrl").
-include_lib("kernel/include/file.hrl").

-export([preprocess/2]).

%% ===================================================================
%% Public API
%% ===================================================================

preprocess(Config, _) ->
    %% Get the list of subdirs specified in the config (if any).
    Cwd = rebar_utils:get_cwd(),
    ListSubdirs = rebar_config:get_local(Config, sub_dirs, []),
    Subdirs0 = lists:flatmap(fun filelib:wildcard/1, ListSubdirs),
    case {rebar_config:is_skip_dir(Config, Cwd), Subdirs0} of
        {true, []} ->
            {ok, []};
        {true, _} ->
            ?WARN("Ignoring sub_dirs for ~s~n", [Cwd]),
            {ok, []};
        {false, _} ->
            Check = check_loop(Cwd),
            ok = lists:foreach(Check, Subdirs0),
            Subdirs = [filename:join(Cwd, Dir) || Dir <- Subdirs0],
            {ok, Subdirs}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

check_loop(Cwd) ->
    RebarConfig = filename:join(Cwd, "rebar.config"),
    fun(Dir0) ->
            IsSymlink = case file:read_link_info(Dir0) of
                            {ok, #file_info{type=symlink}} ->
                                {true, resolve_symlink(Dir0)};
                            _ ->
                                {false, Dir0}
                        end,
            case IsSymlink of
                {false, Dir="."} ->
                    ?ERROR("infinite loop detected:~nsub_dirs"
                           " entry ~p in ~s~n", [Dir, RebarConfig]);
                {true, Cwd} ->
                    ?ERROR("infinite loop detected:~nsub_dirs"
                           " entry ~p in ~s is a symlink to \".\"~n",
                           [Dir0, RebarConfig]);
                _ ->
                    ok
            end
    end.

resolve_symlink(Dir0) ->
    {ok, Dir} = file:read_link(Dir0),
    Dir.
