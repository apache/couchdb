%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
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
%% @author Juhani Rankimies <juhani@juranki.com>
%% @doc Tests functionality of rebar_file_utils module.
%% @copyright 2009, 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_file_utils_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(TMP_DIR, "tmp_file_utils").
-define(DIR_TREE, [{d,"source",[{f,"file1"},
                                {f,"file2"}]},
                   {d,"dest",[]}]).
-define(FILE_CONTENT, <<"1234567890">>).

%% ====================================================================
%% delete_each tests
%% ====================================================================

delete_bogus_test_() ->
    {"delete_each survives nonexisting files",
     [?_assertMatch(ok, rebar_file_utils:delete_each(["bogus"])),
      ?_assertMatch(ok, rebar_file_utils:delete_each(["bogus1","bogus2"]))]}.

delete_each_test_() ->
    {"delete_each removes files",
     setup,
     fun() ->
             setup(),
             rebar_file_utils:delete_each(file_list())
     end,
     fun teardown/1,
     [assert_files_not_in("source", file_list())]}.

%% ====================================================================
%% rm_rf tests
%% ====================================================================

rm_rf_wildcard_test_() ->
    {"rm_rf removes files based on wildcard spec",
     setup,
     fun() ->
             setup(),
             rebar_file_utils:rm_rf(filename:join([?TMP_DIR,"source","file*"]))
     end,
     fun teardown/1,
     [assert_files_not_in("source", file_list())]}.

rm_rf_dir_test_() ->
    {"rm_rf removes directory tree",
     setup,
     fun() ->
             setup(),
             rebar_file_utils:rm_rf(filename:join([?TMP_DIR,"source"]))
     end,
     fun teardown/1,
     [?_assertNot(filelib:is_dir(filename:join([?TMP_DIR,"source"])))]}.

%% ====================================================================
%% cp_r tests
%% ====================================================================

cp_r_file_to_file_test_() ->
    {"cp_r copies a file to file",
     setup,
     fun() ->
             setup(),
             rebar_file_utils:cp_r([filename:join([?TMP_DIR,"source","file1"])],
                                   filename:join([?TMP_DIR,"dest","new_file"]))
     end,
     fun teardown/1,
     [?_assert(filelib:is_regular(filename:join([?TMP_DIR,"dest","new_file"])))]}.

cp_r_file_to_dir_test_() ->
    {"cp_r copies a file to directory",
     setup,
     fun() ->
             setup(),
             rebar_file_utils:cp_r([filename:join([?TMP_DIR,"source","file1"])],
                                   filename:join([?TMP_DIR,"dest"]))
     end,
     fun teardown/1,
     [?_assert(filelib:is_regular(filename:join([?TMP_DIR,"dest","file1"])))]}.

cp_r_dir_to_dir_test_() ->
    {"cp_r copies a directory to directory",
     setup,
     fun() ->
             setup(),
             rebar_file_utils:cp_r([filename:join([?TMP_DIR,"source"])],
                                   filename:join([?TMP_DIR,"dest"]))
     end,
     fun teardown/1,
     [?_assert(filelib:is_dir(filename:join([?TMP_DIR,"dest","source"]))),
      assert_files_in("dest/source",
                      [filename:join([?TMP_DIR,"dest","source",F]) ||
                          F <- ["file1","file2"]])]}.

cp_r_wildcard_file_to_dir_test_() ->
    {"cp_r copies wildcard files to directory",
     setup,
     fun() ->
             setup(),
             rebar_file_utils:cp_r([filename:join([?TMP_DIR,"source","*1"])],
                                   filename:join([?TMP_DIR,"dest"]))
     end,
     fun teardown/1,
     [?_assert(filelib:is_regular(filename:join([?TMP_DIR,"dest","file1"])))]}.

cp_r_wildcard_dir_to_dir_test_() ->
    {"cp_r copies wildcard directory to directory",
     setup,
     fun() ->
             setup(),
             rebar_file_utils:cp_r([filename:join([?TMP_DIR,"sour*"])],
                                   filename:join([?TMP_DIR,"dest"]))
     end,
     fun teardown/1,
     [?_assert(filelib:is_dir(filename:join([?TMP_DIR,"dest","source"]))),
      assert_files_in("dest/source",
                      [filename:join([?TMP_DIR,"dest","source",F]) ||
                          F <- ["file1","file2"]])]}.

cp_r_overwrite_file_test_() ->
    {"cp_r overwrites destination file",
     setup,
     fun() ->
             setup(),
             ok = file:write_file(filename:join([?TMP_DIR,"dest","file1"]),
                                  <<"test">>),
             rebar_file_utils:cp_r([filename:join([?TMP_DIR,"source","file1"])],
                                   filename:join([?TMP_DIR,"dest"]))
     end,
     fun teardown/1,
     [?_assertMatch({ok,?FILE_CONTENT},
                    file:read_file(
                      filename:join([?TMP_DIR,"dest","file1"])))]}.

cp_r_overwrite_dir_test_() ->
    {"cp_r overwrites destination file (xcopy case on win32)",
     setup,
     fun() ->
             setup(),
             ok = file:make_dir(filename:join([?TMP_DIR,"dest","source"])),
             ok = file:write_file(
                    filename:join([?TMP_DIR,"dest","source","file1"]),
                    <<"test">>),
             rebar_file_utils:cp_r([filename:join([?TMP_DIR,"source"])],
                                   filename:join([?TMP_DIR,"dest"]))
     end,
     fun teardown/1,
     [?_assertMatch({ok,?FILE_CONTENT},
                    file:read_file(
                      filename:join([?TMP_DIR,"dest","source","file1"])))]}.

cp_r_overwrite_file_fail_test_() ->
    {"cp_r fails to fs permission errors (file:copy/2 case on win32)",
     setup,
     fun() ->
             setup(),
             ok = file:write_file(
                    filename:join([?TMP_DIR,"dest","file1"]),<<"test">>),
             ok = file:change_mode(
                    filename:join([?TMP_DIR,"dest","file1"]),0)
     end,
     fun teardown/1,
     [?_assertError({badmatch,_},
                    rebar_file_utils:cp_r(
                      [filename:join([?TMP_DIR,"source","file1"])],
                      filename:join([?TMP_DIR,"dest"])))]}.

cp_r_overwrite_dir_fail_test_() ->
    {"cp_r fails to fs permission error (xcopy case on win32)",
     setup,
     fun() ->
             setup(),
             ok = file:make_dir(
                    filename:join([?TMP_DIR,"dest","source"])),
             ok = file:write_file(
                    filename:join([?TMP_DIR,"dest","source","file1"]),
                    <<"test">>),
             ok = file:change_mode(
                    filename:join([?TMP_DIR,"dest","source","file1"]),0)
     end,
     fun teardown/1,
     [?_assertError({badmatch,_},
                    rebar_file_utils:cp_r(
                      [filename:join([?TMP_DIR,"source"])],
                      filename:join([?TMP_DIR,"dest"])))]}.

mv_file_test_() ->
    {"move a file to folder",
     setup,
     fun() ->
             setup(),
             rebar_file_utils:mv(filename:join([?TMP_DIR,"source","file1"]),
                                 filename:join([?TMP_DIR,"dest"]))
     end,
     fun teardown/1,
     [?_assert(filelib:is_regular(filename:join([?TMP_DIR,"dest","file1"]))),
      ?_assertNot(filelib:is_regular(
                    filename:join([?TMP_DIR,"source","file1"])))]}.

%% ====================================================================
%% Utilities
%% ====================================================================

file_list() ->
    [filename:join([?TMP_DIR,"source",F]) || F <- ["file1","file2"]].

%% ====================================================================
%% Setup and Teardown
%% ====================================================================

setup() ->
    file:make_dir(?TMP_DIR),
    make_dir_tree(?TMP_DIR,?DIR_TREE).

make_dir_tree(Parent, [{d,Dir,Contents} | Rest]) ->
    NewDir = filename:join(Parent,Dir),
    ok = file:make_dir(NewDir),
    ok = make_dir_tree(NewDir,Contents),
    ok = make_dir_tree(Parent,Rest);
make_dir_tree(Parent, [{f,File} | Rest]) ->
    ok = file:write_file(filename:join(Parent,File),?FILE_CONTENT),
    ok = make_dir_tree(Parent,Rest);
make_dir_tree(_,[]) ->
    ok.

teardown(_) ->
    case os:type() of
        {unix, _} ->
            os:cmd("rm -rf " ++ ?TMP_DIR ++ " 2>/dev/null");
        {win32, _} ->
            os:cmd("rmdir /S /Q " ++ filename:nativename(?TMP_DIR))
    end.

%% ====================================================================
%% Assert helpers
%% ====================================================================

assert_files_in(Name, [File|T]) ->
    [{Name ++ " has file: " ++ File, ?_assert(filelib:is_regular(File))} |
     assert_files_in(Name, T)];
assert_files_in(_, []) -> [].

assert_files_not_in(Name, [File|T]) ->
    [{Name ++ " does not have file: " ++ File,
      ?_assertNot(filelib:is_regular(File))} |
     assert_files_not_in(Name, T)];
assert_files_not_in(_, []) -> [].
