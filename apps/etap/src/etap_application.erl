%% Copyright (c) 2008-2009 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @author Nick Gerakines <nick@gerakines.net> [http://socklabs.com/]
%% @copyright 2008 Nick Gerakines
%% @reference http://testanything.org/wiki/index.php/Main_Page
%% @reference http://en.wikipedia.org/wiki/Test_Anything_Protocol
%% @todo Explain in documentation why we use a process to handle test input.
%% @todo Add test to verify the number of members in a pg2 group.
%% @doc Provide test functionality to the application and related behaviors.
-module(etap_application).
-export([
    start_ok/2, ensure_loaded/3, load_ok/2,
    pg2_group_exists/2, pg2_group_doesntexist/2
]).

%% @spec load_ok(string(), string()) -> true | false
%% @doc Assert that an application can be loaded successfully.
load_ok(AppName, Desc) ->
    etap:ok(application:load(AppName) == ok, Desc).

%% @spec start_ok(string(), string()) -> true | false
%% @doc Assert that an application can be started successfully.
start_ok(AppName, Desc) ->
    etap:ok(application:start(AppName) == ok, Desc).

%% @spec ensure_loaded(string(), string(), string()) -> true | false
%% @doc Assert that an application has been loaded successfully.
ensure_loaded(AppName, AppVsn, Desc) ->
    etap:any(
        fun(Match) -> case Match of {AppName, _, AppVsn} -> true; _ -> false end end,
        application:loaded_applications(),
        Desc
    ).

%% @spec pg2_group_exists(string(), string()) -> true | false
%% @doc Assert that a pg2 group exists.
pg2_group_exists(GroupName, Desc) ->
    etap:any(
        fun(Match) -> Match == GroupName end,
        pg2:which_groups(),
        Desc
    ).

%% @spec pg2_group_doesntexist(string(), string()) -> true | false
%% @doc Assert that a pg2 group does not exists.
pg2_group_doesntexist(GroupName, Desc) ->
    etap:none(
        fun(Match) -> Match == GroupName end,
        pg2:which_groups(),
        Desc
    ).
