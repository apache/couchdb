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
%% @doc Adds process/pid testing to the etap suite.
-module(etap_process).

-export([is_pid/2, is_alive/2, is_mfa/3]).

% ---
% External / Public functions

%% @doc Assert that a given variable is a pid.
is_pid(Pid, Desc) when is_pid(Pid) -> etap:ok(true, Desc);
is_pid(_, Desc) -> etap:ok(false, Desc).

%% @doc Assert that a given process/pid is alive.
is_alive(Pid, Desc) ->
    etap:ok(erlang:is_process_alive(Pid), Desc).

%% @doc Assert that the current function of a pid is a given {M, F, A} tuple.
is_mfa(Pid, MFA, Desc) ->
    etap:is({current_function, MFA}, erlang:process_info(Pid, current_function), Desc).
