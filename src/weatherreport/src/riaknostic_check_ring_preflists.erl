%% -------------------------------------------------------------------
%%
%% riaknostic - automated diagnostic tools for Riak
%%
%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Diagnostic that checks the local ring for any preflists that do
%% not satisfy n_val
-module(riaknostic_check_ring_preflists).
-behaviour(riaknostic_check).

-export([description/0,
         valid/0,
         check/0,
         format/1]).

-spec description() -> string().
description() ->
    "Check ring satisfies n_val".

-spec valid() -> boolean().
valid() ->
    riaknostic_node:can_connect().

-spec check() -> [{lager:log_level(), term()}].
check() ->
    case riaknostic_node:local_command(riak_core_ring_util, check_ring) of
        [] -> [];
        PrefLists -> [ {warning, {n_val_not_satisfied, PrefLists}} ]
    end.

-spec format(term()) -> {io:format(), [term()]}.
format({n_val_not_satisfied, PrefLists}) ->
    {"The following preflists do not satisfy the n_val: ~p", [PrefLists]}.

