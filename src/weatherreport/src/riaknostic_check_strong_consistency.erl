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

%% @doc Diagnostic that checks Riak's current memory usage. If memory
%% usage is high, a warning message will be sent, otherwise only
%% informational messages.
-module(riaknostic_check_strong_consistency).
-behaviour(riaknostic_check).

-export([description/0,
         valid/0,
         check/0,
         format/1]).

-spec description() -> string().
description() ->
    "Strong consistency configuration valid".

-spec valid() -> boolean().
valid() ->
    riaknostic_node:can_connect().

-spec check() -> [{lager:log_level(), term()}].
check() ->
    StrongConsistencyOption = riaknostic_config:get_app_env([riak_core, enable_consensus]),
    { AAEOption, _ } = riaknostic_config:get_app_env([riak_kv, anti_entropy]),
    maybe_strong_consistency_aae_misconfigured(StrongConsistencyOption, AAEOption).

-spec maybe_strong_consistency_aae_misconfigured(boolean, on | off | any()) -> [ { term(), term() } ] | [].
maybe_strong_consistency_aae_misconfigured(true, off) ->
    [ { critical, { strong_consistency_aae_misconfigured } } ];
maybe_strong_consistency_aae_misconfigured(false, _) ->
    [];
maybe_strong_consistency_aae_misconfigured(true, on) ->
    [].

-spec format(term()) -> {io:format(), [term()]}.
format({ strong_consistency_aae_misconfigured }) ->
    { "Strong consistency has been enabled without AAE -- all consistent operations will timeout until AAE is enabled.", [] }.
