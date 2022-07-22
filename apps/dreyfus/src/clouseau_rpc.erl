% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(clouseau_rpc).

-include("dreyfus.hrl").

-export([open_index/3]).
-export([await/2, commit/2, get_update_seq/1, info/1, search/2]).
-export([group1/7, group2/2]).
-export([delete/2, update/3, cleanup/1, cleanup/2, rename/1]).
-export([analyze/2, version/0, disk_size/1]).
-export([set_purge_seq/2, get_purge_seq/1, get_root_dir/0]).
-export([connected/0]).

open_index(Peer, Path, Analyzer) ->
    rpc({main, clouseau()}, {open, Peer, Path, Analyzer}).

disk_size(Path) ->
    rpc({main, clouseau()}, {disk_size, Path}).
get_root_dir() ->
    rpc({main, clouseau()}, {get_root_dir}).

await(Ref, MinSeq) ->
    rpc(Ref, {await, MinSeq}).

commit(Ref, NewCommitSeq) ->
    rpc(Ref, {commit, NewCommitSeq}).

info(Ref) ->
    rpc(Ref, info).

get_update_seq(Ref) ->
    rpc(Ref, get_update_seq).

set_purge_seq(Ref, Seq) ->
    rpc(Ref, {set_purge_seq, Seq}).

get_purge_seq(Ref) ->
    rpc(Ref, get_purge_seq).

search(Ref, Args) ->
    case rpc(Ref, {search, Args}) of
        {ok, Response} when is_list(Response) ->
            {ok, #top_docs{
                update_seq = couch_util:get_value(update_seq, Response),
                total_hits = couch_util:get_value(total_hits, Response),
                hits = couch_util:get_value(hits, Response),
                counts = couch_util:get_value(counts, Response),
                ranges = couch_util:get_value(ranges, Response)
            }};
        Else ->
            Else
    end.

group1(Ref, Query, GroupBy, Refresh, Sort, Offset, Limit) ->
    rpc(Ref, {group1, Query, GroupBy, Refresh, Sort, Offset, Limit}).

group2(Ref, Args) ->
    rpc(Ref, {group2, Args}).

delete(Ref, Id) ->
    rpc(Ref, {delete, couch_util:to_binary(Id)}).

update(Ref, Id, Fields) ->
    rpc(Ref, {update, Id, Fields}).

cleanup(DbName) ->
    gen_server:cast({cleanup, clouseau()}, {cleanup, DbName}).

rename(DbName) ->
    gen_server:cast({cleanup, clouseau()}, {rename, DbName}).

cleanup(DbName, ActiveSigs) ->
    gen_server:cast({cleanup, clouseau()}, {cleanup, DbName, ActiveSigs}).

analyze(Analyzer, Text) ->
    rpc({analyzer, clouseau()}, {analyze, Analyzer, Text}).

version() ->
    rpc({main, clouseau()}, version).

connected() ->
    HiddenNodes = erlang:nodes(hidden),
    case lists:member(clouseau(), HiddenNodes) of
        true ->
            true;
        false ->
            % We might have just booted up, so let's ping
            pong == net_adm:ping(clouseau())
    end.

rpc(Ref, Msg) ->
    ioq:call(Ref, Msg, erlang:get(io_priority)).

clouseau() ->
    list_to_atom(config:get("dreyfus", "name", "clouseau@127.0.0.1")).
