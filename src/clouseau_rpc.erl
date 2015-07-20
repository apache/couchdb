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
-export([await/2, commit/2, get_update_seq/1, info/1, search/6, search/2]).
-export([group1/7, group2/8, group2/2]).
-export([delete/2, update/3, cleanup/1, cleanup/2]).
-export([analyze/2, version/0]).

open_index(Peer, Path, Analyzer) ->
    rpc({main, clouseau()}, {open, Peer, Path, Analyzer}).

await(Ref, MinSeq) ->
    rpc(Ref, {await, MinSeq}).

commit(Ref, NewCommitSeq) ->
    rpc(Ref, {commit, NewCommitSeq}).

info(Ref) ->
    rpc(Ref, info).

get_update_seq(Ref) ->
    rpc(Ref, get_update_seq).

%% @deprecated
search(Ref, Query, Limit, Refresh, Bookmark, Sort) ->
    rpc(Ref, {search, Query, Limit, Refresh, Bookmark, Sort}).

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

group2(Ref, Query, GroupBy, Refresh, Groups, GroupSort, DocSort, DocLimit) ->
    rpc(Ref, {group2, Query, GroupBy, Refresh, Groups, GroupSort, DocSort, DocLimit}).

group2(Ref, Args) ->
    rpc(Ref, {group2, Args}).

delete(Ref, Id) ->
    rpc(Ref, {delete, Id}).

update(Ref, Id, Fields) ->
    rpc(Ref, {update, Id, Fields}).

cleanup(DbName) ->
    gen_server:cast({cleanup, clouseau()}, {cleanup, DbName}).

cleanup(DbName, ActiveSigs) ->
    gen_server:cast({cleanup, clouseau()}, {cleanup, DbName, ActiveSigs}).

analyze(Analyzer, Text) ->
    rpc({analyzer, clouseau()}, {analyze, Analyzer, Text}).

version() ->
    rpc({main, clouseau()}, version).

rpc(Ref, Msg) ->
    ioq:call(Ref, Msg, erlang:get(io_priority)).

clouseau() ->
    list_to_atom(config:get("dreyfus", "name", "clouseau@127.0.0.1")).
