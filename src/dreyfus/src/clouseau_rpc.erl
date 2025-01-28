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

%% string represented as binary
-type string_as_binary(_Value) :: nonempty_binary().

-type shard() :: string_as_binary(_).
-type path() :: string_as_binary(_).

-type error() :: any().

-type analyzer_name() :: string_as_binary(_).

-type field_name() :: string_as_binary(_).

-type seq() :: non_neg_integer().
-type commit_seq() :: seq().
-type purge_seq() :: seq().
-type committed_seq() :: seq().
-type pending_seq() :: seq().
-type update_seq() :: seq().

-type analyzer_fields() :: [{field_name(), analyzer_name() | [analyzer_name()]}].

-type indexer_pid() :: pid().

%% Example of the message
%%   {[
%%       {<<"name">>,<<"perfield">>},
%%       {<<"default">>,<<"keyword">>},
%%       {<<"fields">>,{[
%%           {<<"$default">>,<<"standard">>}
%%       ]}}
%%   ]}
-type analyzer() ::
    analyzer_name()
    | [
        {string_as_binary(name), analyzer_name()}
        | {string_as_binary(default), analyzer()}
        | {string_as_binary(fields), {analyzer_fields()}}
        | {string_as_binary(stopwords), [field_name()]}
    ].

-spec open_index(Peer :: pid(), Path :: shard(), Analyzer :: analyzer()) ->
    {ok, indexer_pid()} | error().
open_index(Peer, Path, Analyzer) ->
    rpc({main, clouseau()}, {open, Peer, Path, Analyzer}).

-spec disk_size(Path :: shard()) ->
    {ok, {disk_size, non_neg_integer()}} | error().

disk_size(Path) ->
    rpc({main, clouseau()}, {disk_size, Path}).

-spec get_root_dir() ->
    {ok, path()} | error().

get_root_dir() ->
    rpc({main, clouseau()}, {get_root_dir}).

%% not used ???
-spec await(Ref :: indexer_pid(), MinSeq :: commit_seq()) ->
    ok | error().

await(Ref, MinSeq) ->
    rpc(Ref, {await, MinSeq}).

%% deprecated
-spec commit(Ref :: indexer_pid(), NewCommitSeq :: commit_seq()) ->
    ok | error().

commit(Ref, NewCommitSeq) ->
    rpc(Ref, {commit, NewCommitSeq}).

-type info_result_item() ::
    {disk_size, non_neg_integer()}
    | {doc_count, non_neg_integer()}
    | {doc_del_count, non_neg_integer()}
    | {pending_seq, pending_seq()}
    | {committed_seq, committed_seq()}
    | {purge_seq, purge_seq()}.

-spec info(Ref :: indexer_pid()) ->
    {ok, [info_result_item()]} | error().

info(Ref) ->
    rpc(Ref, info).

-spec get_update_seq(Ref :: indexer_pid()) ->
    {ok, update_seq()} | error().

get_update_seq(Ref) ->
    rpc(Ref, get_update_seq).

-spec set_purge_seq(Ref :: indexer_pid(), Seq :: purge_seq()) ->
    ok | error().

set_purge_seq(Ref, Seq) ->
    rpc(Ref, {set_purge_seq, Seq}).

-spec get_purge_seq(Ref :: indexer_pid()) ->
    {ok, purge_seq()} | error().

get_purge_seq(Ref) ->
    rpc(Ref, get_purge_seq).

-type query() :: string_as_binary(_).
-type range_name() :: string_as_binary(_).
-type range_label() :: string_as_binary(_).
-type range_query() :: string_as_binary(_).
-type partition() :: string_as_binary(_).

-type limit() :: non_neg_integer().
-type bookmark() :: string_as_binary(_).

-type search_arg() ::
    {query, query()}
    | {partition, partition()}
    | {limit, limit()}
    | {refresh, boolean()}
    | {'after', bookmark()}
    | {sort, group_sort()}
    | {include_fields, [field_name()]}
    | {counts, [field_name()]}
    | {ranges, [{range_name(), [{range_label(), range_query()}]}]}
    | {highlight_fields, [field_name()]}
    | {highlight_pre_tag, string_as_binary(_)}
    | {highlight_post_tag, string_as_binary(_)}
    | {highlight_number, pos_integer()}
    | {highlight_size, pos_integer()}
    | {legacy, boolean()}.

-spec search(Ref :: indexer_pid(), Args :: [search_arg()]) ->
    {ok, #top_docs{}} | error().

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

-type offset() :: non_neg_integer().
-type group_sort() ::
    relevance
    | field_name()
    | [field_name()].

-spec group1(
    Ref :: indexer_pid(),
    Query :: query(),
    GroupBy :: field_name(),
    Refresh :: boolean(),
    Sort :: group_sort(),
    Offset :: offset(),
    Limit :: limit()
) -> {ok, [{field_name(), sort_values()}]} | error().

group1(Ref, Query, GroupBy, Refresh, Sort, Offset, Limit) ->
    rpc(Ref, {group1, Query, GroupBy, Refresh, Sort, Offset, Limit}).

-type group_name() :: string_as_binary(_) | null.
-type sort_values() :: [string_as_binary(_) | null].
-type groups() :: [{group_name(), sort_values()}].

-type query_arg() ::
    {query, string_as_binary(_)}
    | {field, field_name()}
    | {refresh, boolean()}
    | {groups, groups()}
    | {group_sort, group_sort()}
    | {sort, group_sort()}
    | {limit, limit()}
    | {include_fields, [field_name()]}
    | {highlight_fields, [field_name()]}
    | {highlight_pre_tag, string_as_binary(_)}
    | {highlight_post_tag, string_as_binary(_)}
    | {highlight_number, pos_integer()}
    | {highlight_size, pos_integer()}.

-type grouped_results() :: [{field_name(), TotalHits :: non_neg_integer(), [#hit{}]}].
-spec group2(Ref :: indexer_pid(), Args :: [query_arg()]) ->
    {ok, {TotalHits :: non_neg_integer(), TotalGroupedHits :: non_neg_integer(), grouped_results()}}.

group2(Ref, Args) ->
    rpc(Ref, {group2, Args}).

-spec delete(Ref :: indexer_pid(), Id :: docid()) ->
    ok.

delete(Ref, Id) ->
    rpc(Ref, {delete, couch_util:to_binary(Id)}).

-type docid() :: string_as_binary(_).

-type field_value() ::
    string_as_binary(_)
    | boolean()
    | number().

-type field_option() ::
    {string_as_binary(store), yes_or_no()}
    | {string_as_binary(boost), number()}
    | {string_as_binary(index), boolean()}
    | {string_as_binary(term_vector), yes_or_no()}
    | {string_as_binary(facet), boolean()}.

-type yes_or_no() :: string_as_binary(yes) | string_as_binary(no).

-spec update(
    Ref :: indexer_pid(), Id :: docid(), Fields :: [{field_name(), field_value(), [field_option()]}]
) ->
    ok.
update(Ref, Id, Fields) ->
    rpc(Ref, {update, Id, Fields}).

-spec cleanup(DbName :: string_as_binary(_)) -> ok.
cleanup(DbName) ->
    gen_server:cast({cleanup, clouseau()}, {cleanup, DbName}).

-spec rename(DbName :: string_as_binary(_)) -> ok.
rename(DbName) ->
    gen_server:cast({cleanup, clouseau()}, {rename, DbName}).

-type sig() :: string_as_binary(_).

%% `Sig` in `ActiveSigs` list here is a hashed version of an index function
%% and an analyzer represented in a Javascript function in a design document.
%% `Sig` is used to check if an index description is changed,
%% and the index needs to be reconstructed.
-spec cleanup(DbName :: string_as_binary(_), ActiveSigs :: [sig()]) ->
    ok.

cleanup(DbName, ActiveSigs) ->
    gen_server:cast({cleanup, clouseau()}, {cleanup, DbName, ActiveSigs}).

%% a binary with value <<"tokens">>
-type tokens_tag() :: string_as_binary(_).

% Example
%  {ok, {[
%      {<<"tokens">>, [
%          <<"ablanks@renovations.com">>
%      ]}
%  ]}}

-spec analyze(Analyzer :: analyzer(), Text :: string_as_binary(_)) ->
    {ok, [{tokens_tag(), [string_as_binary(_)]}]} | error().

analyze(Analyzer, Text) ->
    rpc({analyzer, clouseau()}, {analyze, Analyzer, Text}).

-spec version() -> string_as_binary(_).

version() ->
    rpc({main, clouseau()}, version).

-spec connected() -> boolean().

connected() ->
    HiddenNodes = erlang:nodes(hidden),
    case lists:member(clouseau(), HiddenNodes) of
        true ->
            true;
        false ->
            % We might have just booted up, so let's ping
            case net_adm:ping(clouseau()) of
                pong ->
                    % We can ping, but is the main process up?
                    %
                    % In versions 2.x (at least) this was a possibility
                    %  > clouseau_rpc:version().
                    %     {'EXIT',noconnection}
                    %
                    case (catch version()) of
                        {ok, _} -> true;
                        _ -> false
                    end;
                _ ->
                    false
            end
    end.

rpc(Ref, Msg) ->
    ioq:call_search(Ref, Msg, erlang:get(io_priority)).

clouseau() ->
    list_to_atom(config:get("dreyfus", "name", "clouseau@127.0.0.1")).
