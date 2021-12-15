% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(smoosh_priority_queue).

-export([new/1]).

-export([open/1, last_updated/2, is_key/2, in/4, in/5, out/1, size/1, info/1]).

-export([flush/1, close/1]).

-export([from_list/2, to_list/1]).

-export([file_name/1, write_to_file/1]).

-define(VSN, 1).

-record(priority_queue, {
    name,
    map,
    tree
}).

new(Name) ->
    #priority_queue{name = Name, map = maps:new(), tree = gb_trees:empty()}.

open(#priority_queue{name = Name} = Q) ->
    case do_open(file_name(Q)) of
        {ok, Terms} ->
            Tree = maps:fold(
                fun(Key, {TreeKey, Value}, TreeAcc) ->
                    gb_trees:enter(TreeKey, {Key, Value}, TreeAcc)
                end,
                gb_trees:empty(),
                Terms
            ),
            #priority_queue{name = Name, map = Terms, tree = Tree};
        error ->
            Q
    end.

write_to_file(#priority_queue{map = Map} = Q) ->
    OnDisk = <<?VSN, (erlang:term_to_binary(Map, [compressed, {minor_version, 1}]))/binary>>,
    FileName = file_name(Q),
    TmpFileName = FileName ++ ".tmp",
    file:delete(TmpFileName),
    ok = file:write_file(TmpFileName, OnDisk, [sync]),
    file:delete(FileName),
    ok = file:rename(TmpFileName, FileName).

flush(#priority_queue{name = Name} = Q) ->
    file:delete(file_name(Q)),
    Q#priority_queue{name = Name, map = maps:new(), tree = gb_trees:empty()}.

close(#priority_queue{} = Q) ->
    file:delete(file_name(Q)).

last_updated(Key, #priority_queue{map = Map}) ->
    case maps:find(Key, Map) of
        {ok, {_Priority, {LastUpdatedMTime, _MInt}}} ->
            LastUpdatedMTime;
        error ->
            false
    end.

is_key(Key, #priority_queue{map = Map}) ->
    maps:is_key(Key, Map).

in(Key, Value, Priority, Q) ->
    in(Key, Value, Priority, infinity, Q).

in(Key, Value, Priority, Capacity, #priority_queue{name = Name, map = Map, tree = Tree}) ->
    Tree1 =
        case maps:find(Key, Map) of
            {ok, TreeKey} ->
                gb_trees:delete_any(TreeKey, Tree);
            error ->
                Tree
        end,
    Now = {erlang:monotonic_time(), erlang:unique_integer([monotonic])},
    TreeKey1 = {Priority, Now},
    Tree2 = gb_trees:enter(TreeKey1, {Key, Value}, Tree1),
    Map1 = maps:put(Key, TreeKey1, Map),
    truncate(Capacity, #priority_queue{name = Name, map = Map1, tree = Tree2}).

out(#priority_queue{name = Name, map = Map, tree = Tree}) ->
    case gb_trees:is_empty(Tree) of
        true ->
            false;
        false ->
            {_, {Key, Value}, Tree1} = gb_trees:take_largest(Tree),
            Map1 = maps:remove(Key, Map),
            Q = #priority_queue{name = Name, map = Map1, tree = Tree1},
            {Key, Value, Q}
    end.

size(#priority_queue{tree = Tree}) ->
    gb_trees:size(Tree).

info(#priority_queue{tree = Tree} = Q) ->
    [
        {size, ?MODULE:size(Q)}
        | case gb_trees:is_empty(Tree) of
            true ->
                [];
            false ->
                {Min, _, _} = gb_trees:take_smallest(Tree),
                {Max, _, _} = gb_trees:take_largest(Tree),
                [{min, Min}, {max, Max}]
        end
    ].

from_list(Orddict, #priority_queue{name = Name}) ->
    Map = maps:from_list(Orddict),
    Tree = gb_trees:from_orddict(Orddict),
    #priority_queue{name = Name, map = Map, tree = Tree}.

to_list(#priority_queue{tree = Tree}) ->
    gb_trees:to_list(Tree).

file_name(#priority_queue{name = Name}) ->
    filename:join(config:get("smoosh", "state_dir", "."), Name ++ ".queue").

truncate(infinity, Q) ->
    Q;
truncate(Capacity, Q) when Capacity > 0 ->
    truncate(Capacity, ?MODULE:size(Q), Q).

truncate(Capacity, Size, Q) when Size =< Capacity ->
    Q;
truncate(Capacity, Size, #priority_queue{name = Name, map = Map, tree = Tree}) when Size > 0 ->
    {_, {Key, _}, Tree1} = gb_trees:take_smallest(Tree),
    Q1 = #priority_queue{name = Name, map = maps:remove(Key, Map), tree = Tree1},
    truncate(Capacity, ?MODULE:size(Q1), Q1).

do_open(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            <<Vsn, Binary/binary>> = Content,
            try parse_queue(Vsn, ?VSN, Binary) of
                Bin ->
                    {ok, Bin}
            catch
                error:Reason ->
                    couch_log:error(
                        "~p Invalid queue file (~p). Deleting ~s", [?MODULE, Reason, FilePath]
                    ),
                    file:delete(FilePath),
                    error
            end;
        {error, enoent} ->
            couch_log:notice(
                "~p (~p) Queue file ~s does not exist. Not restoring.", [?MODULE, enoent, FilePath]
            ),
            error;
        {error, Reason} ->
            couch_log:error(
                "~p Cannot read the queue file (~p). Deleting ~s", [?MODULE, Reason, FilePath]
            ),
            file:delete(FilePath),
            error
    end.

parse_queue(1, ?VSN, Binary) ->
    erlang:binary_to_term(Binary, [safe]);
parse_queue(Vsn, ?VSN, _) ->
    error({unsupported_version, Vsn}).
