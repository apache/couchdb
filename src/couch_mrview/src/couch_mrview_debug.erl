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

-module(couch_mrview_debug).

-export([
    help/0,
    help/1
]).

-export([
    view_signature/2,
    index_state/1,
    view_state/1,
    view_state/2,
    index_view_state/1,
    index_view_state/2,
    index_report/1,
    view_report/1,
    view_report/2,
    index_view_report/1,
    index_view_report/2,
    active_signatures/1,
    index_files/1,
    stale_index_files/1,
    purge_checkpoints/1,
    stale_purge_checkpoints/1
]).

-include_lib("couch_mrview/include/couch_mrview.hrl").

help() ->
    [
        view_signature,
        index_state,
        view_state,
        index_view_state,
        index_report,
        view_report,
        index_view_report,
        active_signatures,
        index_files,
        stale_index_files,
        purge_checkpoints,
        stale_purge_checkpoints
    ].

-spec help(Function :: atom()) -> ok.
%% erlfmt-ignore
help(view_signature) ->
    io:format("
    view_signature(ShardName, DDocName)
    --------------

    Returns a view signature for given ddoc for a given (non clustered) database.

    ---
    ", []);
help(index_state) ->
    io:format("
    index_state(Pid)
    --------------

    Pid: Pid of couch_index:init/1, specifically an mrview index.

    Returns a state map for an index that includes the following fields:
        - collator_versions
        - compact_running
        - db_name
        - idx_name
        - language
        - pending_updates
        - purge_seq
        - signature
        - sizes
        - update_options
        - update_seq
        - updater_running
        - view_file_path
        - waiting_clients
        - waiting_commit

    ---
    ", []);
help(view_state) ->
    io:format("
    view_state(PidOrIdxState)
    view_state(Pid, ViewName)
    view_state(IdxState, ViewName)
    --------------

    PidOrIdxState: Pid of couch_index:init/1, specifically an mrview index, or the state
    of an mrview index.
    Pid: Pid of couch_index:init/1, specifically an mrview index.
    IdxState: State of an mrview index (#mrst{} record).
    ViewName: Name of the view to be queried or undefined.

    Returns a state map for a ViewName if specified or all views if not that includes the
    following fields:
        - btree_size
        - def
        - id_num
        - options
        - purge_seq
        - reduce_funs
        - update_seq

    ---
    ", []);
help(index_view_state) ->
    io:format("
    index_view_state(Pid)
    index_view_state(Pid, ViewName)
    --------------

    Pid: Pid of couch_index:init/1, specifically an mrview index.
    ViewName: Name of the view to be queried or undefined.

    Returns a state map that includes the index state returned by index_state/1 and the view
    state returned by view_state/2. Like view_state/2, a ViewName can be specified or not.

    ---
    ", []);
help(index_report) ->
    io:format("
    index_report(Pid)
    --------------

    Pid: Pid of couch_index:init/1, specifically an mrview index.

    Prints a report for the index state of an mrview index that includes the following fields:
        - signature
        - db_name
        - idx_name
        - update_seq
        - purge_seq
        - view_file_path
        - pending_updates

    The output will look similar to:

        |info                 |                                                                                               value
        |  collator_versions  |                                                                                             153.112
        |  compact_running    |                                                                                               false
        |  db_name            |                                                            shards/00000000-ffffffff/dbv1.1658179540
        |  idx_name           |                                                                                    _design/dbv1ddoc
        |  language           |                                                                                          javascript
        |  pending_updates    |                                                                                                   0
        |  purge_seq          |                                                                                                   0
        |  signature          |                                                                    a967fb72089e71e870f790f32bcc6a55
        |  sizes              |                                                          {[{file,4264},{active,163},{external,51}]}
        |  update_options     |
        |  update_seq         |                                                                                                   3
        |  updater_running    |                                                                                               false
        |  view_file_path     |       .shards/00000000-ffffffff/dbv1.1658179540_design/mrview/a967fb72089e71e870f790f32bcc6a55.view
        |  waiting_clients    |                                                                                                   0
        |  waiting_commit     |                                                                                               false

    ---
    ", []);
help(view_report) ->
    io:format("
    view_report(PidOrIdxState)
    view_report(Pid, ViewName)
    view_report(IdxState, ViewName)
    --------------

    PidOrIdxState: Pid of couch_index:init/1, specifically an mrview index, or the state
    of an mrview index.
    Pid: Pid of couch_index:init/1, specifically an mrview index.
    IdxState: State of an mrview index (#mrst{} record).
    ViewName: Name of the view to be queried or undefined.

    Prints a report for a ViewName if specified or all views if not that includes the following
    fields:
        - id_num
        - update_seq
        - purge_seq
        - reduce_funs
        - def
        - btree_size
        - options

    The output will look similar to:

        dbv1view
        |info           |                                                                                               value
        |  btree_size   |                                                                                                  51
        |  def          |                                                                     function(doc){emit(doc.id, 1);}
        |  id_num       |                                                                                                   0
        |  options      |
        |  purge_seq    |                                                                                                   0
        |  reduce_funs  |
        |  update_seq   |                                                                                                   3
        dbv2view
        |info           |                                                                                               value
        |  btree_size   |                                                                                                  50
        |  def          |                                                                     function(doc){emit(doc.id, 2);}
        |  id_num       |                                                                                                   1
        |  options      |
        |  purge_seq    |                                                                                                   0
        |  reduce_funs  |                                                                                                _sum
        |  update_seq   |                                                                                                   3

    ---
    ", []);
help(index_view_report) ->
    io:format("
    index_view_report(Pid)
    index_view_report(Pid, ViewName)
    --------------

    Pid: Pid of couch_index:init/1, specifically an mrview index.
    ViewName: Name of the view to be queried or undefined.

    Prints a report for the index state and views of an mrview index. The report includes the following
    fields for an index state:
        - signature
        - db_name
        - idx_name
        - update_seq
        - purge_seq
        - view_file_path
        - pending_updates
    The report also includes the following fields for a ViewName if specified or all views if not:
        - id_num
        - update_seq
        - purge_seq
        - reduce_funs
        - def
        - btree_size
        - options

    The output will look similar to:

        |info                 |                                                                                               value
        |  collator_versions  |                                                                                             153.112
        |  compact_running    |                                                                                               false
        |  db_name            |                                                            shards/00000000-ffffffff/dbv1.1658179540
        |  idx_name           |                                                                                    _design/dbv1ddoc
        |  language           |                                                                                          javascript
        |  pending_updates    |                                                                                                   0
        |  purge_seq          |                                                                                                   0
        |  signature          |                                                                    a967fb72089e71e870f790f32bcc6a55
        |  sizes              |                                                          {[{file,4264},{active,163},{external,51}]}
        |  update_options     |
        |  update_seq         |                                                                                                   3
        |  updater_running    |                                                                                               false
        |  view_file_path     |       .shards/00000000-ffffffff/dbv1.1658179540_design/mrview/a967fb72089e71e870f790f32bcc6a55.view
        |  waiting_clients    |                                                                                                   0
        |  waiting_commit     |                                                                                               false
        dbv1view
        |info           |                                                                                               value
        |  btree_size   |                                                                                                  51
        |  def          |                                                                     function(doc){emit(doc.id, 1);}
        |  id_num       |                                                                                                   0
        |  options      |
        |  purge_seq    |                                                                                                   0
        |  reduce_funs  |
        |  update_seq   |                                                                                                   3
        dbv2view
        |info           |                                                                                               value
        |  btree_size   |                                                                                                  50
        |  def          |                                                                     function(doc){emit(doc.id, 2);}
        |  id_num       |                                                                                                   1
        |  options      |
        |  purge_seq    |                                                                                                   0
        |  reduce_funs  |                                                                                                _sum
        |  update_seq   |                                                                                                   3

    ---
    ", []);
help(active_signatures) ->
    io:format("
    active_signatures(DbName)
    -------------------------

    DbName: Database name as list or binary

    Returns the list of all view signatures.
    ---
    ", []);
help(index_files) ->
    io:format("
    index_files(DbName)
    -------------------------

    DbName: Database name as list or binary

    Returns the list of all view index files, both active and inactive.
    ---
    ", []);
help(stale_index_files) ->
    io:format("
    stale_index_files(DbName)
    -------------------------

    DbName: Database name as list or binary

    Returns the list of stale (inactive) index files.
    ---
    ", []);
help(purge_checkpoints) ->
    io:format("
    purge_checkpoints(DbName)
    -------------------------

    DbName: Database name as list or binary

    Returns the list of all the view purge checkpoint doc ids.
    ---
    ", []);
help(stale_purge_checkpoints) ->
    io:format("
    stale_purge_checkpoints(DbName)
    -------------------------

    DbName: Database name as list or binary

    Returns the list of stale (inactive) view purge checkpoint doc ids.
    ---
    ", []);
help(Unknown) ->
    io:format("Unknown function: `~p`. Please try one of the following:~n", [Unknown]),
    [io:format("    - ~s~n", [Function]) || Function <- help()],
    io:format("    ---~n", []),
    ok.

view_signature(DbName, DDocName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, DDoc} = couch_db:open_doc_int(Db, <<"_design/", DDocName/binary>>, []),
    {ok, IdxState} = couch_mrview_util:ddoc_to_mrst(DDocName, DDoc),
    couch_util:to_hex(IdxState#mrst.sig).

index_state(Pid) when is_pid(Pid) ->
    {ok, IdxState} = couch_index:get_state(Pid, 0),
    case IdxState of
        #mrst{} ->
            {ok, Info} = couch_index:get_info(Pid),
            Sig = IdxState#mrst.sig,
            DbName = IdxState#mrst.db_name,
            State =
                Info ++
                    [
                        {signature, Sig},
                        {db_name, DbName},
                        {idx_name, IdxState#mrst.idx_name},
                        {view_file_path, couch_mrview_util:index_file(DbName, Sig)}
                    ],
            {ok, maps:from_list(State)};
        _ ->
            {error, not_mrview_index}
    end.

view_state(PidOrIdxState) ->
    view_state(PidOrIdxState, undefined).

view_state(Pid, ViewName) when is_pid(Pid) ->
    {ok, IdxState} = couch_index:get_state(Pid, 0),
    view_state(IdxState, ViewName);
view_state(IdxState, ViewName) ->
    case IdxState of
        #mrst{} ->
            MrViews = lists:foldl(
                fun(MrView, Acc) ->
                    {Name, ReduceFuns} =
                        case MrView#mrview.reduce_funs of
                            [] ->
                                {hd(MrView#mrview.map_names), []};
                            _ ->
                                % reduce_funs contains tuples of {Name, ReduceFuns}
                                hd(MrView#mrview.reduce_funs)
                        end,
                    View = #{
                        id_num => MrView#mrview.id_num,
                        update_seq => MrView#mrview.update_seq,
                        purge_seq => MrView#mrview.purge_seq,
                        reduce_funs => ReduceFuns,
                        def => MrView#mrview.def,
                        btree_size => couch_btree:size(MrView#mrview.btree),
                        options => MrView#mrview.options
                    },
                    maps:put(Name, View, Acc)
                end,
                #{},
                IdxState#mrst.views
            ),
            case ViewName of
                undefined ->
                    {ok, MrViews};
                _ ->
                    case maps:get(ViewName, MrViews) of
                        {badkey, Key} ->
                            io:format("No view named ~p was found.", [Key]),
                            {error, {badkey, Key}};
                        Value ->
                            {ok, #{ViewName => Value}}
                    end
            end;
        _ ->
            {error, not_mrview_index}
    end.

index_view_state(Pid) when is_pid(Pid) ->
    index_view_state(Pid, undefined).

index_view_state(Pid, ViewName) when is_pid(Pid) ->
    {ok, IdxState} = index_state(Pid),
    {ok, ViewState} = view_state(Pid, ViewName),
    {ok, maps:put(views, ViewState, IdxState)}.

index_report(Pid) when is_pid(Pid) ->
    case index_state(Pid) of
        {ok, IdxState} ->
            Sig = maps:get(signature, IdxState),
            IdxState2 = maps:put(signature, couch_util:to_hex(Sig), IdxState),
            % Convert collator versions to strings to print pretty
            IdxState3 = convert_collator_versions_to_strings(IdxState2),
            IdxState4 = maps:update_with(view_file_path, fun format_view_path/1, IdxState3),
            couch_debug:print_report_with_info_width(maps:to_list(IdxState4), 21);
        Error ->
            Error
    end.

view_report(PidOrIdxState) ->
    view_report(PidOrIdxState, undefined).

view_report(Pid, ViewName) when is_pid(Pid) ->
    {ok, IdxState} = couch_index:get_state(Pid, 0),
    view_report(IdxState, ViewName);
view_report(IdxState, ViewName) ->
    case view_state(IdxState, ViewName) of
        {ok, ViewState} ->
            lists:foreach(
                fun({Name, Info}) ->
                    io:format("~s~n", [binary_to_list(Name)]),
                    couch_debug:print_report_with_info_width(maps:to_list(Info), 15)
                end,
                maps:to_list(ViewState)
            );
        Error ->
            Error
    end.

index_view_report(Pid) when is_pid(Pid) ->
    index_view_report(Pid, undefined).
index_view_report(Pid, ViewName) when is_pid(Pid) ->
    case index_view_state(Pid) of
        {ok, State} ->
            AllViews = maps:get(views, State),
            Views =
                case ViewName of
                    undefined ->
                        AllViews;
                    _ ->
                        #{ViewName => maps:get(ViewName, AllViews)}
                end,
            Sig = maps:get(signature, State),
            State2 = maps:put(signature, couch_util:to_hex(Sig), State),
            % Convert collator versions to strings to print pretty
            State3 = convert_collator_versions_to_strings(State2),
            State4 = maps:update_with(view_file_path, fun format_view_path/1, State3),
            couch_debug:print_report_with_info_width(
                maps:to_list(maps:without([views], State4)), 21
            ),
            lists:foreach(
                fun({Name, Info}) ->
                    io:format("~s~n", [binary_to_list(Name)]),
                    couch_debug:print_report_with_info_width(maps:to_list(Info), 15)
                end,
                maps:to_list(Views)
            );
        Error ->
            Error
    end.

convert_collator_versions_to_strings(State) ->
    CollatorVersions = lists:map(
        fun(Version) ->
            binary_to_list(Version)
        end,
        maps:get(collator_versions, State)
    ),
    maps:put(collator_versions, CollatorVersions, State).

active_signatures(DbName) when is_list(DbName) ->
    active_signatures(list_to_binary(DbName));
active_signatures(DbName) when is_binary(DbName) ->
    [binary_to_list(S) || S <- sigs(DbName)].

index_files(DbName) when is_list(DbName) ->
    index_files(list_to_binary(DbName));
index_files(DbName) when is_binary(DbName) ->
    ShardNames = shard_names(DbName),
    Files = get_index_files(ShardNames, #{}),
    [format_view_path(F) || F <- lists:sort(Files)].

stale_index_files(DbName) when is_list(DbName) ->
    index_files(list_to_binary(DbName));
stale_index_files(DbName) when is_binary(DbName) ->
    ShardNames = shard_names(DbName),
    Files = get_index_files(ShardNames, get_sigs(ShardNames)),
    [format_view_path(F) || F <- lists:sort(Files)].

purge_checkpoints(DbName) when is_list(DbName) ->
    purge_checkpoints(list_to_binary(DbName));
purge_checkpoints(DbName) when is_binary(DbName) ->
    ShardNames = shard_names(DbName),
    Purges = get_purge_checkpoints(ShardNames, #{}),
    [binary_to_list(DocId) || DocId <- lists:sort(Purges)].

stale_purge_checkpoints(DbName) when is_list(DbName) ->
    purge_checkpoints(list_to_binary(DbName));
stale_purge_checkpoints(DbName) when is_binary(DbName) ->
    ShardNames = shard_names(DbName),
    Purges = get_purge_checkpoints(ShardNames, get_sigs(ShardNames)),
    [binary_to_list(DocId) || DocId <- lists:sort(Purges)].

get_purge_checkpoints([], #{}) ->
    [];
get_purge_checkpoints([_ | _] = Dbs, #{} = Sigs) ->
    AllPurges = lists:map(fun couch_mrview_util:get_purge_checkpoints/1, Dbs),
    Fun = fun(Purges, Acc) ->
        Filtered = maps:without(maps:keys(Sigs), Purges),
        maps:values(Filtered) ++ Acc
    end,
    lists:foldl(Fun, [], AllPurges).

get_index_files([], #{}) ->
    [];
get_index_files([_ | _] = Dbs, #{} = Sigs) ->
    AllIdxs = lists:map(fun couch_mrview_util:get_index_files/1, Dbs),
    Fun = fun(Idxs, Acc) ->
        Filtered = maps:without(maps:keys(Sigs), Idxs),
        maps:values(Filtered) ++ Acc
    end,
    lists:foldl(Fun, [], AllIdxs).

get_sigs([]) ->
    #{};
get_sigs([AnyShard | _]) ->
    couch_mrview_util:get_signatures(AnyShard).

shard_names(DbName) ->
    [mem3:name(S) || S <- mem3:local_shards(DbName)].

sigs(DbName) ->
    lists:sort(maps:keys(get_sigs(shard_names(DbName)))).

format_view_path(ViewFilePath) ->
    BaseDir = config:get("couchdb", "view_index_dir"),
    lists:flatten(string:replace(ViewFilePath, BaseDir ++ "/", "")).
