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

-module(fabric2).


-export([
    %% % Databases
    %% all_dbs/0,
    %% all_dbs/1,

    create_db/1,
    create_db/2,

    delete_db/1,
    delete_db/2,

    get_db_info/1,
    get_doc_count/1,
    get_doc_count/2,

    %% get_revs_limit/1,
    %% set_revs_limit/3,

    get_security/1,
    set_security/2

    %% get_purge_infos_limit/1,
    %% set_purge_infos_limit/3,
    %%
    %% compact/1,
    %% compact/2,
    %%
    %% get_partition_info/2,
    %%
    %% % Documents
    %% open_doc/3,
    %% open_revs/4,
    %%
    %% get_doc_info/3,
    %% get_full_doc_info/3,
    %%
    %% get_missing_revs/2,
    %% get_missing_revs/3,
    %%
    %% update_doc/3,
    %% update_docs/3,
    %% purge_docs/3,
    %%
    %% att_receiver/2,
    %%
    %% % Views
    %% all_docs/4,
    %% all_docs/5,
    %%
    %% changes/4,
    %% end_changes/0,
    %%
    %% query_view/3,
    %% query_view/4,
    %% query_view/6,
    %% query_view/7,
    %% get_view_group_info/2,
    %%
    %% % Miscellany
    %% design_docs/1,
    %%
    %% reset_validation_funs/1,
    %%
    %% cleanup_index_files/0,
    %% cleanup_index_files/1,
    %% cleanup_index_files_all_nodes/1,
    %%
    %% dbname/1
]).


-include_lib("fabric/include/fabric.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(DEFAULT_SECURITY, <<"{}">>).


create_db(DbName) ->
    create_db(DbName, []).


create_db(DbName, _Options) ->
    DbsDir = fabric_server:get_dir(dbs),
    fabric_server:transactional(fun(Tx) ->
        try
            DbDir = erlfdb_directory:create(Tx, DbsDir, DbName),
            init_db(Tx, DbDir)
        catch error:{erlfdb_directory, {create_error, path_exists, _}} ->
            {error, file_exists}
        end
    end).


delete_db(DbName) ->
    delete_db(DbName, []).


delete_db(DbName, _Options) ->
    try
        DbsDir = fabric_server:get_dir(dbs),
        fabric_server:transactional(fun(Tx) ->
            erlfdb_directory:remove(Tx, DbsDir, DbName)
        end)
    catch error:{erlfdb_directory, {remove_error, path_missing, _}} ->
        erlang:error(database_does_not_exist)
    end.


get_db_info(DbName) ->
    [DbDir, MetaRows, LastChangeRow] = fabric_server:transactional(fun(Tx) ->
        DbDir = open_db(Tx, DbName),
        Meta = erlfdb_directory:pack(DbDir, {<<"meta">>, <<"stats">>}),
        MetaFuture = erlfdb:get_range_startswith(Tx, Meta),
        {CStart, CEnd} = erlfdb_directory:range(DbDir, {<<"changes">>}),
        ChangesFuture = erlfdb:get_range(Tx, CStart, CEnd, [
                {streaming_mode, exact},
                {limit, 1},
                {reverse, true}
            ]),
        [DbDir] ++ erlfdb:wait_for_all([MetaFuture, ChangesFuture])
    end),
    BaseProps = [
        {cluster, {[{n, 0}, {q, 0}, {r, 0}, {w, 0}]}},
        {compact_running, false},
        {data_size, 0},
        {db_name, DbName},
        {disk_format_version, 0},
        {disk_size, 0},
        {instance_start_time, <<"0">>},
        {purge_seq, 0}
    ],
    WithMeta = lists:foldl(fun({KBin, VBin}, PropAcc) ->
        case erlfdb_directory:unpack(DbDir, KBin) of
            {_, _, <<"doc_count">>} ->
                Val = ?bin2uint(VBin),
                lists:keystore(doc_count, 1, PropAcc, {doc_count, Val});
            {_, _, <<"doc_del_count">>} ->
                Val = ?bin2uint(VBin),
                lists:keystore(doc_del_count, 1, PropAcc, {doc_del_count, Val});
            {_, _, <<"size">>} ->
                Val = ?bin2uint(VBin),
                Other = {other, {[{data_size, Val}]}},
                Sizes = {sizes, {[
                        {active, 0},
                        {external, Val},
                        {file, 0}
                    ]}},
                PA1 = lists:keystore(other, 1, PropAcc, Other),
                lists:keystore(sizes, 1, PA1, Sizes);
            _ ->
                PropAcc
        end
    end, BaseProps, MetaRows),
    RawSeq = case LastChangeRow of
        [] ->
            <<0:80>>;
        [{KBin, _}] ->
            {<<"changes">>, SeqBin} = erlfdb_directory:unpack(DbDir, KBin),
            SeqBin
    end,
    Seq = ?l2b(couch_util:to_hex(RawSeq)),
    {ok, lists:keystore(update_seq, 1, WithMeta, {update_seq, Seq})}.


get_doc_count(DbName) ->
    get_doc_count(DbName, <<"_all_docs">>).


get_doc_count(DbName, <<"_all_docs">>) ->
    get_doc_count(DbName, <<"doc_count">>);

get_doc_count(DbName, <<"_design">>) ->
    get_doc_count(DbName, <<"doc_design_count">>);

get_doc_count(DbName, <<"_local">>) ->
    get_doc_count(DbName, <<"doc_local_count">>);

get_doc_count(DbName, Key) ->
    fabric_server:transactional(fun(Tx) ->
        DbDir = open_db(Tx, DbName),
        Key = erlfdb_directory:pack(DbDir, {<<"meta">>, <<"stats">>, Key}),
        VBin = erlfdb:wait(erlfdb:get(Tx, Key)),
        ?bin2uint(VBin)
    end).


get_security(DbName) ->
    SecJson = fabric_server:transactional(fun(Tx) ->
        DbDir = open_db(Tx, DbName),
        Tuple = {<<"meta">>, <<"config">>, <<"security_doc">>},
        Key = erlfdb_directory:pack(DbDir, Tuple),
        erlfdb:wait(erlfdb:get(Tx, Key))
    end),
    ?JSON_DECODE(SecJson).


set_security(DbName, ErlJson) ->
    SecJson = ?JSON_ENCODE(ErlJson),
    fabric_server:transactional(fun(Tx) ->
        DbDir = open_db(Tx, DbName),
        Tuple = {<<"meta">>, <<"config">>, <<"security_doc">>},
        Key = erlfdb_directory:pack(DbDir, Tuple),
        erlfdb:set(Tx, Key, SecJson)
    end).


init_db(Tx, DbDir) ->
    Defaults = [
        {{<<"meta">>, <<"config">>, <<"revs_limit">>}, ?uint2bin(1000)},
        {{<<"meta">>, <<"config">>, <<"security_doc">>}, ?DEFAULT_SECURITY},
        {{<<"meta">>, <<"stats">>, <<"doc_count">>}, ?uint2bin(0)},
        {{<<"meta">>, <<"stats">>, <<"doc_del_count">>}, ?uint2bin(0)},
        {{<<"meta">>, <<"stats">>, <<"doc_design_count">>}, ?uint2bin(0)},
        {{<<"meta">>, <<"stats">>, <<"doc_local_count">>}, ?uint2bin(0)},
        {{<<"meta">>, <<"stats">>, <<"size">>}, ?uint2bin(2)}
    ],
    lists:foreach(fun({K, V}) ->
        BinKey = erlfdb_directory:pack(DbDir, K),
        erlfdb:set(Tx, BinKey, V)
    end, Defaults).


open_db(Tx, DbName) ->
    % We'll eventually want to cache this in the
    % fabric_server ets table.
    DbsDir = fabric_server:get_dir(dbs),
    try
        erlfdb_directory:open(Tx, DbsDir, DbName)
    catch error:{erlfdb_directory, {open_error, path_missing, _}} ->
        erlang:error(database_does_not_exist)
    end.
