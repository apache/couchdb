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

    open_db/1,
    open_db/2,

    get_db_info/1,
    get_doc_count/1,
    get_doc_count/2,

    %% get_revs_limit/1,
    %% set_revs_limit/3,

    get_security/1,
    set_security/2,

    %% get_purge_infos_limit/1,
    %% set_purge_infos_limit/3,
    %%
    %% compact/1,
    %% compact/2,
    %%
    %% get_partition_info/2,
    %%
    % Documents
    open_doc/3,
    open_revs/4,
    %%
    %% get_doc_info/3,
    %% get_full_doc_info/3,
    %%
    %% get_missing_revs/2,
    %% get_missing_revs/3,

    update_doc/3,
    update_docs/3

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
            fabric2_db:init(DbName, Tx, DbDir)
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


open_db(DbName) ->
    open_db(DbName, []).


open_db(DbName, Options) ->
    fabric2_db:open(DbName, Options).


get_db_info(DbName) when is_binary(DbName) ->
    get_db_info(open_db(DbName));

get_db_info(Db) ->
    DbProps = fabric2_db:with_tx(Db, fun(TxDb) ->
        {CStart, CEnd} = fabric2_db:range_bounds(TxDb, {<<"changes">>}),
        ChangesFuture = fabric2_db:get_range(TxDb, {CStart}, {CEnd}, [
            {streaming_mode, exact},
            {limit, 1},
            {reverse, true}
        ]),

        StatsPrefix = {<<"meta">>, <<"stats">>},
        MetaFuture = fabric2_db:get_range_startswith(TxDb, StatsPrefix),

        RawSeq = case erlfdb:wait(ChangesFuture) of
            [] ->
                <<0:80>>;
            [{SeqKey, _}] ->
                {<<"changes">>, SeqBin} = fabric2_db:unpack(TxDb, SeqKey),
                SeqBin
        end,
        CProp = {update_seq, ?l2b(couch_util:to_hex(RawSeq))},

        MProps = lists:flatmap(fun({K, V}) ->
            case fabric2_db:unpack(TxDb, K) of
                {_, _, <<"doc_count">>} ->
                    [{doc_count, ?bin2uint(V)}];
                {_, _, <<"doc_del_count">>} ->
                    [{doc_del_count, ?bin2uint(V)}];
                {_, _, <<"size">>} ->
                    Val = ?bin2uint(V),
                    [
                        {other, {[{data_size, Val}]}},
                        {sizes, {[
                            {active, 0},
                            {external, Val},
                            {file, 0}
                        ]}}
                    ];
                _ ->
                    []
            end
        end, erlfdb:wait(MetaFuture)),

        [CProp | MProps]
    end),

    BaseProps = [
        {cluster, {[{n, 0}, {q, 0}, {r, 0}, {w, 0}]}},
        {compact_running, false},
        {data_size, 0},
        {db_name, fabric2_db:name(Db)},
        {disk_format_version, 0},
        {disk_size, 0},
        {instance_start_time, <<"0">>},
        {purge_seq, 0}
    ],

    {ok, lists:foldl(fun({Key, Val}, Acc) ->
        lists:keystore(Key, 1, Acc, {Key, Val})
    end, BaseProps, DbProps)}.


get_doc_count(DbName) ->
    get_doc_count(DbName, <<"_all_docs">>).


get_doc_count(DbName, <<"_all_docs">>) ->
    get_doc_count(DbName, <<"doc_count">>);

get_doc_count(DbName, <<"_design">>) ->
    get_doc_count(DbName, <<"doc_design_count">>);

get_doc_count(DbName, <<"_local">>) ->
    get_doc_count(DbName, <<"doc_local_count">>);

get_doc_count(Db, Key) ->
    fabric2_db:with_tx(Db, fun(TxDb) ->
        Future = fabric2_db:get(TxDb, {<<"meta">>, <<"stats">>, Key}),
        ?bin2uint(erlfdb:wait(Future))
    end).


get_security(DbName) when is_binary(DbName) ->
    get_security(open_db(DbName));

get_security(Db) ->
    Key = {<<"meta">>, <<"config">>, <<"security_doc">>},
    SecJson = fabric2_db:with_tx(Db, fun(TxDb) ->
        erlfdb:wait(fabric2_db:get(TxDb, Key))
    end),
    ?JSON_DECODE(SecJson).


set_security(DbName, ErlJson) when is_binary(DbName) ->
    set_security(open_db(DbName), ErlJson);

set_security(Db, ErlJson) ->
    Key = {<<"meta">>, <<"config">>, <<"security_doc">>},
    SecJson = ?JSON_ENCODE(ErlJson),
    fabric2_db:with_tx(Db, fun(TxDb) ->
        fabric2_db:set(TxDb, Key, SecJson)
    end).


open_doc(Db, DocId, Options) ->
    fabric2_db:with_tx(Db, fun(TxDb) ->
        case fabric2_doc:get_fdi(TxDb, DocId) of
            not_found ->
                {not_found, missing};
            #full_doc_info{} = FDI ->
                {_, Path} = couch_doc:to_doc_info_path(FDI),
                case fabric2_doc:open(TxDb, DocId, Path) of
                    #doc{} = Doc -> {ok, Doc};
                    Error -> Error
                end
        end
    end).


open_revs(Db, DocId, Revs, Options) ->
    fabric2_db:with_tx(Db, fun(TxDb) ->
        case fabrci2_doc:get_fdi(TxDb, DocId) of
            not_found ->
                {not_found, missing};
            #full_doc_info{} = FDI ->
                case fabric2_doc:open_revs(TxDb, FDI, Revs, Options) of
                    [_ | _] = Opened -> {ok, Opened};
                    Error -> Error
                end
        end
    end).


update_doc(Db, Doc, Options) ->
    fabric2_db:with_tx(Db, fun(TxDb) ->
        case fabric2_doc:update(TxDb, Doc, opts(Options)) of
            {ok, []} ->
                % replication no-op
                #doc{revs = {Pos, [RevId | _]}} = doc(Db, Doc),
                {ok, {Pos, RevId}};
            {ok, NewRev} ->
                {ok, NewRev};
           {error, Error} ->
               throw(Error)
        end
    end).


update_docs(DbName, Docs, Options) when is_binary(DbName) ->
    update_docs(open_db(DbName, Options), Docs, Options);

update_docs(Db, Docs, Options) ->
    fabric2_db:with_tx(Db, fun(TxDb) ->
        {Resps, Status} = lists:mapfoldl(fun(Doc, Acc) ->
            case fabric2_doc:update(TxDb, Doc, opts(Options)) of
                {ok, _} = Resp ->
                    {Resp, Acc};
                {error, _} = Resp ->
                    {Resp, error}
            end
        end, ok, Docs),
        {Status, Resps}
    end).


docs(Db, Docs) ->
    lists:map(fun(Doc) -> doc(Db, Doc) end, Docs).


doc(_Db, #doc{} = Doc) ->
    Doc;

doc(Db, {_} = Doc) ->
    couch_db:doc_from_json_obj_validate(Db, Doc);

doc(_Db, Doc) ->
    erlang:error({illegal_doc_format, Doc}).


opts(Options) ->
    lists:foldl(fun(Opt, Acc) ->
        add_option(Opt, Acc)
    end, Options, [user_ctx, io_priority]).


add_option(Key, Options) ->
    case couch_util:get_value(Key, Options) of
        undefined ->
            case erlang:get(Key) of
                undefined ->
                    Options;
                Value ->
                    [{Key, Value} | Options]
            end;
        _ ->
            Options
    end.
