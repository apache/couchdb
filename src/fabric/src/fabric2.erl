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
    create_db/2

    %% delete_db/1,
    %% delete_db/2,
    %%
    %% get_db_info/1,
    %% get_doc_count/1,
    %% get_doc_count/2,
    %%
    %% get_revs_limit/1,
    %% set_revs_limit/3,
    %%
    %% get_security/1,
    %% get_security/2,
    %% get_all_security/1,
    %% get_all_security/2,
    %% set_security/2,
    %% set_security/3,
    %%
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


init_db(Tx, DbDir) ->
    Defaults = [
        {{<<"meta">>, <<"config">>, <<"revs_limit">>}, 1000},
        {{<<"meta">>, <<"config">>, <<"security_doc">>}, ?DEFAULT_SECURITY},
        {{<<"meta">>, <<"stats">>, <<"doc_count">>}, 0},
        {{<<"meta">>, <<"stats">>, <<"doc_del_count">>}, 0},
        {{<<"meta">>, <<"stats">>, <<"size">>}, 2}
    ],
    lists:foreach(fun({K, V}) ->
        BinKey = erlfdb_directory:pack(DbDir, K),
        BinVal = erlfdb_tuple:pack({V}),
        erlfdb:set(Tx, BinKey, BinVal)
    end, Defaults).

