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

-module(dreyfus_rpc).
-include_lib("couch/include/couch_db.hrl").
-include("dreyfus.hrl").
-import(couch_query_servers, [get_os_process/1, ret_os_process/1, proc_prompt/2]).

% public api.
-export([search/4, group1/4, group2/4, info/3, disk_size/3]).

% private callback
-export([call/5, info_int/3]).

search(DbName, DDoc, IndexName, QueryArgs) ->
    MFA = {?MODULE, call, [search, DbName, DDoc, IndexName, QueryArgs]},
    dreyfus_util:time([rpc, search], MFA).

group1(DbName, DDoc, IndexName, QueryArgs) ->
    MFA = {?MODULE, call, [group1, DbName, DDoc, IndexName, QueryArgs]},
    dreyfus_util:time([rpc, group1], MFA).

group2(DbName, DDoc, IndexName, QueryArgs) ->
    MFA = {?MODULE, call, [group2, DbName, DDoc, IndexName, QueryArgs]},
    dreyfus_util:time([rpc, group2], MFA).

call(Fun, DbName, DDoc, IndexName, QueryArgs0) ->
    QueryArgs = dreyfus_util:upgrade(QueryArgs0),
    erlang:put(io_priority, {search, DbName}),
    check_interactive_mode(),
    {ok, Db} = get_or_create_db(DbName, []),
    #index_query_args{
        stale = Stale
    } = QueryArgs,
    {_LastSeq, MinSeq} = calculate_seqs(Db, Stale),
    case dreyfus_index:design_doc_to_index(DDoc, IndexName) of
        {ok, Index} ->
            case dreyfus_index_manager:get_index(DbName, Index) of
                {ok, Pid} ->
                    case dreyfus_index:await(Pid, MinSeq) of
                        {ok, IndexPid, _Seq} ->
                            Result = dreyfus_index:Fun(IndexPid, QueryArgs),
                            rexi:reply(Result);
                        % obsolete clauses, remove after upgrade
                        ok ->
                            Result = dreyfus_index:Fun(Pid, QueryArgs),
                            rexi:reply(Result);
                        {ok, _Seq} ->
                            Result = dreyfus_index:Fun(Pid, QueryArgs),
                            rexi:reply(Result);
                        Error ->
                            rexi:reply(Error)
                    end;
                Error ->
                    rexi:reply(Error)
            end;
        Error ->
            rexi:reply(Error)
    end.

info(DbName, DDoc, IndexName) ->
    MFA = {?MODULE, info_int, [DbName, DDoc, IndexName]},
    dreyfus_util:time([rpc, info], MFA).

info_int(DbName, DDoc, IndexName) ->
    erlang:put(io_priority, {search, DbName}),
    check_interactive_mode(),
    case dreyfus_index:design_doc_to_index(DDoc, IndexName) of
        {ok, Index} ->
            case dreyfus_index_manager:get_index(DbName, Index) of
                {ok, Pid} ->
                    Result = dreyfus_index:info(Pid),
                    rexi:reply(Result);
                Error ->
                    rexi:reply(Error)
            end;
        Error ->
            rexi:reply(Error)
    end.

disk_size(DbName, DDoc, IndexName) ->
    erlang:put(io_priority, {search, DbName}),
    check_interactive_mode(),
    case dreyfus_index:design_doc_to_index(DDoc, IndexName) of
        {ok, Index} ->
            Result = dreyfus_index_manager:get_disk_size(DbName, Index),
            rexi:reply(Result);
        Error ->
            rexi:reply(Error)
    end.

get_or_create_db(DbName, Options) ->
    case couch_db:open_int(DbName, Options) of
        {not_found, no_db_file} ->
            couch_log:warning("~p creating ~s", [?MODULE, DbName]),
            mem3_util:get_or_create_db(DbName, Options);
        Else ->
            Else
    end.


calculate_seqs(Db, Stale) ->
    LastSeq = couch_db:get_update_seq(Db),
    if
        Stale == ok orelse Stale == update_after ->
            {LastSeq, 0};
        true ->
            {LastSeq, LastSeq}
    end.

check_interactive_mode() ->
    case config:get("couchdb", "maintenance_mode", "false") of
        "true" ->
            % Do this to avoid log spam from rexi_server
            rexi:reply({rexi_EXIT, {maintenance_mode, node()}}),
            exit(normal);
        _ ->
            ok
    end.
