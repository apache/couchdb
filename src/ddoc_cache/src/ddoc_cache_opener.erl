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

-module(ddoc_cache_opener).
-behaviour(gen_server).


-include_lib("mem3/include/mem3.hrl").


-export([
    start_link/0
]).

-export([
    open_ddoc/1
]).

-export([
    init/1,
    terminate/2,

    handle_call/3,
    handle_cast/2,
    handle_info/2,

    code_change/3
]).

-export([
    evictor/1
]).


-define(OPENING, ddoc_cache_opening).


-record(opener, {
    key,
    pid,
    clients
}).

-record(st, {
    db_ddocs,
    evictor
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    process_flag(trap_exit, true),
    ets:new(?OPENING, [set, protected, named_table, {keypos, #opener.key}]),
    {ok, Evictor} = couch_db_update_notifier:start_link(fun ?MODULE:evictor/1),
    {ok, #st{
        evictor = Evictor
    }}.


terminate(_Reason, St) ->
    case is_pid(St#st.evictor) of
        true -> exit(St#st.evictor, kill);
        false -> ok
    end,
    ok.


handle_call({open, {_DbName, _DDocId}=Key}, From, St) ->
    case ets:lookup(?OPENING, Key) of
        [#opener{clients=Clients}=O] ->
            ets:insert(?OPENING, O#opener{clients=[From | Clients]}),
            {noreply, St};
        [] ->
            Pid = spawn_link(?MODULE, open_ddoc, [Key]),
            ets:insert(?OPENING, #opener{key=Key, pid=Pid, clients=[From]}),
            {noreply, St}
    end;

handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.


handle_cast({evict, DbName}, St) ->
    gen_server:abcast(mem3:nodes(), ?MODULE, {do_evict, DbName}),
    {noreply, St};

handle_cast({evict, DbName, DDocIds}, St) ->
    gen_server:abcast(mem3:nodes(), ?MODULE, {do_evict, DbName, DDocIds}),
    {noreply, St};

handle_cast({do_evict, DbName}, St) ->
    % Bit of hack to introspect the ets_lru ETS tables directly
    % but I think this is better than having to manage our own
    % DbName -> DDocIdList table
    DDocIds = ets:foldl(fun(Obj, Acc) ->
        entry = element(1, Obj), % assert this is an entry record
        {EntryDbName, EntryDDocId} = element(2, Obj),
        case EntryDbName == DbName of
            true -> [EntryDDocId | Acc];
            false -> Acc
        end
    end, [], ddoc_cache_lru_objects),
    handle_cast({do_evict, DbName, DDocIds}, St);

handle_cast({do_evict, DbName, DDocIds}, St) ->
    ets_lru:remove(ddoc_cache_lru, {DbName, validation_funs}),
    lists:foreach(fun(DDocId) ->
        ets_lru:remove(ddoc_cache_lru, {DbName, DDocId})
    end, DDocIds),
    {noreply, St};

handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info({'EXIT', Pid, Reason}, #st{evictor=Pid}=St) ->
    twig:log(err, "ddoc_cache_opener evictor died ~w", [Reason]),
    {ok, Evictor} = couch_db_update_notifier:start_link(fun ?MODULE:evictor/1),
    {noreply, St#st{evictor=Evictor}};

handle_info({'EXIT', _Pid, {open_ok, Key, Resp}}, St) ->
    respond(Key, {open_ok, Resp}),
    {noreply, St};

handle_info({'EXIT', _Pid, {open_error, Key, Type, Error}}, St) ->
    respond(Key, {open_error, Type, Error}),
    {noreply, St};

handle_info({'EXIT', Pid, Reason}, St) ->
    Pattern = #opener{pid=Pid, _='_'},
    case ets:match_object(?OPENING, Pattern) of
        [#opener{key=Key, clients=Clients}] ->
            [gen_server:reply(C, {error, Reason}) || C <- Clients],
            ets:delete(?OPENING, Key),
            {noreply, St};
        [] ->
            {stop, {unknown_pid_died, {Pid, Reason}}, St}
    end;

handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


evictor({created, ShardDbName}) ->
    DbName = mem3:dbname(ShardDbName),
    gen_server:cast(?MODULE, {evict, DbName});
evictor({deleted, ShardDbName}) ->
    DbName = mem3:dbname(ShardDbName),
    gen_server:cast(?MODULE, {evict, DbName});
evictor(_) ->
    ok.


open_ddoc({DbName, validation_funs}=Key) ->
    {ok, DDocs} = fabric:design_docs(mem3:dbname(DbName)),
    Funs = lists:flatmap(fun(DDoc) ->
        case couch_doc:get_validate_doc_fun(DDoc) of
            nil -> [];
            Fun -> [Fun]
        end
    end, DDocs),
    ok = ets_lru:insert(ddoc_cache_lru, {DbName, validation_funs}, Funs),
    exit({open_ok, Key, {ok, Funs}});
open_ddoc({DbName, DDocId}=Key) ->
    try fabric:open_doc(DbName, DDocId, [ejson_body]) of
        {ok, Doc} ->
            ok = ets_lru:insert(ddoc_cache_lru, {DbName, DDocId}, Doc),
            exit({open_ok, Key, {ok, Doc}});
        Else ->
            exit({open_ok, Key, Else})
    catch
        Type:Reason ->
            exit({open_error, Key, Type, Reason})
    end.


respond(Key, Resp) ->
    [#opener{clients=Clients}] = ets:lookup(?OPENING, Key),
    [gen_server:reply(C, Resp) || C <- Clients],
    ets:delete(?OPENING, Key).
