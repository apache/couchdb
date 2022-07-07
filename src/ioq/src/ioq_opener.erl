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

-module(ioq_opener).
-behavior(gen_server).


-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
-export([
    start_link/0,
    fetch_pid_for/1,
    fetch_pid_for/2,
    fetch_pid_for/3,
    get_pid_for/1,
    set_pid_for/2,
    get_ioq_pids/0,
    get_pid_idx/0,
    get_monitor_idx/0
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("ioq/include/ioq.hrl").


-define(BY_USER, by_user).
-define(BY_SHARD, by_shard).
-define(BY_CLASS, by_class).
-define(BY_FD, by_fd).
-define(BY_DB, by_db).
-define(DEFAULT_DISPATCH, ?BY_SHARD).
-define(PDICT_MARKER, ioq_pid_for).


-record(st, {
    idle = [] :: [{erlang:timestamp(), pid()}],
    pid_idx :: khash:khash(),
    monitors :: khash:khash(),
    dispatch :: ?BY_SHARD | ?BY_DB | ?BY_USER | ?BY_CLASS | ?BY_FD | undefined
 }).


%% HACK: experiment to allow for spawning IOQ2 pids prior to the spawning
%% the associated couch_file pids
%%fetch_pid_for(DbName) when is_binary(DbName) ->
%%    fetch_pid_for(DbName, self()).


fetch_pid_for(#ioq_request{}=Req) ->
    gen_server:call(?MODULE, {fetch, Req}, infinity).


%% TODO: cleanup the overloaded arity once experiments concluded
%%fetch_pid_for(DbName, undefined) when is_binary(DbName) ->
%%    fetch_pid_for(DbName, undefined, self());
%%fetch_pid_for(DbName, #user_ctx{}=Ctx) when is_binary(DbName) ->
%%    fetch_pid_for(DbName, Ctx, self());
fetch_pid_for(DbName, FdPid) when is_binary(DbName), is_pid(FdPid) ->
    fetch_pid_for(DbName, undefined, FdPid).


fetch_pid_for(DbName, UserCtx, FdPid) when is_binary(DbName), is_pid(FdPid) ->
    gen_server:call(?MODULE, {fetch, DbName, UserCtx, FdPid}, infinity).


get_pid_for(undefined) ->
    undefined;
get_pid_for(DbName) when is_binary(DbName) ->
    %% HACK: use the same shard format as per #ioq_request{} to post facto
    %% associate an IOQ pid with a dbname for when we set the IOQ2 pid prior
    %% to having a db handle
    erlang:get({?PDICT_MARKER, filename:rootname(DbName)});
get_pid_for(FdPid) when is_pid(FdPid) ->
    erlang:get({?PDICT_MARKER, FdPid}).


set_pid_for(_, undefined) ->
    ok;
set_pid_for(DbName, IOQPid) when is_binary(DbName), is_pid(IOQPid) ->
    %% HACK: use the same shard format as per #ioq_request{} to post facto
    %% associate an IOQ pid with a dbname for when we set the IOQ2 pid prior
    %% to having a db handle
    erlang:put({?PDICT_MARKER, filename:rootname(DbName)}, IOQPid),
    ok;
set_pid_for(FdPid, IOQPid) when is_pid(FdPid), is_pid(IOQPid) ->
    erlang:put({?PDICT_MARKER, FdPid}, IOQPid),
    ok.


get_pid_idx() ->
    gen_server:call(?MODULE, get_pid_idx, infinity).


get_monitor_idx() ->
    gen_server:call(?MODULE, get_monitor_idx, infinity).


get_ioq_pids() ->
    lists:foldl(
      fun
        ({K, _V}, Acc) when is_pid(K) ->
              [K | Acc];
        (_, Acc) ->
              Acc
      end, [], get_pid_idx()).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    process_flag(trap_exit, true),
    {ok, PidIdx} = khash:new(),
    {ok, Monitors} = khash:new(),
    Dispatch = case config:get("ioq.opener", "dispatch", undefined) of
        "by_shard" -> ?BY_SHARD;
        "by_db"    -> ?BY_DB;
        "by_user"  -> ?BY_USER;
        "by_class" -> ?BY_CLASS;
        "by_fd"    -> ?BY_FD;
        _          -> ?DEFAULT_DISPATCH
    end,
    St = #st{
        pid_idx = PidIdx,
        monitors = Monitors,
        dispatch = Dispatch
    },
    {ok, St}.


handle_call({fetch, #ioq_request{}=Req}, _From, #st{dispatch=Dispatch}=St) ->
    Key = case Dispatch of
        ?BY_SHARD ->
            Req#ioq_request.shard;
        ?BY_DB ->
            Req#ioq_request.db;
        ?BY_USER ->
            Req#ioq_request.user;
        ?BY_CLASS ->
            Req#ioq_request.class;
        ?BY_FD ->
            {fd, Req#ioq_request.fd}
    end,
    IOQPid = case khash:get(St#st.pid_idx, Key, not_found) of
        not_found ->
            {ok, Pid} = ioq_server2:start_link({Dispatch, Key}),
            khash:put(St#st.pid_idx, Key, Pid),
            khash:put(St#st.pid_idx, Pid, Key),
            Pid;
        Pid ->
            Pid
    end,
    ok = add_monitor(St#st.monitors, Req#ioq_request.fd, IOQPid),
    {reply, IOQPid, St};
handle_call({fetch, _DbName, UserCtx, FdPid}, From, #st{dispatch=?BY_USER}=St) ->
    Caller = case FdPid of
        undefined -> From;
        _ when is_pid(FdPid) -> FdPid
    end,
    Name = case UserCtx of
        #user_ctx{name=Name0} -> Name0;
        %% TODO: support unknown user
        undefined -> throw(unknown_user)
    end,
    IOQPid = case khash:get(St#st.pid_idx, Name, not_found) of
        not_found ->
            {ok, Pid} = ioq_server2:start_link({?BY_USER, Name}),
            khash:put(St#st.pid_idx, Name, Pid),
            khash:put(St#st.pid_idx, Pid, Name),
            Pid;
        Pid ->
            Pid
    end,
    ok = add_monitor(St#st.monitors, Caller, IOQPid),
    {reply, IOQPid, St};
handle_call({fetch, DbName, _UserCtx, FdPid}, From, #st{dispatch=?BY_SHARD}=St) ->
    Caller = case FdPid of
        undefined -> From;
        _ when is_pid(FdPid) -> FdPid
    end,
    %% TODO: DbName = drop_compact_ext(DbName0),
    IOQPid = case khash:get(St#st.pid_idx, DbName, not_found) of
        not_found ->
            {ok, Pid} = ioq_server2:start_link({?BY_SHARD, DbName}),
            khash:put(St#st.pid_idx, DbName, Pid),
            khash:put(St#st.pid_idx, Pid, DbName),
            Pid;
        Pid ->
            Pid
    end,
    ok = add_monitor(St#st.monitors, Caller, IOQPid),
    {reply, IOQPid, St};
handle_call(get_pid_idx, _From, #st{}=St) ->
    {reply, khash:to_list(St#st.pid_idx), St};
handle_call(get_monitor_idx, _From, #st{}=St) ->
    {reply, khash:to_list(St#st.monitors), St};
handle_call(_, _From, St) ->
    {reply, ok, St}.


handle_cast(_Msg, St) ->
    {noreply, St}.


handle_info({'DOWN', Ref, process, _Pid, _Reason}, St) ->
    case drop_monitor(St#st.monitors, Ref) of
        {IOQPid, []} ->
            Name = khash:get(St#st.pid_idx, IOQPid), %% TODO: assert found?
            khash:del(St#st.pid_idx, IOQPid),
            khash:del(St#st.pid_idx, Name);
        {_IOQPid, _Refs} ->
            ok
    end,
    {noreply, St};
handle_info({'EXIT', Pid, _}, St) ->
    case khash:get(St#st.pid_idx, Pid, not_found) of
        not_found ->
            %% TODO: shouldn't happen, throw error?
            ok;
        Name ->
            khash:del(St#st.pid_idx, Pid),
            khash:del(St#st.pid_idx, Name)
    end,
    {noreply, St};
handle_info(_Info, St) ->
    {noreply, St}.


terminate(_Reason, _St) ->
    ok.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


add_monitor(Mons, FdPid, IOQPid) ->
    PidKey = {FdPid, IOQPid},
    Ref = case khash:get(Mons, PidKey, not_found) of
        not_found ->
            Ref0 = erlang:monitor(process, FdPid),
            khash:put(Mons, Ref0, PidKey),
            khash:put(Mons, PidKey, Ref0),
            khash:put(Mons, FdPid, Ref0);
        Ref0 ->
            Ref0
    end,
    case khash:get(Mons, IOQPid, not_found) of
        not_found ->
            khash:put(Mons, IOQPid, [Ref]);
        Refs ->
            case lists:member(Ref, Refs) of
                true ->
                    ok;
                false ->
                    khash:put(Mons, IOQPid, [Ref | Refs])
            end
    end,
    ok.


drop_monitor(Mons, Ref) when is_reference(Ref) ->
    case khash:get(Mons, Ref, not_found) of
        not_found ->
            %% TODO: shouldn't happen
            throw(unexpected);
        {FdPid, IOQPid}=PidKey ->
            case khash:get(Mons, IOQPid, not_found) of
                not_found ->
                    %% TODO: shouldn't happen
                    throw(unexpected);
                Refs ->
                    khash:del(Mons, FdPid),
                    khash:del(Mons, Ref),
                    khash:del(Mons, PidKey),
                    case lists:delete(Ref, Refs) of
                        [] ->
                            unlink(IOQPid),
                            khash:del(Mons, IOQPid),
                            exit(IOQPid, idle),
                            {IOQPid, []};
                        Refs1 ->
                            khash:put(Mons, IOQPid, Refs1),
                            {IOQPid, Refs1}
                    end
            end
    end.

