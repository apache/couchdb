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

-module(dreyfus_index_manager).
-behaviour(gen_server).
-vsn(1).
-include_lib("couch/include/couch_db.hrl").
-include("dreyfus.hrl").

-define(BY_SIG, dreyfus_by_sig).
-define(BY_PID, dreyfus_by_pid).

% public api.
-export([start_link/0, get_index/2]).

% gen_server api.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([handle_db_event/3]).

% public functions.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_index(DbName, Index) ->
    gen_server:call(?MODULE, {get_index, DbName, Index}, infinity).

% gen_server functions.

init([]) ->
    ets:new(?BY_SIG, [set, private, named_table]),
    ets:new(?BY_PID, [set, private, named_table]),
    couch_event:link_listener(?MODULE, handle_db_event, nil, [all_dbs]),
    process_flag(trap_exit, true),
    {ok, nil}.

handle_call({get_index, DbName, #index{sig=Sig}=Index}, From, State) ->
    case ets:lookup(?BY_SIG, {DbName, Sig}) of
    [] ->
        Pid = spawn_link(fun() -> new_index(DbName, Index) end),
        ets:insert(?BY_PID, {Pid, opening, {DbName, Sig}}),
        ets:insert(?BY_SIG, {{DbName,Sig}, [From]}),
        {noreply, State};
    [{_, WaitList}] when is_list(WaitList) ->
        ets:insert(?BY_SIG, {{DbName, Sig}, [From | WaitList]}),
        {noreply, State};
    [{_, ExistingPid}] ->
        {reply, {ok, ExistingPid}, State}
    end;

handle_call({open_ok, DbName, Sig, NewPid}, {OpenerPid, _}, State) ->
    link(NewPid),
    [{_, WaitList}] = ets:lookup(?BY_SIG, {DbName, Sig}),
    [gen_server:reply(From, {ok, NewPid}) || From <- WaitList],
    ets:delete(?BY_PID, OpenerPid),
    add_to_ets(NewPid, DbName, Sig),
    {reply, ok, State};

handle_call({open_error, DbName, Sig, Error}, {OpenerPid, _}, State) ->
    [{_, WaitList}] = ets:lookup(?BY_SIG, {DbName, Sig}),
    [gen_server:reply(From, Error) || From <- WaitList],
    ets:delete(?BY_PID, OpenerPid),
    ets:delete(?BY_SIG, {DbName, Sig}),
    {reply, ok, State}.

handle_cast({cleanup, DbName}, State) ->
    clouseau_rpc:cleanup(DbName),
    {noreply, State}.

handle_info({'EXIT', FromPid, Reason}, State) ->
    case ets:lookup(?BY_PID, FromPid) of
    [] ->
        if Reason =/= normal ->
            couch_log:error("Exit on non-updater process: ~p", [Reason]),
            exit(Reason);
        true -> ok
        end;
    % Using Reason /= normal to force a match error
    % if we didn't delete the Pid in a handle_call
    % message for some reason.
    [{_, opening, {DbName, Sig}}] when Reason /= normal ->
        Msg = {open_error, DbName, Sig, Reason},
        {reply, ok, _} = handle_call(Msg, {FromPid, nil}, State);
    [{_, {DbName, Sig}}] ->
        delete_from_ets(FromPid, DbName, Sig)
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, nil, _Extra) ->
    {ok, nil}.

% private functions

handle_db_event(DbName, created, _St) ->
    gen_server:cast(?MODULE, {cleanup, DbName}),
    {ok, nil};
handle_db_event(DbName, deleted, _St) ->
    gen_server:cast(?MODULE, {cleanup, DbName}),
    {ok, nil};
handle_db_event(_DbName, _Event, _St) ->
    {ok, nil}.

new_index(DbName, #index{sig=Sig}=Index) ->
    case (catch dreyfus_index:start_link(DbName, Index)) of
    {ok, NewPid} ->
        Msg = {open_ok, DbName, Sig, NewPid},
        ok = gen_server:call(?MODULE, Msg, infinity),
        unlink(NewPid);
    Error ->
        Msg = {open_error, DbName, Sig, Error},
        ok = gen_server:call(?MODULE, Msg, infinity)
    end.

add_to_ets(Pid, DbName, Sig) ->
    true = ets:insert(?BY_PID, {Pid, {DbName, Sig}}),
    true = ets:insert(?BY_SIG, {{DbName, Sig}, Pid}).

delete_from_ets(Pid, DbName, Sig) ->
    true = ets:delete(?BY_PID, Pid),
    true = ets:delete(?BY_SIG, {DbName, Sig}).

