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

-module(couch_views_server).


-behaviour(gen_server).


-export([
    start_link/0
]).

-export([
    accepted/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    format_status/2
]).

-define(MAX_ACCEPTORS, 5).
-define(MAX_WORKERS, 100).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


accepted(Worker) when is_pid(Worker) ->
    gen_server:call(?MODULE, {accepted, Worker}, infinity).


init(_) ->
    process_flag(trap_exit, true),
    couch_views_jobs:set_timeout(),
    St = #{
        acceptors => #{},
        workers => #{},
        max_acceptors => max_acceptors(),
        max_workers => max_workers()
    },
    {ok, spawn_acceptors(St)}.


terminate(_, _St) ->
    ok.


handle_call({accepted, Pid}, _From, St) ->
    #{
        acceptors := Acceptors,
        workers := Workers
    } = St,
    case maps:is_key(Pid, Acceptors) of
        true ->
            St1 = St#{
                acceptors := maps:remove(Pid, Acceptors),
                workers := Workers#{Pid => true}
            },
            {reply, ok, spawn_acceptors(St1)};
        false ->
            LogMsg = "~p : unknown acceptor processs ~p",
            couch_log:error(LogMsg, [?MODULE, Pid]),
            {stop, {unknown_acceptor_pid, Pid}, St}
    end;

handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'EXIT', Pid, Reason}, St) ->
    #{
        acceptors := Acceptors,
        workers := Workers
    } = St,

    % In Erlang 21+ could check map keys directly in the function head
    case {maps:is_key(Pid, Acceptors), maps:is_key(Pid, Workers)} of
        {true, false} -> handle_acceptor_exit(St, Pid, Reason);
        {false, true} -> handle_worker_exit(St, Pid, Reason);
        {false, false} -> handle_unknown_exit(St, Pid, Reason)
    end;

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


format_status(_Opt, [_PDict, State]) ->
    #{
        workers := Workers,
        acceptors := Acceptors
    } = State,
    Scrubbed = State#{
        workers => {map_size, maps:size(Workers)},
        acceptors => {map_size, maps:size(Acceptors)}
    },
    [{data, [{"State",
        Scrubbed
    }]}].


% Worker process exit handlers

handle_acceptor_exit(#{acceptors := Acceptors} = St, Pid, Reason) ->
    St1 = St#{acceptors := maps:remove(Pid, Acceptors)},
    LogMsg = "~p : acceptor process ~p exited with ~p",
    couch_log:error(LogMsg, [?MODULE, Pid, Reason]),
    {noreply, spawn_acceptors(St1)}.


handle_worker_exit(#{workers := Workers} = St, Pid, normal) ->
    St1 = St#{workers := maps:remove(Pid, Workers)},
    {noreply, spawn_acceptors(St1)};

handle_worker_exit(#{workers := Workers} = St, Pid, Reason) ->
    St1 = St#{workers := maps:remove(Pid, Workers)},
    LogMsg = "~p : indexer process ~p exited with ~p",
    couch_log:error(LogMsg, [?MODULE, Pid, Reason]),
    {noreply, spawn_acceptors(St1)}.


handle_unknown_exit(St, Pid, Reason) ->
    LogMsg = "~p : unknown process ~p exited with ~p",
    couch_log:error(LogMsg, [?MODULE, Pid, Reason]),
    {stop, {unknown_pid_exit, Pid}, St}.


spawn_acceptors(St) ->
    #{
        workers := Workers,
        acceptors := Acceptors,
        max_acceptors := MaxAcceptors,
        max_workers := MaxWorkers
    } = St,
    ACnt = maps:size(Acceptors),
    WCnt = maps:size(Workers),
    case ACnt < MaxAcceptors andalso (ACnt + WCnt) < MaxWorkers of
        true ->
            Pid = couch_views_indexer:spawn_link(),
            NewSt = St#{acceptors := Acceptors#{Pid => true}},
            spawn_acceptors(NewSt);
        false ->
            St
    end.


max_acceptors() ->
    config:get_integer("couch_views", "max_acceptors", ?MAX_ACCEPTORS).


max_workers() ->
    config:get_integer("couch_views", "max_workers", ?MAX_WORKERS).
