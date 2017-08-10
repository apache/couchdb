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

-module(rexi_server).
-behaviour(gen_server).
-vsn(1).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/1, init_p/2, init_p/3]).

-include_lib("rexi/include/rexi.hrl").

-record(job, {
    client::reference(),
    worker::reference(),
    client_pid::pid(),
    worker_pid::pid()
}).

-record(st, {
    workers = ets:new(workers, [private, {keypos, #job.worker}]),
    clients = ets:new(clients, [private, {keypos, #job.client}]),
    errors = queue:new(),
    error_limit = 0,
    error_count = 0
}).

start_link(ServerId) ->
    gen_server:start_link({local, ServerId}, ?MODULE, [], []).

init([]) ->
    {ok, #st{}}.

handle_call(get_errors, _From, #st{errors = Errors} = St) ->
    {reply, {ok, lists:reverse(queue:to_list(Errors))}, St};

handle_call(get_last_error, _From, #st{errors = Errors} = St) ->
    try
        {reply, {ok, queue:get_r(Errors)}, St}
    catch error:empty ->
        {reply, {error, empty}, St}
    end;

handle_call({set_error_limit, N}, _From, #st{error_count=Len, errors=Q} = St) ->
    if N < Len ->
        {NewQ, _} = queue:split(N, Q);
    true ->
        NewQ = Q
    end,
    NewLen = queue:len(NewQ),
    {reply, ok, St#st{error_limit=N, error_count=NewLen, errors=NewQ}};

handle_call(_Request, _From, St) ->
    {reply, ignored, St}.


handle_cast({doit, From, MFA}, St) ->
    handle_cast({doit, From, undefined, MFA}, St);

handle_cast({doit, {ClientPid, ClientRef} = From, Nonce, MFA}, State) ->
    {LocalPid, Ref} = spawn_monitor(?MODULE, init_p, [From, MFA, Nonce]),
    Job = #job{
        client = ClientRef,
        worker = Ref,
        client_pid = ClientPid,
        worker_pid = LocalPid
    },
    {noreply, add_job(Job, State)};


handle_cast({kill, FromRef}, #st{clients = Clients} = St) ->
    case find_worker(FromRef, Clients) of
    #job{worker = KeyRef, worker_pid = Pid} = Job ->
        erlang:demonitor(KeyRef),
        exit(Pid, kill),
        {noreply, remove_job(Job, St)};
    false ->
        {noreply, St}
    end;

handle_cast(_, St) ->
    couch_log:notice("rexi_server ignored_cast", []),
    {noreply, St}.

handle_info({'DOWN', Ref, process, _, normal}, #st{workers=Workers} = St) ->
    case find_worker(Ref, Workers) of
    #job{} = Job ->
        {noreply, remove_job(Job, St)};
    false ->
        {noreply, St}
    end;

handle_info({'DOWN', Ref, process, Pid, Error}, #st{workers=Workers} = St) ->
    case find_worker(Ref, Workers) of
    #job{worker_pid=Pid, worker=Ref, client_pid=CPid, client=CRef} =Job ->
        case Error of #error{reason = {_Class, Reason}, stack = Stack} ->
            notify_caller({CPid, CRef}, {Reason, Stack}),
            St1 = save_error(Error, St),
            {noreply, remove_job(Job, St1)};
        _ ->
            notify_caller({CPid, CRef}, Error),
            {noreply, remove_job(Job, St)}
        end;
    false ->
        {noreply, St}
    end;

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, St) ->
    ets:foldl(fun(#job{worker_pid=Pid},_) -> exit(Pid,kill) end, nil,
        St#st.workers),
    ok.

code_change(_OldVsn, #st{}=State, _Extra) ->
    {ok, State}.

init_p(From, MFA) ->
    init_p(From, MFA, undefined).

%% @doc initializes a process started by rexi_server.
-spec init_p({pid(), reference()}, {atom(), atom(), list()},
    string() | undefined) -> any().
init_p(From, {M,F,A}, Nonce) ->
    put(rexi_from, From),
    put('$initial_call', {M,F,length(A)}),
    put(nonce, Nonce),
    try apply(M, F, A) catch exit:normal -> ok; Class:Reason ->
        Stack = clean_stack(),
        {ClientPid, _ClientRef} = From,
        couch_log:error(
            "rexi_server: from: ~s(~p) mfa: ~s:~s/~p ~p:~p ~100p", [
            node(ClientPid), ClientPid, M, F, length(A),
            Class, Reason, Stack]),
        exit(#error{
            timestamp = now(),
            reason = {Class, Reason},
            mfa = {M,F,A},
            nonce = Nonce,
            stack = Stack
        })
    end.

%% internal

save_error(_E, #st{error_limit = 0} = St) ->
    St;
save_error(E, #st{errors=Q, error_limit=L, error_count=C} = St) when C >= L ->
    St#st{errors = queue:in(E, queue:drop(Q))};
save_error(E, #st{errors=Q, error_count=C} = St) ->
    St#st{errors = queue:in(E, Q), error_count = C+1}.

clean_stack() ->
    lists:map(fun({M,F,A}) when is_list(A) -> {M,F,length(A)}; (X) -> X end,
        erlang:get_stacktrace()).

add_job(Job, #st{workers = Workers, clients = Clients} = State) ->
    ets:insert(Workers, Job),
    ets:insert(Clients, Job),
    State.

remove_job(Job, #st{workers = Workers, clients = Clients} = State) ->
    ets:delete_object(Workers, Job),
    ets:delete_object(Clients, Job),
    State.

find_worker(Ref, Tab) ->
    case ets:lookup(Tab, Ref) of [] -> false; [Worker] -> Worker end.

notify_caller({Caller, Ref}, Reason) ->
    rexi_utils:send(Caller, {Ref, {rexi_EXIT, Reason}}).
