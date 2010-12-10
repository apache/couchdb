% Copyright 2010 Cloudant
% 
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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/0, init_p/2, init_p/3]).

-include("rexi.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(st, {
    workers = ets:new(workers, [private, {keypos,2}]),
    errors = queue:new(),
    error_limit = 20,
    error_count = 0
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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

handle_cast({doit, From, Nonce, MFA}, #st{workers=Workers} = St) ->
    {LocalPid, Ref} = spawn_monitor(?MODULE, init_p, [From, MFA, Nonce]),
    {noreply, St#st{workers = add_worker({LocalPid, Ref, From}, Workers)}};


handle_cast({kill, FromRef}, #st{workers=Workers} = St) ->
    case find_worker_from(FromRef, Workers) of
    {Pid, KeyRef, {_, FromRef}} ->
        erlang:demonitor(KeyRef),
        exit(Pid, kill),
        {noreply, St#st{workers = remove_worker(KeyRef, Workers)}};
    false ->
        {noreply, St}
    end.

handle_info({'DOWN', Ref, process, _, normal}, #st{workers=Workers} = St) ->
    {noreply, St#st{workers = remove_worker(Ref, Workers)}};

handle_info({'DOWN', Ref, process, Pid, Error}, #st{workers=Workers} = St) ->
    case find_worker(Ref, Workers) of
    {Pid, Ref, From} ->
        case Error of #error{reason = {_Class, Reason}, stack = Stack} ->
            notify_caller(From, {Reason, Stack}),
            St1 = save_error(Error, St),
            {noreply, St1#st{workers = remove_worker(Ref, Workers)}};
        _ ->
            notify_caller(From, Error),
            {noreply, St#st{workers = remove_worker(Ref, Workers)}}
        end;
    false ->
        {noreply, St}
    end;

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, St) ->
    ets:foldl(fun({Pid, _, _}, _) -> exit(Pid,kill) end, nil, St#st.workers),
    ok.

code_change(_OldVsn, {st, Workers}, _Extra) ->
    {ok, #st{workers = Workers}};

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

init_p(From, MFA) ->
    init_p(From, MFA, undefined).

%% @doc initializes a process started by rexi_server.
-spec init_p({pid(), reference()}, {atom(), atom(), list()},
    string() | undefined) -> any().
init_p(From, {M,F,A}, Nonce) ->
    put(rexi_from, From),
    put(initial_call, {M,F,length(A)}),
    try apply(M, F, A) catch exit:normal -> ok; Class:Reason ->
        Stack = clean_stack(),
        error_logger:error_report([{?MODULE, Nonce, {Class, Reason}}, Stack]),
        exit(#error{
            timestamp = now(),
            reason = {Class, Reason},
            mfa = {M,F,A},
            nonce = Nonce,
            stack = Stack
        })
    end.

%% internal

save_error(E, #st{errors=Q, error_limit=L, error_count=C} = St) when C >= L ->
    St#st{errors = queue:in(E, queue:drop(Q))};
save_error(E, #st{errors=Q, error_count=C} = St) ->
    St#st{errors = queue:in(E, Q), error_count = C+1}.

clean_stack() ->
    lists:map(fun({M,F,A}) when is_list(A) -> {M,F,length(A)}; (X) -> X end,
        erlang:get_stacktrace()).

add_worker(Worker, Tab) ->
    ets:insert(Tab, Worker), Tab.

remove_worker(Ref, Tab) ->
    ets:delete(Tab, Ref), Tab.

find_worker(Ref, Tab) ->
    case ets:lookup(Tab, Ref) of [] -> false; [Worker] -> Worker end.

find_worker_from(Ref, Tab) ->
    case ets:match_object(Tab, {'_', '_', {'_', Ref}}) of
    [] ->
        false;
    [Worker] ->
        Worker
    end.

notify_caller({Caller, Ref}, Reason) ->
    Caller ! {Ref, {rexi_EXIT, Reason}}.
