-module(rexi_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
    code_change/3]).

-export([start_link/0, init_p/2]).

-include_lib("eunit/include/eunit.hrl").

-record(st, {
    workers = []
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #st{}}.

handle_call(_Request, _From, St) ->
    {reply, ok, St}.

handle_cast({doit, From, MFA}, #st{workers=Workers} = St) ->
    {LocalPid, Ref} = spawn_monitor(?MODULE, init_p, [From, MFA]),
    {noreply, St#st{workers = add_worker({LocalPid, Ref, From}, Workers)}}.

handle_info({'DOWN', Ref, process, _, normal}, #st{workers=Workers} = St) ->
    {noreply, St#st{workers = remove_worker(Ref, Workers)}};

handle_info({'DOWN', Ref, process, Pid, Reason}, #st{workers=Workers} = St) ->
    case find_worker(Ref, Workers) of
    {Pid, Ref, From} ->
        notify_caller(From, Pid, Reason);
    false -> ok end,
    {noreply, St#st{workers = remove_worker(Ref, Workers)}};

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, St) ->
    [exit(Pid,kill) || {Pid, _, _} <- St#st.workers],
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

init_p(From, {M,F,A}) ->
    put(rexi_from, From),
    try apply(M, F, A) catch _:Reason -> exit(Reason) end.

%% internal

add_worker(Worker, List) ->
    [Worker | List].

remove_worker(Ref, List) ->
    lists:keydelete(Ref, 2, List).

find_worker(Ref, List) ->
    lists:keyfind(Ref, 2, List).

notify_caller({Caller, CallerRef}, Pid, Reason) ->
    Caller ! {worker_died, CallerRef, Pid, Reason}.
