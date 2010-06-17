-module(rexi_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/0, init_p/2]).

-include_lib("eunit/include/eunit.hrl").

-record(st, {
    workers = ets:new(workers, [private, {keypos,2}])
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #st{}}.

handle_call(_Request, _From, St) ->
    {reply, ignored, St}.

handle_cast({doit, From, MFA}, #st{workers=Workers} = St) ->
    {LocalPid, Ref} = spawn_monitor(?MODULE, init_p, [From, MFA]),
    {noreply, St#st{workers = add_worker({LocalPid, Ref, From}, Workers)}};

handle_cast({kill, Ref}, #st{workers=Workers} = St) ->
    case find_worker(Ref, Workers) of
    {Pid, Ref, _} ->
        exit(Pid, kill);
    false -> ok end,
    {noreply, St#st{workers = remove_worker(Ref, Workers)}}.

handle_info({'DOWN', Ref, process, _, normal}, #st{workers=Workers} = St) ->
    {noreply, St#st{workers = remove_worker(Ref, Workers)}};

handle_info({'DOWN', Ref, process, Pid, Reason}, #st{workers=Workers} = St) ->
    case find_worker(Ref, Workers) of
    {Pid, Ref, From} ->
        notify_caller(From, Reason);
    false -> ok end,
    {noreply, St#st{workers = remove_worker(Ref, Workers)}};

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, St) ->
    ets:foldl(fun({Pid, _, _}, _) -> exit(Pid,kill) end, nil, St#st.workers),
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% @doc initializes a process started by rexi_server.
-spec init_p({pid(),reference()}, mfa()) -> any().
init_p(From, {M,F,A}) ->
    put(rexi_from, From),
    try apply(M, F, A) catch _:Reason -> exit(Reason) end.

%% internal

add_worker(Worker, Tab) ->
    ets:insert(Tab, Worker), Tab.

remove_worker(Ref, Tab) ->
    ets:delete(Tab, Ref), Tab.

find_worker(Ref, Tab) ->
    case ets:lookup(Tab, Ref) of [] -> false; [Worker] -> Worker end.

notify_caller({Caller, Ref}, Reason) ->
    Caller ! {Ref, {rexi_EXIT, Reason}}.
