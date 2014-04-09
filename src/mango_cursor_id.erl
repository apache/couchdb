-module(mango_cursor_id).

-behavior(gen_server).


-export([
    start_link/0,
    register/1,
    lookup/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-record(st, {
    curr_id = 1
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


register(Pid) ->
    gen_server:call(?MODULE, {register, Pid}).


lookup(Id) ->
    case ets:lookup(?MODULE, Id) of
        [{Id, Pid}] ->
            {ok, Pid};
        [] ->
            throw({cursor_not_found, Id})
    end.


init(_) ->
    ets:new(?MODULE, [named_table, set, public]),
    {ok, #st{}}.


terminate(_Reason, _St) ->
    ok.


handle_call({register, Pid}, _From, St) when is_pid(Pid) ->
    Id = St#st.curr_id,
    erlang:monitor(process, Pid),
    ets:insert(?MODULE, {Id, Pid}),
    ets:insert(?MODULE, {Pid, Id}),
    {reply, {ok, Id}, St};

handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info({'DOWN', _, _, Pid, _}, St) ->
    case ets:lookup(?MODULE, Pid) of
        [{Pid, Id}] ->
            ets:delete(?MODULE, Id),
            ets:delete(?MODULE, Pid);
        [] ->
            ok
    end,
    {noreply, St};

handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
