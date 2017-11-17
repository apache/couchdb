
-module(couch_compact_status).
-behaviour(gen_server).
-vsn(1).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    dbname,
    filepath,
    stop=false
    }).

-define(TIMEOUT,  500).

start_link(DbName, Filepath) ->
    gen_server:start_link(?MODULE, [DbName, Filepath], []).

init([DbName, Filepath]) ->
    process_flag(trap_exit, true),
    {ok, #state{dbname=DbName, filepath=Filepath}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({add_task, [Retry, TotalChanges]}, State) ->
    TaskProps0 = [
        {type, database_compaction},
        {database, State#state.dbname},
        {progress, 0},
        {changes_done, 0},
        {meta_size, 0},
        {data_size, 0},
        {merged_on, 0},
        {total_changes, TotalChanges}
    ],
    case (Retry =/= nil) and couch_task_status:is_task_added() of
    true ->
        couch_task_status:update([
            {retry, true},
            {progress, 0},
            {changes_done, 0},
            {total_changes, TotalChanges}
        ]);
    false ->
        couch_task_status:add_task(TaskProps0),
        couch_task_status:set_update_frequency(?TIMEOUT),
        erlang:send_after(800, self(), track_fsize)
    end,
    {noreply, State};
handle_cast({update_task, NumChanges}, State) ->
    [Changes, Total] = couch_task_status:get([changes_done, total_changes]),
    Changes2 = Changes + NumChanges,
    Progress = case Total of
    0 ->
        0;
    _ ->
        (Changes2 * 100) div Total
    end,
    couch_task_status:update([{changes_done, Changes2}, {progress, Progress}]),
    {noreply, State};
handle_cast(merge, State) ->
    couch_task_status:update([{merged_on, timestamp()}]),
    {noreply, State};
handle_cast(done, State) ->
    {noreply, State#state{stop=true}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(track_fsize, #state{filepath=Filepath, stop=Stop}=State) ->
    case Stop of
        true -> ok;
        _ -> erlang:send_after(?TIMEOUT, self(), track_fsize)
    end,
    Meta = Filepath ++ ".compact.meta",
    Data = Filepath ++ ".compact.data",
    MetaSize = filelib:file_size(Meta),
    DataSize = filelib:file_size(Data),
    couch_task_status:update([{meta_size, MetaSize}, {data_size, DataSize}]),
    {noreply, State};
handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

timestamp() ->
    timestamp(now()).

timestamp({Mega, Secs, _}) ->
    Mega * 1000000 + Secs.
