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

-module(mem3_reshard).

-behaviour(gen_server).

-export([
   start_split_job/1,
   remove_job/1,
   stop_job/2,
   resume_job/1,
   jobs/0,
   job/1,
   shard_from_name/1,
   report/2,
   checkpoint/2,
   start/0,
   stop/1,
   get_state/0,
   reset_state/0,
   db_monitor/1,
   now_sec/0,
   update_history/3,

   start_link/0,

   init/1,
   terminate/2,
   handle_call/3,
   handle_info/2,
   handle_cast/2,
   code_change/3
]).


-include("mem3_reshard.hrl").


-define(JOB_ID_VERSION, 1).
-define(JOB_STATE_VERSION, 1).
-define(DEFAULT_MAX_JOBS, 25).
-define(DEFAULT_MAX_HISTORY, 20).
-define(JOB_PREFIX, <<"reshard-job-">>).
-define(STATE_PREFIX, <<"reshard-state-">>).


%% Public API

-spec start_split_job(#shard{} | binary()) -> {ok, binary()} | {error, term()}.
start_split_job(#shard{} = Shard) ->
    start_split_job(Shard, 2);

start_split_job(ShardName) when is_binary(ShardName) ->
    start_split_job(shard_from_name(ShardName), 2).


-spec start_split_job(#shard{}, split()) -> {ok, binary()} | {error, any()}.
start_split_job(#shard{} = Source, Split) ->
    case mem3_reshard_validate:start_args(Source, Split) of
        ok ->
            Targets = target_shards(Source, Split),
            case mem3_reshard_validate:targets(Source, Targets) of
                ok ->
                    TStamp = now_sec(),
                    Job = #job{
                        type = split,
                        job_state = new,
                        split_state = new,
                        state_history = [{new, TStamp}],
                        node = node(),
                        source = Source,
                        targets = Targets
                   },
                   Job1 = Job#job{id = job_id(Job)},
                   gen_server:call(?MODULE, {start_job, Job1}, infinity);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


-spec stop_job(binary(), binary()) -> ok | {error, any()}.
stop_job(JobId, Reason) when is_binary(JobId), is_binary(Reason) ->
    gen_server:call(?MODULE, {stop_job, JobId, Reason}, infinity).


-spec resume_job(binary()) -> ok | {error, any()}.
resume_job(JobId) when is_binary(JobId) ->
    gen_server:call(?MODULE, {resume_job, JobId}, infinity).


-spec remove_job(binary()) -> ok | {error, not_found}.
remove_job(JobId) when is_binary(JobId) ->
    gen_server:call(?MODULE, {remove_job, JobId}, infinity).


-spec jobs() -> [[tuple()]].
jobs() ->
    ets:foldl(fun(Job, Acc) ->
        Opts = [iso8601],
        Props = mem3_reshard_store:job_to_ejson_props(Job, Opts),
        [{Props} | Acc]
    end, [], ?MODULE).


-spec job(job_id()) -> {ok, {[_ | _]}} | {error, not_found}.
job(JobId) ->
    case job_by_id(JobId) of
        #job{} = Job ->
            Opts = [iso8601],
            Props = mem3_reshard_store:job_to_ejson_props(Job, Opts),
            {ok, {Props}};
        not_found ->
            {error, not_found}
    end.


-spec report(pid(), #job{}) -> ok.
report(Server, #job{} = Job) when is_pid(Server) ->
    ok = gen_server:call(Server, {report, Job}, infinity).


-spec checkpoint(pid(), #job{}) -> ok.
checkpoint(Server, #job{} = Job) ->
    ok = gen_server:call(Server, {checkpoint, Job}, infinity).


-spec get_state() -> {[_ | _]}.
get_state() ->
    gen_server:call(?MODULE, get_state, infinity).


-spec start() -> ok | {error, any()}.
start() ->
    gen_server:call(?MODULE, start, infinity).


-spec stop(binary()) -> ok | {error, any()}.
stop(Reason) ->
    gen_server:call(?MODULE, {stop, Reason}, infinity).


-spec reset_state() -> ok.
reset_state() ->
    gen_server:call(?MODULE, reset_state, infinity).


-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    config:enable_feature('shard-splitting'),
    couch_log:notice("~p start init()", [?MODULE]),
    EtsOpts = [named_table, {keypos, #job.id}, {read_concurrency, true}],
    ?MODULE = ets:new(?MODULE, EtsOpts),
    State = #state{
        state = running,
        state_info = [],
        time_updated = now_sec(),
        node = node(),
        db_monitor = spawn_link(?MODULE, db_monitor, [self()])
    },
    State1 = mem3_reshard_store:init(State, ?JOB_PREFIX, state_id()),
    State2 = mem3_reshard_store:load_state(State1),
    gen_server:cast(self(), reload_jobs),
    {ok, State2}.


terminate(Reason, State) ->
    couch_log:notice("~p terminate ~p ~p", [?MODULE, Reason, statefmt(State)]),
    catch unlink(State#state.db_monitor),
    catch exit(State#state.db_monitor, kill),
    [kill_job_int(Job) || Job <- running_jobs()],
    ok.


handle_call(start, _From, #state{state = stopped} = State) ->
    State1 = State#state{
        state = running,
        time_updated = now_sec(),
        state_info = info_delete(reason, State#state.state_info)
    },
    ok = mem3_reshard_store:store_state(State1),
    State2 = reload_jobs(State1),
    {reply, ok, State2};

handle_call(start, _From, State) ->
    {reply, ok, State};

handle_call({stop, Reason}, _From, #state{state = running} = State) ->
    State1 = State#state{
        state = stopped,
        time_updated = now_sec(),
        state_info = info_update(reason, Reason, State#state.state_info)
    },
    ok = mem3_reshard_store:store_state(State1),
    [kill_job_int(Job) || Job <- running_jobs()],
    {reply, ok, State1};

handle_call({stop, _}, _From, State) ->
    {reply, ok, State};

handle_call({start_job, _}, _From, #state{state = stopped} = State) ->
    case couch_util:get_value(reason, State#state.state_info) of
        undefined ->
            {reply, {error, stopped}, State};
        Reason ->
            {reply, {error, {stopped, Reason}}, State}
    end;

handle_call({start_job, #job{id = Id, source = Source} = Job}, _From, State) ->
    couch_log:notice("~p start_job call ~p", [?MODULE, jobfmt(Job)]),
    MaxJobs = get_max_jobs(),
    RunningJobs = mem3_reshard_job_sup:count_children(),
    SourceOk = mem3_reshard_validate:source(Source),
    case {job_by_id(Id), RunningJobs =< MaxJobs, SourceOk} of
        {not_found, true, ok} ->
            case start_job_int(Job, State) of
                ok ->
                    {reply, {ok, Job#job.id}, State};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        {#job{}, _, _} ->
            {reply, {error, job_already_exists}, State};
        {_, false, _} ->
            {reply, {error, max_jobs_exceeded}, State};
        {_, _, {error, _} = SourceError} ->
            {reply, SourceError, State}
    end;

handle_call({resume_job, _}, _From, #state{state = stopped} = State) ->
    case couch_util:get_value(reason, State#state.state_info) of
        undefined ->
            {reply, {error, stopped}, State};
        Reason ->
            {reply, {error, {stopped, Reason}}, State}
    end;

handle_call({resume_job, Id}, _From, State) ->
    couch_log:notice("~p resume_job call ~p", [?MODULE, Id]),
    case job_by_id(Id) of
        #job{job_state = stopped} = Job ->
            case start_job_int(Job, State) of
                ok ->
                    {reply, ok, State};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        #job{} ->
            {reply, ok, State};
        not_found ->
            {reply, {error, not_found}, State}
    end;

handle_call({stop_job, Id, Reason}, _From, State) ->
    couch_log:notice("~p stop_job Id:~p Reason:~p", [?MODULE, Id, Reason]),
    case job_by_id(Id) of
        #job{job_state = running} = Job ->
            ok = stop_job_int(Job, stopped, Reason, State),
            {reply, ok, State};
        #job{} ->
            {reply, ok, State};
        not_found ->
            {reply, {error, not_found}, State}
    end;


handle_call({remove_job, Id}, _From, State) ->
    couch_log:notice("~p call remove_job Id:~p", [?MODULE, Id]),
    case job_by_id(Id) of
        #job{} = Job ->
            ok = stop_job_int(Job, stopped, "Removed by user", State),
            ok = mem3_reshard_store:delete_job(State, Id),
            ets:delete(?MODULE, Job#job.id),
            {reply, ok, State};
        not_found ->
            {reply, {error, not_found}, State}
    end;

handle_call({checkpoint, Job}, {FromPid, _}, State) ->
    {reply, ok, checkpoint_int(Job, State, FromPid)};

handle_call({report, Job}, {FromPid, _}, State) ->
    report_int(Job, FromPid),
    {reply, ok, State};

handle_call(get_state, _From, #state{state = GlobalState} = State) ->
    StateProps = mem3_reshard_store:state_to_ejson_props(State),
    Stats0 =  #{running => 0, completed => 0, failed => 0, stopped => 0},
    StateStats = ets:foldl(fun(#job{job_state = JS}, Acc) ->
        % When jobs are disabled globally their state is not checkpointed as
        % "stopped", but it stays as "running". But when returning the state we
        % don't want to mislead and indicate that there are "N running jobs"
        % when the global state is "stopped".
        JS1 = case GlobalState =:= stopped andalso JS =:= running of
            true -> stopped;
            false -> JS
        end,
        Acc#{JS1 => maps:get(JS1, Acc, 0) + 1}
    end, Stats0, ?MODULE),
    Total = ets:info(?MODULE, size),
    StateStats1 = maps:to_list(StateStats) ++ [{total, Total}],
    Result = {lists:sort(StateProps ++ StateStats1)},
    {reply, Result, State};

handle_call(reset_state, _From, State) ->
    {reply, ok, reset_state(State)};

handle_call(Call, From, State) ->
    couch_log:error("~p unknown call ~p from: ~p", [?MODULE, Call, From]),
    {noreply, State}.


handle_cast(reload_jobs, State) ->
    StartupDelaySec = get_start_delay_sec(),
    couch_log:notice("~p startup delay ~p", [?MODULE, StartupDelaySec]),
    timer:sleep(StartupDelaySec * 1000),
    couch_log:notice("~p starting reloading jobs", [?MODULE]),
    State1 = reload_jobs(State),
    couch_log:notice("~p finished reloading jobs", [?MODULE]),
    {noreply, State1};

handle_cast({db_deleted, Jobs}, State) ->
    couch_log:notice("~p db_deleted: ~B jobs", [?MODULE, length(Jobs)]),
    fail_jobs_for_deleted_sources(Jobs, State),
    {noreply, State};

handle_cast(Cast, State) ->
    couch_log:error("~p unexpected cast ~p", [?MODULE, Cast]),
    {noreply, State}.


handle_info({'DOWN', _Ref, process, Pid, Info}, State) ->
    couch_log:notice("~p handle_info DOWN Pid:~p Info:~p", [?MODULE, Pid, Info]),
    case job_by_pid(Pid) of
        {ok, Job} ->
            couch_log:notice("~p job ~s exited ~p", [?MODULE, Job#job.id, Info]),
            ok = handle_job_exit(Job, Info, State);
        {error, not_found} ->
            couch_log:error("~p DOWN msg job not found: ~p ~p", [?MODULE, Pid, Info])
    end,
    {noreply, State};

handle_info(Info, State) ->
    couch_log:error("~p unexpected info ~p", [?MODULE, Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Private API

-spec reload_jobs(#state{}) -> #state{}.
reload_jobs(State) ->
    Jobs = mem3_reshard_store:get_jobs(State),
    lists:foldl(fun reload_job/2, State, Jobs).


% This is a case when main application is stopped but a job is reloaded that was
% checkpointed in running state. Set that state to stopped to avoid the API
% results looking odd.
-spec reload_job(#job{}, #state{}) -> #state{}.
reload_job(#job{job_state = running} = Job, #state{state = stopped} = State) ->
    OldInfo = Job#job.state_info,
    Job1 = Job#job{
        job_state = stopped,
        time_updated = now_sec(),
        time_started = 0,
        state_info = info_update(reason, <<"Shard splitting disabled">>, OldInfo),
        pid = undefined,
        ref = undefined
    },
    true = ets:insert(?MODULE, update_job_state_history(Job1)),
    State;

% This is a case when a job process should be spawend
reload_job(#job{job_state = running} = Job, #state{state = running} = State) ->
    case start_job_int(Job, State) of
        ok ->
            State;
        {error, Error} ->
            Msg = "~p could not resume ~s error: ~p",
            couch_log:error(Msg, [?MODULE, jobfmt(Job), Error]),
            State
    end;

% The default case is to just load the job into the ets table. This would be
% failed or completed jobs for example
reload_job(#job{} = Job, #state{} = State) ->
    true = ets:insert(?MODULE, Job),
    State.


-spec get_max_jobs() -> integer().
get_max_jobs() ->
    config:get_integer("mem3_reshard", "max_jobs", ?DEFAULT_MAX_JOBS).


-spec get_start_delay_sec() -> integer().
get_start_delay_sec() ->
    config:get_integer("mem3_reshard", "start_delay_sec", 0).


-spec start_job_int(#job{}, #state{}) -> ok | {error, term()}.
start_job_int(Job, State) ->
    case spawn_job(Job) of
        {ok, #job{} = Job1} ->
            Job2 = update_job_state_history(Job1),
            ok = mem3_reshard_store:store_job(State, Job2),
            true = ets:insert(?MODULE, Job2),
            ok;
        {error, Error} ->
            {error, Error}
    end.

-spec spawn_job(#job{}) -> {ok, pid()} | {error, term()}.
spawn_job(#job{} = Job0) ->
    Job = Job0#job{
        job_state = running,
        time_started = 0,
        time_updated = now_sec(),
        state_info = info_delete(reason, Job0#job.state_info),
        manager = self(),
        workers = [],
        retries = 0
    },
    case mem3_reshard_job_sup:start_child(Job) of
        {ok, Pid} ->
            Ref = monitor(process, Pid),
            {ok, Job#job{pid = Pid, ref = Ref}};
        {error, Reason} ->
            {error, Reason}
    end.


-spec stop_job_int(#job{}, job_state(), term(), #state{}) -> ok.
stop_job_int(#job{pid = undefined}, _JobState, _Reason, _State) ->
    ok;

stop_job_int(#job{} = Job, JobState, Reason, State) ->
    couch_log:info("~p stop_job_int ~p : ~p", [?MODULE, jobfmt(Job), Reason]),
    Job1 = kill_job_int(Job),
    Job2 = Job1#job{
        job_state = JobState,
        time_updated = now_sec(),
        state_info = [{reason, Reason}]
    },
    ok = mem3_reshard_store:store_job(State, Job2),
    couch_log:info("~p stop_job_int stopped ~p", [?MODULE, jobfmt(Job2)]),
    ok.


-spec kill_job_int(#job{}) -> #job{}.
kill_job_int(#job{pid = undefined} = Job) ->
    Job;

kill_job_int(#job{pid = Pid, ref = Ref} = Job) ->
    couch_log:info("~p kill_job_int ~p", [?MODULE, jobfmt(Job)]),
    case erlang:is_process_alive(Pid) of
        true ->
            ok = mem3_reshard_job_sup:terminate_child(Pid);
        false ->
            ok
    end,
    demonitor(Ref, [flush]),
    Job1 = Job#job{pid = undefined, ref = undefined},
    true = ets:insert(?MODULE, Job1),
    Job1.


-spec handle_job_exit(#job{}, term(), #state{}) -> ok.
handle_job_exit(#job{split_state = completed} = Job, normal, State) ->
    couch_log:notice("~p completed job ~s exited", [?MODULE, Job#job.id]),
    Job1 = Job#job{
        pid = undefined,
        ref = undefined,
        job_state = completed,
        time_updated = now_sec(),
        state_info = []
    },
    Job2 = update_job_state_history(Job1),
    ok = mem3_reshard_store:store_job(State, Job2),
    true = ets:insert(?MODULE, Job2),
    ok;

handle_job_exit(#job{job_state = running} = Job, normal, _State) ->
    couch_log:notice("~p running job ~s stopped", [?MODULE, Job#job.id]),
    OldInfo = Job#job.state_info,
    Job1 = Job#job{
        pid = undefined,
        ref = undefined,
        job_state = stopped,
        time_updated = now_sec(),
        state_info = info_update(reason, <<"Job stopped">>, OldInfo)
    },
    true = ets:insert(?MODULE, update_job_state_history(Job1)),
    ok;

handle_job_exit(#job{job_state = running} = Job, shutdown, _State) ->
    couch_log:notice("~p job ~s shutdown", [?MODULE, Job#job.id]),
    OldInfo = Job#job.state_info,
    Job1 = Job#job{
        pid = undefined,
        ref = undefined,
        job_state = stopped,
        time_updated = now_sec(),
        state_info = info_update(reason, <<"Job shutdown">>, OldInfo)
    },
    true = ets:insert(?MODULE, update_job_state_history(Job1)),
    ok;

handle_job_exit(#job{job_state = running} = Job, {shutdown, Msg},  _State) ->
    couch_log:notice("~p job ~s shutdown ~p", [?MODULE, Job#job.id, Msg]),
    OldInfo = Job#job.state_info,
    Job1 = Job#job{
        pid = undefined,
        ref = undefined,
        job_state = stopped,
        time_updated = now_sec(),
        state_info = info_update(reason, <<"Job shutdown">>, OldInfo)
    },
    true = ets:insert(?MODULE, update_job_state_history(Job1)),
    ok;

handle_job_exit(#job{} = Job, Error, State) ->
    couch_log:notice("~p job ~s failed ~p", [?MODULE, Job#job.id, Error]),
    OldInfo = Job#job.state_info,
    Job1 = Job#job{
        pid = undefined,
        ref = undefined,
        job_state = failed,
        time_updated = now_sec(),
        state_info = info_update(reason, Error, OldInfo)
    },
    Job2 = update_job_state_history(Job1),
    ok = mem3_reshard_store:store_job(State, Job2),
    true = ets:insert(?MODULE, Job2),
    ok.


-spec job_by_id(job_id()) -> #job{} | not_found.
job_by_id(Id) ->
    case ets:lookup(?MODULE, Id) of
        [] ->
            not_found;
        [#job{}=Job] ->
            Job
    end.


-spec job_by_pid(pid()) -> {ok, #job{}} | {error, not_found}.
job_by_pid(Pid) when is_pid(Pid) ->
    case ets:match_object(?MODULE, #job{pid=Pid, _='_'}) of
        [] ->
            {error, not_found};
        [#job{}=Job] ->
            {ok, Job}
    end.


-spec state_id() -> binary().
state_id() ->
    Ver = iolist_to_binary(io_lib:format("~3..0B", [?JOB_STATE_VERSION])),
    <<?STATE_PREFIX/binary, Ver/binary>>.


-spec job_id(#job{}) -> binary().
job_id(#job{source = #shard{name = SourceName}}) ->
    HashInput = [SourceName, atom_to_binary(node(), utf8)],
    IdHashList = couch_util:to_hex(crypto:hash(sha256, HashInput)),
    IdHash = iolist_to_binary(IdHashList),
    Prefix = iolist_to_binary(io_lib:format("~3..0B", [?JOB_ID_VERSION])),
    <<Prefix/binary, "-", IdHash/binary>>.


-spec target_shards(#shard{}, split()) -> [#shard{}].
target_shards(#shard{name = Name, range = [B, E], dbname = DbName}, Split) when
        is_integer(Split), Split >= 2, (E - B + 1) >= Split ->
    Ranges = target_ranges([B, E], Split),
    <<"shards/", _:8/binary, "-", _:8/binary, "/", DbAndSuffix/binary>>  = Name,
    [DbName, Suffix] = binary:split(DbAndSuffix, <<".">>),
    [build_shard(R, DbName, Suffix) || R <- Ranges].


-spec target_ranges([range_pos()], split()) ->
        [[range_pos()]].
target_ranges([Begin, End], Split) when (End - Begin + 1) >= Split, Split >=2 ->
    Len = End - Begin + 1,  % + 1 since intervals are inclusive
    NewLen = Len div Split,
    Rem = Len rem Split,
    Ranges = [[I, I + NewLen - 1] || I <- lists:seq(Begin, End - Rem, NewLen)],
    % Adjust last end to always match the original end to ensure we always
    % cover the whole range. In case when remainder is larger this will make
    % the last range larger. Improve the algorithm later to re-distribute
    % the remainder equally amonst the chunks.
    {BeforeLast, [[BeginLast, _]]} = lists:split(Split - 1, Ranges),
    BeforeLast ++ [[BeginLast, End]].


-spec shard_from_name(binary()) -> #shard{}.
shard_from_name(<<"shards/", _:8/binary,"-", _:8/binary,"/", Rest/binary>> = Shard) ->
    Range = mem3:range(Shard),
    [DbName, Suffix] = binary:split(Rest, <<".">>),
    build_shard(Range, DbName, Suffix).


-spec build_shard([non_neg_integer()], binary(), binary()) -> #shard{}.
build_shard(Range, DbName, Suffix) ->
    Shard = #shard{dbname = DbName, range = Range, node = node()},
    mem3_util:name_shard(Shard, <<".", Suffix/binary>>).


-spec now_sec() -> non_neg_integer().
now_sec() ->
    {Mega, Sec, _Micro} = os:timestamp(),
    Mega * 1000000 + Sec.


-spec jobs_by_dbname(binary()) -> [#job{}].
jobs_by_dbname(Name) ->
    Pat = #job{source = #shard{name = Name, _ = '_'}, _ = '_'},
    [Job || [Job] <- ets:match_object(?MODULE, Pat)].


-spec running_jobs() -> [#job{}].
running_jobs() ->
    Pat = #job{job_state = running, _ = '_'},
    [Job || [Job] <- ets:match_object(?MODULE, Pat)].


-spec fail_jobs_for_deleted_sources([#job{}], #state{}) -> ok.
fail_jobs_for_deleted_sources(Jobs, #state{} = State) ->
    JobCount = length(Jobs),
    couch_log:notice("~p fail ~B jobs, source deleted", [?MODULE, JobCount]),
    [stop_job_int(Job, failed, "Source was deleted", State) || Job <- Jobs],
    ok.


-spec info_update(atom(), any(), [tuple()]) -> [tuple()].
info_update(Key, Val, StateInfo) ->
    lists:keystore(Key, 1, StateInfo, {Key, Val}).


-spec info_delete(atom(), [tuple()]) -> [tuple()].
info_delete(Key, StateInfo) ->
    lists:keydelete(Key, 1, StateInfo).


-spec checkpoint_int(#job{}, #state{}, pid()) -> #state{}.
checkpoint_int(Job, State, From) ->
    couch_log:notice("~p checkpoint ~s", [?MODULE, jobfmt(Job)]),
    case report_int(Job, From) of
        ok ->
            ok = mem3_reshard_store:store_job(State, Job),
            State;
        not_found ->
            couch_log:error("~p checkpoint : couldn't find ~p", [?MODULE, Job]),
            State
    end.


-spec report_int(#job{}, pid()) -> ok | not_found.
report_int(Job, From) ->
    case ets:lookup(?MODULE, Job#job.id) of
        [#job{pid = From}] ->
            true = ets:insert(?MODULE, Job),
            ok;
        _ ->
            couch_log:error("~p reporting : couldn't find ~p", [?MODULE, Job]),
            not_found
    end.


-spec db_monitor(pid()) -> no_return().
db_monitor(Server) ->
    couch_log:notice("~p db monitor ~p starting", [?MODULE, self()]),
    EvtRef = erlang:monitor(process, couch_event_server),
    couch_event:register_all(self()),
    db_monitor_loop(Server, EvtRef).


-spec db_monitor_loop(pid(), reference()) -> no_return().
db_monitor_loop(Server, EvtRef) ->
    receive
        {'$couch_event', DbName, deleted} ->
            case jobs_by_dbname(DbName) of
                [] ->
                    ok;
                Jobs ->
                    gen_server:cast(Server, {db_deleted, Jobs})
            end,
            db_monitor_loop(Server, EvtRef);
        {'$couch_event', _, _} ->
            db_monitor_loop(Server, EvtRef);
        {'DOWN', EvtRef, _, _, Info} ->
            couch_log:error("~p db monitor listener died ~p", [?MODULE, Info]),
            exit({db_monitor_died, Info});
        Msg ->
            couch_log:error("~p db monitor unexpected msg ~p", [?MODULE, Msg]),
            db_monitor_loop(Server, EvtRef)
    end.


-spec reset_state(#state{}) -> #state{}.
reset_state(#state{} = State) ->
    couch_log:warning("~p deleting state", [?MODULE]),
    ok = mem3_reshard_store:delete_state(State),
    Jobs = mem3_reshard_store:get_jobs(State),
    lists:foldl(fun(#job{id = Id}, StateAcc) ->
        couch_log:warning("~p deleting job ~p", [?MODULE, Id]),
        ok = mem3_reshard_store:delete_job(StateAcc, Id),
        StateAcc
    end, State, Jobs).


-spec iso8601(non_neg_integer()) -> string().
iso8601(UnixSec) ->
    Base = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    GSec = Base + UnixSec,
    {{Y, Mon, D}, {H, Min, S}} = calendar:gregorian_seconds_to_datetime(GSec),
    Format = "~B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    lists:flatten(io_lib:format(Format, [Y, Mon, D, H, Min, S])).


-spec update_job_state_history(#job{}) -> #job{}.
update_job_state_history(#job{job_state = St, time_updated = Ts} = Job) ->
    Hist = Job#job.state_history,
    Job#job{state_history = update_history(St, Ts, Hist)}.


-spec update_history(atom(), time_sec(), list()) -> list().
update_history(State, Ts, [{State, _} | Rest]) ->
    lists:sublist([{State, Ts} | Rest], max_history());

update_history(State, Ts, History) ->
    lists:sublist([{State, Ts} | History], max_history()).


-spec max_history() -> non_neg_integer().
max_history() ->
    config:get_integer("reshard", "max_history", ?DEFAULT_MAX_HISTORY).


-spec statefmt(#state{} | term()) -> string().
statefmt(#state{} = State) ->
    #state{
        state = StateName,
        time_updated = Updated,
        db_monitor = Pid
    } = State,
    Total = ets:info(?MODULE, size),
    Active = mem3_reshard_job_sup:count_children(),
    UpdatedFmt = iso8601(Updated),
    Msg = "#state{~s updated:~s total:~B active:~B mon:~p}",
    Fmt = io_lib:format(Msg, [StateName, UpdatedFmt, Total, Active, Pid]),
    lists:flatten(Fmt);

statefmt(State) ->
    Fmt = io_lib:format("<Unknown split state:~p>", [State]),
    lists:flatten(Fmt).


-spec jobfmt(#job{}) -> string().
jobfmt(#job{} = Job) ->
    mem3_reshard_job:jobfmt(Job).
