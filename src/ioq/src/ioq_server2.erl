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

-module(ioq_server2).
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
    start_link/1,
    start_link/3,
    call/3,
    pcall/1,
    pcall/2
]).
-export([
    get_state/0,
    get_state/1,
    update_config/0,
    get_queue_depths/0,
    get_queue_depths/1,
    get_concurrency/0,
    set_concurrency/1,
    get_counters/0
]).


-include_lib("ioq/include/ioq.hrl").


-define(DEFAULT_RESIZE_LIMIT, 1000).
-define(DEFAULT_CONCURRENCY, 1).
-define(DEFAULT_SCALE_FACTOR, 2.0).
-define(DEFAULT_MAX_PRIORITY, 10000.0).


-record(state, {
    reqs :: khash:khash(),
    waiters :: khash:khash(),
    queue :: hqueue:hqueue(),
    concurrency = ?DEFAULT_CONCURRENCY :: pos_integer(),
    iterations = 0 :: non_neg_integer(),
    class_p :: khash:khash(),  %% class priorities
    user_p :: khash:khash(),   %% user priorities
    shard_p :: khash:khash(),  %% shard priorities
    scale_factor = ?DEFAULT_SCALE_FACTOR :: float(),
    dedupe = true :: boolean(),
    resize_limit = ?DEFAULT_RESIZE_LIMIT :: pos_integer(),
    next_key = 1 :: pos_integer(),
    server_name :: atom(),
    scheduler_id = 0 :: non_neg_integer(),
    max_priority = ?DEFAULT_MAX_PRIORITY :: float(),
    fd :: pid() | undefined
}).


-type state() :: #state{}.
-type waiter_key() :: {pid(), integer()} | pos_integer().
-type priority() :: float(). %% should be non_negative_float().

%% Hacky queue_depth type due to existing fixed element lists for JSON in API
%% Actual type is:
%% [
%%   {compaction, non_neg_integer()},
%%   {replication, non_neg_integer()},
%%   {low, non_neg_integer()},
%%   {channels, {[{username(), [Interactive, DbUpdate, ViewUpdate]}]}}
%% ]
%% when Interactive = DbUpdate = ViewUpdate = non_neg_integer().
-type depths() :: compaction | replication | low.
-type user_depth() :: {binary(), [non_neg_integer()]}.
-type depth_ele() :: {depths(), non_neg_integer()} | user_depth().
-type queue_depths() :: [depth_ele()].
-type read_write() :: reads | writes | unknown.


-define(SERVER_ID(SID), list_to_atom("ioq_server_" ++ integer_to_list(SID))).


-spec call(pid(), term(), io_dimensions()) -> term().
%%call(Fd, Msg, Dimensions) when Dimensions =/= undefined ->
call(Fd, Msg, Dimensions) ->
    Req0 = #ioq_request{
        fd = Fd,
        msg = Msg,
        t0 = os:timestamp()
    },
    Req = add_request_dimensions(Req0, Dimensions),
    call_int(Fd, Req).

%% TODO: handle Clouseau requests with isolated IOQ2 pid
%%call_int(#ioq_file{fd={clouseau, _}=IOF, Req) ->
call_int({Name, Node}=Server, #ioq_request{msg=Msg}=Req) when is_atom(Name) andalso is_atom(Node) ->
    case should_bypass(Req) of
        true ->
            gen_server:call(Server, Msg, infinity);
        false ->
            %% TODO: add dedicated clouseau IOQ pid
            gen_server:call(ioq_server2, Req, infinity)
    end;
call_int(Pid, #ioq_request{msg=Msg}=Req) when is_pid(Pid) ->
    case should_bypass(Req) of
        true ->
            gen_server:call(Pid, Msg, infinity);
        false ->
            %% TODO: add dedicated clouseau IOQ pid
            gen_server:call(ioq_server2, Req, infinity)
    end;
call_int(#ioq_file{ioq=undefined, fd=Fd}, #ioq_request{msg=Msg}=Req) ->
    Class = atom_to_list(Req#ioq_request.class),
    case config:get_boolean("ioq2.bypass", Class, false) of
        true ->
            RW = rw(Msg),
            couch_stats:increment_counter([couchdb, io_queue2, bypassed_count]),
            couch_stats:increment_counter(
                [couchdb, io_queue2, RW, bypassed_count]),
            gen_server:call(Fd, Msg, infinity);
        _ ->
            Server = ioq_server2,
            %%Server = case ioq_opener:get_pid_for(Fd) of
            %%    undefined ->
            %%        IOQPid = case ioq_opener:fetch_pid_for(Req) of
            %%            undefined ->
            %%                ioq_server2;
            %%            IOQPid0 ->
            %%                IOQPid0
            %%        end,
            %%        ioq_opener:set_pid_for(Fd, IOQPid),
            %%        IOQPid;
            %%    IOQPid ->
            %%        IOQPid
            %%end,
            gen_server:call(Server, Req, infinity)
    end;
call_int(#ioq_file{ioq=IOP}, #ioq_request{}=Req) ->
    gen_server:call(IOP, Req, infinity).


-spec pcall(any()) -> any().
pcall(Msg) ->
    pcall(Msg, 500).


-spec pcall(any(), non_neg_integer()) -> any().
pcall(Msg, Timeout) ->
    {MainPid, MainRef} = spawn_monitor(fun() ->
        PidRefs = lists:map(fun(Name) ->
            spawn_monitor(fun() ->
                Resp = gen_server:call(Name, Msg, Timeout),
                exit({resp_ok, Resp})
            end)
        end, ioq_sup:get_ioq2_servers()),
        Resps = lists:map(fun({Pid, _}) ->
            receive
                {'DOWN', _, _, Pid, {resp_ok, Resp}} ->
                    Resp;
                {'DOWN', _, _, Pid, Error} ->
                    exit(Error)
            end
        end, PidRefs),
        exit({resp_ok, Resps})
    end),
    receive
        {'DOWN', _, _, MainPid, {resp_ok, Resps}} ->
            {ok, Resps};
        {'DOWN', _, _, MainPid, Error} ->
            {error, Error}
    after Timeout ->
        erlang:demonitor(MainRef, [flush]),
        exit(MainPid, kill),
        {error, timeout}
    end.


-spec get_queue_depths() -> queue_depths().
get_queue_depths() ->
    case pcall(get_pending_reqs, 500) of
        {ok, PReqs} ->
            get_queue_depths([Req || {_Priority, Req} <- lists:flatten(PReqs)]);
        {error, _} ->
            [
                {compaction, ?BAD_MAGIC_NUM},
                {replication, ?BAD_MAGIC_NUM},
                {low, ?BAD_MAGIC_NUM},
                {channels, {[]}}
            ]
    end.


-spec get_queue_depths([ioq_request()]) -> queue_depths().
get_queue_depths(Reqs) ->
    {ok, Users0} = khash:new(),
    {Compaction, Replication, Low, Users} = lists:foldl(
        fun
            (#ioq_request{class=db_compact}, {C, R, L, U}) ->
                {C+1, R, L, U};
            (#ioq_request{class=view_compact}, {C, R, L, U}) ->
                {C+1, R, L, U};
            (#ioq_request{class=internal_repl}, {C, R, L, U}) ->
                {C, R+1, L, U};
            (#ioq_request{class=low}, {C, R, L, U}) ->
                {C, R, L+1, U};
            (#ioq_request{class=Class, user=User}, {C, R, L, U}) ->
                [UI0, UDB0, UV0] = case khash:get(U, User) of
                    undefined ->
                        [0,0,0];
                    UC0 ->
                        UC0
                end,
                UC = case Class of
                    db_update ->
                        [UI0, UDB0+1, UV0];
                    view_update ->
                        [UI0, UDB0, UV0+1];
                    _Interactive ->
                        [UI0+1, UDB0, UV0]
                end,
                ok = khash:put(U, User, UC),
                {C, R, L, U}
        end,
        {0, 0, 0, Users0},
        Reqs
    ),
    [
        {compaction, Compaction},
        {replication, Replication},
        {low, Low},
        {channels, {khash:to_list(Users)}}
    ].


-spec get_concurrency() -> non_neg_integer().
get_concurrency() ->
    case pcall(get_concurrency, 500) of
        {ok, Concurrencies} ->
            lists:sum(Concurrencies);
        {error, _} ->
            ?BAD_MAGIC_NUM
    end.


-spec set_concurrency(non_neg_integer()) -> non_neg_integer().
set_concurrency(C) when is_integer(C), C > 0 ->
    lists:foldl(
        fun(Pid, Total) ->
            {ok, Old} = gen_server:call(Pid, {set_concurrency, C}, 1000),
            Total + Old
        end,
        0,
        ioq_sup:get_ioq2_servers()
    );
set_concurrency(_) ->
    erlang:error(badarg).


get_counters() ->
    undefined.


%% @equiv get_state(?SERVER_ID(1))
-spec get_state() -> any().
get_state() ->
    get_state(?SERVER_ID(1)).


%% Returns a mutated #state{} with list representations of khash/hqueue objects
-spec get_state(atom()) -> any().
get_state(Server) ->
    gen_server:call(Server, get_state, infinity).


-spec update_config() -> ok.
update_config() ->
    gen_server:call(?SERVER_ID(1), update_config, infinity).


start_link() ->
    start_link(?MODULE).


start_link(?MODULE) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{global, ?MODULE}], []);
start_link({by_user, _Name}=User) ->
    gen_server:start_link(?MODULE, [User], []);
start_link({by_shard, _Name}=Shard) ->
    gen_server:start_link(?MODULE, [Shard], []);
start_link({by_shard, _Name, _Fd}=Shard) ->
    gen_server:start_link(?MODULE, [Shard], []).


start_link(Name, SID, Bind) ->
    Options = case Bind of
        true -> [{scheduler, SID}];
        false -> []
    end,
    gen_server:start_link({local, Name}, ?MODULE, [Name, SID], Options).


init([Init]) ->
    {Type, Name, Fd} = case Init of
        {_, _, FdPid} = Init0 ->
            erlang:link(FdPid),
            Init0;
        {Type0, Name0} ->
            {Type0, Name0, undefined}
    end,
    {ok, HQ} = hqueue:new(),
    {ok, Reqs} = khash:new(),
    {ok, Waiters} = khash:new(),
    erlang:put(couch_file, Name),
    State = #state{
        queue = HQ,
        reqs = Reqs,
        waiters = Waiters,
        server_name = Name,
        scheduler_id = Type,
        fd = Fd
    },
    {ok, update_config_int(State)}.


handle_call(get_state, _From, State) ->
    Resp = State#state{
        user_p = khash:to_list(State#state.user_p),
        class_p = khash:to_list(State#state.class_p),
        shard_p = khash:to_list(State#state.shard_p),
        reqs = khash:to_list(State#state.reqs),
        waiters = khash:to_list(State#state.waiters),
        queue = hqueue:to_list(State#state.queue)
    },

    {reply, Resp, State, 0};
handle_call(#ioq_request{} = Req, From, State) ->
    {noreply, enqueue_request(Req#ioq_request{from=From}, State), 0};
handle_call({hqueue, Method, Args}, _From, #state{queue=HQ}=State) ->
    Resp = erlang:apply(hqueue, Method, [HQ | Args]),
    {reply, Resp, State, 0};
handle_call(update_config, _From, State) ->
    {reply, ok, update_config_int(State), 0};
handle_call(get_concurrency, _From, State) ->
    {reply, State#state.concurrency, State, 0};
handle_call({set_concurrency, C}, _From, State) when is_integer(C), C > 0 ->
    {reply, {ok, State#state.concurrency}, State#state{concurrency = C}, 0};
handle_call(get_reqs, _From, #state{reqs=Reqs}=State) ->
    {reply, khash:to_list(Reqs), State, 0};
handle_call(get_pending_reqs, _From, #state{queue=HQ}=State) ->
    {reply, hqueue:to_list(HQ), State, 0};
handle_call(get_counters, _From, State) ->
    {reply, undefined, State, 0};
handle_call(Msg, _From, State) ->
    {reply, {error, unknown, Msg}, State, 0}.


handle_cast(update_config, State) ->
    {noreply, update_config_int(State), 0};
handle_cast(_Msg, State) ->
    {noreply, State, 0}.


handle_info({Ref, Reply}, #state{reqs = Reqs} = State) ->
    case khash:get(Reqs, Ref) of
        undefined ->
            ok;
        #ioq_request{ref=Ref}=Req ->
            ok = khash:del(Reqs, Ref),
            TResponse = os:timestamp(),
            ServiceTime = time_delta(TResponse, Req#ioq_request.tsub),
            IOWait = time_delta(TResponse, Req#ioq_request.t0),
            couch_stats:update_histogram(
                [couchdb, io_queue2, svctm], ServiceTime),
            couch_stats:update_histogram([couchdb, io_queue2, iowait], IOWait),
            erlang:demonitor(Ref, [flush]),
            send_response(State#state.waiters, Req, Reply)
    end,
    {noreply, State, 0};
handle_info({'DOWN', Ref, _, _, Reason}, #state{reqs = Reqs} = State) ->
    case khash:get(Reqs, Ref) of
        undefined ->
            ok;
        #ioq_request{ref=Ref}=Req ->
            couch_stats:increment_counter([couchdb, io_queue2, io_errors]),
            ok = khash:del(Reqs, Ref),
            send_response(State#state.waiters, Req, {'EXIT', Reason})
    end,
    {noreply, State, 0};
handle_info(timeout, State) ->
    {noreply, maybe_submit_request(State)};
handle_info(_Info, State) ->
    {noreply, State, 0}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, #state{}=State, _Extra) ->
    {ok, State}.


-spec update_config_int(state()) -> state().
update_config_int(State) ->
    Concurrency = config:get_integer("ioq2", "concurrency", ?DEFAULT_CONCURRENCY),
    ResizeLimit = config:get_integer("ioq2", "resize_limit", ?DEFAULT_RESIZE_LIMIT),
    DeDupe = config:get_boolean("ioq2", "dedupe", true),

    ScaleFactor = ioq_config:to_float(
        config:get("ioq2", "scale_factor"),
        ?DEFAULT_SCALE_FACTOR
    ),

    MaxPriority = ioq_config:to_float(
        config:get("ioq2", "max_priority"),
        ?DEFAULT_MAX_PRIORITY
    ),

    {ok, ClassP} = ioq_config:build_class_priorities(),
    {ok, UserP} = ioq_config:build_user_priorities(),
    {ok, ShardP} = ioq_config:build_shard_priorities(),

    State#state{
        user_p = UserP,
        class_p = ClassP,
        shard_p = ShardP,
        scale_factor = ScaleFactor,
        concurrency = Concurrency,
        dedupe = DeDupe,
        resize_limit = ResizeLimit,
        max_priority = MaxPriority
    }.


-spec maybe_submit_request(state()) -> state().
maybe_submit_request(#state{reqs=Reqs, concurrency=C}=State) ->
    NumReqs = khash:size(Reqs),
    case NumReqs < C of
        true ->
            case make_next_request(State) of
                State ->
                    State;
                NewState when NumReqs >= C-1 ->
                    NewState;
                NewState ->
                    maybe_submit_request(NewState)
            end;
        false ->
            State
    end.


-spec make_next_request(state()) -> state().
make_next_request(#state{queue=HQ}=State) ->
    case hqueue:extract_max(HQ) of
        {error, empty} ->
            State;
        {Priority, #ioq_request{} = Req} ->
            submit_request(Req#ioq_request{fin_priority=Priority}, State)
    end.


-spec submit_request(ioq_request(), state()) -> state().
submit_request(Req, #state{iterations=I, resize_limit=RL}=State) when I >= RL ->
    ok = hqueue:scale_by(State#state.queue, State#state.scale_factor),
    submit_request(Req, State#state{iterations=0});
submit_request(Req, #state{iterations=Iterations}=State) ->
    #ioq_request{
        fd = Fd0,
        msg = Call,
        class = Class,
        t0 = T0
    } = Req,
    #state{reqs = Reqs} = State,

    Fd = case Fd0 of
        #ioq_file{fd=Fd1} -> Fd1;
        _                 -> Fd0
    end,

    % make the request
    Ref = erlang:monitor(process, Fd),
    Fd ! {'$gen_call', {self(), Ref}, Call},

    % record some stats
    RW = rw(Call),

    SubmitTime = os:timestamp(),
    Latency = time_delta(SubmitTime, T0),
    couch_stats:increment_counter([couchdb, io_queue2, Class, count]),
    couch_stats:increment_counter([couchdb, io_queue2, RW, count]),
    couch_stats:update_histogram([couchdb, io_queue2, submit_delay], Latency),
    khash:put(Reqs, Ref, Req#ioq_request{tsub=SubmitTime, ref=Ref}),
    State#state{iterations=Iterations+1}.


-spec send_response(khash:khash(), ioq_request(), term()) -> [ok].
send_response(Waiters, #ioq_request{key=Key}, Reply) ->
    Waiting = khash:get(Waiters, Key),
    khash:del(Waiters, Key),
    [gen_server:reply(W, Reply) || W <- Waiting].


-spec waiter_key(ioq_request(), state()) -> {waiter_key(), state()}.
waiter_key(Req, State) ->
    case {State#state.dedupe, Req#ioq_request.msg} of
        {true, {pread_iolist, Pos}} ->
            {{Req#ioq_request.fd, Pos}, State};
        _ ->
            Next = State#state.next_key,
            {Next, State#state{next_key = Next + 1}}
    end.


-spec enqueue_request(ioq_request(), state()) -> state().
enqueue_request(Req, #state{queue=HQ, waiters=Waiters}=State0) ->
    #ioq_request{
        from = From,
        msg = Msg
    } = Req,
    {ReqKey, State} = waiter_key(Req, State0),
    RW = rw(Msg),

    couch_stats:increment_counter([couchdb, io_queue2, queued]),
    couch_stats:increment_counter([couchdb, io_queue2, RW, queued]),

    case khash:get(State#state.waiters, ReqKey, not_found) of
        not_found ->
            Priority = prioritize_request(Req, State),
            Req1 = Req#ioq_request{
                key = ReqKey,
                init_priority = Priority
            },
            hqueue:insert(HQ, Priority, Req1),
            khash:put(State#state.waiters, ReqKey, [From]);
        Pids ->
            couch_stats:increment_counter([couchdb, io_queue2, merged]),
            khash:put(Waiters, ReqKey, [From | Pids])
    end,
    State.


-spec add_request_dimensions(ioq_request(), io_dimensions()) -> ioq_request().
add_request_dimensions(Request, {Class, Shard}) ->
    add_request_dimensions(Request, {Class, Shard, undefined});
add_request_dimensions(Request, {Class, Shard0, GroupId}) ->
    {Shard, User, DbName} = case {Class, Shard0} of
        {interactive, "dbcopy"} ->
            {undefined, undefined, undefined};
        {db_meta, security} ->
            {undefined, undefined, undefined};
        _ ->
            Shard1 = filename:rootname(Shard0),
            {User0, DbName0} = shard_info(Shard1),
            {Shard1, User0, DbName0}
    end,
    Request#ioq_request{
        shard = Shard,
        user = User,
        db = DbName,
        ddoc = GroupId,
        class = Class
    };
add_request_dimensions(Request, undefined) ->
    Request#ioq_request{class=other}.


-spec shard_info(dbname()) -> {any(), any()}.
shard_info(Shard) ->
    try split(Shard) of
        [<<"shards">>, _, <<"heroku">>, AppId, DbName] ->
            {<<AppId/binary, ".heroku">>, DbName};
        [<<"shards">>, _, DbName] ->
            {system, DbName};
        [<<"shards">>, _, Account, DbName] ->
            {Account, DbName};
        [<<"shards">>, _, Account | DbParts] ->
            {Account, filename:join(DbParts)};
        _ ->
            {undefined, undefined}
    catch _:_ ->
        {undefined, undefined}
    end.


-spec split(binary()) -> [binary()]
    ; ([binary()]) -> [binary()].
split(B) when is_binary(B) ->
    split(B, 0, 0, []);
split(B) ->
    B.

-spec split(binary(), non_neg_integer(), non_neg_integer(), [binary()]) -> [binary()].
split(B, O, S, Acc) ->
    case B of
    <<_:O/binary>> ->
        Len = O - S,
        <<_:S/binary, Part:Len/binary>> = B,
        lists:reverse(Acc, [Part]);
    <<_:O/binary, $/, _/binary>> ->
        Len = O - S,
        <<_:S/binary, Part:Len/binary, _/binary>> = B,
        split(B, O+1, O+1, [Part | Acc]);
    _ ->
        split(B, O+1, S, Acc)
    end.

-spec time_delta(T1, T0) -> Tdiff when
      T1 :: erlang:timestamp(),
      T0 :: erlang:timestamp(),
      Tdiff :: integer().
time_delta(T1, T0) ->
    trunc(timer:now_diff(T1, T0) / 1000).


-spec rw(io_dimensions()) -> read_write().
rw({pread_iolist, _}) ->
    reads;
rw({append_bin, _}) ->
    writes;
rw({append_bin, _, _}) ->
    writes;
rw(_) ->
    unknown.


-spec prioritize_request(ioq_request(), state()) -> priority().
prioritize_request(Req, State) ->
    #state{
        class_p = ClassP,
        user_p = UserP,
        shard_p = ShardP,
        max_priority = Max
    } = State,
    case ioq_config:prioritize(Req, ClassP, UserP, ShardP) of
        Priority when Priority < 0.0 -> 0.0;
        Priority when Priority > Max -> Max;
        Priority -> Priority
    end.


should_bypass(#ioq_request{class=Class}) ->
    config:get_boolean("ioq2.bypass", atom_to_list(Class), false).


%% ioq_server2 Tests


-ifdef(TEST).


-include_lib("eunit/include/eunit.hrl").


mock_server() ->
    mock_server([]).


mock_server(Config) ->
    meck:new(config),
    meck:expect(config, get, fun(Group) ->
        couch_util:get_value(Group, Config, [])
    end),
    meck:expect(config, get, fun(_,_) ->
        undefined
    end),
    meck:expect(config, get, fun("ioq2", _, Default) ->
        Default
    end),
    meck:expect(config, get_integer, fun("ioq2", _, Default) ->
        Default
    end),
    meck:expect(config, get_boolean, fun("ioq2", _, Default) ->
        Default
    end),
    {ok, State} = ioq_server2:init([{global, ?SERVER_ID(1)}]),
    State.


unmock_server(_) ->
    true = meck:validate(config),
    ok = meck:unload(config).


empty_config_test_() ->
    {
        "Empty config tests",
        {
            foreach,
            fun mock_server/0,
            fun unmock_server/1,
            [
                fun test_basic_server_config/1,
                fun test_simple_request_priority/1,
                fun test_simple_dedupe/1,
                fun test_io_error/1
            ]
        }
    }.


simple_config_test_() ->
    {
        "Simple config tests",
        {
            foreach,
            fun() ->
                Config = [
                    {"ioq2.classes", [{"db_compact", "0.9"}]},
                    {"ioq2", [{"resize_limit", "10"}]}
                ],
                mock_server(Config)
            end,
            fun unmock_server/1,
            [
                fun test_simple_config/1,
                fun test_auto_scale/1
            ]
        }
    }.


priority_extremes_test_() ->
    {
        "Test min/max priorities",
        {
            foreach,
            fun() ->
                Config = [
                    {"ioq2.classes", [
                        {"db_compact", "9999999.0"},
                        {"interactive", "-0.00000000000000001"}
                    ]}
                ],
                mock_server(Config)
            end,
            fun unmock_server/1,
            [
                fun test_min_max_priorities/1
            ]
        }
    }.


queue_depths_test_() ->
    Foo = <<"foo">>,
    Bar = <<"bar">>,
    Reqs = [
        #ioq_request{user=Foo, class=db_compact},
        #ioq_request{user=Bar, class=db_compact},
        #ioq_request{user=Bar, class=view_compact},
        #ioq_request{user=Foo, class=internal_repl},
        #ioq_request{user=Bar, class=internal_repl},
        #ioq_request{user=Bar, class=internal_repl},
        #ioq_request{class=low},

        #ioq_request{user=Foo, class=interactive},
        #ioq_request{user=Foo, class=interactive},
        #ioq_request{user=Foo, class=db_update},
        #ioq_request{user=Foo, class=view_update},
        #ioq_request{user=Foo, class=view_update},
        #ioq_request{user=Foo, class=view_update},
        #ioq_request{user=Foo, class=view_update},

        #ioq_request{user=Bar, class=interactive},
        #ioq_request{user=Bar, class=db_update},
        #ioq_request{user=Bar, class=db_update},
        #ioq_request{user=Bar, class=db_update},
        #ioq_request{user=Bar, class=view_update}
    ],
    Expected = [
        {compaction, 3},
        {replication, 3},
        {low, 1},
        {channels, {[
            {<<"foo">>, [2,1,4]},
            {<<"bar">>, [1,3,1]}
        ]}}
    ],

    {
        "Test queue depth stats",
        ?_assertEqual(
            Expected,
            get_queue_depths(Reqs)
        )
    }.


test_basic_server_config(St0) ->
    {reply, RespState, _St1, 0} = handle_call(get_state, pid, St0),
    [
        ?_assertEqual([], RespState#state.user_p),
        ?_assertEqual(
            lists:sort(?DEFAULT_CLASS_PRIORITIES),
            lists:sort(RespState#state.class_p)
        ),
        ?_assertEqual([], RespState#state.shard_p)
    ].


test_simple_request_priority(St0) ->
    From = pid1,
    Request0 = #ioq_request{class=db_compact},
    Priority = prioritize_request(Request0, St0),
    Request1 = Request0#ioq_request{
        init_priority = Priority,
        from = From,
        key = St0#state.next_key
    },
    {noreply, St1, 0} = handle_call(Request0, From, St0),
    {reply, RespState, _St2, 0} = handle_call(get_state, From, St1),
    [
        ?_assertEqual(
            [{Priority, Request1}],
            RespState#state.queue
        )
    ].


test_simple_dedupe(St0) ->
    FromA = pid1,
    FromB = pid2,
    Fd = fd,
    Pos = 1234,
    Msg = {pread_iolist, Pos},
    Request0 = #ioq_request{
        class=db_compact,
        fd = Fd,
        msg = Msg
    },
    {ReqKey, St1} = waiter_key(Request0, St0),
    Priority = prioritize_request(Request0, St1),
    Request1A = Request0#ioq_request{
        init_priority = Priority,
        from = FromA,
        key = {Fd, Pos}
    },
    {noreply, St2, 0} = handle_call(Request0, FromA, St1),
    {noreply, St3, 0} = handle_call(Request0, FromB, St2),
    {reply, RespState, _St4, 0} = handle_call(get_state, FromA, St3),
    [
        ?_assertEqual(
            [{Priority, Request1A}],
            RespState#state.queue
        ),
        ?_assertEqual(
            [{ReqKey, [FromB, FromA]}],
            RespState#state.waiters
        )
    ].


test_simple_config(St) ->
    RequestA = #ioq_request{},
    RequestB = #ioq_request{class=db_compact},
    PriorityA = prioritize_request(RequestA, St),
    PriorityB = prioritize_request(RequestB, St),

    [
        ?_assertEqual(
            1.0,
            PriorityA
        ),
        ?_assertEqual(
            0.9,
            PriorityB
        )
    ].


test_min_max_priorities(St) ->
    RequestA = #ioq_request{class=interactive},
    RequestB = #ioq_request{class=db_compact},
    PriorityA = prioritize_request(RequestA, St),
    PriorityB = prioritize_request(RequestB, St),

    [
        ?_assertEqual(
            0.0,
            PriorityA
        ),
        ?_assertEqual(
            ?DEFAULT_MAX_PRIORITY,
            PriorityB
        )
    ].


test_auto_scale(#state{queue=HQ}=St0) ->
    %% start with iterations=2 so we can tell when we auto-scaled
    St1 = St0#state{resize_limit=10, iterations=2},
    Pid = spawn(fun() -> receive baz -> ok end end),
    T0 = os:timestamp(),
    BaseReq = #ioq_request{t0=T0, fd=Pid},

    RequestA = BaseReq#ioq_request{ref=make_ref()},
    RequestB = BaseReq#ioq_request{class=db_compact, ref=make_ref()},
    PriorityA = prioritize_request(RequestA, St1),
    PriorityB = prioritize_request(RequestB, St1),

    {noreply, St2, 0} = handle_call(RequestB, Pid, St1),
    {noreply, St3, 0} = handle_call(RequestA, Pid, St2),

    {_, #ioq_request{init_priority=PriorityA2}} = hqueue:extract_max(HQ),
    Tests0 = [?_assertEqual(PriorityA, PriorityA2)],
    {_St, Tests} = lists:foldl(
        fun(_N, {#state{iterations=I, resize_limit=_RL}=StN0, TestsN}) ->
            ReqN = BaseReq#ioq_request{ref=make_ref()},
            ExpectedPriority = case I == 1 of
                false -> PriorityA;
                true -> PriorityB
            end,
            {noreply, StN1, 0} = handle_call(ReqN, Pid, StN0),
            StN2 = submit_request(ReqN, StN1),
            {_, #ioq_request{init_priority=PriorityN}} = hqueue:extract_max(HQ),
            {
                StN2,
                [?_assertEqual(ExpectedPriority, PriorityN) | TestsN]
            }
        end,
        {St3, Tests0},
        lists:seq(1, St3#state.resize_limit + 7)
    ),
    lists:reverse(Tests).


all_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun instantiate/1}.


many_clients_test_() ->
    FDCount = 50,
    ClientCount = 10,
    MaxDelay = 20,
    {
        setup,
        fun() -> setup_many(FDCount, MaxDelay) end,
        fun cleanup/1,
        fun(Servers) -> test_many_clients(Servers, ClientCount) end
    }.


setup() ->
    meck:new(config, [passthrough]),
    meck:expect(config, get_boolean,
        fun
            ("ioq2", "enabled", _) ->
                true;
            ("ioq2", "server_per_scheduler", _) ->
                false;
            (_, _, Default) ->
                Default
        end
    ),
    {ok, _} = application:ensure_all_started(ioq),
    FakeServer = fun(F) ->
        receive {'$gen_call', {Pid, Ref}, Call} ->
            Pid ! {Ref, {reply, Call}}
        end,
        F(F)
    end,
    #ioq_file{fd=spawn(fun() -> FakeServer(FakeServer) end)}.


setup_many(Count, RespDelay) ->
    {ok, _} = application:ensure_all_started(ioq),
    meck:new(config, [passthrough]),
    meck:expect(config, get_boolean,
        fun
            ("ioq2", "enabled", _) ->
                true;
            ("ioq2", "server_per_scheduler", _) ->
                false;
            (_, _, Default) ->
                Default
        end
    ),
    FakeServer = fun(F) ->
        receive {'$gen_call', {Pid, Ref}, Call} ->
            timer:sleep(rand:uniform(RespDelay)),
            Pid ! {Ref, {reply, Call}}
        end,
        F(F)
    end,
    [#ioq_file{fd=spawn(fun() -> FakeServer(FakeServer) end)} || _ <- lists:seq(1, Count)].


cleanup(Server) when not is_list(Server) ->
    cleanup([Server]);
cleanup(Servers) ->
    ok = application:stop(ioq),
    true = meck:validate(config),
    ok = meck:unload(config),
    [exit(ioq:fd_pid(Server), kill) || Server <- Servers].


instantiate(S) ->
    %%Old = ?DEFAULT_CONCURRENCY * (1 + length(shards())),
    Old = ?DEFAULT_CONCURRENCY,
    [{inparallel, lists:map(fun(IOClass) ->
        lists:map(fun(Shard) ->
            check_call(S, make_ref(), priority(IOClass, Shard))
        end, shards())
    end, io_classes())},
    ?_assertEqual(Old, ioq:set_disk_concurrency(10)),
    ?_assertError(badarg, ioq:set_disk_concurrency(0)),
    ?_assertError(badarg, ioq:set_disk_concurrency(-1)),
    ?_assertError(badarg, ioq:set_disk_concurrency(foo))].


check_call(Server, Call, Priority) ->
    ?_assertEqual({reply, Call}, ioq_server2:call(Server, Call, Priority)).


io_classes() -> [interactive, view_update, db_compact, view_compact,
    internal_repl, other, db_meta].


shards() ->
    [
        <<"shards/0-1/heroku/app928427/couchrest.1317609656.couch">>,
        <<"shards/0-1/foo">>,
        <<"shards/0-3/foo">>,
        <<"shards/0-1/bar">>,
        <<"shards/0-1/kocolosk/stats.1299297461.couch">>,
        <<"shards/0-1/kocolosk/my/db.1299297457.couch">>,
        other
    ].


priority(view_update, Shard) ->
    {view_update, Shard, <<"_design/foo">>};
priority(Any, Shard) ->
    {Any, Shard}.


test_many_clients(Servers, ClientCount) ->
    ClientFun = fun() ->
        ok = lists:foreach(fun(IOClass) ->
            ok = lists:foreach(fun(Shard) ->
                Server = random_server(Servers),
                Ref = make_ref(),
                Priority = priority(IOClass, Shard),
                {reply, Ref} = ioq_server2:call(Server, Ref, Priority),
                ok
            end, shards())
        end, io_classes()),
        ok
    end,
    ok = lists:foreach(fun(_) -> spawn_monitor(ClientFun) end, lists:seq(1, ClientCount)),

    Status = wait_for_success(ClientCount),
    ?_assert(Status).


wait_for_success(0) ->
    true;
wait_for_success(Count) when Count > 0 ->
    receive
        {'DOWN', _Ref, process, _Pid, normal} ->
            wait_for_success(Count - 1);
        Msg ->
            ?debugFmt("UNEXPECTED CLIENT EXIT: ~p~n", [Msg]),
            false
    end.


random_server(Servers) ->
    lists:nth(rand:uniform(length(Servers)), Servers).


test_io_error(#state{waiters=Waiters, reqs=Reqs}=State) ->
    Key = asdf,
    Ref = make_ref(),
    RefTag = make_ref(),
    Req = #ioq_request{ref=Ref, key=Key},
    khash:put(Waiters, Key, [{self(), RefTag}]),
    khash:put(Reqs, Ref, Req),
    Error = {exit, foo},
    {noreply, _State1, 0} = handle_info({'DOWN', Ref, baz, zab, Error}, State),
    Resp = receive
        {RefTag, {'EXIT', Error}} ->
            {ok, Error};
        Else ->
            {error, Else}
        after 5000 ->
            {error, timeout}
    end,
    ?_assertEqual({ok, Error}, Resp).


-endif.
