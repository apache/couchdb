% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_batch_save).

-behaviour(gen_server).

%% API
-export([start_link/2, eventually_save_doc/3, commit_now/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-include("couch_db.hrl").

-record(batch_state, {
    batch_size=1000,
    batch_interval=1000
    }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(BatchSize, BatchInterval) ->
    gen_server:start_link({local, couch_batch_save}, ?MODULE, [BatchSize, BatchInterval], []).

%%--------------------------------------------------------------------
%% Function: commit_doc(Doc) -> committed
%% Description: Puts the doc into the set to commit. Does not reply until
%% the commit is complete.
%%--------------------------------------------------------------------
eventually_save_doc(DbName, Doc, UserCtx) ->
    % find or create a process for the {DbName, UserCtx} pair
    {ok, Pid} = batch_pid_for_db_and_user(DbName, UserCtx),
    % hand it the document 
    ?LOG_DEBUG("sending doc to batch ~p",[Pid]),
    ok = send_doc_to_batch(Pid, Doc).
    
%%--------------------------------------------------------------------
%% Function: commit_now(DbName) -> committed
%% Description: Commits all docs for the DB. Does not reply until
%% the commit is complete.
%%--------------------------------------------------------------------
commit_now(DbName, UserCtx) ->
    % find the process for the {DbName, UserCtx} pair
    {ok, Pid} = batch_pid_for_db_and_user(DbName, UserCtx, false),
    case Pid of
        none -> committed;
        _Else ->
            ok = send_commit(Pid),
            committed
    end.

%%--------------------------------------------------------------------
%% Function: commit_now() -> committed
%% Description: Commits all docs for all DBs. Does not reply until
%% the commit is complete.
%%--------------------------------------------------------------------
% commit_all() ->
%     committed = gen_server:call(couch_batch_save, commit_now, infinity).
%  
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([BatchSize, BatchInterval]) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server with the meanings
%%--------------------------------------------------------------------
init([BatchSize, BatchInterval]) ->
    ets:new(couch_batch_save_by_db, [set, public, named_table]),
    {ok, #batch_state{batch_size=BatchSize, batch_interval=BatchInterval}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({make_pid, DbName, UserCtx}, _From, #batch_state{
        batch_size=BatchSize,
        batch_interval=BatchInterval
    }=State) ->
    % Create the pid in a serialized process. 
    % We checked before to see that we need the Pid, but the check is outside
    % the gen_server for parellelism. We check again here to ensure we don't 
    % make a duplicate.
    Resp = case ets:lookup(couch_batch_save_by_db, {DbName,UserCtx}) of
        [{_, Pid}] ->
            % we have a pid
            {ok, Pid};
        [] ->
            % no match
            % start and record the doc collector process
            ?LOG_DEBUG("making a batch pid ~p",[{DbName, UserCtx}]),
            Pid = spawn_link(fun() -> 
                doc_collector(DbName, UserCtx, {BatchSize, BatchInterval}, new) 
            end),
            true = ets:insert_new(couch_batch_save_by_db, {{DbName, UserCtx}, Pid}),
            {ok, Pid}
    end,
    {reply, Resp, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
% handle_info({'EXIT', Pid, Reason}, State) ->
%     {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    % todo shutdown the interval loop
    % todo kill all the Pids and drop the ets table
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

commit_user_docs(_DbName, _UserCtx, []) ->
    {ok, []};
    
commit_user_docs(DbName, UserCtx, Docs) ->
    ?LOG_INFO("Committing ~p batch docs to ~p",[length(Docs), DbName]),
    case couch_db:open(DbName, [{user_ctx, UserCtx}]) of
    {ok, Db} ->
        try
            {ok, Revs} = couch_db:update_docs(Db, Docs),
            ?LOG_INFO("Committed ~p batch docs to ~p",[length(Docs), DbName]),
            {ok, Revs}
        after
            couch_db:close(Db)
        end;
    Error ->
        throw(Error)
    end.

% spawned to trigger commits on an interval
commit_every_ms(Pid, BatchInterval) ->
    receive
        after BatchInterval ->
            ok = send_commit(Pid),
            commit_every_ms(Pid, BatchInterval)
    end.

send_commit(Pid) ->
    Pid ! {self(), commit},
    receive 
        {Pid, committed} ->
           ok
    end.

batch_pid_for_db_and_user(DbName, UserCtx) ->
    batch_pid_for_db_and_user(DbName, UserCtx, true).

batch_pid_for_db_and_user(DbName, UserCtx, Create) ->
    % look in the ets table
    case ets:lookup(couch_batch_save_by_db, {DbName,UserCtx}) of
        [{_, Pid}] ->
            % we have a pid
            {ok, Pid};
        [] ->
            % no match
            if Create ->
                {ok, Pid} = gen_server:call(couch_batch_save, {make_pid, DbName, UserCtx}, infinity),
                {ok, Pid};
            true ->
                {ok, none}
            end
    end.

send_doc_to_batch(Pid, Doc) ->
    Pid ! {self(), add_doc, Doc},
    receive
        {Pid, doc_added} -> ok
    end.

% the loop that holds documents between commits
doc_collector(DbName, UserCtx, {BatchSize, BatchInterval}, new) -> 
    % start a process that triggers commit every BatchInterval milliseconds
    _IntervalPid = spawn_link(fun() -> commit_every_ms(self(), BatchInterval) end),
    doc_collector(DbName, UserCtx, {BatchSize, BatchInterval}, []);

doc_collector(DbName, UserCtx, {BatchSize, BatchInterval}, Docs) when length(Docs) >= BatchSize->
    collector_commit(DbName, UserCtx, BatchInterval, Docs),
    exit(normal);
    
doc_collector(DbName, UserCtx, {BatchSize, BatchInterval}, Docs) ->
    receive
        {From, add_doc, Doc} ->
            From ! {self(), doc_added},
            doc_collector(DbName, UserCtx, {BatchSize, BatchInterval}, [Doc|Docs]);
        {From, commit} ->
            collector_commit(DbName, UserCtx, BatchInterval, Docs),
            From ! {self(), committed},
            exit(normal)
    end.

collector_commit(DbName, UserCtx, BatchInterval, Docs) ->
    % unregister
    unregister_collector(DbName, UserCtx, self()),
    % wait and collect
    Docs2 = shutdown_collector(DbName, UserCtx, BatchInterval, Docs),
    {ok, _Revs} = commit_user_docs(DbName, UserCtx, Docs2).

unregister_collector(DbName, UserCtx, Pid) ->
    % remove from ets
    ets:delete_object(couch_batch_save_by_db, {{DbName, UserCtx}, Pid}).

shutdown_collector(DbName, UserCtx, BatchInterval, Docs) ->
    receive
        {From, add_doc, Doc} ->
            From ! {self(), doc_added},
            shutdown_collector(DbName, UserCtx, BatchInterval, [Doc|Docs])
    % this interval will be waited for each time ensure-full-commit is called
    after BatchInterval ->
        Docs
    end.