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

-module(couch_view_group).
-behaviour(gen_server).

%% API
-export([start_link/1, request_group/2]).
% -export([design_doc_to_view_group/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("couch_db.hrl").
	 
-record(group_state, {
    spawn_fun,
    target_seq=0,
    group_seq=0,
    group=nil,
    updater_pid=nil,
    waiting_list=[]
}).

% api methods
request_group(Pid, Seq) ->
    ?LOG_DEBUG("request_group {Pid, Seq} ~p", [{Pid, Seq}]),
    case gen_server:call(Pid, {request_group, Seq}, infinity) of
    {ok, Group} ->
        ?LOG_DEBUG("get_updated_group replied with group", []),
        {ok, Group};
    Else ->
        ?LOG_DEBUG("get_updated_group replied with _Else ~p", [Else]),
        Else
    end.


% from template
start_link(InitArgs) ->
    gen_server:start_link(couch_view_group, InitArgs, []).

% init differentiates between temp and design_doc views. It creates a closure
% which spawns the appropriate view_updater. (It might also spawn the first
% view_updater run.)
init(InitArgs) ->
    SpawnFun = fun() -> spawn_updater(InitArgs) end,
    process_flag(trap_exit, true),
    {ok, #group_state{spawn_fun=SpawnFun}}.

% There are two sources of messages: couch_view, which requests an up to date
% view group, and the couch_view_updater, which when spawned, updates the
% group and sends it back here. We employ a caching mechanism, so that between
% database writes, we don't have to spawn a couch_view_updater with every view
% request. This should give us more control, and the ability to request view
% statuses eventually.

% The caching mechanism: each request is submitted with a seq_id for the
% database at the time it was read. We guarantee to return a view from that
% sequence or newer.

% If the request sequence is higher than our current high_target seq, we set
% that as the highest seqence. If the updater is not running, we launch it.

handle_call({request_group, RequestSeq}, From, 
        #group_state{
            target_seq=TargetSeq, 
            spawn_fun=SpawnFun,
            updater_pid=Up,
            waiting_list=WaitList
            }=State) when RequestSeq > TargetSeq, Up == nil  ->    
    UpdaterPid = SpawnFun(),
    {noreply, State#group_state{
        target_seq=RequestSeq, 
        updater_pid=UpdaterPid,
        waiting_list=[{From,RequestSeq}|WaitList]
    }, infinity};

handle_call({request_group, RequestSeq}, From, 
        #group_state{
            target_seq=TargetSeq,
            waiting_list=WaitList
            }=State) when RequestSeq > TargetSeq  ->
    {noreply, State#group_state{
        target_seq=RequestSeq, 
        waiting_list=[{From,RequestSeq}|WaitList]
    }, infinity};
        

% If the request seqence is less than or equal to the seq_id of a known Group,
% we respond with that Group.
handle_call({request_group, RequestSeq}, _From, 
        State=#group_state{
            group_seq=GroupSeq,
            group=Group 
            }) when RequestSeq =< GroupSeq  ->
    {reply, {ok, Group}, State};

% Otherwise: TargetSeq => RequestSeq > GroupSeq
% We've already initiated the appropriate action, so just hold the response until the group is up to the RequestSeq
handle_call({request_group, RequestSeq}, From,
    #group_state{
        waiting_list=WaitList
        }=State) ->
    {noreply, State#group_state{
        waiting_list=[{From, RequestSeq}|WaitList]
    }, infinity}.


% When the updater finishes, it will return a group with a seq_id, we should
% store that group and seq_id in our state. If our high_target is higher than
% the returned group, start a new updater.

handle_cast({new_group, Group=#group{current_seq=NewGroupSeq}}, 
        State=#group_state{
            target_seq=TargetSeq, 
            waiting_list=WaitList,
            spawn_fun=SpawnFun}) when TargetSeq > NewGroupSeq ->
    StillWaiting = reply_with_group(Group, WaitList, []),
    UpdaterPid = SpawnFun(),
    {noreply, State#group_state{ 
        updater_pid=UpdaterPid,
        waiting_list=StillWaiting,
        group_seq=NewGroupSeq,
        group=Group}};
        
handle_cast({new_group, Group=#group{current_seq=NewGroupSeq}},
        State=#group_state{waiting_list=WaitList}) ->
    StillWaiting = reply_with_group(Group, WaitList, []),
    {noreply, State#group_state{
        updater_pid=nil,
        waiting_list=StillWaiting,
        group_seq=NewGroupSeq,
        group=Group}}.
   
handle_info({'EXIT', _FromPid, normal}, State) ->
    {noreply, State};
    
handle_info({'EXIT', FromPid, Reason}, State) ->
    ?LOG_DEBUG("Exit from updater: ~p", [{FromPid, Reason}]),
    {stop, Reason, State};
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State=#group_state{waiting_list=WaitList}) ->
    lists:foreach(fun({Waiter, _}) -> gen_server:reply(Waiter, {error, Reason}) end, WaitList),    
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% error handling? the updater could die on us, we can save ourselves here.
% but we shouldn't, we could be dead for a reason, like the view got changed, or something.


%% Local Functions

% reply_with_group/3
% for each item in the WaitingList {Pid, Seq}
% if the Seq is =< GroupSeq, reply
reply_with_group(Group=#group{current_seq=GroupSeq}, [{Pid, Seq}|WaitList], StillWaiting) when Seq =< GroupSeq ->
    gen_server:reply(Pid, {ok, Group}),
    reply_with_group(Group, WaitList, StillWaiting);

% else
% put it in the continuing waiting list    
reply_with_group(Group, [{Pid, Seq}|WaitList], StillWaiting) ->
    reply_with_group(Group, WaitList, [{Pid, Seq}|StillWaiting]);

% return the still waiting list
reply_with_group(_Group, [], StillWaiting) ->
    StillWaiting.

spawn_updater({RootDir, DbName, GroupId}) -> 
    spawn_link(couch_view_updater, update,
        [RootDir, DbName, GroupId, self()]);

spawn_updater({DbName, Fd, Lang, MapSrc, RedSrc}) ->
    spawn_link(couch_view_updater, temp_update,
        [DbName, Fd, Lang, MapSrc, RedSrc, self()]).
    

