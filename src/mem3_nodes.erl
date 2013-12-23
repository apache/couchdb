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

-module(mem3_nodes).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/0, get_nodelist/0, get_node_info/2]).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-record(state, {changes_pid, update_seq}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_nodelist() ->
    try
        lists:sort([N || {N,_} <- ets:tab2list(?MODULE)])
    catch error:badarg ->
        gen_server:call(?MODULE, get_nodelist)
    end.

get_node_info(Node, Key) ->
    try
        couch_util:get_value(Key, ets:lookup_element(?MODULE, Node, 2))
    catch error:badarg ->
        gen_server:call(?MODULE, {get_node_info, Node, Key})
    end.

init([]) ->
    ets:new(?MODULE, [named_table, {read_concurrency, true}]),
    UpdateSeq = initialize_nodelist(),
    {Pid, _} = spawn_monitor(fun() -> listen_for_changes(UpdateSeq) end),
    {ok, #state{changes_pid = Pid, update_seq = UpdateSeq}}.

handle_call(get_nodelist, _From, State) ->
    {reply, lists:sort([N || {N,_} <- ets:tab2list(?MODULE)]), State};
handle_call({get_node_info, Node, Key}, _From, State) ->
    Resp = try
        couch_util:get_value(Key, ets:lookup_element(?MODULE, Node, 2))
    catch error:badarg ->
        error
    end,
    {reply, Resp, State};
handle_call({add_node, Node, NodeInfo}, _From, State) ->
    gen_event:notify(mem3_events, {add_node, Node}),
    ets:insert(?MODULE, {Node, NodeInfo}),
    {reply, ok, State};
handle_call({remove_node, Node}, _From, State) ->
    gen_event:notify(mem3_events, {remove_node, Node}),
    ets:delete(?MODULE, Node),
    {reply, ok, State};
handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, _, Pid, Reason}, #state{changes_pid=Pid} = State) ->
    twig:log(notice, "~p changes listener died ~p", [?MODULE, Reason]),
    StartSeq = State#state.update_seq,
    Seq = case Reason of {seq, EndSeq} -> EndSeq; _ -> StartSeq end,
    erlang:send_after(5000, self(), start_listener),
    {noreply, State#state{update_seq = Seq}};
handle_info(start_listener, #state{update_seq = Seq} = State) ->
    {NewPid, _} = spawn_monitor(fun() -> listen_for_changes(Seq) end),
    {noreply, State#state{changes_pid=NewPid}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, {state, ChangesPid, UpdateSeq, _}, _Extra) ->
    ets:new(?MODULE, [named_table, {read_concurrency, true}]),
    initialize_nodelist(),
    {ok, #state{changes_pid = ChangesPid, update_seq = UpdateSeq}};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

initialize_nodelist() ->
    DbName = config:get("mem3", "node_db", "nodes"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    {ok, _, Db} = couch_btree:fold(Db#db.id_tree, fun first_fold/3, Db, []),
    % add self if not already present
    case ets:lookup(?MODULE, node()) of
    [_] ->
        ok;
    [] ->
        ets:insert(?MODULE, {node(), []}),
        Doc = #doc{id = couch_util:to_binary(node())},
        {ok, _} = couch_db:update_doc(Db, Doc, [])
    end,
    couch_db:close(Db),
    Db#db.update_seq.

first_fold(#full_doc_info{id = <<"_design/", _/binary>>}, _, Acc) ->
    {ok, Acc};
first_fold(#full_doc_info{deleted=true}, _, Acc) ->
    {ok, Acc};
first_fold(#full_doc_info{id=Id}=DocInfo, _, Db) ->
    {ok, #doc{body={Props}}} = couch_db:open_doc(Db, DocInfo, [ejson_body]),
    ets:insert(?MODULE, {mem3_util:to_atom(Id), Props}),
    {ok, Db}.

listen_for_changes(Since) ->
    DbName = config:get("mem3", "node_db", "nodes"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    Args = #changes_args{
        feed = "continuous",
        since = Since,
        heartbeat = true,
        include_docs = true
    },
    ChangesFun = couch_changes:handle_changes(Args, nil, Db),
    ChangesFun(fun changes_callback/2).

changes_callback(start, _) ->
    {ok, nil};
changes_callback({stop, EndSeq}, _) ->
    exit({seq, EndSeq});
changes_callback({change, {Change}, _}, _) ->
    Node = couch_util:get_value(<<"id">>, Change),
    case Node of <<"_design/", _/binary>> -> ok; _ ->
        case mem3_util:is_deleted(Change) of
        false ->
            {Props} = couch_util:get_value(doc, Change),
            gen_server:call(?MODULE, {add_node, mem3_util:to_atom(Node), Props});
        true ->
            gen_server:call(?MODULE, {remove_node, mem3_util:to_atom(Node)})
        end
    end,
    {ok, couch_util:get_value(<<"seq">>, Change)};
changes_callback(timeout, _) ->
    {ok, nil}.
