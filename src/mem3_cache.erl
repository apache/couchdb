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

-module(mem3_cache).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/0]).

-record(state, {changes_pid}).

-include("mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(partitions, [bag, public, named_table, {keypos,#shard.dbname}]),
    {Pid, _} = spawn_monitor(fun() -> listen_for_changes(0) end),
    {ok, #state{changes_pid = Pid}}.

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, _, Pid, {badarg, [{ets,delete,[partitions,_]}|_]}},
        #state{changes_pid=Pid} = State) ->
    % fatal error, somebody deleted our ets table
    {stop, ets_table_error, State};
handle_info({'DOWN', _, _, Pid, Reason}, #state{changes_pid=Pid} = State) ->
    twig:log(notice, "~p changes listener died ~p", [?MODULE, Reason]),
    Seq = case Reason of {seq, EndSeq} -> EndSeq; _ -> 0 end,
    erlang:send_after(5000, self(), {start_listener, Seq}),
    {noreply, State};
handle_info({start_listener, Seq}, State) ->
    {NewPid, _} = spawn_monitor(fun() -> listen_for_changes(Seq) end),
    {noreply, State#state{changes_pid=NewPid}};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{changes_pid=Pid}) ->
    exit(Pid, kill),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

listen_for_changes(Since) ->
    DbName = couch_config:get("mem3", "shard_db", "dbs"),
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
    DbName = couch_util:get_value(<<"id">>, Change),
    case DbName of <<"_design/", _/binary>> -> ok; _Else ->
        case couch_util:get_value(<<"deleted">>, Change, false) of
        true ->
            ets:delete(partitions, DbName);
        false ->
            case couch_util:get_value(doc, Change) of
            {error, Reason} ->
                twig:log(error, "missing partition table for ~s: ~p", [DbName, Reason]);
            {Doc} ->
                ets:delete(partitions, DbName),
                Shards = mem3_util:build_shards(DbName, Doc),
                ets:insert(partitions, Shards),
                [create_if_missing(Name) || #shard{name=Name, node=Node}
                    <- Shards, Node =:= node()]
            end
        end
    end,
    {ok, couch_util:get_value(<<"seq">>, Change)};
changes_callback(timeout, _) ->
    {ok, nil}.

create_if_missing(Name) ->
    DbDir = couch_config:get("couchdb", "database_dir"),
    Filename = filename:join(DbDir, ?b2l(Name) ++ ".couch"),
    case filelib:is_regular(Filename) of
    true ->
        ok;
    false ->
        Options = [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}],
        case couch_server:create(Name, Options) of
        {ok, Db} ->
            couch_db:close(Db);
        Error ->
            twig:log(error, "~p tried to create ~s, got ~p", [?MODULE, Name, Error])
        end
    end.
