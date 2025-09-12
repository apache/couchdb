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

-module(couch_index_cleanup).
-behaviour(gen_server).

-export([
    start_link/0,
    schedule/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([
    handle_db_event/3
]).

-define(DEFAULT_DELAY_MSEC, 30000).

-record(st, {
    pending = #{} :: #{binary() => reference()}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

schedule(DbName) when is_binary(DbName) ->
    gen_server:cast(?MODULE, {schedule, DbName, fanout}).

init([]) ->
    {ok, _} = couch_event:link_listener(?MODULE, handle_db_event, nil, [all_dbs]),
    {ok, #st{}}.

handle_call(Msg, _From, #st{} = St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.

handle_cast({schedule, DbName, Mode}, #st{pending = Pending} = St) ->
    case maps:is_key(DbName, Pending) of
        true ->
            {noreply, St};
        false ->
            case Mode of
                fanout -> fanout(DbName);
                no_fanout -> ok
            end,
            TRef = erlang:send_after(delay_msec(), self(), {run_cleanup, DbName}),
            {noreply, St#st{pending = Pending#{DbName => TRef}}}
    end;
handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.

handle_info({run_cleanup, DbName}, #st{pending = Pending} = St) ->
    spawn(fun() ->
        try
            fabric:cleanup_index_files_this_node(DbName)
        catch
            Class:Reason:Stack ->
                WArgs = [?MODULE, DbName, Class, Reason, Stack],
                couch_log:warning("~p: cleanup ~s failed ~p:~p~n~p", WArgs)
        end
    end),
    {noreply, St#st{pending = maps:remove(DbName, Pending)}};
handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.

handle_db_event(<<"shards/", _/binary>> = DbName, {ddoc_updated, _DDocId}, St) ->
    % Clustered dbs only
    schedule(mem3:dbname(DbName)),
    {ok, St};
handle_db_event(_DbName, _Event, St) ->
    {ok, St}.

fanout(DbName) ->
    try mem3:shards(DbName) of
        Shards ->
            Nodes = lists:usort([mem3:node(S) || S <- Shards]) -- [node()],
            Args = {schedule, DbName, no_fanout},
            lists:foreach(fun(N) -> gen_server:cast({?MODULE, N}, Args) end, Nodes)
    catch
        _:_ -> ok
    end.

delay_msec() ->
    config:get_integer("couchdb", "index_cleanup_delay_msec", ?DEFAULT_DELAY_MSEC).
