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

-module(couch_stats_aggregator).

-behaviour(gen_server).

-export([
    fetch/0,
    flush/0
]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(st, {
    descriptions,
    stats,
    collect_timer,
    reload_timer
}).

fetch() ->
    {ok, Stats} = gen_server:call(?MODULE, fetch),
    Stats.

flush() ->
    gen_server:call(?MODULE, flush).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Descs} = reload_metrics(),
    Interval = case application:get_env(couch_stats, collection_interval) of
        {ok, I} -> I * 1000
    end,
    {ok, CT} = timer:send_interval(Interval, self(), collect),
    {ok, RT} = timer:send_interval(600000, self(), reload),
    {ok, #st{descriptions=Descs, stats=[], collect_timer=CT, reload_timer=RT}}.

handle_call(fetch, _from, #st{stats = Stats}=State) ->
    {reply, {ok, Stats}, State};
handle_call(flush, _From, State) ->
    {reply, ok, collect(State)};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(collect, State) ->
    {noreply, collect(State)};
handle_info(reload, State) ->
    {ok, Descriptions} = reload_metrics(),
    {noreply, State#st{descriptions=Descriptions}};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reload_metrics() ->
    Current = load_metrics_for_applications(),
    CurrentSet = sets:from_list(Current),
    ExistingSet = sets:from_list(couch_stats:list()),
    ToDelete = sets:subtract(ExistingSet, CurrentSet),
    ToCreate = sets:subtract(CurrentSet, ExistingSet),
    sets:fold(
        fun({Name, _}, _) -> couch_stats:delete(Name), nil end,
        nil,
        ToDelete
    ),
    sets:fold(
        fun({Name, Props}, _) ->
            Type = proplists:get_value(type, Props),
            couch_stats:new(Type, Name),
            nil
        end,
        nil,
        ToCreate
    ),
    {ok, Current}.

load_metrics_for_applications() ->
    Apps = [element(1, A) || A <- application:loaded_applications()],
    lists:foldl(
        fun(AppName, Acc) ->
            case load_metrics_for_application(AppName) of
                error -> Acc;
                Descriptions -> Descriptions ++ Acc
            end
        end,
        [],
        Apps
    ).

load_metrics_for_application(AppName) ->
    case code:priv_dir(AppName) of
        {error, _Error} ->
            error;
        Dir ->
            case file:consult(Dir ++ "/stat_descriptions.cfg") of
                {ok, Descriptions} ->
                    Descriptions;
                {error, _Error} ->
                    error
            end
    end.

collect(State) ->
    Stats = lists:map(
        fun({Name, Props}) ->
            {Name, [{value, couch_stats:sample(Name)}|Props]}
        end,
        State#st.descriptions
    ),
    State#st{stats=Stats}.
