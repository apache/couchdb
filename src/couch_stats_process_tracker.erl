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

-module(couch_stats_process_tracker).
-behaviour(gen_server).

-export([
    track/1,
    track/2
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

}).

-spec track(any()) -> ok.
track(Name) ->
    track(self(), Name).

-spec track(pid(), any()) -> ok.
track(Name, Pid) ->
    gen_server:cast(?MODULE, {track, Name, Pid}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?MODULE, [named_table, public, set]),
    {ok, #st{}}.

handle_call(Msg, _From, State) ->
    twig:log(notice, "~p received unknown call ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_cast({track, Pid, Name}, State) ->
    couch_stats:increment_counter(Name),
    Ref = erlang:monitor(process, Pid),
    ets:insert(?MODULE, {Ref, Name}),
    {noreply, State};
handle_cast(Msg, State) ->
    twig:log(notice, "~p received unknown cast ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info({'DOWN', Ref, _, _, _}=Msg, State) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            twig:log(
                notice,
                "~p received unknown exit; message was ~p", [?MODULE, Msg]
            );
        [{Ref, Name}] ->
            couch_stats:decrement_counter(Name),
            ets:delete(?MODULE, Ref)
    end,
    {noreply, State};
handle_info(Msg, State) ->
    twig:log(notice, "~p received unknown message ~p", [?MODULE, Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
