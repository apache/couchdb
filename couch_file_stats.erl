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

-module(couch_file_stats).
-behaviour(gen_server).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3,
        handle_info/2]).

-export([start_link/0,track_file/1,get_stats/0]).

-record(stats,{
    opened=0,
    closed=0
    }).


track_file(Fd) ->
    catch gen_server:cast(couch_file_stats, {track, Fd}).


get_stats() ->
    #stats{opened=Opened,closed=Closed}=gen_server:call(couch_file_stats,get),
    [{files_open,Opened-Closed}, {files_closed,Closed}].


start_link() ->
    gen_server:start_link({local, couch_file_stats}, couch_file_stats, [], []).


init([]) ->
    {ok, #stats{}}.


terminate(_Reason, _Fd) ->
    ok.


handle_call(get, _From, Stats) ->
    {reply, Stats, Stats}.


handle_cast({track, Fd}, #stats{opened=Opened,closed=Closed}=Stats) ->
    try erlang:monitor(process, Fd) of
    _Ref ->
        {noreply, Stats#stats{opened=Opened+1}}
    catch
    _Error ->
        {noreply, Stats#stats{opened=Opened+1, closed=Closed+1}}
    end.


handle_info({'DOWN',_MonRef,_Type,_Pid,_Info}, #stats{closed=Closed}=Stats) ->
    {noreply, Stats#stats{closed=Closed+1}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

