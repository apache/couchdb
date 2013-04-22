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

-module(couch_event_registry).
-behavior(gen_server).


-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-include("couch_event_int.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).


init(_) ->
    EtsOpts = [
        protected,
        named_table,
        bag,
        {keypos, #client.dbname}
    ],
    ets:new(?REGISTRY_TABLE, EtsOpts),
    {ok, nil}.


terminate(_Reason, _St) ->
    ok.


handle_call({register, Pid, DbName}, _From, St) ->
    Client = #client{
        dbname = DbName,
        pid = Pid,
        ref = erlang:monitor(process, Pid)
    },
    ets:insert(?REGISTRY_TABLE, Client),
    {reply, ok, St};

handle_call({unregister, Pid, DbName}, _From, St) ->
    Pattern = #client{dbname=DbName, pid=Pid, _='_'},
    case ets:match_object(?REGISTRY_TABLE, Pattern) of
        [] ->
            ok;
        [#client{ref=Ref}=Cli] ->
            erlang:demonitor(Ref, [flush]),
            ets:delete_object(?REGISTRY_TABLE, Cli)
    end,
    {reply, ok, St};

handle_call({unregister_all, Pid}, _From, St) ->
    Pattern = #client{pid=Pid, _='_'},
    case ets:match_object(?REGISTRY_TABLE, Pattern) of
        [] ->
            ok;
        Clients ->
            lists:foreach(fun(Cli) ->
                erlang:demonitor(Cli#client.ref, [flush]),
                % I wonder if match_delete/2 is faster
                % than repeated calls to delete_object.
                ets:delete_object(Cli)
            end, Clients)
    end,
    {reply, ok, St};

handle_call(Msg, From, St) ->
    couch_log:notice("~s ignoring call ~w from ~w", [?MODULE, Msg, From]),
    {reply, ignored, St, 0}.


handle_cast(Msg, St) ->
    couch_log:notice("~s ignoring cast ~w", [?MODULE, Msg]),
    {noreply, St, 0}.


handle_info({'DOWN', Ref, process, Pid, _Reason}, St) ->
    Pattern = #client{pid=Pid, ref=Ref, _='_'},
    ets:match_delete(?REGISTRY_TABLE, Pattern),
    {noreply, St};

handle_info(Msg, St) ->
    couch_log:notice("~s ignoring info ~w", [?MODULE, Msg]),
    {noreply, St, 0}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
