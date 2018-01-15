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

-module(mem3_dns).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/0]).
-export([discover_peers/0]).

-include_lib("couch/include/couch_db.hrl").

-record(state, {
    record_name,
    discovered_names,
    last_error,
    timeout = 2000,
    max_tries = 10
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

discover_peers() ->
    case config:get("mem3", "use_dns_service_discovery", "false") of
    "true" ->
        gen_server:cast(?MODULE, discover_peers);
    _ ->
        ok
    end.

init([]) ->
    {ok, ServiceRecord} = construct_service_record(),
    {ok, #state{record_name = ServiceRecord}}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast(discover_peers, #state{max_tries = 0} = State) ->
    {noreply, State};

handle_cast(discover_peers, St) ->
    case query_dns(St#state.record_name) of
    {ok, Hostnames} ->
        add_nodes(Hostnames),
        {noreply, St#state{discovered_names = Hostnames}};
    {error, Error} ->
        #state{max_tries = Max, timeout = Timeout} = St,
        NewSt = St#state{
            last_error = Error,
            max_tries = Max-1,
            timeout = 2*Timeout
        },
        {noreply, NewSt, Timeout}
    end.

handle_info(timeout, State) ->
    handle_cast(discover_peers, State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, #state{}=State, _Extra) ->
    {ok, State}.

query_dns(ServiceRecord) ->
    Opts = [{edns, 0}],
    case inet_res:resolve(ServiceRecord, in, srv, Opts) of
    {ok, DnsMsg} ->
        Hostnames = lists:foldl(fun(Answer, Acc) ->
            case inet_dns:rr(Answer, type) of
            srv ->
                {_, _, _, Service} = inet_dns:rr(Answer, data),
                [Service | Acc];
            _ ->
                Acc
            end
        end, [], inet_dns:msg(DnsMsg, anlist)),
        {ok, Hostnames};
    Error ->
        Error
    end.

construct_service_record() ->
    case config:get("mem3", "service_record_name") of
    undefined ->
        [_ | Domain] = string:tokens(net_adm:localhost(), "."),
        {ok, string:join(["_couchdb", "_tcp" | Domain], ".")};
    ServiceRecord ->
        {ok, ServiceRecord}
    end.

add_nodes(Hostnames) ->
    DbName = config:get("mem3", "nodes_db", "_nodes"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    Peers = lists:map(fun(Hostname) ->
        #doc{id = couch_util:to_binary("couchdb@" ++ Hostname)}
    end, Hostnames),
    couch_db:update_docs(Db, Peers, []).
