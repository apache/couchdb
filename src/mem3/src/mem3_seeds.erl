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

-module(mem3_seeds).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([
    start_link/0,
    get_seeds/0,
    get_status/0
]).

-record(st, {
    ready = false,
    seeds = [],
    jobref = nil,
    % map keyed on node name
    status = #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_seeds() ->
    case config:get("cluster", "seedlist") of
        undefined ->
            [];
        List ->
            Nodes = string:tokens(List, ","),
            Seeds = [list_to_atom(Node) || Node <- Nodes] -- [node()],
            mem3_util:rotate_list(node(), Seeds)
    end.

get_status() ->
    gen_server:call(?MODULE, get_status).

init([]) ->
    Seeds = get_seeds(),
    Ready =
        case Seeds of
            [] -> true;
            _ -> false
        end,
    St = #st{
        seeds = Seeds,
        ready = Ready,
        jobref = start_replication(Seeds),
        status = maps:from_keys(Seeds, #{})
    },
    {ok, St}.

handle_call(get_status, _From, St) ->
    Ready =
        case St#st.ready of
            true -> ok;
            false -> seeding
        end,
    Status = #{status => Ready, seeds => St#st.status},
    {reply, {ok, Status}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(start_replication, #st{jobref = nil} = St) ->
    JobRef = start_replication(St#st.seeds),
    {noreply, St#st{jobref = JobRef}};
handle_info({'DOWN', Ref, _, Pid, Output}, #st{jobref = {Pid, Ref}} = St) ->
    {noreply, update_state(St, Output)};
handle_info(_Msg, St) ->
    {noreply, St}.

% internal functions

start_replication([]) ->
    nil;
start_replication([Seed | _]) ->
    spawn_monitor(fun() ->
        Reply = mem3_rpc:pull_replication(Seed),
        exit({ok, Reply})
    end).

update_state(#st{} = St, {ok, Data}) ->
    #st{seeds = [Current | Tail], status = #{} = Status} = St,
    Report = #{
        timestamp => list_to_binary(mem3_util:iso8601_timestamp()),
        pending_updates => lists:foldl(fun pending_updates/2, #{}, Data),
        last_replication_status => ok
    },
    Ready = is_ready(St#st.ready, Data),
    case Ready of
        true ->
            Seeds = Tail ++ [Current],
            Job = nil;
        false ->
            % Try to progress this same seed again
            Seeds = [Current | Tail],
            Job = start_replication([Current | Tail])
    end,
    St#st{
        seeds = Seeds,
        jobref = Job,
        ready = Ready,
        status = Status#{Current => Report}
    };
update_state(#st{} = St, {_Error, _Stack}) ->
    #st{seeds = [Current | Tail], status = #{} = Status} = St,
    Report = #{
        timestamp => list_to_binary(mem3_util:iso8601_timestamp()),
        last_replication_status => error
    },
    Seeds = Tail ++ [Current],
    if
        not St#st.ready ->
            erlang:send_after(1000, self(), start_replication);
        true ->
            ok
    end,
    St#st{
        seeds = Seeds,
        jobref = nil,
        status = Status#{Current => Report}
    }.

is_ready(true, _) ->
    true;
is_ready(false, Data) ->
    lists:all(fun({_DbName, Pending}) -> Pending =:= {ok, 0} end, Data).

pending_updates({DbName, Status}, #{} = Acc) ->
    case Status of
        {ok, Pending} when is_number(Pending) ->
            Acc#{DbName => Pending};
        {error, Tag} ->
            Acc#{DbName => list_to_binary(io_lib:format("~p", [Tag]))};
        _Else ->
            Acc#{DbName => unknown_error}
    end.
