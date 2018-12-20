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

-module(fabric_streams).

-export([
    start/2,
    start/4,
    cleanup/1
]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").


start(Workers, Keypos) ->
    start(Workers, Keypos, undefined, undefined).

start(Workers0, Keypos, StartFun, Replacements) ->
    Fun = fun handle_stream_start/3,
    Acc = #stream_acc{
        workers = fabric_dict:init(Workers0, waiting),
        start_fun = StartFun,
        replacements = Replacements
    },
    Timeout = fabric_util:request_timeout(),
    case rexi_utils:recv(Workers0, Keypos, Fun, Acc, Timeout, infinity) of
        {ok, #stream_acc{workers=Workers}} ->
            true = fabric_view:is_progress_possible(Workers),
            AckedWorkers = fabric_dict:fold(fun(Worker, From, WorkerAcc) ->
                rexi:stream_start(From),
                [Worker | WorkerAcc]
            end, [], Workers),
            {ok, AckedWorkers};
        Else ->
            Else
    end.


cleanup(Workers) ->
    fabric_util:cleanup(Workers).


handle_stream_start({rexi_DOWN, _, {_, NodeRef}, _}, _, St) ->
    case fabric_util:remove_down_workers(St#stream_acc.workers, NodeRef) of
    {ok, Workers} ->
        {ok, St#stream_acc{workers=Workers}};
    error ->
        Reason = {nodedown, <<"progress not possible">>},
        {error, Reason}
    end;

handle_stream_start({rexi_EXIT, Reason}, Worker, St) ->
    Workers = fabric_dict:erase(Worker, St#stream_acc.workers),
    Replacements = St#stream_acc.replacements,
    case {fabric_view:is_progress_possible(Workers), Reason} of
    {true, _} ->
        {ok, St#stream_acc{workers=Workers}};
    {false, {maintenance_mode, _Node}} when Replacements /= undefined ->
        % Check if we have replacements for this range
        % and start the new workers if so.
        case lists:keytake(Worker#shard.range, 1, Replacements) of
            {value, {_Range, WorkerReplacements}, NewReplacements} ->
                FinalWorkers = lists:foldl(fun(Repl, NewWorkers) ->
                    NewWorker = (St#stream_acc.start_fun)(Repl),
                    fabric_dict:store(NewWorker, waiting, NewWorkers)
                end, Workers, WorkerReplacements),
                % Assert that our replaced worker provides us
                % the oppurtunity to make progress.
                true = fabric_view:is_progress_possible(FinalWorkers),
                NewRefs = fabric_dict:fetch_keys(FinalWorkers),
                {new_refs, NewRefs, St#stream_acc{
                    workers=FinalWorkers,
                    replacements=NewReplacements
                }};
            false ->
                % If we progress isn't possible and we don't have any
                % replacements then we're dead in the water.
                Error = {nodedown, <<"progress not possible">>},
                {error, Error}
        end;
    {false, _} ->
        {error, fabric_util:error_info(Reason)}
    end;

handle_stream_start(rexi_STREAM_INIT, {Worker, From}, St) ->
    case fabric_dict:lookup_element(Worker, St#stream_acc.workers) of
    undefined ->
        % This worker lost the race with other partition copies, terminate
        rexi:stream_cancel(From),
        {ok, St};
    waiting ->
        % Don't ack the worker yet so they don't start sending us
        % rows until we're ready
        Workers0 = fabric_dict:store(Worker, From, St#stream_acc.workers),
        Workers1 = fabric_view:remove_overlapping_shards(Worker, Workers0),
        case fabric_dict:any(waiting, Workers1) of
            true ->
                {ok, St#stream_acc{workers=Workers1}};
            false ->
                {stop, St#stream_acc{workers=Workers1}}
        end
    end;

handle_stream_start({ok, ddoc_updated}, _, St) ->
    cleanup(St#stream_acc.workers),
    {stop, ddoc_updated};

handle_stream_start(Else, _, _) ->
    exit({invalid_stream_start, Else}).
