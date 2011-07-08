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

-module(couch_replicator_rev_finder).

-export([start_link/5]).

-include("couch_db.hrl").

-import(couch_replicator_utils, [
    open_db/1,
    close_db/1
]).


start_link(Cp, Target, ChangesQueue, MissingRevsQueue, BatchSize) ->
    Pid = spawn_link(fun() ->
        missing_revs_finder_loop(
            Cp, Target, ChangesQueue, MissingRevsQueue, BatchSize)
        end),
    {ok, Pid}.


missing_revs_finder_loop(Cp, Target, ChangesQueue, RevsQueue, BatchSize) ->
    case couch_work_queue:dequeue(ChangesQueue, BatchSize) of
    closed ->
        ok;
    {ok, DocInfos} ->
        {Ts, #doc_info{high_seq = Seq}} = lists:last(DocInfos),
        ReportSeq = {Ts, Seq},
        ok = gen_server:cast(Cp, {report_seq, ReportSeq}),
        ?LOG_DEBUG("Missing revs finder defined report seq ~p", [ReportSeq]),
        IdRevs = [{Id, [Rev || #rev_info{rev = Rev} <- RevsInfo]} ||
                    {_, #doc_info{id = Id, revs = RevsInfo}} <- DocInfos],
        Target2 = open_db(Target),
        {ok, Missing} = couch_api_wrap:get_missing_revs(Target2, IdRevs),
        close_db(Target2),
        queue_missing_revs(Missing, DocInfos, ReportSeq, RevsQueue),
        missing_revs_finder_loop(Cp, Target2, ChangesQueue, RevsQueue, BatchSize)
    end.


queue_missing_revs(Missing, DocInfos, ReportSeq, Queue) ->
    IdRevsSeqDict = dict:from_list(
        [{Id, {[Rev || #rev_info{rev = Rev} <- RevsInfo], Seq}} ||
            {_, #doc_info{id = Id, revs = RevsInfo, high_seq = Seq}} <- DocInfos]),
    AllDict = lists:foldl(
        fun({Id, MissingRevs, PAs}, Acc) ->
            {_, Seq} = dict:fetch(Id, IdRevsSeqDict),
            dict:store(Seq, {Id, MissingRevs, 0, PAs}, Acc)
        end,
        dict:new(), Missing),
    AllDict2 = dict:fold(
        fun(Id, {NotMissingRevs, Seq}, Acc) ->
            case dict:find(Seq, Acc) of
            error ->
                dict:store(Seq, {Id, [], length(NotMissingRevs), []}, Acc);
            {ok, {Id, MissingRevs, NotMissingCount, PAs}} ->
                NotMissingCount2 = NotMissingCount + length(NotMissingRevs),
                dict:store(Seq, {Id, MissingRevs, NotMissingCount2, PAs}, Acc)
            end
        end,
        AllDict, non_missing(IdRevsSeqDict, Missing)),
    ?LOG_DEBUG("Missing revs finder adding batch of ~p IdRevs to work queue",
        [dict:size(AllDict2)]),
    ok = couch_work_queue:queue(Queue, {ReportSeq, dict:to_list(AllDict2)}).


non_missing(NonMissingDict, []) ->
    NonMissingDict;
non_missing(IdRevsSeqDict, [{MissingId, MissingRevs, _} | Rest]) ->
    {AllRevs, Seq} = dict:fetch(MissingId, IdRevsSeqDict),
    case AllRevs -- MissingRevs of
    [] ->
        non_missing(dict:erase(MissingId, IdRevsSeqDict), Rest);
    NotMissing ->
        non_missing(
            dict:store(MissingId, {NotMissing, Seq}, IdRevsSeqDict),
            Rest)
    end.
