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

-module(couch_replicator_changes_reader).

% Public API
-export([start_link/4]).

% Exported for code reloading
-export([read_changes/5]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").

-import(couch_util, [
    get_value/2
]).

start_link(StartSeq, #httpdb{} = Db, ChangesQueue, Options) ->
    Parent = self(),
    {ok,
        spawn_link(fun() ->
            put(last_seq, StartSeq),
            put(retries_left, Db#httpdb.retries),
            ?MODULE:read_changes(
                Parent,
                StartSeq,
                Db#httpdb{retries = 0},
                ChangesQueue,
                Options
            )
        end)}.

read_changes(Parent, StartSeq, Db, ChangesQueue, Options) ->
    Continuous = couch_util:get_value(continuous, Options),
    Style =
        case couch_util:get_value(winning_revs_only, Options, false) of
            true -> main_only;
            false -> all_docs
        end,
    try
        couch_replicator_api_wrap:changes_since(
            Db,
            Style,
            StartSeq,
            fun(Item) ->
                process_change(Item, {Parent, Db, ChangesQueue, Continuous})
            end,
            Options
        ),
        couch_work_queue:close(ChangesQueue)
    catch
        throw:recurse ->
            LS = get(last_seq),
            read_changes(Parent, LS, Db, ChangesQueue, Options);
        throw:retry_no_limit ->
            LS = get(last_seq),
            read_changes(Parent, LS, Db, ChangesQueue, Options);
        throw:{retry_limit, Error} ->
            couch_stats:increment_counter(
                [couch_replicator, changes_read_failures]
            ),
            case get(retries_left) of
                N when N > 0 ->
                    put(retries_left, N - 1),
                    LastSeq = get(last_seq),
                    Db2 =
                        case LastSeq of
                            StartSeq ->
                                couch_log:notice(
                                    "Retrying _changes request to source database ~s"
                                    " with since=~p in ~p seconds",
                                    [
                                        couch_replicator_api_wrap:db_uri(Db),
                                        LastSeq,
                                        Db#httpdb.wait / 1000
                                    ]
                                ),
                                ok = timer:sleep(Db#httpdb.wait),
                                Db#httpdb{wait = 2 * Db#httpdb.wait};
                            _ ->
                                couch_log:notice(
                                    "Retrying _changes request to source database ~s"
                                    " with since=~p",
                                    [couch_replicator_api_wrap:db_uri(Db), LastSeq]
                                ),
                                Db
                        end,
                    read_changes(Parent, LastSeq, Db2, ChangesQueue, Options);
                _ ->
                    exit(Error)
            end
    end.

process_change(#doc_info{id = <<>>} = DocInfo, {_, Db, _, _}) ->
    % Previous CouchDB releases had a bug which allowed a doc with an empty ID
    % to be inserted into databases. Such doc is impossible to GET.
    couch_log:error(
        "Replicator: ignoring document with empty ID in "
        "source database `~s` (_changes sequence ~p)",
        [couch_replicator_api_wrap:db_uri(Db), DocInfo#doc_info.high_seq]
    );
process_change(#doc_info{id = Id} = DocInfo, {Parent, Db, ChangesQueue, _}) ->
    case is_doc_id_too_long(byte_size(Id)) of
        true ->
            SourceDb = couch_replicator_api_wrap:db_uri(Db),
            couch_log:error(
                "Replicator: document id `~s...` from source db "
                " `~64s` is too long, ignoring.",
                [Id, SourceDb]
            ),
            Stats = couch_replicator_stats:new([{doc_write_failures, 1}]),
            ok = couch_replicator_scheduler_job:sum_stats(Parent, Stats);
        false ->
            ok = couch_work_queue:queue(ChangesQueue, DocInfo),
            put(last_seq, DocInfo#doc_info.high_seq)
    end;
process_change({last_seq, LS}, {_Parent, _, ChangesQueue, true = _Continuous}) ->
    % LS should never be undefined, but it doesn't hurt to be defensive inside
    % the replicator.
    Seq =
        case LS of
            undefined -> get(last_seq);
            _ -> LS
        end,
    queue_last_seq(ChangesQueue, Seq),
    throw(recurse);
process_change({last_seq, _}, _) ->
    % This clause is unreachable today, but let's plan ahead for the future
    % where we checkpoint against last_seq instead of the sequence of the last
    % change.  The two can differ substantially in the case of a restrictive
    % filter.
    ok;
process_change({heartbeat_seq, Seq}, {_Parent, _, ChangesQueue, _Continuous}) when
    is_binary(Seq); is_integer(Seq)
->
    queue_last_seq(ChangesQueue, Seq);
process_change({heartbeat_seq, _}, _) ->
    % We're being defensive here since we're in the replicator, and have to
    % handle other, possibly non-ASF CouchDB implementations, or strange
    % proxies. Here, we'd catch heartbeat objects without a seq or seq set to
    % null, for example and we ignore them instead of crashing everything.
    ok.

queue_last_seq(ChangesQueue, Seq) ->
    case get(last_seq) of
        Seq -> ok;
        _ -> ok = couch_work_queue:queue(ChangesQueue, {last_seq, Seq})
    end,
    put(last_seq, Seq),
    ok.

is_doc_id_too_long(IdLength) ->
    case config:get("replicator", "max_document_id_length", "infinity") of
        "infinity" ->
            false;
        ConfigMaxStr ->
            ConfigMax = list_to_integer(ConfigMaxStr),
            ConfigMax > 0 andalso IdLength > ConfigMax
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

heartbeat_seq_test() ->
    {ok, Q} = couch_work_queue:new([{max_size, 100}, {max_items, 100}]),
    Ctx = {self(), #httpdb{}, Q, true},
    put(last_seq, ~"1-a"),
    % new sequence => last_seq
    ?assertEqual(ok, process_change({heartbeat_seq, ~"2-b"}, Ctx)),
    ?assertEqual(~"2-b", get(last_seq)),
    ?assertEqual({ok, [{last_seq, ~"2-b"}]}, couch_work_queue:dequeue(Q)),
    % dedup works
    ?assertEqual(ok, process_change({heartbeat_seq, ~"2-b"}, Ctx)),
    ?assertEqual(0, couch_work_queue:item_count(Q)),
    % new seq is queued
    ?assertEqual(ok, process_change({heartbeat_seq, ~"4-d"}, Ctx)),
    ?assertEqual({ok, [{last_seq, ~"4-d"}]}, couch_work_queue:dequeue(Q)),
    ?assertEqual(~"4-d", get(last_seq)),
    % old replicators had integer sequences, we're maximally backwards
    % compatible so test them too
    ?assertEqual(ok, process_change({heartbeat_seq, 5}, Ctx)),
    ?assertEqual({ok, [{last_seq, 5}]}, couch_work_queue:dequeue(Q)),
    ?assertEqual(5, get(last_seq)),
    erase(last_seq),
    couch_work_queue:close(Q).

-endif.
