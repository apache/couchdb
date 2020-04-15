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

-module(chttpd_stats).


-export([
    init/1,
    report/1,

    incr_reads/0,
    incr_reads/1,

    incr_writes/0,
    incr_writes/1,

    incr_rows/0,
    incr_rows/1,

    update_interval/1
]).


-record(st, {
    reads = 0,
    writes = 0,
    rows = 0,
    reporter,
    last_report_ts = 0,
    interval,
    request
}).


-define(KEY, chttpd_stats).
-define(INTERVAL_IN_SEC, 60).

init(Request) ->
    Reporter = config:get("chttpd", "stats_reporter"),
    Time = erlang:monotonic_time(second),
    Interval = config:get_integer("chttpd", "stats_reporting_interval",
        ?INTERVAL_IN_SEC),
    put(?KEY, #st{reporter = Reporter, last_report_ts = Time,
        interval = Interval, request = Request}).


report(HttpResp) ->
    try
        case get(?KEY) of
            #st{} = St ->
                report(HttpResp, St);
            _ ->
                ok
        end
    catch T:R ->
        S = erlang:get_stacktrace(),
        Fmt = "Failed to report chttpd request stats: ~p:~p ~p",
        couch_log:error(Fmt, [T, R, S])
    end.


report(HttpResp, #st{reporter = Reporter} = St) ->
    case Reporter of
        undefined ->
            ok;
        ModStr ->
            Mod = list_to_existing_atom(ModStr),
            #st{
                reads = Reads,
                writes = Writes,
                rows = Rows,
                request = HttpReq
            } = St,
            Mod:report(HttpReq, HttpResp, Reads, Writes, Rows)
    end.


incr_reads() ->
    incr(#st.reads, 1).


incr_reads(N) when is_integer(N), N >= 0 ->
    incr(#st.reads, N).


incr_writes() ->
    incr(#st.writes, 1).


incr_writes(N) when is_integer(N), N >= 0 ->
    incr(#st.writes, N).


incr_rows() ->
    incr(#st.rows, 1).


incr_rows(N) when is_integer(N), N >= 0 ->
    incr(#st.rows, N).


incr(Idx, Count) ->
    case get(?KEY) of
        #st{} = St ->
            Total = element(Idx, St) + Count,
            NewSt = setelement(Idx, St, Total),
            put(?KEY, NewSt),
            maybe_report_intermittent(St);
        _ ->
            ok
    end.


maybe_report_intermittent(State) ->
    #st{last_report_ts = LastTime, interval = Interval} = State,
    CurrentTime = erlang:monotonic_time(second),
    case CurrentTime - LastTime of
        Change when Change >= Interval ->
            % Since response is not available during the request, we set
            % this undefined. Modules that call:
            % Mod:report(HttpReq, HttpResp, Reads, Writes, Rows) should
            % be aware of this. Mod:report should also return a boolean
            % to indicate if reset should occur
            case report(undefined) of
                true ->
                    reset_stats(State, CurrentTime);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.


update_interval(Interval) ->
    case get(?KEY) of
        #st{} = St ->
            put(?KEY, St#st{interval = Interval});
        _ ->
            ok
    end.


reset_stats(State, NewTime) ->
    put(?KEY, State#st{
        reads = 0,
        writes = 0,
        rows = 0,
        last_report_ts = NewTime
    }).
