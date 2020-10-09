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
    init/0,
    report/2,

    incr_reads/0,
    incr_reads/1,

    incr_writes/0,
    incr_writes/1,

    incr_rows/0,
    incr_rows/1
]).


-record(st, {
    reads = 0,
    writes = 0,
    rows = 0
}).


-define(KEY, chttpd_stats).


init() ->
    put(?KEY, #st{}).


report(HttpReq, HttpResp) ->
    try
        case get(?KEY) of
            #st{} = St ->
                report(HttpReq, HttpResp, St);
            _ ->
                ok
        end
    catch T:R ->
        S = erlang:get_stacktrace(),
        Fmt = "Failed to report chttpd request stats: ~p:~p ~p",
        couch_log:error(Fmt, [T, R, S])
    end.


report(HttpReq, HttpResp, St) ->
    case config:get("chttpd", "stats_reporter") of
        undefined ->
            ok;
        ModStr ->
            Mod = list_to_existing_atom(ModStr),
            #st{
                reads = Reads,
                writes = Writes,
                rows = Rows
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
            put(?KEY, NewSt);
        _ ->
            ok
    end.
