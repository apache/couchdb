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

-module(couch_log_writer_ets).
-behaviour(couch_log_writer).


-export([
    init/0,
    terminate/2,
    write/2
]).


-include("couch_log.hrl").


init() ->
    ets:new(?COUCH_LOG_TEST_TABLE, [named_table, public, ordered_set]),
    {ok, 0}.


terminate(_, _St) ->
    ets:delete(?COUCH_LOG_TEST_TABLE),
    ok.


write(Entry0, St) ->
    Entry = Entry0#log_entry{
        msg = lists:flatten(Entry0#log_entry.msg),
        time_stamp = lists:flatten(Entry0#log_entry.time_stamp)
    },
    Ignored = application:get_env(couch_log, ignored_pids, []),
    case lists:member(Entry#log_entry.pid, Ignored) of
        true ->
            {ok, St};
        false ->
            ets:insert(?COUCH_LOG_TEST_TABLE, {St, Entry}),
            {ok, St + 1}
    end.
