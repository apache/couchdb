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
%
% @doc Modules wishing to handle writing log
% messages should implement this behavior.

-module(couch_log_writer).

-export([
    init/0,
    terminate/2,
    write/2
]).

-include("couch_log.hrl").

-define(DEFAULT_WRITER, couch_log_writer_stderr).

-callback init() -> {ok, State :: term()}.
-callback terminate(Reason :: term(), State :: term()) -> ok.
-callback write(LogEntry :: #log_entry{}, State :: term()) ->
    {ok, NewState :: term()}.

-spec init() -> {atom(), term()}.
init() ->
    Writer = get_writer_mod(),
    {ok, St} = Writer:init(),
    {Writer, St}.

-spec terminate(term(), {atom(), term()}) -> ok.
terminate(Reason, {Writer, St}) ->
    ok = Writer:terminate(Reason, St).

-spec write(#log_entry{}, {atom(), term()}) -> {atom(), term()}.
write(Entry, {Writer, St}) ->
    {ok, NewSt} = Writer:write(Entry, St),
    {Writer, NewSt}.

get_writer_mod() ->
    WriterStr = config:get("log", "writer", "stderr"),
    ModName1 = to_atom("couch_log_writer_" ++ WriterStr),
    case mod_exists(ModName1) of
        true ->
            ModName1;
        false ->
            ModName2 = to_atom(WriterStr),
            case mod_exists(ModName2) of
                true ->
                    ModName2;
                false ->
                    ?DEFAULT_WRITER
            end
    end.

to_atom(Str) ->
    try list_to_existing_atom(Str) of
        Atom -> Atom
    catch
        _:_ ->
            undefined
    end.

mod_exists(ModName) ->
    code:which(ModName) /= non_existing.
