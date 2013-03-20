% Copyright 2011 Cloudant
%
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

-module(twig).

-export([log/2, log/3, log/4, set_level/1]).

-include("twig_int.hrl").

set_level(LevelAtom) ->
    application:set_env(twig, level, twig_util:level(LevelAtom)).

log(LevelAtom, String) ->
    log(LevelAtom, String, [], []).

log(LevelAtom, Format, Data) ->
    log(LevelAtom, Format, Data, []).

log(LevelAtom, Format, Data, _Options) ->
    %% TODO do something useful with options
    Level = twig_util:level(LevelAtom),
    case application:get_env(twig, level) of
        {ok, Threshold} when Level =< Threshold ->
            send_message(Level, Format, Data);
        undefined when Level =< ?LEVEL_INFO ->
            send_message(Level, Format, Data);
        _ ->
            ok
    end.

%% internal

send_message(Level, Format, Data) ->
    gen_event:sync_notify(error_logger, format(Level, Format, Data)).

format(Level, Format, Data) ->
    %% TODO truncate large messages
    #twig{
        level = Level,
        msg = iolist_to_binary(twig_util:format(Format, Data)),
        msgid = erlang:get(nonce),
        pid = self()
    }.

