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

-export([debug/1, info/1, notice/1, warn/1, err/1, crit/1, alert/1, emerg/1,
         debug/2, info/2, notice/2, warn/2, err/2, crit/2, alert/2, emerg/2,
         debug/3, info/3, notice/3, warn/3, err/3, crit/3, alert/3, emerg/3]).

-export([set_level/1]).

-include("twig_int.hrl").

debug(Format) ->                    log(debug, Format, [], []).
debug(Format, Data) ->              log(debug, Format, Data, []).
debug(Format, Data, Options) ->     log(debug, Format, Data, Options).

info(Format) ->                     log(info, Format, [], []).
info(Format, Data) ->               log(info, Format, Data, []).
info(Format, Data, Options) ->      log(info, Format, Data, Options).

notice(Format) ->                   log(notice, Format, [], []).
notice(Format, Data) ->             log(notice, Format, Data, []).
notice(Format, Data, Options) ->    log(notice, Format, Data, Options).

warn(Format) ->                     log(warn, Format, [], []).
warn(Format, Data) ->               log(warn, Format, Data, []).
warn(Format, Data, Options) ->      log(warn, Format, Data, Options).

err(Format) ->                      log(err, Format, [], []).
err(Format, Data) ->                log(err, Format, Data, []).
err(Format, Data, Options) ->       log(err, Format, Data, Options).

crit(Format) ->                     log(crit, Format, [], []).
crit(Format, Data) ->               log(crit, Format, Data, []).
crit(Format, Data, Options) ->      log(crit, Format, Data, Options).

alert(Format) ->                    log(alert, Format, [], []).
alert(Format, Data) ->              log(alert, Format, Data, []).
alert(Format, Data, Options) ->     log(alert, Format, Data, Options).

emerg(Format) ->                    log(emerg, Format, [], []).
emerg(Format, Data) ->              log(emerg, Format, Data, []).
emerg(Format, Data, Options) ->     log(emerg, Format, Data, Options).

set_level(LevelAtom) ->
    application:set_env(twig, {level, twig_util:level(LevelAtom)}).

%% internal

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

send_message(Level, Format, Data) ->
    gen_event:sync_notify(error_logger, format(Level, Format, Data)).

format(Level, Format, Data) ->
    %% TODO truncate large messages
    {twig, Level, iolist_to_binary(io_lib:format(Format, Data))}.

