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

-module(couch_log).

-export([log/3]).

log(debug, Msg, Args) ->
    lager:debug(Msg, Args);
log(info, Msg, Args) ->
    lager:info(Msg, Args);
log(notice, Msg, Args) ->
    lager:notice(Msg, Args);
log(warn, Msg, Args) ->
    lager:warning(Msg, Args);
log(warning, Msg, Args) ->
    lager:warning(Msg, Args);
log(err, Msg, Args) ->
    lager:error(Msg, Args);
log(error, Msg, Args) ->
    lager:error(Msg, Args);
log(critical, Msg, Args) ->
    lager:critical(Msg, Args);
log(alert, Msg, Args) ->
    lager:alert(Msg, Args);
log(emerg, Msg, Args) ->
    lager:emergency(Msg, Args);
log(emergency, Msg, Args) ->
    lager:emergency(Msg, Args).
