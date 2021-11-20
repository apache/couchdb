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

-module(custodian_noop_monitor).

-behaviour(custodian_monitor).

-export([
    send_missing_db_alert/1,
    clear_missing_dbs_alert/0,
    send_event/3
]).

send_missing_db_alert(_DbName) ->
    false.

clear_missing_dbs_alert() ->
    false.

send_event(_Name, _Count, _Description) ->
    false.
