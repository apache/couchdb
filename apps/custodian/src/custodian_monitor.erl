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

-module(custodian_monitor).

% N.B. that callback return values are ignored

-callback send_missing_db_alert(DbName :: binary()) ->
    Ignored :: any().

-callback clear_missing_dbs_alert() ->
    Ignored :: any().

-callback send_event(
    Name :: string(), Count :: non_neg_integer(), Description :: string()
) ->
    Ignored :: any().
