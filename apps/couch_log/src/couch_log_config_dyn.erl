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
% This module gets replaced at runtime with a dynamically
% compiled version so don't rely on these default's making
% sense. They only mirror what's in the default.ini checked
% into the root Apache CouchDB Git repository.

-module(couch_log_config_dyn).

-export([
    get/1
]).

get(level) -> info;
get(level_int) -> 2;
get(max_message_size) -> 16000;
get(strip_last_msg) -> true;
get(filter_fields) -> [pid, registered_name, error_info, messages].
