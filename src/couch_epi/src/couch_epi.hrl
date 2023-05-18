% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-record(couch_epi_spec, {
    behaviour, app, kind, options, key, value, codegen, type
}).

% Copied from `couch_db.hrl` which cannot be included in this application
-define(term_to_bin(T), term_to_binary(T, [{minor_version, 1}])).
