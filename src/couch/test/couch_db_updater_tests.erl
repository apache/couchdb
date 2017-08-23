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

-module(couch_db_updater_tests).

-include_lib("couch/include/couch_eunit.hrl").

% Tests to test
% 10k docs
% kill during first pass
% kill during docid copy
% kill before docid copy starts
% kill during docid copy
% kill after docid copy
% 5k docs, write docs during compaction
%