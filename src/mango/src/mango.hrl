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

-define(MANGO_ERROR(R), throw({mango_error, ?MODULE, R})).

-define(MANGO_IDX_BUILD_STATUS, 1).
-define(MANGO_UPDATE_SEQ, 2).
-define(MANGO_IDX_RANGE, 3).

-define(MANGO_INDEX_JOB_TYPE, <<"mango">>).

-define(MANGO_INDEX_BUILDING, <<"building">>).
-define(MANGO_INDEX_READY, <<"ready">>).
