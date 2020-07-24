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

% Index info/data subspaces
-define(VIEW_INFO, 0).
-define(VIEW_DATA, 1).
-define(VIEW_TREES, 3).

% Index info keys
-define(VIEW_UPDATE_SEQ, 0).
-define(VIEW_ROW_COUNT, 1).
-define(VIEW_KV_SIZE, 2).
-define(VIEW_BUILD_STATUS, 3).
-define(VIEW_CREATION_VS, 4).

% Data keys
-define(VIEW_ID_RANGE, 0).
-define(VIEW_MAP_RANGE, 1).

% Tree keys
-define(VIEW_ID_TREE, 0).
-define(VIEW_ROW_TREES, 1).

% jobs api
-define(INDEX_JOB_TYPE, <<"views">>).

% indexing progress
-define(INDEX_BUILDING, <<"building">>).
-define(INDEX_READY, <<"ready">>).
