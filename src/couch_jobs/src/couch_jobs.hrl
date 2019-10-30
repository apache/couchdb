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

-include_lib("fabric/include/fabric2.hrl").


% Job map/json field definitions
%
-define(OPT_PRIORITY, <<"priority">>).
-define(OPT_DATA, <<"data">>).
-define(OPT_CANCEL, <<"cancel">>).
-define(OPT_RESUBMIT, <<"resubmit">>).

% These might be in a fabric public hrl eventually
%
-define(UNSET_VS, {versionstamp, 16#FFFFFFFFFFFFFFFF, 16#FFFF}).

% Data model definitions
%
-define(JOBS, 51).  % coordinate with fabric2.hrl
-define(DATA, 1).
-define(PENDING, 2).
-define(WATCHES_PENDING, 3).
-define(WATCHES_ACTIVITY, 4).
-define(ACTIVITY_TIMEOUT, 5).
-define(ACTIVITY, 6).


-define(COUCH_JOBS_EVENT, '$couch_jobs_event').
-define(COUCH_JOBS_CURRENT, '$couch_jobs_current').
-define(UNDEFINED_MAX_SCHEDULED_TIME, 1 bsl 36).


-type jtx() :: map() | undefined | tuple().
-type job_id() :: binary().
-type job_type() :: tuple() | binary() | non_neg_integer().
-type job() :: map().
-type job_data() :: map() | undefined.
-type job_accept_opts() :: map().
-type scheduled_time() :: non_neg_integer() | undefined.
-type job_state() :: running | pending | finished.
-type job_subscription() :: {pid(), reference()}.
