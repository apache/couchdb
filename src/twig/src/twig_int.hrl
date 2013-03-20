% Copyright 2011 Cloudant
%
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

-define(SYSLOG_VERSION, 1).

-define(LEVEL_DEBUG, 7).
-define(LEVEL_INFO, 6).
-define(LEVEL_NOTICE, 5).
-define(LEVEL_WARN, 4).
-define(LEVEL_ERR, 3).
-define(LEVEL_CRIT, 2).
-define(LEVEL_ALERT, 1).
-define(LEVEL_EMERG, 0).

-record(twig, {level, msgid, msg, pid}).
