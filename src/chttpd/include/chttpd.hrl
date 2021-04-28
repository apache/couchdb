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


-record(httpd_resp, {
    end_ts,
    code,
    response,
    status,
    nonce,
    should_log = true,
    reason
}).

-define(is_hex(C), (
    (C >= $0 andalso C =< $9) orelse
    (C >= $a andalso C =< $f) orelse
    (C >= $A andalso C =< $F)
)).
