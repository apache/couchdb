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



-record(httpdb, {
    url,
    oauth = nil,
    headers = [
        {"Accept", "application/json"},
        {"User-Agent", "CouchDB/" ++ couch_server:get_version()}
    ],
    timeout,            % milliseconds
    ibrowse_options = [],
    retries = 10,
    wait = 250,         % milliseconds
    httpc_pool = nil,
    http_connections
}).

-record(oauth, {
    consumer_key,
    token,
    token_secret,
    consumer_secret,
    signature_method
}).
