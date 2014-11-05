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


-define(SUPPORTED_HEADERS, [
    "accept",
    "accept-language",
    "authorization",
    "content-length",
    "content-range",
    "content-type",
    "destination",
    "expires",
    "if-match",
    "last-modified",
    "origin",
    "pragma",
    "x-couch-full-commit",
    "x-couch-id",
    "x-couch-persist",
    "x-couchdb-www-authenticate",
    "x-http-method-override",
    "x-requested-with",
    "x-couchdb-vhost-path"
]).


-define(SUPPORTED_METHODS, [
    "CONNECT",
    "COPY",
    "DELETE",
    "GET",
    "HEAD",
    "OPTIONS",
    "POST",
    "PUT",
    "TRACE"
]).


%% as defined in http://www.w3.org/TR/cors/#terminology
-define(SIMPLE_HEADERS, [
    "cache-control",
    "content-language",
    "content-type",
    "expires",
    "last-modified",
    "pragma"
]).


-define(COUCH_HEADERS, [
    "accept-ranges",
    "etag",
    "server",
    "x-couch-request-id",
    "x-couch-update-newrev",
    "x-couchdb-body-time"
]).


-define(SIMPLE_CONTENT_TYPE_VALUES, [
    "application/x-www-form-urlencoded",
    "multipart/form-data",
    "text/plain"
]).


-define(CORS_DEFAULT_MAX_AGE, 600).


-define(CORS_DEFAULT_ALLOW_CREDENTIALS, false).
