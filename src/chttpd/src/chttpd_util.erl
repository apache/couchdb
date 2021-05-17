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

-module(chttpd_util).


-export([
    get_chttpd_config/1,
    get_chttpd_config/2,
    get_chttpd_config_integer/2,
    get_chttpd_config_boolean/2
]).


get_chttpd_config(Key) ->
    config:get("chttpd", Key, config:get("httpd", Key)).


get_chttpd_config(Key, Default) ->
    config:get("chttpd", Key, config:get("httpd", Key, Default)).


get_chttpd_config_integer(Key, Default) ->
    config:get_integer("chttpd", Key,
        config:get_integer("httpd", Key, Default)).


get_chttpd_config_boolean(Key, Default) ->
    config:get_boolean("chttpd", Key,
        config:get_boolean("httpd", Key, Default)).
