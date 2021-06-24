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
    parse_copy_destination_header/1,
    get_chttpd_config/1,
    get_chttpd_config/2,
    get_chttpd_config_integer/2,
    get_chttpd_config_boolean/2,
    get_chttpd_auth_config/1,
    get_chttpd_auth_config/2,
    get_chttpd_auth_config_integer/2,
    get_chttpd_auth_config_boolean/2
]).

parse_copy_destination_header(Req) ->
    case couch_httpd:header_value(Req, "Destination") of
        undefined ->
            throw({bad_request, "Destination header is mandatory for COPY."});
        Destination ->
            case re:run(Destination, "^https?://", [{capture, none}]) of
                match ->
                    throw({bad_request, "Destination URL must be relative."});
                nomatch ->
                    % see if ?rev=revid got appended to the Destination header
                    case re:run(Destination, "\\?", [{capture, none}]) of
                        nomatch ->
                            {list_to_binary(Destination), {0, []}};
                        match ->
                            [DocId, RevQs] = re:split(Destination, "\\?", [{return, list}]),
                            [_RevQueryKey, Rev] = re:split(RevQs, "=", [{return, list}]),
                            {Pos, RevId} = couch_doc:parse_rev(Rev),
                            {list_to_binary(DocId), {Pos, [RevId]}}
                    end
            end
    end.

get_chttpd_config(Key) ->
    config:get("chttpd", Key, config:get("httpd", Key)).

get_chttpd_config(Key, Default) ->
    config:get("chttpd", Key, config:get("httpd", Key, Default)).

get_chttpd_config_integer(Key, Default) ->
    config:get_integer(
        "chttpd",
        Key,
        config:get_integer("httpd", Key, Default)
    ).

get_chttpd_config_boolean(Key, Default) ->
    config:get_boolean(
        "chttpd",
        Key,
        config:get_boolean("httpd", Key, Default)
    ).

get_chttpd_auth_config(Key) ->
    config:get("chttpd_auth", Key, config:get("couch_httpd_auth", Key)).

get_chttpd_auth_config(Key, Default) ->
    config:get(
        "chttpd_auth",
        Key,
        config:get("couch_httpd_auth", Key, Default)
    ).

get_chttpd_auth_config_integer(Key, Default) ->
    config:get_integer(
        "chttpd_auth",
        Key,
        config:get_integer("couch_httpd_auth", Key, Default)
    ).

get_chttpd_auth_config_boolean(Key, Default) ->
    config:get_boolean(
        "chttpd_auth",
        Key,
        config:get_boolean("couch_httpd_auth", Key, Default)
    ).
