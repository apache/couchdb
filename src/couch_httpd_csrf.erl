% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

%% This module provides optional CSRF protection to any client
%%
%% Clients should use the following pseudo code;
%% if (hasCookie("CouchDB-CSRF")) {
%%   setRequestHeader("X-CouchDB-CSRF", cookieValue("CouchDB-CSRF"));
%% } else {
%%   setRequestHeader("X-CouchDB-CSRF", "true")
%% }
%%
%% If CouchDB sees the CouchDB-CSRF cookie then it checks its validity
%% and whether the X-CouchDB-CSRF request header exists and matches.
%% A 403 is returned if those checks fail.
%% If CouchDB does not see the CouchDB-CSRF cookie but does see
%% the X-CouchDB-CSRF header with value "true", a CouchDB-CSRF cookie
%% is generated and returned.

-module(couch_httpd_csrf).

-export([validate/1, headers/2]).

-include_lib("couch/include/couch_db.hrl").

validate(#httpd{method = 'GET'}) ->
    ok;
validate(#httpd{method = 'HEAD'}) ->
    ok;
validate(#httpd{method = 'OPTIONS'}) ->
    ok;
validate(#httpd{} = Req) ->
    Cookie = csrf_from_req(Req),
    Header = couch_httpd:header_value(Req, "X-CouchDB-CSRF"),
    case {Cookie, Header} of
        {undefined, undefined} ->
            throw_if_mandatory(Req);
        {undefined, "true"} ->
            throw_if_mandatory(Req);
        {"deleted", "true"} ->
            throw_if_mandatory(Req);
        {undefined, _} ->
            throw({forbidden, <<"CSRF header sent without Cookie">>});
        {Csrf, Csrf} ->
            ok = validate(Csrf);
        _ ->
            throw({forbidden, <<"CSRF Cookie/Header mismatch">>})
    end;
%% Check that we generated this CSRF token
validate(Csrf) when is_list(Csrf) ->
    case decode_cookie(Csrf) of
        malformed ->
            throw({forbidden, <<"Malformed CSRF Cookie">>});
        Cookie ->
            case validate_cookie(Cookie) of
                true ->
                    ok;
                false ->
                    throw({forbidden, <<"CSRF Cookie invalid or expired">>})
            end
    end.

throw_if_mandatory(#httpd{path_parts = []}) ->
    ok; %% Welcome message is public / entrypoint
throw_if_mandatory(_) ->
    case csrf_mandatory() of
        true ->
            throw({forbidden, <<"CSRF Cookie/Header is mandatory">>});
        false ->
            ok
    end.


headers(#httpd{} = Req, Headers) ->
    Header = couch_httpd:header_value(Req, "X-CouchDB-CSRF"),
    case {csrf_from_req(Req), csrf_in_headers(Headers), Header} of
        {undefined, false, "true"} ->
            [make_cookie() | Headers];
        {"deleted", false, "true"} ->
            [make_cookie() | Headers];
        {Csrf, false, Csrf} when Csrf /= undefined ->
            case decode_cookie(Csrf) of
                malformed ->
                    [delete_cookie() | Headers];
                Cookie ->
                    case validate_cookie(Cookie) of
                        true ->
                            case refresh_cookie(Cookie) of
                                true ->
                                    valid([make_cookie() | Headers]);
                                false ->
                                    valid(Headers)
                            end;
                        false ->
                            [delete_cookie() | Headers]
                    end
            end;
        _ ->
            Headers
    end.


make_cookie() ->
    Secret = ?l2b(ensure_csrf_secret()),
    Token = crypto:rand_bytes(8),
    Timestamp = timestamp(),
    Data = <<Token/binary, Timestamp:32>>,
    Hmac = crypto:sha_mac(Secret, Data),
    mochiweb_cookies:cookie("CouchDB-CSRF",
        couch_util:encodeBase64Url(<<Data/binary, Hmac/binary>>),
        [{path, "/"}, {max_age, max_age()}]).


delete_cookie() ->
    mochiweb_cookies:cookie("CouchDB-CSRF", "deleted",
        [{path, "/"}, {max_age, 0}]).

csrf_from_req(#httpd{} = Req) ->
    case couch_httpd:header_value(Req, "Cookie") of
        undefined ->
            undefined;
        Value ->
            Cookies = mochiweb_cookies:parse_cookie(Value),
            couch_util:get_value("CouchDB-CSRF", Cookies)
    end.


valid(Headers) ->
    case lists:keyfind("X-CouchDB-CSRF-Valid", 1, Headers) of
        false ->
            [{"X-CouchDB-CSRF-Valid", "true"} | Headers];
        _ ->
            Headers
    end.

csrf_in_headers(Headers) when is_list(Headers) ->
    lists:any(fun is_csrf_header/1, Headers).


is_csrf_header({"Set-Cookie", [$C, $o, $u, $c, $h, $D, $B, $-, $C, $S, $R, $F, $= | _]}) ->
    true;
is_csrf_header(_) ->
    false.


ensure_csrf_secret() ->
    case config:get("csrf", "secret", undefined) of
        undefined ->
            NewSecret = ?b2l(couch_uuids:random()),
            config:set("csrf", "secret", NewSecret),
            NewSecret;
        Secret -> Secret
    end.


decode_cookie(Cookie) ->
    try
        Cookie1 = couch_util:decodeBase64Url(Cookie),
        <<Token:8/binary, Timestamp:32, Hmac:20/binary>> = Cookie1,
        {Token, Timestamp, Hmac}
    catch
        error:_ ->
            malformed
    end.


validate_cookie({Token, Timestamp, ActualHmac}) ->
    Secret = ensure_csrf_secret(),
    ExpectedHmac = crypto:sha_mac(Secret, <<Token/binary, Timestamp:32>>),
    MaxAge = max_age(),
    Expired = Timestamp + MaxAge < timestamp(),
    couch_passwords:verify(ActualHmac, ExpectedHmac) and not Expired.


refresh_cookie({_, Timestamp, _}) ->
    MaxAge = max_age(),
    TimeLeft = Timestamp + MaxAge - timestamp(),
    TimeLeft < MaxAge * 0.5.


max_age() ->
    config:get_integer("csrf", "timeout", 3600).


timestamp() ->
    couch_httpd_auth:make_cookie_time().

csrf_mandatory() ->
    config:get_boolean("csrf", "mandatory", false).
