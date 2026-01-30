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

-module(chttpd_all_docs_range_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_all_docs_range_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

-define(DOCS, #{
    <<"docs">> => [
        #{<<"_id">> => <<"a">>},
        #{<<"_id">> => <<"z">>}
    ]
}).

% seconds
-define(TIMEOUT, 60).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), false),
    Db = ?tempdb(),
    ok = create_db(Db),
    ok = create_docs(Db),
    Db.

teardown(Db) ->
    ok = fabric:delete_db(Db),
    ok = config:delete("admins", ?USER, false).

all_docs_range_test_() ->
    {
        "chttpd all docs range tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_all_docs_inverted_range),
                    ?TDEF_FE(t_all_docs_inverted_range_descending),
                    ?TDEF_FE(t_all_docs_inverted_range_with_start_key_and_end_key_docid)
                ]
            }
        }
    }.

t_all_docs_inverted_range(Db) ->
    % This currently returns 200 with empty rows, but we want 400
    {Code, Res} = req(get, url(Db, "_all_docs", "startkey=\"z\"&endkey=\"a\"")),
    ?assertMatch(
        #{
            <<"error">> := <<"query_parse_error">>,
            <<"reason">> :=
                <<"No rows can match your key range, reverse your start_key and end_key or set descending=true">>
        },
        Res
    ),
    ?assertEqual(400, Code).

t_all_docs_inverted_range_descending(Db) ->
    % This should work because descending=true handles the inversion
    {Code, Res} = req(get, url(Db, "_all_docs", "startkey=\"z\"&endkey=\"a\"&descending=true")),
    ?assertMatch(
        #{
            <<"rows">> := [
                #{<<"id">> := <<"z">>},
                #{<<"id">> := <<"a">>}
            ]
        },
        Res
    ),
    ?assertEqual(200, Code).

t_all_docs_inverted_range_with_start_key_and_end_key_docid(Db) ->
    % start_key_docid validation is implicit but good to check if we touch validation logic
    % couch_mrview_util:check_range also checks docids if provided used in view_cmp
    % For _all_docs, startkey/endkey ARE the docids effectively.
    ok.

%%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%%%%
url(Db) ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port, "/", ?b2l(Db)]).

url(Db, Path, Parameters) ->
    url(Db) ++ "/" ++ Path ++ "?" ++ Parameters.

create_db(Db) ->
    case req(put, url(Db)) of
        {201, #{}} -> ok;
        Error -> error({failed_to_create_test_db, Db, Error})
    end.

create_docs(Db) ->
    case req(post, url(Db) ++ "/_bulk_docs", ?DOCS) of
        {201, _} -> ok;
        Error -> error({failed_to_create_docs, Db, Error})
    end.

req(Method, Url) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    case test_request:request(Method, Url, Headers) of
        {ok, Code, _, Res} -> {Code, jiffy:decode(Res, [return_maps])};
        Error -> error({request_failed, Method, Url, Error})
    end.

req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    case test_request:request(Method, Url, Headers, Body) of
        {ok, Code, _, Res} -> {Code, jiffy:decode(Res, [return_maps])};
        Error -> error({request_failed, Method, Url, Error})
    end.
