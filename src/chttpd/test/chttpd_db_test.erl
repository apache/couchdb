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

-module(chttpd_db_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(FIXTURE_TXT, ?ABS_PATH(?FILE)).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(Url),
    Url.

teardown(Url) ->
    delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist=false).

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).


create_doc(Url, Id) ->
    test_request:put(Url ++ "/" ++ Id,
        [?CONTENT_JSON, ?AUTH], "{\"mr\": \"rockoartischocko\"}").

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

all_test_() ->
    {
        "chttpd db tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0, fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_return_ok_true_on_bulk_update/1,
                    fun should_accept_live_as_an_alias_for_continuous/1,
                    fun should_return_404_for_delete_att_on_notadoc/1,
                    fun should_return_409_for_del_att_without_rev/1,
                    fun should_return_200_for_del_att_with_rev/1
                ]
            }
        }
    }.


should_return_ok_true_on_bulk_update(Url) ->
    ?_assertEqual(true,
        begin
            {ok, _, _, Body} = create_doc(Url, "testdoc"),
            {Json} = ?JSON_DECODE(Body),
            Ref = couch_util:get_value(<<"rev">>, Json, undefined),
            NewDoc = "{\"docs\": [{\"_rev\": \"" ++ ?b2l(Ref) ++ "\", \"_id\": \"testdoc\"}]}",
            {ok, _, _, ResultBody} = test_request:post(Url ++ "/_bulk_docs/",
                [?CONTENT_JSON, ?AUTH], NewDoc),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = lists:nth(1, ResultJson),
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end).


should_accept_live_as_an_alias_for_continuous(Url) ->
    ?_test(begin
        {ok, _, _, ResultBody} =
            test_request:get(Url ++ "/_changes?feed=live&timeout=1", [?AUTH]),
        % https://issues.apache.org/jira/browse/COUCHDB-3415?filter=12340503
        % if the decode fails, print out ResultBody, so we can debug what
        % extra data is coming in.
        {ResultJson} = try ?JSON_DECODE(ResultBody) of
            Json -> Json
        catch
            throw:Error ->
                io:format(user, "~nJSON_DECODE error: ~p~n", [Error]),
                io:format(user, "~nOffending String: ~p~n", [ResultBody]),
                ?assert(false) % should not happen, abort
        end,
        <<LastSeqNum0:1/binary, "-", _/binary>> = couch_util:get_value(
            <<"last_seq">>, ResultJson, undefined),
        LastSeqNum = list_to_integer(binary_to_list(LastSeqNum0)),

        {ok, _, _, _} = create_doc(Url, "testdoc2"),
        {ok, _, _, ResultBody2} = 
            test_request:get(Url ++ "/_changes?feed=live&timeout=1", [?AUTH]),
        io:format(user, "~nDEBUG COUCHDB-3415: ResultBody2: ~p~n", [ResultBody2]),
        [_, CleanedResult] = binary:split(ResultBody2, <<"\n">>),
        io:format(user, "~nDEBUG COUCHDB-3415: CleanedResult: ~p~n", [CleanedResult]),
        {[{_, Seq}, _]} = ?JSON_DECODE(CleanedResult),
        <<SeqNum0:1/binary, "-", _/binary>> = Seq,
        SeqNum = list_to_integer(binary_to_list(SeqNum0)),

        ?assertEqual(LastSeqNum + 1, SeqNum)
    end).


should_return_404_for_delete_att_on_notadoc(Url) ->
    ?_test(begin
        {ok, RC, _, RespBody} = test_request:delete(
            Url ++ "/notadoc/att.pdf",
            [?CONTENT_JSON, ?AUTH],
            []
        ),
        ?assertEqual(404, RC),
        ?assertEqual(
          {[{<<"error">>,<<"not_found">>},
            {<<"reason">>,<<"missing">>}]},
          jiffy:decode(RespBody)
        ),
        {ok, RC1, _, _} = test_request:get(
            Url ++ "/notadoc",
            [?CONTENT_JSON, ?AUTH],
            []
        ),
        ?assertEqual(404, RC1)
    end).


should_return_409_for_del_att_without_rev(Url) ->
    ?_test(begin
        {ok, Data} = file:read_file(?FIXTURE_TXT),
        Doc = {[
            {<<"_attachments">>, {[
                {<<"file.erl">>, {[
                    {<<"content_type">>, <<"text/plain">>},
                    {<<"data">>, base64:encode(Data)}
                ]}
            }]}}
        ]},
        {ok, RC, _, _} = test_request:put(
            Url ++ "/testdoc3",
            [?CONTENT_JSON, ?AUTH],
            jiffy:encode(Doc)
        ),
        ?assertEqual(201, RC),

        {ok, RC1, _, _} = test_request:delete(
            Url ++ "/testdoc3/file.erl",
            [?CONTENT_JSON, ?AUTH],
            []
        ),
        ?assertEqual(409, RC1)
    end).

should_return_200_for_del_att_with_rev(Url) ->
  ?_test(begin
      {ok, Data} = file:read_file(?FIXTURE_TXT),
      Doc = {[
          {<<"_attachments">>, {[
              {<<"file.erl">>, {[
                  {<<"content_type">>, <<"text/plain">>},
                  {<<"data">>, base64:encode(Data)}
              ]}
          }]}}
      ]},
      {ok, RC, _Headers, RespBody} = test_request:put(
          Url ++ "/testdoc4",
          [?CONTENT_JSON, ?AUTH],
          jiffy:encode(Doc)
      ),
      ?assertEqual(201, RC),

      {ResultJson} = ?JSON_DECODE(RespBody),
      Rev = couch_util:get_value(<<"rev">>, ResultJson, undefined),

      {ok, RC1, _, _} = test_request:delete(
          Url ++ "/testdoc4/file.erl?rev=" ++ Rev,
          [?CONTENT_JSON, ?AUTH],
          []
      ),
      ?assertEqual(200, RC1)
    end).
