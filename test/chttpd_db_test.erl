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


setup() ->
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(Url),
    Url.

teardown(Url) ->
    delete_db(Url).

create_db(Url) ->
    {ok, _, _, _} = test_request:put(Url,
                [{"Content-Type", "application/json"}], "{}").


create_doc(Url, Id) ->
    test_request:put(Url ++ "/" ++ Id,
        [{"Content-Type", "application/json"}], "{\"mr\": \"rockoartischocko\"}").

delete_db(Url) ->
    {ok, _, _, _} = test_request:delete(Url).

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
                    fun should_accept_live_as_an_alias_for_continuous/1
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
                [{"Content-Type", "application/json"}], NewDoc),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = lists:nth(1, ResultJson),
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end).


should_accept_live_as_an_alias_for_continuous(Url) ->
    {ok, _, _, ResultBody} = test_request:get(Url ++ "/_changes?feed=live&timeout=1"),
    {ResultJson} = ?JSON_DECODE(ResultBody),
    [Last_Seq, _] = couch_util:get_value(<<"last_seq">>, ResultJson, undefined),
    {ok, _, _, _} = create_doc(Url, "testdoc2"),

    ?_assertEqual(Last_Seq + 1,
        begin
            {ok, _, _, ResultBody2} = test_request:get(Url ++ "/_changes?feed=live&timeout=1"),
            [_, CleanedResult] = binary:split(ResultBody2, <<"\n">>),
            {[{_, [Seq, _]}, _]} = ?JSON_DECODE(CleanedResult),
            Seq
        end).
