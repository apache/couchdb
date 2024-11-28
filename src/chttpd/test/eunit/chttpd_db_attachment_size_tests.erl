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

-module(chttpd_db_attachment_size_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_att_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(CONTENT_MULTI_RELATED, {"Content-Type", "multipart/related;boundary=\"bound\""}).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    Persist = false,
    ok = config:set("admins", ?USER, ?b2l(Hashed), Persist),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    Url = "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(TmpDb),
    create_db(Url),
    add_doc(Url, "doc1"),
    Url.

teardown(Url) ->
    delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist = false),
    ok = config:delete("couchdb", "max_attachment_size").

attachment_size_test_() ->
    {
        "chttpd max_attachment_size tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(put_inline),
                    ?TDEF_FE(put_simple),
                    ?TDEF_FE(put_simple_chunked),
                    ?TDEF_FE(put_mp_related),
                    ?TDEF_FE(put_chunked_mp_related)
                ]
            }
        }
    }.

attachment_size_db_active_size_test_() ->
    {
        "attachment sizes are accounted properly in db size",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_add_attachments),
                    ?TDEF_FE(t_remove_attachments),
                    ?TDEF_FE(t_delete_doc),
                    ?TDEF_FE(t_delete_doc_but_keep_attachment),
                    ?TDEF_FE(t_conflicting_attachments)
                ]
            }
        }
    }.

put_inline(Url) ->
    ok = config:set("couchdb", "max_attachment_size", "50", false),
    Status = put_inline(Url, "doc2", 50),
    ?assert(Status =:= 201 orelse Status =:= 202),
    ?assertEqual(413, put_inline(Url, "doc3", 51)).

put_simple(Url) ->
    ok = config:set("couchdb", "max_attachment_size", "50", false),
    Headers = [{"Content-Type", "app/binary"}],
    Rev1 = doc_rev(Url, "doc1"),
    Data1 = data(50),
    Status1 = put_req(Url ++ "/doc1/att2?rev=" ++ Rev1, Headers, Data1),
    ?assert(Status1 =:= 201 orelse Status1 =:= 202),
    Data2 = data(51),
    Rev2 = doc_rev(Url, "doc1"),
    Status2 = put_req(Url ++ "/doc1/att3?rev=" ++ Rev2, Headers, Data2),
    ?assertEqual(413, Status2).

put_simple_chunked(Url) ->
    ok = config:set("couchdb", "max_attachment_size", "50", false),
    Headers = [{"Content-Type", "app/binary"}],
    Rev1 = doc_rev(Url, "doc1"),
    DataFun1 = data_stream_fun(50),
    Status1 = put_req_chunked(Url ++ "/doc1/att2?rev=" ++ Rev1, Headers, DataFun1),
    ?assert(Status1 =:= 201 orelse Status1 =:= 202),
    DataFun2 = data_stream_fun(51),
    Rev2 = doc_rev(Url, "doc1"),
    Status2 = put_req_chunked(Url ++ "/doc1/att3?rev=" ++ Rev2, Headers, DataFun2),
    ?assertEqual(413, Status2).

put_mp_related(Url) ->
    ok = config:set("couchdb", "max_attachment_size", "50", false),
    Headers = [?CONTENT_MULTI_RELATED],
    Body1 = mp_body(50),
    Status1 = put_req(Url ++ "/doc2", Headers, Body1),
    ?assert(Status1 =:= 201 orelse Status1 =:= 202),
    Body2 = mp_body(51),
    Status2 = put_req(Url ++ "/doc3", Headers, Body2),
    ?assertEqual(413, Status2).

put_chunked_mp_related(Url) ->
    ok = config:set("couchdb", "max_attachment_size", "50", false),
    Headers = [?CONTENT_MULTI_RELATED],
    Body = mp_body(50),
    Status = put_req_chunked(Url ++ "/doc4", Headers, Body),
    ?assert(Status =:= 201 orelse Status =:= 202).

t_add_attachments(Url) ->
    ?assertEqual(201, put_att(Url, "doc1", "att1", 10000)),
    Active1 = active_size(Url),
    ?assert(Active1 >= 10000 andalso Active1 < 12000),
    ?assertEqual(201, put_att(Url, "doc1", "att2", 10000)),
    Active2 = active_size(Url),
    ?assert(Active2 >= 20000 andalso Active2 < 22000).

t_remove_attachments(Url) ->
    ?assertEqual(201, put_att(Url, "doc1", "att1", 10000)),
    ?assertEqual(201, put_att(Url, "doc1", "att2", 10000)),
    Active1 = active_size(Url),
    ?assert(Active1 >= 20000 andalso Active1 < 22000),
    ?assertEqual(201, remove_att(Url, "doc1", "att1")),
    Active2 = active_size(Url),
    ?assert(Active2 >= 10000 andalso Active2 < 12000),
    ?assertEqual(201, remove_att(Url, "doc1", "att2")),
    ?assert(active_size(Url) < 2000).

t_delete_doc(Url) ->
    ?assertEqual(201, put_att(Url, "doc1", "att1", 10000)),
    ?assertEqual(201, put_att(Url, "doc1", "att2", 10000)),
    Active1 = active_size(Url),
    ?assert(Active1 >= 20000 andalso Active1 < 22000),
    delete_doc(Url, "doc1"),
    ?assert(active_size(Url) < 2000).

t_delete_doc_but_keep_attachment(Url) ->
    ?assertEqual(201, put_att(Url, "doc1", "att1", 10000)),
    ?assertEqual(201, put_att(Url, "doc1", "att2", 10000)),
    Active1 = active_size(Url),
    ?assert(Active1 >= 20000 andalso Active1 < 22000),
    % Here we are deleting the document but keeping the
    % body and one of the attachments (att2) around
    ?assertEqual(201, delete_remove_att(Url, "doc1", "att1")),
    Active2 = active_size(Url),
    ?assert(Active2 >= 10000 andalso Active2 < 12000).

t_conflicting_attachments(Url) ->
    ?assertEqual(201, put_att(Url, "doc1", "att1", 10000)),
    % The rev tree should look like: (active ~= 10KB)
    %          1-...
    %          |
    %          2-... (att1)
    Active1 = active_size(Url),
    ?assert(Active1 >= 10000 andalso Active1 < 12000),

    % Add two conflicting revisions to doc1
    Rev = doc_rev(Url, "doc1"),
    new_edits_false_update(Url, "doc1", Rev, <<"a">>),
    new_edits_false_update(Url, "doc1", Rev, <<"b">>),
    % Rev tree shape: (active still ~= 10KB):
    %          1-...
    %          |
    %          2-... (att1)
    %         /      \
    %        /        \
    %       /          \
    %     3-a          3-b
    %
    Active2 = active_size(Url),
    ?assert(Active2 >= 10000 andalso Active2 < 12000),

    ?assertEqual(201, put_att_rev(Url, "doc1", "3-a", "att2", 10000)),
    % Updated 3-a with att2. Active size ~= 20KB
    %          1-...
    %          |
    %          2-... (att1)
    %         /      \
    %        /        \
    %       /          \
    %     3-a          3-b
    %      |
    %     4-a (att2)
    %
    Active3 = active_size(Url),
    ?assert(Active3 >= 20000 andalso Active3 < 22000),

    % Update 3-b with att3 (active ~= 30KB)
    %
    %          1-...
    %          |
    %          2-...(att1)
    %         /      \
    %        /        \
    %       /          \
    %     3-a          3-b
    %      |            |
    %     4-...(att2)  4-...(att3)
    %
    ?assertEqual(201, put_att_rev(Url, "doc1", "3-b", "att3", 10000)),
    Active4 = active_size(Url),
    ?assert(Active4 >= 30000 andalso Active4 < 32000),

    delete_doc(Url, "doc1"),
    % Delete the winning branch, whichever it is (active ~= 20KB)
    %
    %          1-...
    %          |
    %          2-...(att1)
    %         /      \
    %        /        \
    %       /          \
    %     3-a          3-b
    %      |            |
    %     4-...(att2)  4-...(att3)
    %      |
    %     5-...
    %     deleted
    %
    Active5 = active_size(Url),
    ?assert(Active5 >= 20000 andalso Active5 < 22000),

    delete_doc(Url, "doc1"),
    % Delete the new (other) winning branch: (active < 10KB)
    %
    %          1-...
    %          |
    %          2-...(att1)
    %         /      \
    %        /        \
    %       /          \
    %     3-a          3-b
    %      |            |
    %     4-...(att2)  4-...(att3)
    %      |            |
    %     5-...        5-...
    %     deleted      deleted
    %
    ?assert(active_size(Url) < 2000).

% Helper functions

create_db(Url) ->
    Status = put_req(Url ++ "?q=1&n=1", "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

add_doc(Url, DocId) ->
    Status = put_req(Url ++ "/" ++ DocId, "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

put_inline(Url, DocId, Size) ->
    Doc = #{
        <<"_attachments">> => #{
            <<"att1">> => #{
                <<"content_type">> => <<"app/binary">>,
                <<"data">> => data_b64(Size)
            }
        }
    },
    put_req(Url ++ "/" ++ DocId, [], Doc).

mp_body(AttSize) ->
    AttData = data(AttSize),
    SizeStr = integer_to_list(AttSize),
    string:join(
        [
            "--bound",

            "Content-Type: application/json",

            "",

            "{\"_id\":\"doc2\", \"_attachments\":{\"att\":"
            "{\"content_type\":\"app/binary\", \"length\":" ++ SizeStr ++
                ","
                "\"follows\":true}}}",

            "--bound",

            "Content-Disposition: attachment; filename=\"att\"",

            "Content-Type: app/binary",

            "",

            AttData,

            "--bound--"
        ],
        "\r\n"
    ).

doc_rev(Url, DocId) ->
    {200, #{<<"_rev">> := Rev}} = get_req(Url ++ "/" ++ DocId),
    binary_to_list(Rev).

put_att(Url, DocId, AttName, Size) ->
    put_att_rev(Url, DocId, doc_rev(Url, DocId), AttName, Size).

put_att_rev(Url, DocId, Rev, AttName, Size) ->
    Headers = [{"Content-Type", "app/binary"}],
    Data = data(Size),
    put_req(Url ++ "/" ++ DocId ++ "/" ++ AttName ++ "?rev=" ++ Rev, Headers, Data).

remove_att(Url, DocId, AttName) ->
    {200, #{} = Doc} = get_req(Url ++ "/" ++ DocId),
    #{<<"_attachments">> := Atts} = Doc,
    {#{}, Atts1} = maps:take(?l2b(AttName), Atts),
    {Status, _} = req(post, Url, [], Doc#{<<"_attachments">> := Atts1}),
    Status.

delete_remove_att(Url, DocId, AttName) ->
    {200, #{} = Doc} = get_req(Url ++ "/" ++ DocId),
    #{<<"_attachments">> := Atts} = Doc,
    {#{}, Atts1} = maps:take(?l2b(AttName), Atts),
    Doc1 = Doc#{<<"_attachments">> => Atts1, <<"_deleted">> => true},
    {Status, _} = req(post, Url, [], Doc1),
    Status.

new_edits_false_update(Url, DocId, OldRev, Rev) ->
    {200, #{} = Doc} = get_req(Url ++ "/" ++ DocId ++ "?rev=" ++ OldRev ++ "&revs=true"),
    #{<<"_revisions">> := Revisions} = Doc,
    #{<<"ids">> := RevIds, <<"start">> := Start} = Revisions,
    RevIds1 = [Rev] ++ RevIds,
    Revisions1 = #{<<"ids">> => RevIds1, <<"start">> => Start + 1},
    {_, Doc1} = maps:take(<<"_rev">>, Doc),
    {_, Doc2} = maps:take(<<"_id">>, Doc1),
    Doc3 = Doc2#{<<"_revisions">> => Revisions1},
    {Status, Res} = req(put, Url ++ "/" ++ DocId ++ "?new_edits=false", [], Doc3),
    ?assertEqual(201, Status),
    ?assertMatch(#{<<"ok">> := true}, Res),
    ok.

delete_doc(Url, DocId) ->
    Rev = doc_rev(Url, "doc1"),
    {200, _} = req(delete, Url ++ "/" ++ DocId ++ "?rev=" ++ Rev, []),
    ok.

active_size(Url) ->
    {200, #{<<"sizes">> := Sizes}} = get_req(Url),
    #{<<"active">> := Active} = Sizes,
    Active.

put_req(Url, Body) ->
    put_req(Url, [], Body).

put_req(Url, Headers, Body) ->
    {Status, _} = req(put, Url, Headers, Body),
    Status.

put_req_chunked(Url, Headers, Body) ->
    Opts = [{transfer_encoding, {chunked, 1}}],
    {ok, Status, _, _} = test_request:put(Url, Headers ++ [?AUTH], Body, Opts),
    Status.

get_req(Url) ->
    {ok, Status, _, Res} = test_request:get(Url, [?CONTENT_JSON, ?AUTH]),
    {Status, json_decode(Res)}.

req(Method, Url, Headers) ->
    Headers1 = Headers ++ [?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers1),
    {Code, json_decode(Res)}.

req(Method, Url, Headers, #{} = Body) ->
    req(Method, Url, Headers ++ [?CONTENT_JSON], jiffy:encode(Body));
req(Method, Url, Headers, Body) ->
    Headers1 = Headers ++ [?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers1, Body),
    {Code, json_decode(Res)}.

% Data streaming generator for ibrowse client. ibrowse will repeatedly call the
% function with State and it should return {ok, Data, NewState} or eof at end.
data_stream_fun(Size) ->
    Fun = fun
        (0) -> eof;
        (BytesLeft) -> {ok, <<"x">>, BytesLeft - 1}
    end,
    {Fun, Size}.

data(Size) ->
    string:copies("x", Size).

data_b64(Size) ->
    base64:encode(data(Size)).

json_decode(Bin) when is_binary(Bin) ->
    jiffy:decode(Bin, [return_maps]).
