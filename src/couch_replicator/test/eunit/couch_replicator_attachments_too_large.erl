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

-module(couch_replicator_attachments_too_large).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").


setup(_) ->
    Ctx = test_util:start_couch([couch_replicator]),
    Source = create_db(),
    create_doc_with_attachment(Source, <<"doc">>, 1000),
    Target = create_db(),
    {Ctx, {Source, Target}}.


teardown(_, {Ctx, {Source, Target}}) ->
    delete_db(Source),
    delete_db(Target),
    config:delete("couchdb", "max_attachment_size"),
    ok = test_util:stop_couch(Ctx).


attachment_too_large_replication_test_() ->
    Pairs = [{remote, remote}],
    {
        "Attachment size too large replication tests",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Pair, fun should_succeed/2} || Pair <- Pairs] ++
            [{Pair, fun should_fail/2} || Pair <- Pairs]
        }
    }.


should_succeed({From, To}, {_Ctx, {Source, Target}}) ->
    RepObject = {[
        {<<"source">>, db_url(From, Source)},
        {<<"target">>, db_url(To, Target)}
    ]},
    config:set("couchdb", "max_attachment_size", "1000", _Persist = false),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    ?_assertEqual(ok, couch_replicator_test_helper:compare_dbs(Source, Target)).


should_fail({From, To}, {_Ctx, {Source, Target}}) ->
    RepObject = {[
        {<<"source">>, db_url(From, Source)},
        {<<"target">>, db_url(To, Target)}
    ]},
    config:set("couchdb", "max_attachment_size", "999", _Persist = false),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    ?_assertError({badmatch, {not_found, missing}},
        couch_replicator_test_helper:compare_dbs(Source, Target)).


create_db() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.


create_doc_with_attachment(DbName, DocId, AttSize) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    Doc = #doc{id = DocId, atts = att(AttSize)},
    {ok, _} = couch_db:update_doc(Db, Doc, []),
    couch_db:close(Db),
    ok.


att(Size) when is_integer(Size), Size >= 1 ->
    [couch_att:new([
        {name, <<"att">>},
        {type, <<"app/binary">>},
        {att_len, Size},
        {data, fun(_Bytes) ->
            << <<"x">> || _ <- lists:seq(1, Size) >>
        end}
    ])].


delete_db(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]).


db_url(remote, DbName) ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(couch_httpd, port),
    ?l2b(io_lib:format("http://~s:~b/~s", [Addr, Port, DbName])).
