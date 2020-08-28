-module(couch_replicator_small_max_request_size_target).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


reduce_max_request_size_test_() ->
    {
        "Replicate docs when target has a small max_http_request_size",
        {
            setup,
            fun couch_replicator_test_helper:start_couch/0,
            fun couch_replicator_test_helper:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(should_replicate_all_docs, 120),
                    ?TDEF_FE(should_replicate_one, 120),
                    ?TDEF_FE(should_replicate_one_with_attachment, 120)
                ]
            }
        }
    }.


setup() ->
    Source = couch_replicator_test_helper:create_db(),
    Target = couch_replicator_test_helper:create_db(),
    config:set("httpd", "max_http_request_size", "10000", false),
    {Source, Target}.


teardown({Source, Target}) ->
    config:delete("httpd", "max_http_request_size", false),
    couch_replicator_test_helper:delete_db(Source),
    couch_replicator_test_helper:delete_db(Target).


% Test documents which are below max_http_request_size but when batched, batch
% size will be greater than max_http_request_size. Replicator could
% automatically split the batch into smaller batches and POST those separately.
should_replicate_all_docs({Source, Target}) ->
    ?assertEqual(ok, add_docs(Source, 5, 3000, 0)),
    replicate(Source, Target),
    compare_dbs(Source, Target, []).


% If a document is too large to post as a single request, that document is
% skipped but replication overall will make progress and not crash.
should_replicate_one({Source, Target}) ->
    ?assertEqual(ok, one_large_one_small(Source, 12000, 3000)),
    replicate(Source, Target),
    compare_dbs(Source, Target, [<<"doc0">>]).


% If a document has an attachment > 64 * 1024 bytes, replicator will switch to
% POST-ing individual documents directly and skip bulk_docs. Test that case
% separately See note in main test function why this was disabled.
should_replicate_one_with_attachment({Source, Target}) ->
    ?assertEqual(ok, one_large_attachment(Source, 70000, 70000)),
    ?assertEqual(ok, add_docs(Source, 5, 3000, 0)),
    replicate(Source, Target),
    compare_dbs(Source, Target, [<<"doc0">>]).


binary_chunk(Size) when is_integer(Size), Size > 0 ->
    << <<"x">> || _ <- lists:seq(1, Size) >>.


add_docs(DbName, DocCount, DocSize, AttSize) ->
    [begin
        DocId = iolist_to_binary(["doc", integer_to_list(Id)]),
        add_doc(DbName, DocId, DocSize, AttSize)
    end || Id <- lists:seq(1, DocCount)],
    ok.


one_large_one_small(DbName, Large, Small) ->
    add_doc(DbName, <<"doc0">>, Large, 0),
    add_doc(DbName, <<"doc1">>, Small, 0),
    ok.


one_large_attachment(DbName, Size, AttSize) ->
    add_doc(DbName, <<"doc0">>, Size, AttSize),
    ok.


add_doc(DbName, DocId, Size, AttSize) when is_binary(DocId) ->
    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    Doc0 = #doc{id = DocId, body = {[{<<"x">>, binary_chunk(Size)}]}},
    Doc = Doc0#doc{atts = atts(AttSize)},
    {ok, _} = fabric2_db:update_doc(Db, Doc, []),
    ok.


atts(0) ->
    [];

atts(Size) ->
    [couch_att:new([
        {name, <<"att1">>},
        {type, <<"app/binary">>},
        {att_len, Size},
        {data, fun(Bytes) -> binary_chunk(Bytes) end}
    ])].


replicate(Source, Target) ->
    ?assertMatch({ok, _}, couch_replicator_test_helper:replicate(#{
        <<"source">> => Source,
        <<"target">> => Target,
        <<"worker_processes">> => 1  % This make batch_size predictable
    })).


compare_dbs(Source, Target, ExceptIds) ->
    ?assertEqual(ok, couch_replicator_test_helper:compare_dbs(Source, Target,
        ExceptIds)).
