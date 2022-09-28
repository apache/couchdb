-module(couch_replicator_small_max_request_size_target).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT_EUNIT, 360).

reduce_max_request_size_test_() ->
    {
        "Replicate docs when target has a small max_http_request_size",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(should_replicate_all_docs, ?TIMEOUT_EUNIT),
                ?TDEF_FE(should_replicate_one, ?TIMEOUT_EUNIT),
                ?TDEF_FE(should_replicate_one_with_attachment, ?TIMEOUT_EUNIT)
            ]
        }
    }.

% Test documents which are below max_http_request_size but when batched, batch size
% will be greater than max_http_request_size. Replicator could automatically split
% the batch into smaller batches and POST those separately.
should_replicate_all_docs({_Ctx, {Source, Target}}) ->
    config:set("chttpd", "max_http_request_size", "10000", false),
    populate_source(Source),
    replicate(Source, Target),
    compare(Source, Target, []).

% If a document is too large to post as a single request, that document is
% skipped but replication overall will make progress and not crash.
should_replicate_one({_Ctx, {Source, Target}}) ->
    config:set("chttpd", "max_http_request_size", "10000", false),
    populate_source_one_large_one_small(Source),
    replicate(Source, Target),
    compare(Source, Target, [<<"doc0">>]).

% If a document has an attachment > 64 * 1024 bytes, replicator will switch to
% POST-ing individual documents directly and skip bulk_docs. Test that case
% separately
% See note in main test function why this was disabled.
should_replicate_one_with_attachment({_Ctx, {Source, Target}}) ->
    config:set("chttpd", "max_http_request_size", "10000", false),
    populate_source_one_large_attachment(Source),
    populate_source(Source),
    replicate(Source, Target),
    compare(Source, Target, [<<"doc0">>]).

populate_source(Source) ->
    add_docs(Source, 5, 3000, 0).

populate_source_one_large_one_small(Source) ->
    one_large_one_small(Source, 12000, 3000).

populate_source_one_large_attachment(Source) ->
    one_large_attachment(Source, 70000, 70000).

binary_chunk(Size) when is_integer(Size), Size > 0 ->
    <<<<"x">> || _ <- lists:seq(1, Size)>>.

add_docs(DbName, DocCount, DocSize, AttSize) ->
    [
        begin
            DocId = iolist_to_binary(["doc", integer_to_list(Id)]),
            add_doc(DbName, DocId, DocSize, AttSize)
        end
     || Id <- lists:seq(1, DocCount)
    ],
    ok.

one_large_one_small(DbName, Large, Small) ->
    add_doc(DbName, <<"doc0">>, Large, 0),
    add_doc(DbName, <<"doc1">>, Small, 0).

one_large_attachment(DbName, Size, AttSize) ->
    add_doc(DbName, <<"doc0">>, Size, AttSize).

add_doc(DbName, DocId, Size, AttSize) when is_binary(DocId) ->
    Doc0 = #doc{id = DocId, body = {[{<<"x">>, binary_chunk(Size)}]}},
    Doc = Doc0#doc{atts = atts(AttSize)},
    {ok, _} = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]).

atts(0) ->
    [];
atts(Size) ->
    [
        couch_att:new([
            {name, <<"att1">>},
            {type, <<"app/binary">>},
            {att_len, Size},
            {data, fun(Bytes) -> binary_chunk(Bytes) end}
        ])
    ].

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

replicate(Source, Target) ->
    couch_replicator_test_helper:replicate(
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            %  This makes batch_size more predictable
            {<<"worker_processes">>, "1"}
        ]}
    ).

compare(Source, Target, ExceptIds) ->
    couch_replicator_test_helper:cluster_compare_dbs(Source, Target, ExceptIds).
