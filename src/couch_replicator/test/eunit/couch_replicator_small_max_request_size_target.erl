-module(couch_replicator_small_max_request_size_target).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-import(couch_replicator_test_helper, [
    db_url/1,
    replicate/1,
    compare_dbs/3
]).

-define(TIMEOUT_EUNIT, 360).


setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.


setup(remote) ->
    {remote, setup()};

setup({A, B}) ->
    Ctx = test_util:start_couch([couch_replicator]),
    config:set("httpd", "max_http_request_size", "10000", false),
    Source = setup(A),
    Target = setup(B),
    {Ctx, {Source, Target}}.


teardown({remote, DbName}) ->
    teardown(DbName);
teardown(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.

teardown(_, {Ctx, {Source, Target}}) ->
    teardown(Source),
    teardown(Target),
    ok = application:stop(couch_replicator),
    ok = test_util:stop_couch(Ctx).


reduce_max_request_size_test_() ->
    Pairs = [{remote, remote}],
    {
        "Replicate docs when target has a small max_http_request_size",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Pair, fun should_replicate_all_docs/2}
             || Pair <- Pairs]
            ++ [{Pair, fun should_replicate_one/2}
             || Pair <- Pairs]
            % Disabled. See issue 574. Sometimes PUTs with a doc and
            % attachment which exceed maximum request size are simply
            % closed instead of returning a 413 request. That makes these
            % tests flaky.
            ++ [{Pair, fun should_replicate_one_with_attachment/2}
             || Pair <- Pairs]
        }
    }.


% Test documents which are below max_http_request_size but when batched, batch size
% will be greater than max_http_request_size. Replicator could automatically split
% the batch into smaller batches and POST those separately.
should_replicate_all_docs({From, To}, {_Ctx, {Source, Target}}) ->
    {lists:flatten(io_lib:format("~p -> ~p", [From, To])),
     {inorder, [should_populate_source(Source),
                should_replicate(Source, Target),
                should_compare_databases(Source, Target, [])]}}.


% If a document is too large to post as a single request, that document is
% skipped but replication overall will make progress and not crash.
should_replicate_one({From, To}, {_Ctx, {Source, Target}}) ->
    {lists:flatten(io_lib:format("~p -> ~p", [From, To])),
     {inorder, [should_populate_source_one_large_one_small(Source),
                should_replicate(Source, Target),
                should_compare_databases(Source, Target, [<<"doc0">>])]}}.


% If a document has an attachment > 64 * 1024 bytes, replicator will switch to
% POST-ing individual documents directly and skip bulk_docs. Test that case
% separately
% See note in main test function why this was disabled.
should_replicate_one_with_attachment({From, To}, {_Ctx, {Source, Target}}) ->
   {lists:flatten(io_lib:format("~p -> ~p", [From, To])),
    {inorder, [should_populate_source_one_large_attachment(Source),
               should_populate_source(Source),
               should_replicate(Source, Target),
               should_compare_databases(Source, Target, [<<"doc0">>])]}}.


should_populate_source({remote, Source}) ->
    should_populate_source(Source);

should_populate_source(Source) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(add_docs(Source, 5, 3000, 0))}.


should_populate_source_one_large_one_small({remote, Source}) ->
    should_populate_source_one_large_one_small(Source);

should_populate_source_one_large_one_small(Source) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(one_large_one_small(Source, 12000, 3000))}.


should_populate_source_one_large_attachment({remote, Source}) ->
   should_populate_source_one_large_attachment(Source);

should_populate_source_one_large_attachment(Source) ->
  {timeout, ?TIMEOUT_EUNIT, ?_test(one_large_attachment(Source, 70000, 70000))}.


should_replicate({remote, Source}, Target) ->
    should_replicate(db_url(Source), Target);

should_replicate(Source, {remote, Target}) ->
    should_replicate(Source, db_url(Target));

should_replicate(Source, Target) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(replicate(Source, Target))}.


should_compare_databases({remote, Source}, Target, ExceptIds) ->
    should_compare_databases(Source, Target, ExceptIds);

should_compare_databases(Source, {remote, Target}, ExceptIds) ->
    should_compare_databases(Source, Target, ExceptIds);

should_compare_databases(Source, Target, ExceptIds) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(compare_dbs(Source, Target, ExceptIds))}.


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
    add_doc(DbName, <<"doc1">>, Small, 0).


one_large_attachment(DbName, Size, AttSize) ->
   add_doc(DbName, <<"doc0">>, Size, AttSize).


add_doc(DbName, DocId, Size, AttSize) when is_binary(DocId) ->
     {ok, Db} = couch_db:open_int(DbName, []),
     Doc0 = #doc{id = DocId, body = {[{<<"x">>, binary_chunk(Size)}]}},
     Doc = Doc0#doc{atts = atts(AttSize)},
     {ok, _} = couch_db:update_doc(Db, Doc, []),
     couch_db:close(Db).


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
    replicate({[
        {<<"source">>, Source},
        {<<"target">>, Target},
        {<<"worker_processes">>, "1"} %  This make batch_size predictable
    ]}).
