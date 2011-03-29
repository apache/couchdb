-module(mem3_rep).

-export([go/2, changes_enumerator/2, make_local_id/2]).

-include("mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(CTX, #user_ctx{roles = [<<"_admin">>]}).

-record(acc, {revcount = 0, infos = [], seq, localid, source, target}).

go(DbName, Node) when is_binary(DbName), is_atom(Node) ->
    go(#shard{name=DbName, node=node()}, #shard{name=DbName, node=Node});

go(#shard{} = Source, #shard{} = Target) ->
    LocalId = make_local_id(Source, Target),
    case couch_db:open(Source#shard.name, [{user_ctx,?CTX}]) of
    {ok, Db} ->
        try
            go(Db, Target, LocalId)
        catch error:{not_found, no_db_file} ->
            {error, missing_target}
        after
            couch_db:close(Db)
        end;
    {not_found, no_db_file} ->
        {error, missing_source}
    end.

go(#db{name=DbName} = Db, #shard{} = Target, LocalId) ->
    erlang:put(io_priority, {internal_repl, DbName}),
    Seq = calculate_start_seq(Db, Target, LocalId),
    Acc0 = #acc{source=Db, target=Target, seq=Seq, localid=LocalId},
    Fun = fun ?MODULE:changes_enumerator/2,
    {ok, AccOut} = couch_db:changes_since(Db, all_docs, Seq, Fun, [], Acc0),
    {ok, _} = replicate_batch(AccOut).

make_local_id(#shard{node=SourceNode}, #shard{node=TargetNode}) ->
    S = couch_util:encodeBase64Url(couch_util:md5(term_to_binary(SourceNode))),
    T = couch_util:encodeBase64Url(couch_util:md5(term_to_binary(TargetNode))),
    <<"_local/shard-sync-", S/binary, "-", T/binary>>.

changes_enumerator(DocInfo, #acc{revcount = C} = Acc) when C >= 99 ->
    Seq = DocInfo#doc_info.high_seq,
    replicate_batch(Acc#acc{seq = Seq, infos = [DocInfo | Acc#acc.infos]});

changes_enumerator(DocInfo, #acc{revcount = C, infos = Infos} = Acc) ->
    RevCount = C + length(DocInfo#doc_info.revs),
    Seq = DocInfo#doc_info.high_seq,
    {ok, Acc#acc{seq = Seq, revcount = RevCount, infos = [DocInfo | Infos]}}.

replicate_batch(#acc{target = #shard{node=Node, name=Name}} = Acc) ->
    case find_missing_revs(Acc) of
    [] ->
        ok;
    Missing ->
        ok = save_on_target(Node, Name, open_docs(Acc#acc.source, Missing))
    end,
    update_locals(Acc),
    {ok, Acc#acc{revcount=0, infos=[]}}.

find_missing_revs(Acc) ->
    #acc{target = #shard{node=Node, name=Name}, infos = Infos} = Acc,
    IdsRevs = lists:map(fun(#doc_info{id=Id, revs=RevInfos}) ->
        {Id, [R || #rev_info{rev=R} <- RevInfos]}
    end, Infos),
    Options = [{io_priority, {internal_repl, Name}}],
    rexi_call(Node, {fabric_rpc, get_missing_revs, [Name, IdsRevs, Options]}).

open_docs(Db, Missing) ->
    lists:flatmap(fun({Id, Revs, _}) ->
        {ok, Docs} = couch_db:open_doc_revs(Db, Id, Revs, [latest]),
        [Doc || {ok, Doc} <- Docs]
    end, Missing).

save_on_target(Node, Name, Docs) ->
    Options = [replicated_changes, full_commit, {user_ctx, ?CTX},
        {io_priority, {internal_repl, Name}}],
    rexi_call(Node, {fabric_rpc, update_docs, [Name, Docs, Options]}),
    ok.

update_locals(Acc) ->
    #acc{seq=Seq, source=Db, target=Target, localid=Id} = Acc,
    #shard{name=Name, node=Node} = Target,
    Doc = #doc{id = Id, body = {[
        {<<"seq">>, Seq},
        {<<"node">>, list_to_binary(atom_to_list(Node))},
        {<<"timestamp">>, list_to_binary(iso8601_timestamp())}
    ]}},
    {ok, _} = couch_db:update_doc(Db, Doc, []),
    Options = [{user_ctx, ?CTX}, {io_priority, {internal_repl, Name}}],
    rexi_call(Node, {fabric_rpc, update_docs, [Name, [Doc], Options]}).

rexi_call(Node, MFA) ->
    Ref = rexi:cast(Node, MFA),
    receive {Ref, {ok, Reply}} ->
        Reply;
    {Ref, Error} ->
        erlang:error(Error)
    after 60000 ->
        erlang:error(timeout)
    end.

calculate_start_seq(Db, #shard{node=Node, name=Name}, LocalId) ->
    case couch_db:open_doc(Db, LocalId, []) of
    {ok, #doc{body = {SProps}}} ->
        Opts = [{io_priority, {internal_repl, Name}}],
        try rexi_call(Node, {fabric_rpc, open_doc, [Name, LocalId, Opts]}) of
        #doc{body = {TProps}} ->
            SourceSeq = couch_util:get_value(<<"seq">>, SProps, 0),
            TargetSeq = couch_util:get_value(<<"seq">>, TProps, 0),
            erlang:min(SourceSeq, TargetSeq)
        catch error:_ ->
            0
        end;
    _ ->
        0
    end.

iso8601_timestamp() ->
    {_,_,Micro} = Now = os:timestamp(),
    {{Year,Month,Date},{Hour,Minute,Second}} = calendar:now_to_datetime(Now),
    Format = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
    io_lib:format(Format, [Year, Month, Date, Hour, Minute, Second, Micro]).
