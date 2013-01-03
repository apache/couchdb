% Copyright 2013 Cloudant. All rights reserved.

-module(custodian).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-export([scan/0]).

% public functions.

scan() ->
    {ok, TrulyDown} = custodian_server:truly_down(),
    ExpectedN = list_to_integer(couch_config:get("cluster", "n", "3")),
    DbName = couch_config:get("mem3", "shards_db", "dbs"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    try
        Fun = fun(#full_doc_info{id=Id}, _, _) ->
            scan(Id, TrulyDown, ExpectedN), {ok, nil} end,
        {ok, _, _} = couch_db:enum_docs(Db, Fun, ok, []),
        ok
    after
        couch_db:close(Db)
    end.

% private functions.

scan(<<"_design/", _/binary>>, _TrulyDown, _ExpectedN) ->
    ok;
scan(Id, TrulyDown, ExpectedN) ->
    Shards = mem3:shards(Id),
    Rs = [R || #shard{range=R} <- lists:ukeysort(#shard.range, Shards)],
    ActualN = [{R1, [N || #shard{node=N,range=R2} <- Shards, R1 == R2]} ||  R1 <- Rs],
    scan(Id, TrulyDown, ExpectedN, ActualN).

scan(_Id, _TrulyDown, _ExpectedN, []) ->
    ok;
scan(Id, TrulyDown, ExpectedN, [{[Lo,Hi], Nodes}|Rest]) ->
    Nodes1 = [maybe_redirect(Node) || Node <- Nodes],
    Nodes2 = Nodes1 -- TrulyDown,
    case length(Nodes2) < ExpectedN of
        true ->
            twig:log(emerg, "Underprotected shard ~s-~s of ~s (~B/~B)",
                [couch_util:to_hex(<<Lo:32/integer>>),
                 couch_util:to_hex(<<Hi:32/integer>>),
                 Id, length(Nodes2), ExpectedN]);
        false ->
            ok
    end,
    scan(Id, TrulyDown, ExpectedN, Rest).

maybe_redirect(Node) ->
    case couch_config:get("mem3.redirects", atom_to_list(Node)) of
        undefined ->
            Node;
        Redirect ->
            list_to_existing_atom(Redirect)
    end.
