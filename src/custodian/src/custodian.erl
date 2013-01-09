% Copyright 2013 Cloudant. All rights reserved.

-module(custodian).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-export([summary/0, report/0]).

% public functions.

summary() ->
    scan(0, fun summary/5).

report() ->
    scan([], fun report/5).

% private functions.

scan(Init, AccFun) ->
    Live = [atom_to_list(N) || N <- [node() | nodes()]],
    Down = custodian_util:hosting_nodes() -- Live,
    ExpectedN = list_to_integer(couch_config:get("cluster", "n", "3")),
    ok.

fold_fun(#full_doc_info{id = <<"_", _/binary>>}, _, Acc) ->
    {ok, Acc};
fold_fun(#full_doc_info{deleted=true}, _, Acc) ->
    {ok, Acc};
fold_fun(#full_doc_info{id = Id}, _, Acc) ->
    Shards = mem3:shards(Id),
    Rs = [R || #shard{range=R} <- lists:ukeysort(#shard.range, Shards)],
    ActualN = [{R1, [N || #shard{node=N,range=R2} <- Shards, R1 == R2]} ||  R1 <- Rs],
    fold_fun(Id, ActualN, Acc);
fold_fun(_Id, [], Acc) ->
    {ok, Acc};
fold_fun(Id, [{Range, Nodes}|Rest], {TrulyDown, ExpectedN, Acc, Fun}) ->
    Nodes1 = [maybe_redirect(Node) || Node <- Nodes],
    Nodes2 = Nodes1 -- TrulyDown,
    NewAcc = case length(Nodes2) < ExpectedN of
        true ->
            Fun(Id, Range, length(Nodes2), ExpectedN, Acc);
        false ->
            Acc
    end,
    fold_fun(Id, Rest, {TrulyDown, ExpectedN, NewAcc, Fun}).

maybe_redirect(Node) ->
    case couch_config:get("mem3.redirects", atom_to_list(Node)) of
        undefined ->
            Node;
        Redirect ->
            list_to_existing_atom(Redirect)
    end.

summary(_Id, _Range, _ActualN, _ExpectedN, Acc) ->
    Acc + 1.

report(Id, Range, ActualN, ExpectedN, Acc) ->
    [{Id, Range, ActualN, ExpectedN}|Acc].

