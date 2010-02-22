-module(cluster_ops_test).

-include("../../couchdb/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").


% read_quorum_test() ->
%     % we need to be running a cluster here...
%     % not sure how to start things up for unit tests

%     % but we're testing reads when a node is missing a doc, so disable internal
%     % replication - a bit harsh if anything else is here, but hey, it's a test
%     rpc:multicall(showroom, stop, []),
%     rpc:multicall(supervisor, terminate_child,
%                   [couch_primary_services, couch_replication_supervisor]),
%     rpc:multicall(supervisor, delete_child,
%                   [couch_primary_services, couch_replication_supervisor]),

%     % create db
%     DbName = <<"cluster_ops_test">>,
%     showroom_db:delete_db(DbName, []),
%     {Status, #db{name=DbName}} = showroom_db:create_db(DbName, []),
%     ?assertEqual(ok, Status),

%     % open db
%     {ok, Db} = showroom_db:open_db(DbName, []),

%     % make a test doc
%     Key = <<"a">>,
%     Json = {[{<<"_id">>,Key}]},
%     Doc = couch_doc:from_json_obj(Json),
%     Clock = vector_clock:create(node()),
%     NewDoc = Doc#doc{clock=Clock},

%     % insert a doc in two shards out of three
%     % TODO: we need N=3, need to fix that at db create time Options above
%     %       (fb 1001)
%     {M,F,A} = {dynomite_couch_api, put,[Db, NewDoc, []]},
%     CorrectNodeParts = membership2:nodeparts_for_key(Key),
%     [{MissingNode, MissingPart} | BadNodeParts] = CorrectNodeParts,
%     MapFun = fun({Node,Part}) ->
%         rpc:call(Node, M, F, [[Part | A]])
%     end,
%     {Good, Bad} = pcall(MapFun, BadNodeParts, 2),
%     ?assertEqual(2, length(Good)),
%     ?assertEqual([], Bad),

%     % make sure it's notfound on the MissingNode
%     MissingNodeGet = rpc:call(MissingNode, dynomite_couch_api, get,
%                               [[MissingPart, Db, Key, nil, []]]),
%     ?assertEqual({not_found, {[], [missing]}}, MissingNodeGet),

%     JsonDoc = {[{<<"_id">>,<<"a">>},
%                 {<<"_rev">>,
%                  <<"1-967a00dff5e02add41819138abb3284d">>}]},

%     % r=3 should fail
%     {r_quorum_not_met, {[{message, _M}, {good, G}, {bad, B}]}} =
%         showroom_doc:open_doc(Db, Key, nil, [{r, "3"}]),
%     ?assertEqual([JsonDoc,JsonDoc], G),
%     ?assertEqual([{not_found, missing}], B),

%     % r=2 should never fail (run it many times to make sure)
%     do_opens({Db, Key, nil, [{r, "2"}]}, 20),

%     ok.


% pcall(MapFun, Servers, Const) ->
%   Replies = lib_misc:pmap(MapFun, Servers, Const),
%   lists:partition(fun valid/1, Replies).


% valid({ok, _}) -> true;
% valid(ok) -> true;
% valid(_) -> false.


% do_opens(_,0) -> ok;
% do_opens({Db, DocId, Refs, Options} = Payload, Times) ->
%     {Status, _Doc} = showroom_doc:open_doc(Db, DocId, Refs, Options),
%     ?assertEqual(ok, Status),
%     do_opens(Payload, Times-1).
