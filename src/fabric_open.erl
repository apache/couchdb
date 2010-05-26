-module(fabric_open).

-export([open_doc/4]).
-export([open_doc_endpoint/4]).

-include("../../couch/src/couch_db.hrl").


% open_db(<<"S", ShardFileName/binary>> = Name, Options) ->
%     case couch_db:open(ShardFileName, Options) of
%     {ok, Db} ->
%         {ok, Db#db{name = Name}};
%     {not_found, no_db_file} ->
%         {not_found, no_db_file}
%     end;
%
% open_db(DbName, Options) ->
%     Part = case lists:keyfind(node(), 1, membership2:all_nodes_parts(false)) of
%     {_, P} -> P;
%     _ -> throw({node_has_no_parts, node()})
%     end,
%     ShardName = partitions:shard_name(Part, DbName),
%     open_db(<<"S", ShardName/binary>>, Options).


open_doc(DbName, DocId, _Revs, _Options) ->
    NPs = partitions:key_nodes_parts(DbName, DocId),
    ?debugFmt("~nNPs: ~p~n", [NPs]),
    {ok, #doc{}}.





% open_doc(Db, DocId, Revs, Options) ->
%     {R,N} = get_quorum_constants(r, Options),
%     case cluster_ops:key_lookup(DocId, {dynomite_couch_api, get,
%             [Db, DocId, Revs, Options]}, r, R, N) of
%     {ok, [Doc|_Rest]} ->
%         {ok, Doc};
%     {ok, {not_found, Reason}, _Reasons} ->
%         {not_found, Reason};
%     [{error, Error}, {good, Good}, {bad, Bad}] ->
%         showroom_quorum_utils:handle_error(Error, Good, Bad);
%     Other ->
%         ?LOG_DEBUG("~nopen_doc Other: ~p~n", [Other]),
%         throw(Other)
%     end.
