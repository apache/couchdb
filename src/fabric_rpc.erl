-module(fabric_rpc).

-export([get_db_info/1, get_doc_count/1, get_update_seq/1]).
-export([open_doc/3, open_revs/4, get_missing_revs/2, update_docs/3]).

-include("fabric.hrl").

%% rpc endpoints
%%  call to with_db will supply your M:F with a #db{} and then remaining args

get_db_info(DbName) ->
    with_db(DbName, {couch_db, get_db_info, []}).

get_doc_count(DbName) ->
    rexi:reply(case couch_db:open(DbName) of
    {ok, Db} ->
        {ok, {Count, _DelCount}} = couch_btree:full_reduce(Db#db.id_tree),
        {ok, Count};
    Error ->
        Error
    end).

get_update_seq(DbName) ->
    rexi:reply(case couch_db:open(DbName) of
    {ok, #db{update_seq = Seq}} ->
        {ok, Seq};
    Error ->
        Error
    end).

open_doc(DbName, DocId, Options) ->
    with_db(DbName, {couch_db, open_doc_int, [DocId, Options]}).

open_revs(DbName, Id, Revs, Options) ->
    with_db(DbName, {couch_db, open_doc_revs, [Id, Revs, Options]}).

get_missing_revs(DbName, IdRevsList) ->
    % reimplement here so we get [] for Ids with no missing revs in response
    rexi:reply(case couch_db:open(DbName, []) of
    {ok, Db} ->
        Ids = [Id1 || {Id1, _Revs} <- IdRevsList],
        {ok, lists:zipwith(fun({Id, Revs}, FullDocInfoResult) ->
            case FullDocInfoResult of
            {ok, #full_doc_info{rev_tree=RevisionTree}} ->
                {Id, couch_key_tree:find_missing(RevisionTree, Revs)};
            not_found ->
                {Id, Revs}
            end
        end, IdRevsList, couch_btree:lookup(Db#db.id_tree, Ids))};
    Error ->
        Error
    end).

update_docs(DbName, Docs, Options) ->
    with_db(DbName, {couch_db, update_docs, [Docs, Options]}).

%%
%% internal
%%

with_db(DbName, {M,F,A}) ->
    case couch_db:open(DbName, []) of
    {ok, Db} ->
        rexi:reply(apply(M, F, [Db | A]));
    Error ->
        rexi:reply(Error)
    end.


%%
%% helper funs
%%
