-module(fabric_rpc).

-export([open_doc/3, open_doc/4, get_db_info/1, update_docs/3,
    get_missing_revs/2]).

-include("../../couch/src/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

open_doc(DbName, DocId, Options) ->
    with_db(DbName, {couch_db, open_doc_int, [DocId, Options]}).

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
        
%% rpc endpoints
%%  call to with_db will supply your M:F with a #db{} and then remaining args

open_doc(DbName, DocId, Revs, Options) ->
    with_db(DbName, {couch_api, open_doc, [DocId, Revs, Options]}).

get_db_info(DbName) ->
    with_db(DbName, {couch_db, get_db_info, []}).

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
