-module(fabric_rpc).

-export([open_doc/3, open_doc/4, get_db_info/1]).

open_doc(DbName, DocId, Options) ->
    with_db(DbName, {couch_db, open_doc, [DocId, Options]}).

open_doc(DbName, DocId, Revs, Options) ->
    with_db(DbName, {couch_api, open_doc, [DocId, Revs, Options]}).

get_db_info(DbName) ->
    with_db(DbName, {couch_db, get_db_info, []}).

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
